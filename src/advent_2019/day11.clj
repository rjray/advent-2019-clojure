(ns advent-2019.day11
  (:require [clojure.math.numeric-tower :refer [abs]]
            [clojure.string :as str]))

;;; https://adventofcode.com/2019/day/11

;; Read the given file as a stream of comma-separated data values. Return a
;; vector of the numerical values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Long/parseLong %))
       (vec)))

;; Get the flags for the given mode. Flags should be 0, 1 or 2 for each digit
;; in mode. Return a list of the flags in left-to-right order.
(defn- get-flags [mode]
  (loop [mode mode, flags ()]
    (let [flag (rem mode 10)
          rest (quot mode 10)]
      (cond
        (and (zero? flag)
             (zero? rest)) (reverse flags)
        :else              (recur rest (cons flag flags))))))

;; Take an opcode and see if it has flags in the >99 range. If so, set them as
;; appropriate. Return a vector of the lower two digits and flags.
(defn- split-opcode [op]
  (let [code (rem op 100)
        mode (quot op 100)]
    (cons code (get-flags mode))))

;; Get the value for the specified memory location, depending on the mode given
;; in the mode parameter.
(defn- get-value [memory base loc mode]
  (case mode 1 loc, 2 (get memory (+ base loc) 0), (get memory loc 0)))

;; Prepare the loc value for the call to get-value, as it may be an indirect
;; read depending on mode.
(defn- read-value [memory base loc mode]
  (get-value memory base (get memory loc 0) mode))

;; Make a completely new VM state with the given opcode/memory stream.
;; Everything else defaults to 0/empty/false.
(defn- make-state [memory]
  {:memory (reduce (fn [m p] (apply assoc (cons m p)))
                   {} (map-indexed (fn [x y] (list x y)) memory)),
   :pc 0, :base 0,
   :input (), :output (),
   :blocked false, :halted false})

;; Update an existing state with the given subset of keys, returning a new
;; state.
(defn- update-state [state & params]
  (apply assoc (cons state params)))

(defn- update-memory [memory loc val]
  (assoc memory loc val))

(defn- debug-op [name state len]
  (let [pc  (:pc state)
        mem (:memory state)]
    (loop [cnt 0, ops ()]
      (cond
        (= cnt len) (println (str "PC=" pc " " name " " (reverse ops)))
        :else       (recur (inc cnt) (cons (get mem (+ pc cnt)) ops))))))

;; Add operation.
(defn- apply-add [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1 m2 m3] code
        op1          (read-value memory base (inc pc) m1)
        op2          (read-value memory base (+ pc 2) m2)
        op3          (get memory (+ pc 3) 0)
        dst          (case m3 2 (+ base op3), op3)

        sum          (+ op1 op2)]
    (update-state state :memory (update-memory memory dst sum) :pc (+ pc 4))))

;; Multiply operation.
(defn- apply-mult [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1 m2 m3] code
        op1          (read-value memory base (inc pc) m1)
        op2          (read-value memory base (+ pc 2) m2)
        op3          (get memory (+ pc 3) 0)
        dst          (case m3 2 (+ base op3), op3)

        prd          (* op1 op2)]
    (update-state state :memory (update-memory memory dst prd) :pc (+ pc 4))))

;; Input operation. Takes the next element off the input list and stores it in
;; the location indicated by the one argument. If there is no new element on the
;; input list, returns the current state with :blocked set to true.
(defn- apply-input [state code]
  (let [{memory :memory
         pc :pc
         base :base
         input :input} state
        [_ m1]         code
        op1            (get memory (inc pc) 0)
        pos            (case m1 2 (+ base op1), op1)]
    (if (zero? (count input))
      (update-state state :blocked true)
      (update-state state :memory (update-memory memory pos (first input))
                    :input (rest input) :pc (+ pc 2)))))

;; Output operation. Takes the value in the location indicated by the one arg
;; and puts it at the tail of the output list.
(defn- apply-output [state code]
  (let [{memory :memory
         pc :pc
         base :base
         output :output} state
        [_ m1]           code
        out              (read-value memory base (inc pc) m1)]
    (update-state state :output (concat output (list out)) :pc (+ pc 2))))

;; Jump-if-true operation. If the value indicated by the first argument is
;; non-zero, then move the PC to the value indicated by the second argument.
;; If it was zero, then just advance the PC by 3.
(defn- apply-jump-true [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1 m2]    code
        op1          (read-value memory base (inc pc) m1)
        op2          (read-value memory base (+ pc 2) m2)
        newpc        (if-not (zero? op1) op2 (+ pc 3))]
    (update-state state :pc newpc)))

;; Jump-if-false operation. As above, but jumps on false rather than true.
(defn- apply-jump-false [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1 m2]    code
        op1          (read-value memory base (inc pc) m1)
        op2          (read-value memory base (+ pc 2) m2)
        newpc        (if (zero? op1) op2 (+ pc 3))]
    (update-state state :pc newpc)))

;; Less-than operation. If the value indicated by argument 1 is less than the
;; value indicated by argument 2, write a 1 to the destination specified by
;; argument 3. Otherwise write a 0.
(defn- apply-less-than [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1 m2 m3] code
        op1          (read-value memory base (inc pc) m1)
        op2          (read-value memory base (+ pc 2) m2)
        op3          (get memory (+ pc 3) 0)
        dst          (case m3 2 (+ base op3), op3)

        val          (if (< op1 op2) 1 0)]
    (update-state state :memory (update-memory memory dst val) :pc (+ pc 4))))

;; Equals operation. As above, but with an equality comparison rather than a
;; less-than.
(defn- apply-equals [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1 m2 m3] code
        op1          (read-value memory base (inc pc) m1)
        op2          (read-value memory base (+ pc 2) m2)
        op3          (get memory (+ pc 3) 0)
        dst          (case m3 2 (+ base op3), op3)

        val          (if (= op1 op2) 1 0)]
    (update-state state :memory (update-memory memory dst val) :pc (+ pc 4))))

;; Adjust the base value for relative mode.
(defn- apply-adjust-base [state code]
  (let [{memory :memory
         pc :pc
         base :base} state
        [_ m1]       code
        op1          (read-value memory base (inc pc) m1)]
    (update-state state :pc (+ pc 2) :base (+ base op1))))

;; Execute the VM over the opcodes/data, input, etc. encapsulated within the
;; given state. When opcode 99 is reached, returns the current value of the
;; output list.
(defn- execute [state]
  (loop [state state]
    (let [{memory :memory
           pc :pc} state
          code     (split-opcode (memory pc))
          op       (first code)]
      (cond
        (:blocked state) state
        (= op 99)        (update-state state :halted true)
        (= op 1)         (recur (apply-add state code))
        (= op 2)         (recur (apply-mult state code))
        (= op 3)         (recur (apply-input state code))
        (= op 4)         (recur (apply-output state code))
        (= op 5)         (recur (apply-jump-true state code))
        (= op 6)         (recur (apply-jump-false state code))
        (= op 7)         (recur (apply-less-than state code))
        (= op 8)         (recur (apply-equals state code))
        (= op 9)         (recur (apply-adjust-base state code))
        :else            (throw (AssertionError.
                                 (str "Uknown opcode: " op
                                      "\nPC: " pc
                                      "\nOriginal mem val: " (memory pc)
                                      "\nState: " (dissoc state :memory))))))))

;; Add an input value to the given state. Adds it at the end of the input list.
(defn- add-input [state val]
  (assoc (update-in state [:input] concat (list val)) :blocked false))

;; Get the output value at the head of the output list. Throws an exception if
;; there is no output available.
(defn- get-output [state]
  (if (empty? (:output state)) nil (first (:output state))))

;; Drop the head of the output list (presumably after it has just been read).
(defn- drop-output [state]
  (update-in state [:output] rest))

;;; Start of the robot

;; Create a state to represent the robot at the very start
(defn- create-robot-state []
  {:direction "up", :visited (), :panels {}, :location (list 0 0)})

;; Table for "turning" the robot from it's current position based on the
;; specified direction.
(def ^:private turn-map
  {"up"    ["left" "right"],
   "right" ["up" "down"],
   "down"  ["right" "left"],
   "left"  ["down" "up"]})

(def ^:private move-map
  {"up"    (list 0 1),
   "right" (list 1 0),
   "down"  (list 0 -1),
   "left"  (list -1 0)})

(defn- turn-robot [r turn]
  (update-state r :direction (get (turn-map (:direction r)) turn)))

(defn- move-robot [robot]
  (let [dir (:direction robot)
        cur (:location robot)
        new (map + cur (move-map dir))]
    (update-state robot :location new)))

(defn- update-robot [r paint turn]
  (let [{location :location,
         visited :visited,
         panels :panels} r
        r' (move-robot (turn-robot r turn))]
    (update-state r'
                  :visited (cons location visited)
                  :panels (assoc panels location paint))))

(defn- get-panel [robot]
  (let [{location :location, panels :panels} robot]
    (get panels location 0)))

(defn- run-machine [machine input]
  (if (:mock machine)
    (if (empty? (:mock machine))
      (assoc machine :halted true)
      (assoc machine
             :output (first (:mock machine))
             :mock (rest (:mock machine))))
    (execute (add-input machine input))))

(defn- create-mock-machine [m input]
  (assoc m :mock input))

(def mock-input (quote ((1 0) (0 0) (1 0) (1 0) (0 1) (1 0) (1 0))))

(defn- run-robot [machine robot]
  (loop [robot robot, machine machine]
    (let [m'           (run-machine machine (get-panel robot))
          [paint turn] (:output m')
          m'           (update-state m' :output ())
          r'           (update-robot robot paint turn)]
      (cond
        (:halted m') robot
        :else        (recur r' m')))))

(defn- adjust-points [panels x y]
  (let [offset (list x y)]
    (reduce (fn [m p]
              (assoc m (map + p offset) (panels p)))
            {} (keys panels))))

;; Display the "raster lines" of the image.
(defn- display [lines]
  (println (str/join "\n" (map #(apply str %) lines))))

(defn- print-code [panels]
  (let [points (keys panels)
        off-x  (abs (apply min (map first points)))
        off-y  (abs (apply min (map last points)))
        panels (adjust-points panels off-x off-y)
        points (keys panels)
        max-x  (apply max (map first points))
        max-y  (apply max (map last points))
        grid   (vec (repeat (inc max-y) (vec (repeat (inc max-x) " "))))
        paint  (map first (get (group-by last panels) 1))]
    (->> (reduce (fn [g p]
                   (assoc-in g (vec (reverse p)) "*"))
                 grid paint)
         (reverse)
         (display))))

;;; Problem 1
(defn p01 [file]
  (as-> file $
    (read-opcodes $)
    (make-state $)
    ;;(create-mock-machine $ mock-input)
    (run-robot $ (create-robot-state))
    (count (keys (:panels $)))))

;;; Problem 2
(defn p02 [file]
  (as-> file $
    (read-opcodes $)
    (make-state $)
    ;;(create-mock-machine $ mock-input)
    (run-robot $ (update-state (create-robot-state) :panels {(list 0 0) 1}))
    (print-code (:panels $))))
