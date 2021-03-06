(ns advent-2019.day09)

;;; https://adventofcode.com/2019/day/9

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
    (update-state state :memory (assoc memory dst sum) :pc (+ pc 4))))

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
    (update-state state :memory (assoc memory dst prd) :pc (+ pc 4))))

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
        pos            (case m1
                         1 op1
                         2 (+ base op1)
                         (get memory op1 0))]
    (if (zero? (count input))
      (update-state state :blocked true)
      (update-state state :memory (assoc memory pos (first input))
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
    (update-state state :memory (assoc memory dst val) :pc (+ pc 4))))

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
    (update-state state :memory (assoc memory dst val) :pc (+ pc 4))))

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
                                      "\nState: " state)))))))

;; Add an input value to the given state. Adds it at the end of the input list.
(defn- add-input [state val]
  (update-in state [:input] concat (list val)))

;; Get the output value at the head of the output list. Throws an exception if
;; there is no output available.
(defn- get-output [state]
  (if (empty? (:output state))
    (throw (AssertionError. "fetch on empty output stream"))
    (first (:output state))))

;; Drop the head of the output list (presumably after it has just been read).
(defn- drop-output [state]
  (update-in state [:output] rest))

;;; Problem 1
(defn p01 [file]
  (as-> file $
    (read-opcodes $)
    (make-state $)
    (add-input $ 1)
    (execute $)
    (:output $)))

;;; Problem 2
(defn p02 [file]
  (as-> file $
    (read-opcodes $)
    (make-state $)
    (add-input $ 2)
    (execute $)
    (:output $)))
