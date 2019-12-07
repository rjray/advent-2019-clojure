(ns advent-2019.day07
  (:require [clojure.math.combinatorics :as comb]))

;;; https://adventofcode.com/2019/day/7

;; Read the given file as a stream of comma-separated data values. Return a
;; vector of the integer values.
(defn- read-opcodes [file]
  (->> file
       (slurp)
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))
       (vec)))

;; Take an opcode and see if it has flags in the >99 range. If so, set them as
;; appropriate. Return a vector of the lower two digits and flags.
(defn- split-opcode [op]
  (let [code (rem op 100)
        mode (quot op 100)
        p1im (not (zero? (bit-and mode 2r001)))
        p2im (not (zero? (bit-and mode 2r010)))
        p3im (not (zero? (bit-and mode 2r100)))]
    [code p1im p2im p3im]))

;; Make a new VM state with the given opcode/data stream, PC, input and output
;; lists.
(defn- make-state [ops input output pc blocked halted]
  {:op ops, :pc pc,
   :input input, :output output,
   :blocked blocked, :halted halted})

;; Add operation (modified from day 2).
(defn- apply-add [state code]
  (let [{ops :op
         pc  :pc
         input :input
         output :output}   state
        [_ p1im p2im]      code
        op1                (ops (inc pc))
        op2                (ops (+ pc 2))
        dst                (ops (+ pc 3))

        sum                (+ (if p1im op1 (ops op1))
                              (if p2im op2 (ops op2)))]
    (make-state (assoc ops dst sum) input output (+ pc 4) false false)))

;; Multiply operation (modfied from day 2).
(defn- apply-mult [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output}   state
        [_ p1im p2im]      code
        op1                (ops (inc pc))
        op2                (ops (+ pc 2))
        dst                (ops (+ pc 3))

        prd                (* (if p1im op1 (ops op1))
                              (if p2im op2 (ops op2)))]
    (make-state (assoc ops dst prd) input output (+ pc 4) false false)))

;; Input operation. Takes the next element off the input list and stores it in
;; the location indicated by the one argument. If there is no new element on the
;; input list, returns the current state with :blocked set to true.
(defn- apply-input [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        pos              (ops (inc pc))]
    (if (zero? (count (:input state)))
      (make-state ops input output pc true false)
      (make-state (assoc ops pos (first input)) (rest input) output (+ pc 2)
                  false false))))

;; Output operation. Takes the value in the location indicated by the one arg
;; and puts it at the tail of the output list.
(defn- apply-output [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        [_ p1im]         code
        op1              (ops (inc pc))
        out              (if p1im op1 (ops op1))]
    (make-state ops input (concat output (list out)) (+ pc 2) false false)))

;; Jump-if-true operation. If the value indicated by the first argument is
;; non-zero, then move the PC to the value indicated by the second argument.
;; If it was zero, then just advance the PC 3.
(defn- apply-jump-true [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        [_ p1im p2im]    code
        op1              (ops (inc pc))
        op2              (ops (+ pc 2))
        newpc            (if (not (zero? (if p1im op1 (ops op1))))
                           (if p2im op2 (ops op2)) (+ pc 3))]
    (make-state ops input output newpc false false)))

;; Jump-if-false operation. As above, but jumps on false rather than true.
(defn- apply-jump-false [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        [_ p1im p2im]    code
        op1              (ops (inc pc))
        op2              (ops (+ pc 2))
        newpc            (if (zero? (if p1im op1 (ops op1)))
                           (if p2im op2 (ops op2)) (+ pc 3))]
    (make-state ops input output newpc false false)))

;; Less-than operation. If the value indicated by argument 1 is less than the
;; value indicated by argument 2, write a 1 to the destination specified by
;; argument 3. Otherwise write a 0.
(defn- apply-less-than [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        [_ p1im p2im] code
        op1                (ops (inc pc))
        op2                (ops (+ pc 2))
        dst                (ops (+ pc 3))
        val                (if (< (if p1im op1 (ops op1))
                                  (if p2im op2 (ops op2)))
                             1 0)]
    (make-state (assoc ops dst val) input output (+ pc 4) false false)))

;; Equals operation. As above, but with an equality comparison rather than a
;; less-than.
(defn- apply-equals [state code]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        [_ p1im p2im] code
        op1                (ops (inc pc))
        op2                (ops (+ pc 2))
        dst                (ops (+ pc 3))
        val                (if (= (if p1im op1 (ops op1))
                                  (if p2im op2 (ops op2)))
                             1 0)]
    (make-state (assoc ops dst val) input output (+ pc 4) false false)))

;; Execute the VM over the opcodes/data, input, etc. encapsulated within the
;; given state. When opcode 99 is reached, returns the current value of the
;; output list.
(defn- execute [state]
  (loop [state state]
    (let [ops  (:op state)
          pc   (:pc state)
          code (split-opcode (ops pc))
          op   (code 0)]
      (cond
        (:blocked state) state
        (= op 99)        (assoc state :halted true)
        (= op 1)         (recur (apply-add state code))
        (= op 2)         (recur (apply-mult state code))
        (= op 3)         (recur (apply-input state code))
        (= op 4)         (recur (apply-output state code))
        (= op 5)         (recur (apply-jump-true state code))
        (= op 6)         (recur (apply-jump-false state code))
        (= op 7)         (recur (apply-less-than state code))
        (= op 8)         (recur (apply-equals state code))
        :else            (throw (AssertionError.
                                 (str "Uknown opcode: " op)))))))

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

;; Cycle the vector of amps by updating the amp at idx with new-state (while
;; dropping the top-most output value) and adding said output value to the
;; input of the next amp in the vector (with wrap-around).
(defn- cycle-amps [amps new-state idx idx']
  (assoc amps
         idx (assoc (drop-output new-state) :blocked false)
         idx' (add-input (amps idx') (get-output new-state))))

;; For problem 1, run the given permutation on the "program" given in ops, in
;; which each amp runs just once and feeds its output to the next one in line.
;; When the last has run, its output is the value for problem 1.
(defn- run-permutation-normal [ops perm]
  (loop [[p & ps] perm, input (list 0)]
    (cond
      (nil? p) (cons (first input) perm)
      :else    (let [state  (make-state ops (list p (first input))
                                        () 0 false false)
                     output (:output (execute state))]
                 (recur ps output)))))

;; For problem 2, run the given permutations on the "program", in which the
;; amps keep feeding into each other until the last amp has halted. At that
;; point, it's output value is the value for problem 2.
(defn- run-permutation-with-feedback [ops perm]
  (let [amps (vec (map #(make-state ops (list %) () 0 false false) perm))
        amps (update-in amps [0] add-input 0)]
    (loop [amps amps, idx 0]
      (let [new-state (execute (amps idx))]
        (cond
          (:halted new-state)  (if (= (inc idx) (count amps))
                                 (cons (get-output new-state) perm)
                                 (let [idx' (mod (inc idx) 5)]
                                   (recur (cycle-amps amps new-state idx idx')
                                          idx')))
          (:blocked new-state) (let [idx' (mod (inc idx) 5)]
                                 (recur (cycle-amps amps new-state idx idx')
                                        idx')))))))

;; Run all the permutations for the "program" given in ops, based on the vector
;; of settings. When done, return the largest result value from all the
;; permutations. Use the given function to choose how the amps are run.
(defn- run-all-permutations [fn settings ops]
  (let [perms (comb/permutations settings)]
    (loop [[p & ps] perms, results ()]
      (cond
        (nil? p) (first (sort #(compare (first %2) (first %1)) results))
        :else    (recur ps (cons (fn ops p) results))))))

;;; Problem 1
(defn p01 [file]
  (->> file
       (read-opcodes)
       (run-all-permutations run-permutation-normal (vec (range 5)))))

;;; Problem 2
(defn p02 [file]
  (->> file
       (read-opcodes)
       (run-all-permutations run-permutation-with-feedback (vec (range 5 10)))))
