(ns advent-2019.day05)

;;; https://adventofcode.com/2019/day/5

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
(defn- make-state [ops input output pc]
  {:op ops, :pc pc, :input input, :output output})

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
    (make-state (assoc ops dst sum) input output (+ pc 4))))

;; Multiply operation (modfied from day 2).
(defn- apply-mul [state code]
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
    (make-state (assoc ops dst prd) input output (+ pc 4))))

;; Input operation. Takes the next element off the input list and stores it in
;; the location indicated by the one argument.
(defn- apply-inp [state]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        pos              (ops (inc pc))]
    (make-state (assoc ops pos (first input)) (rest input) output (+ pc 2))))

;; Output operation. Takes the value in the location indicated by the one arg
;; and puts it at the head of the output list.
(defn- apply-out [state]
  (let [{ops :op
         pc :pc
         input :input
         output :output} state
        pos              (ops (inc pc))]
    (make-state ops input (cons (ops pos) output) (+ pc 2))))

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
    (make-state ops input output newpc)))

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
    (make-state ops input output newpc)))

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
    (make-state (assoc ops dst val) input output (+ pc 4))))

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
    (make-state (assoc ops dst val) input output (+ pc 4))))

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
        (= op 99) (:output state)
        (= op 1)  (recur (apply-add state code))
        (= op 2)  (recur (apply-mul state code))
        (= op 3)  (recur (apply-inp state))
        (= op 4)  (recur (apply-out state))
        (= op 5)  (recur (apply-jump-true state code))
        (= op 6)  (recur (apply-jump-false state code))
        (= op 7)  (recur (apply-less-than state code))
        (= op 8)  (recur (apply-equals state code))
        :else     (str "Uknown opcode: " op)))))

;;; Problem 1
(defn p01 [file]
  (as-> file $
    (read-opcodes $)
    (make-state $ (list 1) () 0)
    (execute $)))

;;; Problem 2
(defn p02 [file]
  (as-> file $
    (read-opcodes $)
    (make-state $ (list 5) () 0)
    (execute $)))
