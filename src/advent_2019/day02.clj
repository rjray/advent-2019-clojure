(ns advent-2019.day02)

;;; https://adventofcode.com/2019/day/2

;;; The "add" opcode. Takes the current stream and pc, and implements the add
;;; operation as defined in the description.
(defn- apply-add [ops pc]
  (let [op1 (ops (inc pc))
        op2 (ops (+ pc 2))
        dst (ops (+ pc 3))
        sum (+ (ops op1) (ops op2))]
    (assoc ops dst sum)))

;;; The "mul" opcode. Takes the current stream and pc, and implements the mult
;;; operation as defined in the description.
(defn- apply-mul [ops pc]
  (let [op1 (ops (inc pc))
        op2 (ops (+ pc 2))
        dst (ops (+ pc 3))
        prd (* (ops op1) (ops op2))]
    (assoc ops dst prd)))

;;; Process the stream of opcodes given, halting when "99" is reached. Returns
;;; the final value of slot 0. Patches the stream before starting with the given
;;; noun/verb pair (noun goes in slot 1, verb in slot 2).
(defn- proc-ops [noun verb opcodes]
  (let [opcodes (assoc opcodes 1 noun 2 verb)]
    (loop [opcodes opcodes, pc 0]
      (cond
        (= (opcodes pc) 99) (opcodes 0)
        (= (opcodes pc) 1)  (recur (apply-add opcodes pc) (+ pc 4))
        (= (opcodes pc) 2)  (recur (apply-mul opcodes pc) (+ pc 4))
        :else               nil))))

;;; Fine the noun/verb pair that results in the given program returning a value
;;; equal to v. For now, brute-force it over the range that noun and verb can
;;; take (0-99).
(defn- find-noun-verb [v opcodes]
  (loop [noun 0, verb 0]
    (let [result (proc-ops noun verb opcodes)]
      (cond
        (= result v) (+ (* 100 noun) verb)
        (< verb 99)  (recur noun (inc verb))
        (< noun 99)  (recur (inc noun) 0)
        :else        "No solution"))))

;;; Problem 1
;;; Run the opcodes in "file", after patching the stream to have 12 in position
;;; 1 and 2 in position 2. Return the value that remains in position 0 after
;;; the "program" halts.
(defn p01 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)
       (proc-ops 12 2)))

;;; Problem 2
;;; This time, try to figure out what pair of noun/verb (the patches that go
;;; into pos 1 and 2, respectively) will result in a final value equal to val.
(defn- p02 [file val]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (vec)
       (find-noun-verb val)))
