(ns looping-is-recursion)

(defn power [base exp]
  (let [iterator (fn [n k]
                   (cond
                    (= 0 k) 1
                    (= 1 k) n
                    :else (recur (* n base) (dec k))))]
    (iterator base exp)))

(defn last-element [a-seq]
  (if (or
       (= (count a-seq) 1)
       (empty? a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (not= (count seq1) (count seq2)) false
   (and (empty? seq1) (empty? seq2)) true
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
    (loop [seq a-seq
           i 0]
           (cond
            (empty? seq) nil
            (pred (first seq)) i
            :else (recur (rest seq) (inc i)))))

(defn avg [a-seq]
  (let [count-of-nums (count a-seq)]
    (loop [seq a-seq
           sum 0]
       (if (empty? seq)
         (/ sum count-of-nums)
         (recur (rest seq) (+ sum (first seq)))))))

(defn parity [a-seq]
    (loop [seq a-seq
           each {}]
      (cond
       (empty? seq) (set (filter (fn [x] (> (mod (each x) 2) 0)) (keys each)))
       (contains? each (first seq)) (recur (rest seq) (assoc each (first seq) (inc (each (first seq)))))
       :else (recur (rest seq) (assoc each (first seq) 1)))))

(defn fast-fibo [n]
  (loop [i 0
         fn-1 0
         fn-2 0]
    (cond
     (> i n) fn-1
     (= i 0) (recur (inc i) 0 0)
     (= i 1) (recur (inc i) 1 0)
     :else (recur (inc i) (+ fn-1 fn-2) fn-1))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         each #{}
         final []]
        (cond
         (empty? seq) final
         (contains? each (first seq)) final
         :else (recur (rest seq) (conj each (first seq)) (conj final (first seq))))))

