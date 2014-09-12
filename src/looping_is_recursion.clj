(ns looping-is-recursion)

(defn power [n k]
  (let [helper (fn [acc k]
                 (if (zero? k)
                   acc
                   (recur (* n acc) (dec k))))]
    (helper 1 k)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (== (count a-seq) 1)
      (first a-seq)
      (recur (rest a-seq)))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (if (empty? seq1)
                   true
                   (and (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2)))))]
    (if (not (== (count seq1) (count seq2)))
      false
      (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (let [helper (fn [pred a-seq index]
                  (if (empty? a-seq)
                    nil
                    (if (pred (first a-seq))
                      index
                      (recur pred (rest a-seq) (inc index)))))]
    (helper pred a-seq 0)))

(defn avg [a-seq]
  (let [sum (fn [a-seq s]
              (if (empty? a-seq)
                s
                (recur (rest a-seq) (+ s (first a-seq)))))]
    (/ (sum a-seq 0) (count a-seq))))

(defn parity-add [a-set elem]
  (if (contains? a-set elem)
    (clojure.set/difference a-set #{elem})
    (conj a-set elem)))

(defn parity [a-seq]
  (let [helper (fn [a-seq result]
                 (if (empty? a-seq)
                   result
                   (recur (rest a-seq) (parity-add result (first a-seq)))))]
    (helper a-seq #{})))

(defn fast-fibo [n]
  (cond (== n 0) 0
        (== n 1) 1
        :else    (let [fibo-helper (fn [fibs k n]
                                     (if (== k n)
                                       fibs
                                       (recur (cons (+ (first fibs) (second fibs)) fibs) (inc k) n)))]
                   (first (fibo-helper '(1 1) 2 n)))))

(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [helper (fn [a-seq result elems]
                   (let [f (first a-seq)]
                     (if (or (empty? a-seq) (contains? elems f))
                       result
                       (recur (rest a-seq) (conj result (first a-seq)) (conj elems f)))))]
      (if (empty? a-seq)
        a-seq
        (let [f (first a-seq)]
          (helper (vec (rest a-seq)) [(first a-seq)] #{f}))))))

