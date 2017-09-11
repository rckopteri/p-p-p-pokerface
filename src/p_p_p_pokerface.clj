(ns p-p-p-pokerface)

(defn rank-string->rank-numeric [fst]
  (cond
    (= fst "T") 10
    (= fst "J") 11
    (= fst "Q") 12
    (= fst "K") 13
    (= fst "A") 14
    :else ""))

(defn rank [card]
  (let [[fst _] card]
    (if(Character/isDigit fst)
      (Integer/valueOf (str fst))
      (rank-string->rank-numeric (str fst)))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn many-of-kind? [how-many-kind hand]
  (let [rank-values (map rank hand)
        result (frequencies rank-values)
        max-second-key (apply max (vals result))] ;; Take only values, not keys - vals
    (if(= max-second-key how-many-kind) true false)))

(defn pair? [hand]
  (let [rank-values (map rank hand)
        result (frequencies rank-values)
        second-key-seq (sort (vals result))] ;; Take only values, not keys - vals -> (1 1 1 2)
    (if(= second-key-seq (seq [1 1 1 2])) true false)))

(defn three-of-a-kind? [hand]
  (many-of-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (many-of-kind? 4 hand))

(defn flush? [hand]
  (let [suit-values  (map suit hand)]
    (apply = suit-values)))

(defn full-house? [hand]
  (let [rank-values (map rank hand)
   similar-values (vals (frequencies rank-values))]
    (or
      (= (seq similar-values) (seq [2 3]))
      (= (seq similar-values) (seq [3 2]))
    )))

(defn two-pairs? [hand]
  (let [rank-values (map rank hand)
        similar-values (sort (vals (frequencies rank-values)))]
    (println "similar-values" similar-values)
      (= (seq similar-values) (seq [1 2 2]))))

(defn straight? [hand]
  (let [rank-values  (map rank hand)
        sorted-rank-values-low-ace (sort (map (fn [x] (if(= x 14) 1 x)) rank-values))
        sorted-rank-values (sort rank-values)
        min-sorted-value-low-ace (apply min sorted-rank-values-low-ace)
        min-sorted-value (apply min sorted-rank-values)
        ]
    (if(or
      (= (seq sorted-rank-values-low-ace) (seq (range min-sorted-value-low-ace (+ min-sorted-value-low-ace 5 ))))
      (= (seq sorted-rank-values) (seq (range min-sorted-value (+ min-sorted-value 5 ))))
     ) true false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
