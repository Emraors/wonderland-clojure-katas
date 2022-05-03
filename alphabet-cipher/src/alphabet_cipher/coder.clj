(ns alphabet-cipher.coder)

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

(def substitution-chart
  (->> alphabet
       (iterate  (fn  [[x & xs]] (concat xs (list x))))
       (take 26)))

(defn encode [keyword message]
  (apply str (map (fn [x y]
                    (let [i (.indexOf alphabet y)
                          j (.indexOf alphabet x)]
                      (nth (nth substitution-chart i) j)))
                  (flatten (repeat (seq keyword))) message)))

(defn decode [keyword message]
  (apply str (map (fn [x y]
                    (let [i (.indexOf alphabet x)]
                      (nth alphabet (.indexOf (nth substitution-chart i) y))))
                  (flatten (repeat (seq keyword))) message)))

(defn vec-multiplier [word times]
  (map (fn [x y] x) (flatten (repeat word)) (range times)))

(defn decipher [cipher message]
  (apply str (loop [[x & tail :as xs] (decode message cipher) result []]
                 (if ( = (vec-multiplier (conj result x) (count tail)) tail)
                   (conj result x)
                   (recur tail (conj result x))))))

