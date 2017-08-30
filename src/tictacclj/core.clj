(ns tictacclj.core
  (require [clojure.string :as str])
  (:gen-class))


; Helper Functions ------------------------------------------------------------

(defn d2->d1 [x y]
  (+ (* x 3) y))

(defn not-nil-unique? [coll]
  (and (every? (complement nil?) coll) (= (count (set coll)) 1)))

(defn process-input []
  (do (println "Enter ROW x COLUMN numbers [0..2] with a space inbetween: ")
      (map #(Integer. %) (str/split (read-line) #" "))))

(defn next-player [player]
  (if (= player :X) :O :X))


; Board -----------------------------------------------------------------------

(defn within-board? [x y]
  (and (>= x 0) (< x 3) (>= y 0) (< y 3)))

(defn get-position [board x y]
  (if (within-board? x y)
    (nth board (d2->d1 x y))))

(defn get-rows [board]
  (let [args (take 3 (range))
        row (fn [x y] (map #(get-position board x %) args))]
    (map #(row % 0) args)))

(defn get-columns [board]
  (let [args (take 3 (range))
        column (fn [x y] (map #(get-position board % y) args))]
    (map #(column 0 %) args)))

(defn get-diagonals [board]
  (letfn [(gp [x y] (get-position board x y))]
    [(map #(apply gp %) [[0 0] [1 1] [2 2]])
     (map #(apply gp %) [[2 0] [1 1] [0 2]])]))

(defn new-board []
  (vec (repeat (* 3 3) nil)))

(defn play [board coords player]
  (let [[x y] coords
        players #{:X :O}]
    (if (and (not (get-position board x y)) (player players))
      (assoc board (d2->d1 x y) player))))

(defn winner? [board]
  (let [rows (map not-nil-unique? (get-rows board))
        columns (map not-nil-unique? (get-columns board))
        diagonals (map not-nil-unique? (get-diagonals board))]
    (reduce #(or %1 %2) false (flatten [rows columns diagonals]))))
          
(defn moves-remaining? [board]
  (some nil? board))

(defn display-board [board]
  (doseq [row (get-rows board)]
    (println row)))


; Game ------------------------------------------------------------------------
    
(defn run-game [board player]
  (do (println)
      (display-board board)
      (if (winner? board)
        (println "You win!")
        (if (not (moves-remaining? board))
          (println "It's a draw.")
          (recur (play board (process-input) player)
                 (next-player player))))))


; Main ------------------------------------------------------------------------

(defn -main [& args]
  (run-game (new-board) :X))
