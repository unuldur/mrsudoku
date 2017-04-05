(ns mrsudoku.reader
  (:require [mrsudoku.grid :as g]
            [mrsudoku.engine :as e]))

(def ^:private empty-grid (var-get #'g/empty-grid))

(defn update-line
      [grid row line]
  (loop [car (clojure.string/split line #"") col 1 grid grid]
    (if (seq car)
      (recur (rest car) (inc col) (g/change-cell grid col row (if (= "." (first car))
                                                                (g/mk-cell)
                                                                (g/mk-cell (read-string (first car))))))
      grid)))

(defn read-sudoku
  "lit un sudoku dans un fichier retourne nil si ce fichier comptient un faux sudoku"
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (loop [lines (line-seq rdr) row 1 grid empty-grid]
      (if (seq lines)
        (recur (rest lines) (inc row) (update-line grid row (first lines)))
        (let [conflic (e/grid-conflicts grid)]
          (if (< 0 (count conflic))
            nil
            grid))))))