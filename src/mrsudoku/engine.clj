(ns mrsudoku.engine
  (:use midje.sweet)
  (:require [mrsudoku.grid :as g]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))

(defn values
  "Return the set of values of a vector or grid `cells`."
  [cells]
  ;;; à compléter
  (reduce (fn [res cell] (if-let [val (:value cell)]
                           (conj res val)
                           res)) #{} cells))

(fact
 (values (g/block sudoku-grid 1)) => #{5 3 6 9 8})

(fact
 (values (g/row sudoku-grid 1)) => #{5 3 7})

(fact
 (values (g/col sudoku-grid 1)) => #{5 6 8 4 7})

(fact
 (values (g/block sudoku-grid 8)) => #{4 1 9 8})

(fact
 (values (g/row sudoku-grid 8)) => #{4 1 9 5})

(fact
 (values (g/col sudoku-grid 8)) => #{6 8 7})

(defn values-except
  "Return the set of values of a vector of cells, except the `except`-th."
  [cells except]
  {:pre [(<= 1 except (count cells))]}
  (let [except-value (:value (get cells (dec except)))]
    (disj (values cells) except-value)))

(fact
 (values-except (g/block sudoku-grid 1) 1) => #{3 9 6 8})

(fact
 (values-except (g/block sudoku-grid 1) 4) => #{3 9 5 8})

(defn mk-conflict [kind cx cy value]
  {:status :conflict
   :kind kind
   :value value})

(defn merge-conflict-kind
  [kind1 kind2]
  (cond
    (and (set? kind1) (set? kind2)) (clojure.set/union kind1 kind2)
    (set? kind1) (conj kind1 kind2)
    (set? kind2) (conj kind2 kind1)
    (= kind1 kind2) kind1
    :else (hash-set kind1 kind2)))

(fact
 (merge-conflict-kind :row :row) => :row)

(fact
 (merge-conflict-kind :row :block) => #{:row :block})

(fact
 (merge-conflict-kind :row #{:row :block}) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} :block) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} #{:block :col}) => #{:row :block :col})


(defn merge-conflict [conflict1 conflict2]
  (assoc conflict1 :kind (merge-conflict-kind (:kind conflict1) (:kind conflict2))))

(defn merge-conflicts [& conflicts]
  (apply (partial merge-with merge-conflict) conflicts)) 

(defn update-conflicts
  [conflict-kind cx cy value conflicts]
  (if-let [conflict (get conflicts [cx, cy])]
    (assoc conflicts [cx, cy] (mk-conflict (merge-conflict-kind conflict-kind (:kind conflict))
                                           cx cy value))
    (assoc conflicts [cx, cy] (mk-conflict conflict-kind cx cy value))))

(defn conflict-value [values except cell]
  (when-let [value (g/cell-value cell)]
    (when (and (not= (:status cell) :init)
               (contains? (values-except values except) value))
      value)))

(defn row-find-conflicts
  [row cx cy res]
  (let [actual-cell (get row (dec cx))]
    (reduce (fn [res cx2] (let [cell (get row (dec cx2))]
                            (if (and (= (:value actual-cell) (:value cell))
                                     (not (= cx cx2))
                                     (not (= :empty (:status actual-cell)))
                                     (not (= :empty (:status cell))))
                              (cond
                                (and (= :set (:status actual-cell)) (= :init (:status cell)))
                                (assoc res [cx cy] (mk-conflict :row cx2 cy (:value actual-cell)))
                                (and (= :init (:status actual-cell)) (= :set (:status cell)))
                                (assoc res [cx2 cy] (mk-conflict :row cx cy (:value actual-cell)))
                                :else
                                (assoc res [cx cy] (mk-conflict :row cx cy (:value actual-cell))
                                           [cx2 cy] (mk-conflict :row cx2 cy (:value actual-cell))))
                              res))) res (range 1 (inc (count row))))))

(defn row-conflicts
  "Returns a map of conflicts in a `row`."
  [row cy]
  (reduce (fn [res, cx] (row-find-conflicts row cx cy res)) {} (range 1 (inc (count row)))))


(fact
 (row-conflicts (mapv #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (row-conflicts (mapv #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :row, :value 1},
     [4 1] {:status :conflict, :kind :row, :value 1}})

(fact
 (row-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[6 4] {:status :conflict, :kind :row, :value 6}})

(defn rows-conflicts [grid]
  (reduce merge-conflicts {}
          (map (fn [r] (row-conflicts (g/row grid r) r)) (range 1 10))))


(defn col-find-conflicts
  [col cx cy res]
  (let [actual-cell (get col (dec cy))]
    (reduce (fn [res cy2] (let [cell (get col (dec cy2))]
                            (if (and (= (:value actual-cell) (:value cell))
                                     (not (= cy cy2))
                                     (not (= :empty (:status actual-cell)))
                                     (not (= :empty (:status cell))))
                              (cond
                                (and (= :set (:status actual-cell)) (= :init (:status cell)))
                                (assoc res [cx cy] (mk-conflict :col cx cy2 (:value actual-cell)))
                                (and (= :init (:status actual-cell)) (= :set (:status cell)))
                                (assoc res [cx cy2] (mk-conflict :col cx cy (:value actual-cell)))
                                :else
                                (assoc res [cx cy] (mk-conflict :col cx cy (:value actual-cell))
                                           [cx cy2] (mk-conflict :col cx cy2 (:value actual-cell))))
                              res))) res (range 1 (inc (count col))))))

(defn col-conflicts
  "Returns a map of conflicts in a `row`."
  [row cx]
  (reduce (fn [res, cy] (col-find-conflicts row cx cy res)) {} (range 1 (inc (count row)))))

(fact
  (col-conflicts (mapv #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
  (col-conflicts (mapv #(g/mk-cell :set %) [1 2 3 1]) 1)
  => {[1 1] {:status :conflict, :kind :col, :value 1},
      [1 4] {:status :conflict, :kind :col, :value 1}})

(fact
  (col-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
  => {[4 6] {:status :conflict, :kind :col, :value 6}})


(defn cols-conflicts
  [grid] (reduce merge-conflicts {}
                 (map (fn [c] (col-conflicts (g/col grid c) c)) (range 1 10))))


(defn block-find-conflicts
  [block pos positions res]
  (let [actual-cell (get block pos)]
    (reduce (fn [res pos2] (let [cell (get block pos2)]
                            (if (and (= (:value actual-cell) (:value cell))
                                     (not (= pos pos2))
                                     (not (= :empty (:status actual-cell)))
                                     (not (= :empty (:status cell))))
                              (let [[cx, cy] (get positions pos)
                                    [cx2, cy2] (get positions pos2)]
                                (cond
                                  (and (= :set (:status actual-cell)) (= :init (:status cell)))
                                  (assoc res [cx cy] (mk-conflict :block cx cy (:value actual-cell)))
                                  (and (= :init (:status actual-cell)) (= :set (:status cell)))
                                  (assoc res [cx2 cy2] (mk-conflict :block cx2 cy2 (:value actual-cell)))
                                  :else
                                  (assoc res [cx cy] (mk-conflict :block cx cy (:value actual-cell))
                                             [cx2 cy2] (mk-conflict :block cx2 cy2 (:value actual-cell)))))
                              res))) res (range 0 (inc (count block))))))

(defn block-conflicts
  [block b]
  (let [positions (g/reduce-block (fn [acc index cx cy cell] (conj acc [cx,cy])) [] block b)]
    (reduce (fn [res, pos] (block-find-conflicts block pos positions res)) {} (range 0 (inc (count block))))))

;;; Ecrire les 'fact' nécessaires...
(fact
  (block-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 6)
  => {[9 5] {:status :conflict, :kind :block, :value 6}})


(defn blocks-conflicts
  [grid]
  (reduce merge-conflicts {}
          (map (fn [b] (block-conflicts (g/block grid b) b)) (range 1 10))))

(defn grid-conflicts
  "Compute all conflicts in the Sudoku grid."
  [grid]
  (merge-conflicts (rows-conflicts grid)
                   (cols-conflicts grid)
                   (blocks-conflicts grid)))
