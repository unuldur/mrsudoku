(ns mrsudoku.solveur
  (:require
    [mrsudoku.grid :as g]
    [csp.alldiff :as all]
    [csp.ac3 :as ac3]))


(defn mkvar
  "creer un keyword x y"
  [x y]
  (keyword (str x y)))

(defn generate-doms-sudoku
  "Genere les domaines de contraintes du sudoku"
  [grid size]
  (let [vals (set (range 1 (inc size)))]
    (reduce (fn [res x] (reduce (fn [res y] (let [cell (g/cell grid x y)]
                                              (if-let [val (:value cell)]
                                                (assoc res (mkvar x y) #{val})
                                                (assoc res (mkvar x y) vals)))) res (range 1 (inc size))))
            {} (range 1 (inc size)))))

(defn alldif-col
  "creer les domaines alldif d'une colonne"
  [doms col]
  (reduce (fn [res row] (let [var (mkvar col row)]
                          (assoc res var (get doms var)))) {} (range 1 10)))

(defn alldif-row
  "creer les domaines alldif d'une ligne"
  [doms row]
  (reduce (fn [res col] (let [var (mkvar col row)]
                          (assoc res var (get doms var)))) {} (range 1 10)))

(defn alldiff-block
  "creer les domaines alldif d'un block"
  [doms pos]
  (reduce (fn [res [col row]] (let [var (mkvar col row)]
                          (assoc res var (get doms var)))) {} pos))

(defn mkvartype
  "Creer un keyword a partir du type et du numero"
  [type num]
  (keyword (str type num)))

(defn partition-doms
  "genere les partitions de la grille de sudoku "
  [doms]
  (clojure.set/union (reduce (fn [res col] (assoc res (mkvartype "col" col) (alldif-col doms col))) {} (range 1 10))
                     (reduce (fn [res row] (assoc res (mkvartype "row" row) (alldif-row doms row))) {} (range 1 10))
                     (reduce (fn [res block] (assoc res (mkvartype "block" block) (alldiff-block doms (g/reduce-block (fn [acc index cx cy cell] (conj acc [cx,cy])) [] {} block)))) {} (range 1 10))))



(defn fusion-case
  "retourne la paire variable valeur correspondante au domaine dans part"
  [x y part]
  (let [var (mkvar x y)
        num-block (+ (inc (int (/ (dec x) 3))) (* (int (/ (dec y) 3)) 3))]
    (clojure.set/intersection (get (get part (mkvartype "col" x)) var)
                              (get (get part (mkvartype "row" y)) var)
                              (get (get part (mkvartype "block" num-block)) var))))

(defn fusion-doms
  "Prend le domaine partitionner et retourne le nouveau domaine"
  [part]
  (reduce (fn [res x] (reduce (fn [res y] (assoc res (mkvar x y) (fusion-case x y part))) res (range 1 10))) {} (range 1 10)))


(defn new-doms
  "prend la partition des somaine set retourne le nombre de modification
  retourne nil si ce n'est pas possible"
  [part]
  (loop [vars (keys part) part' part]
    (if (seq vars)
      (if-let [doms' (all/alldiff (get part' (first vars)))]
        (recur (rest vars) (update part' (first vars) (fn [_] doms')))
        nil)
      part')))

(defn dif-all-constrainte
  "retourne toute les contraintes de difference de `v`"
  [v]
  (loop [v v res []]
    (if (seq v)
      (recur (rest v) (loop [v2 (rest v) res2 res]
                        (if (seq v2)
                          (recur (rest v2) (conj  res2 {:var1 (first v)
                                                        :var2 (first v2)
                                                        :check not=}))
                          res2)))
      res)))

(defn create-constraint-col
  [col]
  (dif-all-constrainte (reduce (fn [res i] (conj res (mkvar col i))) [] (range 1 10))))

(defn create-constrainte-row
  [row]
  (dif-all-constrainte (reduce (fn [res i] (conj res (mkvar i row))) [] (range 1 10))))

(defn create-constrainte-block
  [block]
  (let [pos-block (g/reduce-block (fn [acc index cx cy cell] (conj acc [cx,cy])) [] {} block)]
    (dif-all-constrainte (reduce (fn [res [x y]] (conj res (mkvar x y))) [] pos-block))))


(def sudoku-constraint (reduce (fn [res i] (clojure.set/union (create-constraint-col i)
                                                              (create-constrainte-row i)
                                                              (create-constrainte-block i)
                                                              res))
                               []
                               (range 1 10)))

(defn aux-reduce-doms
  [doms]
  (if-let [part' (new-doms (partition-doms doms))]
    (let [doms' (fusion-doms part')]
      doms')
    nil))

(defn test-sudoku
  "retourne les nouveaux domaines diminuer au maximun d'un sudoku, retourne nil si ce n'est pas un vrai sudoku"
  [doms]
  (ac3/lazy-gen sudoku-constraint doms aux-reduce-doms))


(defn update-grid
  "Met a jour la grille de sudoku a partir des contraintes"
  [grid doms]
  (reduce (fn [res x] (reduce (fn [res y] (let [var (mkvar x y)]
                                              (g/change-cell res x y (g/mk-cell (get doms var)))))
                              res
                              (range 1 10))) grid (range 1 10)))
