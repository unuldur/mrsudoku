(ns csp.ac3
  (:use midje.sweet)
  (:require
    [csp.alldiff :as all]))

(defn choix-variable-min [doms] (first (apply min-key #(count %) doms)))

(defn test-solution [constraints sol]
  (every?
    (fn [constraint] (let [{:keys [check var1 var2]} constraint] (check (sol var1) (sol var2))))
    constraints))

(defn generate-and-test
  "la map des domaines `doms`, le vecteur des contraintes `constraints`, optionellement une solution partielle `sol`"
  [constraints doms sol]
  (if (empty? doms)
    (if (test-solution constraints sol)
      sol
      nil)
    ;;sol incomplete
    (let [x (choix-variable-min doms)]
      (loop [xdom (doms x)]
        (if (seq xdom)
          (let [xval (first xdom)]
            (if-let [sol' (generate-and-test constraints
                                             (dissoc doms x)
                                             (assoc sol x xval))]
              sol'
              (recur (rest xdom))))
          nil)))))


(defn check-constraint
  "verifie si pour 2 valeurs la contraint est respecter `xvar` represente le non de la valeur `xval`"
  [contrainte xvar xval yval]
  (if (= xvar :var1)
    ((:check contrainte) xval yval)
    ((:check contrainte) yval xval)))

(fact
  (let [const {:var1 :r4
               :var2 :m
               :check =}]
    (check-constraint const :var1 4 4))
  => true


  (let [const {:var1 :r4
               :var2 :m
               :check >}]
    (check-constraint const :var1 10 1))
  => true


  (let [const {:var1 :r4
               :var2 :m
               :check >}]
    (check-constraint const :var2 10 1))
  => false)



(defn support
  "retour la premiere valeur de `ydom` qui valide la `contraint` pour `xvar` `xval`"
  [xvar xval ydom contrainte]
  (reduce (fn [res yval]
            (if (check-constraint contrainte
                                  xvar
                                  xval
                                  yval)
              (reduced yval)
              res)) nil ydom))
(fact
  (let [const {:var1 :r4
               :var2 :m
               :check =}]
    (support :var2 4 #{1 2 3 4 5} const))
  => 4
  (let [const {:var1 :r4
               :var2 :m
               :check =}]
    (support :var2 4 #{1 2 3 } const))
  => nil)

;;revision des domaines

(declare check-cache)
(declare update-support)

(defn ovar
  "retour le nom de l'autre variable"
  [xvar]
  (if (= xvar :var1)
    :var2
    :var1))


(defn check-cache
  "verifie si le cahe est a jour et comporte la donner"
  [constraint supp doms const-ref xvar x xval]
  (if-let [yval (get (nth supp const-ref) [x xval])]
    (let [yvar (ovar xvar), y (get constraint yvar)]
      (if (contains? (get doms y) yval)
        [false, supp]
        [true, (update supp const-ref (fn [old] (dissoc old [x xval])))]))
    ;;rien dans le cache
    [true, supp]))

(defn update-support
  "met a jour le support en a sociant yval a [x xval] et reciproquement"
  [supp const-ref x xval y yval]
  (-> supp
      (update const-ref (fn [xsupp] (assoc xsupp [x xval] yval)))
      (update const-ref (fn [ysupp] (assoc ysupp [y yval] xval)))))

(defn revise
  "`constraints` : un vecteur de contraintes
    `doms` : domaines des variables
    `supps` : cache des supports, un vecteur de map  {[x val} yval}
            ex : [{[:v1 12] 34, [:v1 14] 84}, ]
    `contraint-ref` : le numero de la contrainte concernée
    `xvar` : la variable concernée
    la fonction doit retourner un quadruplet [changed, x, doms, supp]
    avec  : changed : vaut true si il y a eu une revision pour x
            x : le nom de la variable
            doms : les domaines
            supp: le cache des support"
  [constraints doms supps contraint-ref xvar]
  (let [contraint (nth constraints contraint-ref)
        x (get contraint xvar)
        y (get contraint (ovar xvar))]
    (loop [xdom (get doms x), doms doms,supp supps, changed false]
      (if (seq xdom)
        (let [xval (first xdom)
              [findnew, supp'] (check-cache contraint supp doms contraint-ref xvar x xval)]
          (if findnew
            ;;il faut trouver un nouveau support
            (if-let [yval (support xvar xval (get doms y) contraint)]
              (recur (rest xdom) doms (update-support supp' contraint-ref x xval y yval) changed)
              ;;pas de support pour x et xval
              (recur (rest xdom) (update doms x (fn [xdom] (disj xdom xval))) supp' true))
            ;;deja dans le cache, rien a faire pour xval
            (recur (rest xdom) doms supp' changed)))
        [changed, x, doms, supp]))))

(defn init-support
  "initialiser le cache"
  [constraint]
  (mapv (fn [_] {}) (range (count constraint))))


;;######### AC3 ##############
(declare init-todo)
(declare select-todo)
(declare update-todo)

(defn ac3
  "reduit les domaine en verifiant que chaque valeur de chaque variable verifi les contraintes  et suprimer des dommaine les valeurs incompatile"
  [constraints doms]
  (let [supp (init-support constraints)
        todo (init-todo constraints)]
    (loop [todo todo, supp supp, doms doms]
      (if (seq todo)
        (let [[const-ref, xvar, todo'](select-todo constraints doms todo)
              [changed, x, doms' supp'](revise constraints doms supp const-ref xvar)]
          (if changed
            (if (seq (get doms' x))
              (recur (update-todo constraints const-ref xvar x todo') supp' doms')
              ;;plus de valeur pour x
              nil)
            ;;pas de changement dans le domaine de x
            (recur todo' supp' doms')))
        ;; todou vide
        doms))))

(defn init-todo
  "initialse la liste des choses a faire"
  [constraints]
  (loop [const-ref 0, todo #{}]
    (if (< const-ref (count constraints))
      (recur (inc const-ref) (conj todo [const-ref :var1] [const-ref :var2]))
      todo)))

(defn select-todo [constraints doms todo]
  "recupere la prochaine tache"
  (let [[const-ref, xvar] (first todo)]
    [const-ref, xvar, (rest todo)]))

;;TODO **EXERCICE** Proposer une autre heuristique pour la selection d'un autre couple variable et comparer les performances pour le zebre
(defn select-todo-min
  "Choisi la vairable avec le domaine minimun"
  [constraints doms todo]
  ())

(defn update-todo
  "met a jour la liste de taches"
  [constraints prev-cref xvar x todo]
  (loop [const-ref 0, todo todo]
    (if (< const-ref (count constraints))
      (let [constraint (nth constraints const-ref)]
        (if (and (= const-ref prev-cref) (= (get constraint xvar) x))
          (let [yvar (ovar xvar)]
            (recur (inc const-ref) (conj todo [const-ref yvar])))
          ;;autre cas
          (recur (inc const-ref) (cond
                                   (= (:var1 constraint) x) (conj todo [const-ref :var2])
                                   (= (:var2 constraint) x) (conj todo [const-ref :var1])
                                   :else todo))))
      todo)))







(defn valider-choix
  "revois le nouveaux domaine pour avec certaine valeur deja fixé"
  [domaine choix contrainte]
  (let [domaine (reduce (fn [res [cle valeur]] ;; rajoute a domaine les choix
                          (assoc res cle (set (list valeur))))
                        domaine
                        choix)]
      (if-let [domaine' (ac3 contrainte domaine)] ;;calcule le domaine qui
        (reduce (fn [res [cle _]] ;;eleve au domaine les choix
                  (dissoc res cle))
              domaine'
              choix)
        nil)))

(fact
 (valider-choix  {:b #{ 1 9 3} :c #{ 1 2 3} } { :a 1} [])
  => {:b #{9 3} :c #{2 3}}

 (valider-choix  {:b #{ 1 9 3} :c #{ 1 2 3} } { :a 1} [
            {:var1 :c
             :var2 :b
             :check =}])
  => {:b #{3} :c #{3}}

 (valider-choix  {:b #{ 1 9 3} :c #{ 1 2 3} } { :a 1} [
            {:var1 :c
             :var2 :b
             :check >=}])

  => {:b #{3}, :c #{3}}
 (valider-choix  {:b #{ 1 9 3} :c #{ 1 2 3} } { :a 1} [
            {:var1 :c
             :var2 :b
             :check >}])
  => nil )

;; structure stack :
;; [doms choix stack-précédente]
;; init    : [ {:a #{ 1 2 3} :b #{ 1 2 3} :c #{ 1 2 3} } {} {} ]

;; etape 2 : [ {:b #{ 2 3} :c #{ 2 3} } {:a 1}
;;            [   {:a { 2 3} :b { 1 2 3} :c { 1 2 3} } {} {} ] ]

;; etape 3 : [ {:c #{ 3} } {:a 1 :b 2 }
;;             [ {:b #{ 3} :c #{ 2 3} } {:a 1}
;;               [   {:a #{ 2 3} :b #{ 1 2 3} :c #{ 1 2 3} } {} {} ] ] ]

;; etape 3 : [{:a 1 :b 2 :c 3}    <-- choix
;;             [ {:c #{} } {:a 1 :b 2}]
;;               [ {:b #{ 3} :c #{ 2 3} } {:a 1}
;;                 [   {:a #{ 2 3} :b #{ 1 2 3} :c #{ 1 2 3} } {} {} ] ] ]






(defn gen-aux
  "construit la stack et retour un couple [choix next-stack fonction de difference]"
  [constraint stack]
  (if (empty? stack)
    nil
    (let [[doms choix next-stack] stack
          [var variables] (first doms)]
      (if (empty? doms)
        [choix next-stack] ;;retour
        (if (empty? variables)
          (gen-aux constraint next-stack)
          (let [ choix-val (first variables)
                new-choix (assoc choix var choix-val)
                doms' (assoc doms var (set (rest variables)))
                new-next-stack [doms' choix next-stack]]    ;; calcule le next stack en y retirant ce que l'on est entreint de tester

            (if-let [new-doms (valider-choix (dissoc doms var ) new-choix  constraint)]
              (gen-aux constraint [new-doms new-choix new-next-stack])
              (gen-aux constraint new-next-stack))))))))


(gen-aux [] [ {:a #{ 1 2 3} :b #{ 1 2 3} :c #{ 1 2 3} } {} {} ])


(gen-aux [] [{:b #{}} {:a 1, :c 3} [{:c #{2}, :b #{3 2}} {:a 1} [{:a #{3 2}, :b #{1 3 2}, :c #{1 3 2}} {} {}]]])


(defn lazy-gen
  "genere la sequence des solutions possibles"
  ([constraint doms] (lazy-gen constraint doms [doms {} {}]))
  ([constraint doms stack]
   (if-let [[sol stack'] (gen-aux constraint stack)]
     (lazy-seq
       (cons
         sol
         (lazy-gen constraint doms stack'))))))
(fact
 (count (lazy-gen  [] {:a #{ 1 2 3} :b #{ 1 2 3} :c #{ 1 2 3} }))
  => (*  2 3 )

 (count (lazy-gen  [] {:a #{ 1 2 3 4 } :b #{ 1 2 3 4 } :c #{ 1 2 3 4 } }))
  => (* 2 4 )

 (count (lazy-gen  [] {:a #{ 1 2 3 4 5} :b #{ 1 2 3 4 5} :c #{ 1 2 3 4 5} }))
  => (* 2 5 ))


