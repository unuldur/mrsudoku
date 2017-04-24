(ns mrsudoku.view
  (:require
    [mrsudoku.grid :as g]
    [seesaw.core :refer [frame label text config! grid-panel
                         horizontal-panel vertical-panel button separator invoke-later pack! show! alert]]
    [seesaw.border :refer [line-border]]
    [seesaw.chooser :refer [choose-file file-filter]]
    [mrsudoku.solveur :as s]
    [mrsudoku.reader :as r])
  (:import (javax.swing.SwingUtilities)))

(def default-color "white")
(def conflict-color "red")
(def set-color "blue")
(def solved-color "gray")

(defn mk-cell-view
  [cell cx cy ctrl]
  (case (:status cell)
    :init (label :text (str (:value cell))
                 :h-text-position :center
                 :v-text-position :center
                 :halign :center
                 :valign :center
                 :background default-color)
    :empty (let [cell-widget (text :columns 1
                                   :halign :center
                                   :id (keyword (str "cell-" cx "-" cy))
                                   :foreground set-color
                                   :background default-color)]
             (config! cell-widget
                      :listen [:document
                               ;; XXX: normally, we should not depend from the controller
                               ;;      but it's an emblamatic counter-example
                               ((resolve 'mrsudoku.control/cell-input-handler) ctrl cell-widget cx cy)])
             cell-widget)
    (throw (ex-info "Can only build widget for :init or :empty cells." {:cell cell,
                                                                        :cx cx,
                                                                        :cy cy}))))

(defn mk-block-view
  [block bref ctrl]
  (let [cell-widgets (g/reduce-block
                      (fn [widgets _ cx cy cell]
                        (conj widgets (mk-cell-view cell cx cy ctrl))) [] block bref)]
    (grid-panel :rows 3
                :columns 3
                :hgap 3
                :vgap 3
                :border (line-border :thickness 2 :color "black")
                :items cell-widgets
                :id (keyword (str "block-" bref)))))
;;#(print (s/update-grid grid (s/test-sudoku (s/generate-doms-sudoku grid 9))))
(defn mk-grid-view [grid ctrl]
  (let [block-widgets (for [i (range 1 10)]
                        (mk-block-view (g/block grid i) i ctrl))]
    (grid-panel :rows 3
                :columns 3
                :border 6
                :hgap 6
                :vgap 6
                :items (into [] block-widgets))))

(defn mk-frame-solve [grid ctrl]
  (let [grid-widget (mk-grid-view grid ctrl)
        main-frame (frame :title "MrSudoku"
                          :content (horizontal-panel
                                     :items [grid-widget
                                             [:fill-h 32]])
                          :minimum-size [380 :by 380])]
    (swap! ctrl #(assoc % :grid-widget grid-widget :main-frame main-frame))
    main-frame))

(defn show-solution
      [grid e]
      (if-let [doms (s/test-sudoku (s/generate-doms-sudoku grid 9))]
        (let [solve (s/update-grid grid doms)]
          (invoke-later
            (-> (mk-frame-solve solve (atom {:grid solve}))
                pack!
                show!)))
        (alert e "Il n'y a pas qu'une unique solution")))

(defn mk-main-frame [grid ctrl]
  (let [grid-widget (mk-grid-view grid ctrl)
        main-frame (frame :title "MrSudoku"
                          :content (horizontal-panel
                                    :items [grid-widget
                                            [:fill-h 32]
                                            (vertical-panel
                                             :items [:fill-v
                                                     (grid-panel
                                                       :columns 1
                                                       :vgap 20
                                                       :items [(button :text "Load"
                                                                      :listen [:action (fn [e] (if-let [file (choose-file
                                                                                                               :filters [["Sudoku file" ["txt" "ssdk"]]])]
                                                                                                 (if-let [grid (r/read-sudoku file)]
                                                                                                   (invoke-later
                                                                                                     (-> (mk-main-frame grid (atom {:grid grid}))
                                                                                                         pack!
                                                                                                         show!))
                                                                                                   (alert e "Ce sudoku est déjà faux !!!!"))))])
                                                              (button :text "Solve"
                                                                      :listen[:action (fn [e] (show-solution grid e))])
                                                              (button :text "Quit all"
                                                                      :listen [:action (fn [e] (System/exit 0))])])
                                                     :fill-v])
                                            [:fill-h 32]])
                          :minimum-size [540 :by 380])]
    (swap! ctrl #(assoc % :grid-widget grid-widget :main-frame main-frame))
    main-frame))



(defn update-cell-view!
  [cell cell-widget]
  (case (:status cell)
    :conflict (config! cell-widget :background conflict-color) 
    (:set :init :empty) (config! cell-widget :background default-color)
    :solved (config! cell-widget :backround solved-color :editable? false)
    (throw (ex-info "Cannot update cell widget." {:cell cell :cell-widget cell-widget}))))



