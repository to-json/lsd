(ns l-system-fun.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [l-system-fun.lsd :as l]))

(def koch-curve
  {:axiom "F"
   :rules {\F "F+F-F-F+F"}
   :rotation 90
   :operations {\F ["step"]
                \+ ["left"]
                \- ["right"]}
   :turtle (l/turtle 0 0 0 true)})

(def pythagoras-tree
  {:axiom "0"
   :rules {\0 "1[0]0"
           \1 "11"}
   :rotation 45
   :operations {\0 ["step"]
                \1 ["step"]
                \[ ["stack-push" "left"]
                \] ["stack-pop" "right"]}
   :turtle (l/turtle 0 0 0 true)})

(def pascals-triangle
  {:axiom "A"
   :rules {\A "B-A-B"
           \B "A+B+A"}
   :rotation 60
   :operations {\B ["step"]
                \A ["step"]
                \+ ["left"]
                \- ["right"]}
   :turtle (l/turtle 0 0 0 true)})

(def dragon-curve
  {:axiom "FX"
   :rules {\X "X+YF+"
           \Y "-FX-Y"}
   :rotation 90
   :operations {\F ["step"]
                \X ["noop"]
                \Y ["noop"]
                \+ ["left"]
                \- ["right"]}
   :turtle (l/turtle 0 0 0 true)})

(defn draw-diff [old-state new-state]
  (letfn [(coords [state] (map (:turtle state) [:x :y]))]
    (let [both-pairs (map coords [old-state new-state])
          pen (:pen (:turtle new-state))]
      (if-not (= (first both-pairs) (last both-pairs))
        (if pen (apply q/line (flatten both-pairs)))))
    new-state))

(defn attach-draw [state]
  (assoc state :effector draw-diff))

(defn attach-iterations [iterations state]
  (assoc state :iterations iterations))

(defn prepare-system [system width height iterations]
  (->> system
       (l/attach-dispatch)
       (l/generate-borders iterations)
       (l/attach-unit width height)
       (l/attach-offsets)
       (attach-iterations iterations)
       (attach-draw)))

(defn draw-system [system iterations]
  (l/walk-string (l/nth-iteration iterations system) system))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (prepare-system pythagoras-tree 800 800 8))

(defn update [state]
  state)

(defn draw [state]
  (q/background 240)
  (q/with-translation [(:x-offset state)
                       (:y-offset state)]
    (draw-system state (:iterations state)))
  state)

(q/defsketch l-system-fun
  :title "LSD for fun and profit"
  :size [800 800]
  :setup setup
  :update update
  :draw draw
  :middleware [m/fun-mode])
