(ns l-system-fun.lsd)
;; For L-system Demonstration Systems, of course.

(defn turtle
  "A constructor for turtles"
  [x y angle pen-state]
  {:x x :y y :angle angle :pen pen-state})

(defn rotate-turtle
  "Returns a rotated turtle"
  [current-turtle angle]
  (turtle (:x current-turtle)
          (:y current-turtle)
          (mod (+ (:angle current-turtle) angle) 360)
          (:pen current-turtle)))

(defn- next-position
  "Calculate new coordinates, given a current position, distance, and angle"
  [x y angle distance]
  (let [rad-angle (Math/toRadians angle)]
    (letfn [(precision [precision d]
              (let [factor (Math/pow 10 precision)]
                (/ (Math/round (* d factor)) factor)))]
      (map (partial precision 10)
           [(+ x (* (Math/cos rad-angle) distance))
            (+ y (* (Math/sin rad-angle) distance))]))))

(defn move-turtle
  "Returns a moved turtle"
  [current-turtle distance]
  (let [current [(:x current-turtle) (:y current-turtle)]
        {:keys [angle pen]} current-turtle
        next (apply next-position (flatten [current angle distance]))]
    (apply turtle (flatten [next angle pen]))))

(move-turtle (turtle 0 0 45 false) 1)

;; Our tool processes a language wherein we use a small set of
;; codified assumptions expressed as state to permit the creation
;; of complex instruction sets expressed in strings where each
;; character maps directly to a behaviour.

;; We take in a piece of structured data, uncreatively named 'state',
;; and return a modified version of that data. In our state, we have
;; a turtle, a representation of our position on the drawing plane,
;; a sub-system, which provides our angle for rotations and distance,
;; for movements, and a stack, which can store turtles to permit
;; branching paths in our L-System processor.

(defn blank-state [subsystem]
  {:stack ()
   :subsystem subsystem 
   :turtle (turtle 0 0 0 true)})

;; We define some operations; step, right, left, stack-push, stack-pop, and noop.

(defn step
  "Move the turtle forward by the distance assigned to the system"
  [state]
  (let [{:keys [unit turtle]} state]
    (update-in state [:turtle] move-turtle unit)))

;; Rightward (clockwise) rotation is subtractive, because maths

(defn right
  "Rotate the turtle rightward/clockwise by the angle assigned to the system"
  [state]
  (let [{:keys [rotation turtle]} state]
    (update-in state [:turtle] rotate-turtle (- rotation))))

(defn left
  "Rotate the turtle leftward/counterclockwise by the angle assigned to the system"
  [state]
  (let [{:keys [rotation turtle]} state]
    (update-in state [:turtle] rotate-turtle rotation)))

(defn stack-push
  "Push the current turtle in the state onto the stack"
  [state]
  (let [turtle (:turtle state)]
    (update-in state [:stack] conj turtle)))

(defn stack-pop
  "Replace the current turtle in the state with the first one on the stack"
  [state]
  (let [new-turtle (first (:stack state))]
    (-> state
        (assoc-in [:turtle] new-turtle)
        (update-in [:stack] pop))))

(def noop identity)

;; Ugh. I'm still a bit unclear on the NS lookup semantics here, so the correct
;; NS is hard coded in, for predictable behaviour both in the repl and jar.
(defn compose-ops [ops]
  "Convert the string representations of turtle operations into a single fn"
  (apply comp (map (fn [op] (ns-resolve 'l-system-fun.lsd (symbol op)))
                   (reverse ops))))

(compose-ops ["right" "right" "right"])

(defn evaluate-rules [system letter]
  (or ((:rules system) letter) letter))

(defn process-string [system string]
  (apply str (map (partial evaluate-rules system) string)))

(defn iterate-system [system]
  (iterate (partial process-string system) (:axiom system)))

(defn nth-iteration [n system]
  (last (take (+ n 1) (iterate-system system))))

(def test-system
  {:axiom "A"
   :rules {\A "AB"
           \B "A"}
   :subsystem {:angle 60, :distance 10}
   :operations {\B ["step"]
                \A ["step"]
                \+ ["left"]
                \- ["right"]}})

;; (defn will-it-blend? [] (= (nth-iteration 7 test-system) "ABAABABAABAABABAABABAABAABABAABAAB"))

(defn create-dispatch [ops]
  (apply hash-map (mapcat (fn [op] (let [[letter steps] op] [letter (compose-ops steps)])) ops)))

(defn attach-dispatch [state]
  (assoc state :dispatch (create-dispatch (:operations state))))

(defn handle-letter [state letter]
  (let [{:keys [turtle dispatch effector]} state
        next-state ((dispatch letter) state)]
    (effector state next-state)))

(defn walk-string [string state]
  (reduce handle-letter state string))

(defn default-borders []
  (letfn [(create-center [coll direction]
            (assoc coll direction (turtle 0 0 0 true)))]
    (reduce create-center {} [:right :left :top :bottom])))

(defn- test-turtle [turtle testers border]
  (let [[direction border-turtle] border
        tester (direction testers)
        [comp-fn axis] tester
        coord (axis turtle)
        border-coord (axis border-turtle)]
    (if (comp-fn coord border-coord) [direction turtle] [direction border-turtle])))

(defn update-borders [turtle borders]
  (let [border-testers {:right [> :x]
                      :left [< :x]
                      :top [> :y]
                      :bottom [< :y]}]
    (apply hash-map (mapcat (partial test-turtle turtle border-testers) borders))))

(defn update-state-borders [_ state]
  (let [{:keys [turtle borders]} state]
    (assoc state :borders (update-borders turtle borders))))

(defn simplify-borders [state]
  (let [borders (:borders state)
        {:keys [top bottom left right]} borders]
    (assoc state :borders {:top (:y top)
                           :bottom (:y bottom)
                           :left (:x left)
                           :right (:x right)})))

(defn generate-borders [iterations state]
  (let [state (-> state
                  (assoc :unit 1)
                  (assoc :borders (default-borders))
                  (assoc :effector update-state-borders))]
    (dissoc (assoc (simplify-borders
              (walk-string
               (nth-iteration iterations state)
               state)) :turtle (turtle 0 0 0 true)) :effector)))

(defn attach-unit [width height state]
  (let [{:keys [top bottom left right]} (:borders state)
        system-height (Math/abs (- top bottom))
        system-width (Math/abs (- left right))]
    (assoc state :unit
           (min (/ width system-width) (/ height system-height)))))

(defn attach-offsets [state]
  (let [{:keys [bottom left]} (:borders state)
        unit (:unit state)]
    (-> state
        (assoc :x-offset (* unit (Math/abs left)))
        (assoc :y-offset (* unit (Math/abs bottom))))))

