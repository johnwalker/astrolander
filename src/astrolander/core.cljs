(ns astrolander.core
  (:require
   [clojure.string :as str]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(def width (min (.-innerWidth js/window)
                (.-innerHeight js/window)))

(def height width)

(def pi q/PI)

(def angle-sensitivity 3)

(def angle-tolerance 5)

(def paused       (atom false))
(def pressed-keys (atom #{}))

(defn toggle-pause []
  (if @paused
    (do (q/start-loop)
        (reset! paused false))
    (do (q/no-loop)
        (reset! paused true))))

(set! (.-onload js/window)
      (fn [& other]
        (set! (.-onkeyup js/document)
              (fn [e]
                (swap! pressed-keys disj (.-keyCode e))))
        (.focus (.getElementById js/document "astrolander"))))

(def default-lander {:head 0.08
                     :base 0.06
                     :angle 180
                     :fuel 400
                     :acceleration-mag 0.00035
                     :gravity-mag 0.00008
                     :acceleration [0 0]
                     :velocity [0 0]
                     :position [0 0]})

(def builtin-levels [{:paths [[[-4.0 -10]
                               [-3.5 0]
                               [-3.0 -5]
                               [-2.5 0]
                               [-2.0 -3]
                               [-1.0 0]
                               [0.0 0.50]
                               [0.5 0.80]
                               [1.0 0.90]
                               [1.1 0.80]
                               [1.5 0.80]
                               ]]
                      :goals [[[1.5 0.75]
                               [2.0 0.75]
                               [2.0 0.85]
                               [1.5 0.85]]]
                      :scores [500]
                      :lander {:position [1.5 -0.5]
                               :fuel 500}}

                     {:paths [[[0 0.70]
                               [0.8 0.60]
                               [1.2 0.70]
                               [1.8 0.40]
                               [2.4 0.70]
                               [3.0 1]
                               [3.2 1]]
                              [[3.5 1]
                               [3.8 1]
                               [4.0 0.70]
                               [5.0 0.40]
                               [5.5 0.70]
                               [6.0 0.60]
                               [6.2 0.70]]]
                      :goals [[[3.2 0.95]
                               [3.5 0.95]
                               [3.5 1.05]
                               [3.2 1.05]]]
                      :scores [500]
                      :lander {:position [2.9 -1]
                               :fuel 500}}

                     {:paths [[[0 0.70]
                               [0.8 0.60]
                               [1.2 0.70]
                               [1.8 0.40]
                               [2.4 0.70]
                               [3.0 1]
                               [3.2 1]]

                              [[3.5 1]
                               [3.8 0.70]
                               [4.2 1]]

                              [[4.5 1]
                               [5.5 0.70]
                               [6.0 0.60]
                               [6.2 0.70]]]
                      :goals [[[3.2 0.95]
                               [3.5 0.95]
                               [3.5 1.05]
                               [3.2 1.05]]

                              [[4.2 0.95]
                               [4.5 0.95]
                               [4.5 1.05]
                               [4.2 1.05]]]

                      :scores [500 1000]
                      :lander {:position [2.9 -1]
                               :fuel 500}}])

(defn update-velocity [{:keys [acceleration-mag] :as lander}]
  (let [angle (q/radians (:angle lander))]
    (assoc lander :velocity
           (mapv + (:velocity lander) [(* -1 acceleration-mag (q/sin angle))
                                       (* acceleration-mag (q/cos angle))]))))

;; right
;; left
;; up
(def keymap {37 [[:lander :angle] - angle-sensitivity]
             39 [[:lander :angle] + angle-sensitivity]
             38 [[:lander] (fn [{:keys [fuel] :as lander}]
                             (if (pos? fuel)
                               (update-in (update-velocity lander) [:fuel] dec)
                               lander))]})

(defn keymap-guard
  "Contract for the keymap. We do some crazy things, but want to make
  sure we don't do anything too crazy."
  [x]
  (and (vector? x)
       (vector? (first x))
       (fn? (second x))))

(defn window-scaled [[x y]]
  [(* x width)
   (* y height)])

(defn rotate [x y angle]
  (let [ra (q/radians angle)
        ry (q/sin ra)
        rx (q/cos ra)]
    [(- (* x rx) (* y ry))
     (+ (* x ry) (* y rx))]))

(defn lander-vertices
  [{:keys [position angle head base]}]
  (let [[x y] position
        [b h] [base head]
        ;; rotate before translate
        [x1 y1] (rotate (/ b  2)     (/ h -3) angle)
        [x2 y2] (rotate (/ b -2)     (/ h -3) angle)
        [x3 y3] (rotate       0      (/ (* 2 h) 3) angle)]
    ;; translate after rotate
    [[(+ x x1) (+ y y1)]
     [(+ x x2) (+ y y2)]
     [(+ x x3) (+ y y3)]]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/smooth 8)
  (q/text-size 16)
  {:activity :start})

(defn move [state]
  (update-in state [:lander]
             (fn [{:keys [velocity position] :as lander}]
               (assoc lander :position
                      (mapv + position velocity)))))

(defn gravity-tick [state]
  (let [gravity-mag (get-in state [:lander :gravity-mag])]
    (update-in state [:lander :velocity]
               #(mapv + % [0 gravity-mag]))))

(defn orientation [[x1 y1] [x2 y2] [x3 y3]]
  (let [c (- (* (- y2 y1) (- x3 x2))
             (* (- y3 y2) (- x2 x1)))]
    (cond (pos? c) :clockwise
          (neg? c) :counter-clockwise
          :else    :collinear)))

(defn x-proj [[x _]] x)

(defn y-proj [[_ y]] y)

(defn intersects? [segment-1 segment-2]
  (let [[p1 q1] segment-1
        [p2 q2] segment-2
        a (orientation p1 q1 p2)
        b (orientation p1 q1 q2)
        c (orientation p2 q2 p1)
        d (orientation p2 q2 q1)]
    (or (and (not= a b) (not= c d))
        (and (= :collinear a b c d)
             ;; not sure if i understand this. we probably don't need all this shit.
             (and (or (<= (x-proj p1) (x-proj p2) (x-proj q1))
                      (<= (x-proj p1) (x-proj q2) (x-proj q1))
                      (>= (x-proj p1) (x-proj p2) (x-proj q1))
                      (>= (x-proj p1) (x-proj q2) (x-proj q1)))
                  (or (<= (y-proj p1) (y-proj p2) (y-proj q1))
                      (<= (y-proj p1) (y-proj q2) (y-proj q1))
                      (>= (y-proj p1) (y-proj p2) (y-proj q1))
                      (>= (y-proj p1) (y-proj q2) (y-proj q1))))))))

(defn path->segments [vs]
  (partition 2 1 vs))

(defn closed->segments [vs]
  (path->segments (take (inc (count vs)) (cycle vs))))

(defn collided? [ss1 ss2]
  (loop [s1 (first ss1)
         sss1 (rest ss1)]
    (if-let [x (some #(intersects? s1 %) ss2)]
      true
      (if (seq sss1)
        (recur (first sss1)
               (rest sss1))))))

(defn check-collision
  ([state coordinates r]
   (check-collision state coordinates r path->segments))
  ([state coordinates r f]
   (let [vs (lander-vertices (get state :lander))
         ss (closed->segments vs)]
     (if-let [i (some (fn [[idx path]] (when (collided? ss (f path)) idx))
                      (map-indexed (fn [idx path] [idx path])
                                   (get-in state coordinates)))]
       (-> state
           (assoc :activity r)
           (assoc :check i))
       state))))

(defn check-victory [state]
  (if (= :goal (:activity state))
    (let [a (get-in state [:lander :angle])]
      (assoc state :activity
             (if (<= (- 180 angle-tolerance)
                     a
                     (+ 180 angle-tolerance))
               :victory
               :death)))
    state))

(defn draw-complete [state]
  (q/background 0)
  (q/text "congrats you won." 25 20)
  (q/text "here is your money back:"  25 50)
  (toggle-pause))

(defn check-completion [state]
  ;; FIXME for non builtin levels...
  (if (>= (:level-number state) (count builtin-levels))
    (assoc state :activity :complete)
    state))

(defn update-state [state]
  (case (:activity state)
    :start
    ;; Initialize the lander
    (let [level  (get builtin-levels 0)
          lander (get level :lander)]
      (-> state
          (assoc :lander (merge default-lander lander))
          (assoc :level  level)
          (assoc :activity :play)
          (assoc :level-number 0)
          (assoc :score 0)))
    :continue
    (let [level  (get builtin-levels (:level-number state))
          lander (get level :lander default-lander)]
      (-> state
          (assoc :lander (merge default-lander lander))
          (assoc :level  level)
          (assoc :activity :play)))
    :pause state
    :death (assoc state :activity :start)
    :complete (assoc state :activity :start)
    :victory  (-> state
                  (assoc :activity :continue)
                  (update-in [:score] +
                             ((get-in state [:level :scores])
                              (:check state))))
    :status (-> state
                (update-in [:level-number] inc)
                (check-completion))
    :play
    (let [state (-> state gravity-tick)
          ;; check if there is a way to fit keymap-guard onto keymap so we don't
          ;; have to filter all the time. then we can replace
          ;; filter with (remove nil? ...) .
          commands (filter keymap-guard (map keymap @pressed-keys))
          state (-> (reduce (fn [s p] (apply update-in s p)) state commands)
                    ;; explanation for above: apply each valid command. we won't
                    ;; worry about the order commands are applied. afterwards,
                    ;; hand the game off to processors that work after player
                    ;; decision.
                    move
                    (check-collision [:level :paths] :death))]
      (if (not= (:activity state) :death)
        (-> state
            (check-collision [:level :goals] :goal closed->segments)
            (check-victory))
        state))))

(defn draw-play [state]
  (q/background 0)
  (q/fill 255 255 255)
  (q/text (str "fuel "  (get-in state [:lander :fuel])) 25 20)
  (q/text (str "score " (get-in state [:score])) 25 50)
  (q/text (str "level " (get-in state [:level-number])) 25 80)
  (q/no-fill)
  (q/stroke-cap :project)
  (let [lander (get state :lander)
        [x y]  (window-scaled (get lander :position))
        ]
    ;; set the camera to follow the lander, and zoom closer
    ;; as the lander approaches the bottom of the screen.
    (let [z (/ (- (/ (* 4 height) 3) y) (* 2 (q/tan (/ pi 6))))
          [y z] (if (> z 5000) [y 5000] [(/ (+ height y) 2) z])]
      (q/camera
       ;; eye
       x
       y
       z

       x
       y
       1

       ;; up
       0
       1
       0))
    (q/stroke-weight 2)
    (q/stroke 0 0 255 130)
    (let [level (:level state)]
      ;; draw each path on the moon
      (doseq [path (:paths level)]
        (q/begin-shape)
        (doseq [[x y] (map window-scaled path)]
          (q/vertex x y))
        (q/end-shape))

      ;; draw the goal
      (q/stroke 255 200 255)
      (doseq [path (:goals level)]
        (q/begin-shape)
        (doseq [[x y] (map window-scaled path)]
          (q/vertex x y))
        (q/end-shape :close)))

    ;; draw the lander last
    (q/stroke 230 100 230)
    (q/stroke-weight 2)
    (q/fill 0)
    (let [vs (mapv window-scaled (lander-vertices lander))]
      ;; we don't have to keep recomputing the vertices, but
      ;; we'll do it since it frees us from some redundancy.
      (q/begin-shape)
      (doseq [[a b] vs]
        (q/vertex a b 0.01))
      (q/end-shape :close)))
  )

(defn draw-death [state]
  (q/fill 255 255 255)
  (q/camera)
  (q/text (str "death. spacebar to try again.") (/ width 3) 20)
  (toggle-pause))

(defn draw-victory [state]
  (q/fill 255 255 255)
  (q/camera)
  (q/text (str "victory. spacebar to continue.") (/ width 3) 20)
  (toggle-pause))


(defn draw-state [state]
  (case (:activity state)
    ;; we play for one more frame to show the intersection.
    :death (do (draw-play state)
               (draw-death state))
    :play (draw-play state)
    :start (draw-play state)
    :continue (draw-play state)
    :complete (draw-complete state)
    :victory (do (draw-play state)
                 (draw-victory state))))

(defn key-handler [state e]
  (case (:key-code e)
    32 (toggle-pause)
    (swap! pressed-keys conj (:key-code e)))
  state)

(q/defsketch astrolander
  :host "astrolander"
  :size [width height]
  :setup setup
  :key-pressed key-handler
  :renderer :p3d
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
