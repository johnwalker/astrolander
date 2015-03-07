(ns astrolander.core
  (:require [clojure.string :as str]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(enable-console-print!)

(def width (min (.-innerWidth js/window)
                (.-innerHeight js/window)))

(def height width)
(def angle-sensitivity 3)
(def pi q/PI)

(def paused       (atom false))
(def pressed-keys (atom #{}))

(set! (.-onload js/window)
      (fn [& other]
        (set! (.-onkeyup js/document)
              (fn [e]
                (swap! pressed-keys disj (keyword (str/lower-case (.-keyIdentifier e))))))
        (.focus (.getElementById js/document "astrolander"))))

(def default-lander {:head 0.08
                     :base 0.04
                     :angle 180
                     :fuel 400
                     :acceleration-mag 0.00042
                     :gravity-mag 0.00016
                     :acceleration [0 0]
                     :velocity [0 0]
                     :position [0 0]})

(def default-levels [{:paths [[[0 0.70]
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
                      :score [500]
                      :lander {:position [0.5 0.2]
                               :fuel 500}}])

(defn update-velocity [{:keys [acceleration-mag] :as lander}]
  (let [angle (q/radians (:angle lander))]
    (assoc lander :velocity
           (mapv + (:velocity lander) [(* -1 acceleration-mag (q/sin angle))
                                       (* acceleration-mag (q/cos angle))]))))

(def keymap {:left  [[:lander :angle] - angle-sensitivity]
             :right [[:lander :angle] + angle-sensitivity]
             :up    [[:lander] (fn [{:keys [fuel] :as lander}]
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
             ;; there's a function with an obvious implementation
             ;; trying to escape, but it's unlikely to be
             ;; used elsewhere. i'll keep the pattern in mind for
             ;; later work.
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
      [s1 x]
      (if (seq sss1)
        (recur (first sss1)
               (rest sss1))))))

(defn check-collision [state]
  (let [vs (lander-vertices (get state :lander))
        ;; we assume that lander-vertices returns vertices ordered
        ;; clockwise or counterclockwise. we always get one of those
        ;; when we deal with triangles. something to think about.
        ;; wish i knew a mathematical term for that kind of ordering.
        ss (closed->segments vs)]
    (if (some (fn [path] (collided? ss (path->segments path)))
              (get-in state [:level :paths]))
      (assoc state :death-collision true)
      state))

  )

(defn update-state [state]
  (case (:activity state)
    :start
    ;; Initialize the lander
    (let [level  (get default-levels (get state :level-number 0))
          lander (get level :lander)]
      (-> state
          (assoc :lander (merge default-lander lander))
          (assoc :level  level)
          (assoc :activity :play)))
    :pause state
    :play
    (let [state (-> state gravity-tick)
          ;; check if there is a way to fit keymap-guard onto keymap so we don't
          ;; have to filter all the time. then we can replace
          ;; filter with (remove nil? ...) .
          commands (filter keymap-guard (map keymap @pressed-keys))]
      (-> (reduce (fn [s p] (apply update-in s p)) state commands)
          ;; explanation for above: apply each valid command. we won't
          ;; worry about the order commands are applied. afterwards,
          ;; hand the game off to processors that work after player
          ;; decision.
          move
          check-collision))))

(defn draw-state [state]
  (q/background 0)
  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text (str "Fuel: " (get-in state [:lander :fuel])) 25 50)
  (q/no-fill)
  (let [lander (get state :lander)
        [x y]  (window-scaled (get lander :position))]
    ;; set the camera to follow the lander, and zoom closer
    ;; as the lander approaches the bottom of the screen.
    (q/camera
     ;; eye
     x
     (/ (+ height y) 2)
     (/ (- (/ (* 4 height) 3) y) (* 2 (q/tan (/ pi 6))))

     ;; center
     x
     (/ (+ height y) 2)
     0

     ;; up
     0
     1
     0)
    ;; draw the lander
    (q/stroke 122 255 122)
    (q/stroke-weight 1)
    (let [vs (mapv window-scaled (lander-vertices lander))]
      ;; we don't have to keep recomputing the vertices, but
      ;; we'll do it since it frees us from some redundancy.
      (q/begin-shape)
      (doseq [[a b] vs]
        (q/vertex a b))
      (q/end-shape :close)))

  (q/stroke-weight 3)
  (q/stroke 100 88 255)
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
      (q/end-shape :close))))

(defn key-handler [state e]
  (when (= (name (:key e)) " ")
    (if @paused
      (do (q/start-loop)
          (reset! paused false))
      (do (q/no-loop)
          (reset! paused true))))
  (swap! pressed-keys conj (:key e))
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
