(ns astrolander.core
  (:require [clojure.string :as str]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def width (min (.-innerWidth js/window) (.-innerHeight js/window)))
(def height width)

(def angle-sensitivity 3)

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
                               [3.2 1.05]
                               [3.2 0.95]]]
                      :score [500]
                      :lander {:position [0.5 0.2]
                               :fuel 500}}])

(def init-state {:activity :start})

(defn update-velocity [{:keys [acceleration-mag] :as lander}]
  (let [angle (q/radians (:angle lander))]
    (assoc lander :velocity
           (mapv + (:velocity lander) [(* -1 acceleration-mag (q/sin angle))
                                       (* acceleration-mag (q/cos angle))]))))

(def pressed-keys (atom #{}))

(set! (.-onload js/window)
      (fn [& other]
        (set! (.-onkeyup js/document)
              (fn [e]
                (swap! pressed-keys disj (keyword (str/lower-case (.-keyIdentifier e))))))
        (.focus (.getElementById js/document "astrolander"))))

(def keymap {:left [[:lander :angle] - angle-sensitivity]
             :right [[:lander :angle] + angle-sensitivity]
             :up   [[:lander] (fn [{:keys [fuel] :as lander}]
                                (if (pos? fuel)
                                  (update-in (update-velocity lander) [:fuel] dec)
                                  lander))]})

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  init-state)

(defn move [state]
  (update-in state [:lander]
             (fn [{:keys [velocity position] :as lander}]
               (assoc lander :position
                      (mapv + position velocity)))))

(defn gravity-tick [state]
  (let [gravity-mag (get-in state [:lander :gravity-mag])]
    (update-in state [:lander :velocity]
               #(mapv + % [0 gravity-mag]))))

(defn coords->line [[x1 y1] [x2 y2]]
  (if (not= x1 x2)
    (let [m (/ (- y2 y1)
               (- x2 x1))
          b (- y1 (* m x1))]
      [m b])
    (.log js/console "special case i haven't handled in coords->line :/")))

(defn path->lines [path]
  (mapv #(apply coords->line %) (partition 2 1 path)))

(defn compute-lines [level]
  (mapv path->lines (get level :paths)))

(defn update-state [state]
  (.log js/console (pr-str (:obstacles state)))
  (case (:activity state)
    :start
    ;; Initialize the lander
    (let [level  (get default-levels  (get state :level 0))
          lander (get level :lander)]
      (-> state
          (assoc :lander (merge default-lander lander))
          (assoc :activity :play)
          (assoc :obstacles (compute-lines level))))
    :pause state
    :play
    (let [commands (filter identity (map keymap @pressed-keys))
          state (reduce (fn [s p] (apply update-in s p)) state commands)]
      (-> state
          gravity-tick
          move))))

(defn intersection [[a b] [c d]]
  (when (not= a c)
    (/ (- d b) (- a c))))

(defn t-intersections [triangle line]
  (map #(intersection % line) triangle))

(defn collide? [triangle line [a b]]
  (seq (filter #(and (number? %)
                     (<= a % b))
               (t-intersections triangle line))))

(defn draw-state [state]
  (q/background 0)
  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text (str "Fuel: " (get-in state [:lander :fuel])) 25 50)

  (let [lander (:lander state)
        [x y] (:position lander)
        [x y] [(* width x) (* height y)]]
    (q/camera x
              (/ (+ height y) 2)
              (/ (- (/ (* 4 height) 3) y) (* 2 (q/tan (/ q/PI 6))))
              x
              (/ (+ height y) 2)
              0
              0
              1
              0))

  (q/stroke-weight 3)
  (q/stroke 100 88 255)
  (q/no-fill)
  (let [level (default-levels (:level state))]
    (doseq [path (:paths level)]
      (q/begin-shape)
      (doseq [[x y] path]
        (q/vertex (* width x)
                  (* height y)))
      (q/end-shape)))
  (q/stroke 255 200 255)
  (let [level (default-levels (:level state))]
    (doseq [path (:goals level)]
      (q/begin-shape)
      (doseq [[x y] path]
        (q/vertex (* width x)
                  (* height y)))
      (q/end-shape)))
  (q/stroke 122 255 122)
  (q/stroke-weight 1)
  (let [lander (:lander state)
        [x y] (:position lander)
        [x y] [(* width x) (* height y)]
        angle (:angle lander)
        h     (* height (:head lander))
        b     (* width (:base lander))]
    (q/translate x y)
    (q/rotate (q/radians angle))
    (q/triangle (/ b 2) (/ h -3)
                (/ (- b) 2) (/ h -3)
                0 (/ (* 2 h) 3))
    (q/rotate 0))
  )

(def paused (atom false))

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
