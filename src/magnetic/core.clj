(ns magnetic.core
  (:import [java.awt.event KeyEvent])
  (:require [quil.core :as qc]))

;; Physics
(def econstant
  (/ 1 (* 4 Math/PI 8.854187817E-12)))

(defn distance [[^Float x1 ^Float y1] [^Float x2 ^Float y2]]
  (Math/sqrt
   (+
    (Math/pow (- x1 x2) 2)
    (Math/pow (- y1 y2) 2))))

;; E = sum(Ei) = econstant * sum(Qi/ri^2 * r^i)
;;(defn point-power [pivot point charge])

(defn int+ [prev [^Float d ^Float c]]
  (if (< (Math/abs d) 0.00001)
    prev
    (+ prev (* econstant (/ c (Math/pow d 2))))))

(defn intensity [point charges]
  (let [components (for [[x y charge] charges]
                     [(distance point [x y]) charge])]
    (reduce
     int+
     0.0
     components)))

;; Logic
(def maxx 2)
(def maxy 2)

(def step (atom 0.1))
(def charges (atom []))
(def field (atom []))
(declare calculate-field)

(defn reset-all! []
  (swap! step (fn [_] 0.1))
  (swap! charges (fn [_] [[1 1 -1]]))
  (swap! field (fn [_] (calculate-field maxx maxy @step @charges))))



(defn intensity->color [^Float i]
  (let [power (* (/ 255 3.0E10) (Math/abs i))
        int-power (int power)]
    (if (> i 0)
      [int-power 0x00 0x00]
      [0x00 int-power 0x00])))

(defn calculate-field [w h grid-step charges]
  (for [x (range 0 w grid-step)
        y (range 0 h grid-step)]
      (let [d (distance [x y] [0 0])
            i (intensity [x y] charges)
            c (intensity->color i)]
        [x y i c])))


;; Drawings
(def blue   [53 108 237])
(def yellow [235 229 20])
(def white  [255 255 255])
(def red    [255 0 0])
(def green  [0 255 0])
(def black  [0 0 0])

(def width 400)
(def height 400)
;; Map abstract coordinates to the screen
(defn coord->pix [[x y]]
  (let [stepx (/ width maxx)
        stepy (/ height maxy)]
    [(* x stepx) (* y stepy)]))

(defn pix->coord [[x y]]
  (let [stepx (/ width maxx)
        stepy (/ height maxy)]
    [(/ x stepx) (/ y stepy)]))

(defn draw []
  (apply qc/background black)
  (apply qc/fill white)
  (let [mx (qc/mouse-x)
        my (qc/mouse-y)
        [x y] (pix->coord [mx my])
        power (intensity [x y] @charges)]
    (qc/text (format "%2.2f:%2.2f %4.4g" (float x) (float y) power) 20 20))

  (qc/no-stroke)

  (doseq [[x y i c] @field]
    (let [[nx ny] (coord->pix [x y])]
      (apply qc/fill c)
      (qc/rect nx ny 1 1)))

  (doseq [[x y sign] @charges]
    (if (> sign 0)
      (apply qc/fill red)
      (apply qc/fill green))
    (let [[nx ny] (coord->pix [x y])]
      (qc/ellipse nx ny 8 8))))

(defn key-pressed []
  (cond
   (= (qc/key-code) KeyEvent/VK_R)
      (reset-all!)
   (= (qc/key-code) KeyEvent/VK_S)
      2
   (= (qc/key-code) KeyEvent/VK_UP)
      (do
        (swap! step (fn [x] (* x 1.5)))
        (swap! field (fn [_] (calculate-field maxx maxy @step @charges))))
   (= (qc/key-code) KeyEvent/VK_DOWN)
      (do
        (swap! step (fn [x] (/ x 1.5)))
        (swap! field (fn [_] (calculate-field maxx maxy @step @charges))))))

(defn mouse-clicked []
  (let [x (qc/mouse-x)
        y (qc/mouse-y)
        orig (pix->coord [x y])
        sign (if (= (qc/mouse-button) :left) 1.0 -1.0)
        charge (conj orig sign)]
    (swap! charges conj charge)
    (swap! field (fn [_] (calculate-field maxx maxy @step @charges)))))

(defn setup []
  (qc/smooth)
  (qc/stroke-weight 12)
  (qc/ellipse-mode :center)
  (qc/text-font (qc/create-font "DejaVu Sans" 12 true))
  (qc/frame-rate 5)
  (reset-all!)
  )


(time (doseq [x (calculate-field maxx maxy 0.01 @charges)]))

(qc/defsketch magnetic
  :title "Magnetic"
  :size [width height]
  :setup setup
  :draw draw
  :key-pressed key-pressed
  :mouse-clicked mouse-clicked
  )

