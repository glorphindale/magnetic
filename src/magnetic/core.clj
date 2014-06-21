(ns magnetic.core
  (:import [java.awt.event KeyEvent])
  (:require [quil.core :as qc]))

(def blue   [53 108 237])
(def yellow [235 229 20])
(def white  [255 255 255])
(def red    [255 0 0])
(def green  [0 255 0])
(def black  [0 0 0])

(def width 400)
(def height 400)
(def maxx 2)
(def maxy 2)

(def step (atom 0.1))

(def charges
  (atom [[1 1 -1]]))

;; Map abstract coordinates to the screen
(defn coord->pix [[x y]]
  (let [stepx (/ width maxx)
        stepy (/ height maxy)]
    [(* x stepx) (* y stepy)]))

(defn pix->coord [[x y]]
  (let [stepx (/ width maxx)
        stepy (/ height maxy)]
    [(/ x stepx) (/ y stepy)]))

(def econstant
  (/ 1 (* 4 Math/PI 8.854187817E-12)))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt
   (+
    (Math/pow (- x1 x2) 2)
    (Math/pow (- y1 y2) 2))))

(defn int+ [prev [d c]]
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

(defn intensity->color [i]
  (let [power (* (/ 255 3.0E10) (Math/abs i))]
    (if (> i 0)
      [power 0x00 0x00]
      [0x00 power 0x00])))


(defn draw []
  (apply qc/background black)
  (apply qc/fill white)
  (let [mx (qc/mouse-x)
        my (qc/mouse-y)
        mouse (pix->coord [mx my])]
    (qc/text (str mouse) 20 20))

  (qc/no-stroke)

  (doseq [x (range 0 maxx @step)
            y (range 0 maxy @step)]
      (let [d (distance [x y] [0 0])
            [nx ny] (coord->pix [x y])
            i (intensity [x y] @charges)
            c (intensity->color i)]
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
   (= (qc/key-code) KeyEvent/VK_W)
      1
   (= (qc/key-code) KeyEvent/VK_S)
      2
   (= (qc/key-code) KeyEvent/VK_UP)
      (swap! step (fn [x] (* x 1.5)))
   (= (qc/key-code) KeyEvent/VK_DOWN)
      (swap! step (fn [x] (/ x 1.5)))))

(defn mouse-clicked []
  (let [x (qc/mouse-x)
        y (qc/mouse-y)
        orig (pix->coord [x y])
        sign (if (= (qc/mouse-button) :left) 1.0 -1.0)
        charge (conj orig sign)]
    (swap! charges conj charge)))

(defn setup []
  (qc/smooth)
  (qc/stroke-weight 12)
  (qc/ellipse-mode :center)
  (qc/text-font (qc/create-font "DejaVu Sans" 12 true))
  (qc/frame-rate 5))

(qc/defsketch magnetic
  :title "Magnetic"
  :size [width height]
  :setup setup
  :draw draw
  :key-pressed key-pressed
  :mouse-clicked mouse-clicked
  )

