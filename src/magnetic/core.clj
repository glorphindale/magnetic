(ns magnetic.core
  (:import [java.awt.event KeyEvent])
  (:require [quil.core :as qc]))

(def width 400)
(def height 400)
(def maxx 2)
(def maxy 2)

(def step (atom 0.2))

(def charges
  (atom [[0 0 -1]]))

;; Map abstract coordinates to the screen
(defn coord->pix [[x y]]
  (let [centerx (/ width 2)
        centery (/ height 2)
        gradex (* 2 maxx)
        gradey (* 2 maxy)
        stepx (/ width gradex)
        stepy (/ height gradey)]
    [(+ centerx (* x stepx)) (+ centery (* y stepy))]))

(defn pix->coord [[x y]]
  (let [centerx (/ width 2)
        centery (/ height 2)
        gradex (* 2 maxx)
        gradey (* 2 maxy)
        stepx (/ width gradex)
        stepy (/ height gradey)]
    [(/ (- x centerx) stepx) (/ (- y centery) stepy)]))

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
  (qc/background-float 0x22)

  (doseq [x (range (- maxx) maxx @step)
          y (range (- maxy) maxy @step)]
    (let [d (distance [x y] [0 0])
          [nx ny] (coord->pix [x y])
          i (intensity [x y] @charges)
          c (intensity->color i)]
      (apply qc/fill c)
      (qc/rect nx ny 1 2)))

  #_(doseq [[x y sign] charges]
    (if (> sign 0)
      (qc/fill 0xff 0x00 0x00)
      (qc/fill 0x00 0xff 0x00))
    (let [[nx ny] (coord->pix [x y])]
      (qc/ellipse nx ny 8 8))))

(defn key-pressed []
  (println "(qc/key-code)")
  (cond
   (= (qc/key-code) KeyEvent/VK_W)
      1
   (= (qc/key-code) KeyEvent/VK_S)
      2
   (= (qc/key-code) KeyEvent/VK_UP)
      (swap! step (fn [x] (* x 2)))
   (= (qc/key-code) KeyEvent/VK_DOWN)
      (swap! step (fn [x] (/ x 2)))))

(defn mouse-clicked []
  (let [x (qc/mouse-x)
        y (qc/mouse-y)
        orig (pix->coord [x y])
        charge (conj orig 1.0)]
    (swap! charges conj charge)))

(qc/defsketch magnetic
  :title "Magnetic"
  :size [width height]
  :setup (fn [] (qc/smooth) (qc/no-stroke) (qc/frame-rate 30))
  :draw draw
  :key-pressed key-pressed
  :mouse-clicked mouse-clicked
  )
