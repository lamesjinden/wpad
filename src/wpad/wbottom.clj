(ns wpad.wbottom
  (:require [wpad.core :as w]))

(def sizing-ratios [0.25 0.33 0.5 0.67 0.85 1.0])

; todo use screen-x-offset

(defn get-placements-by-rate [ratio containing-screen workspace-dimensions]
  (let [screen-width (:width containing-screen)
        screen-height (:height containing-screen)
        workspace-height (:height workspace-dimensions)
        y-delta (- screen-height workspace-height)
        width (int (* screen-width ratio))
        height (int (/ workspace-height 2))
        x (int (/ (- screen-width width) 2))
        y (+ height y-delta)]
    {:x      x
     :y      y
     :width  width
     :height height}))

(defn get-placement-options [active-window-dimensions screens-dimensions workspace-dimensions]
  (let [containing-screen (w/get-containing-screen active-window-dimensions screens-dimensions)]
    (map
      #(get-placements-by-rate % containing-screen workspace-dimensions)
      sizing-ratios)))

(defn -main []
  (let [active-window-dimensions (w/get-active-window-dimensions)
        frame-dimensions (w/get-frame-dimensions)
        screens-dimensions (w/get-screens-dims)
        workspace-dimensions (w/get-workspace-area)
        placement-options (get-placement-options
                            active-window-dimensions
                            screens-dimensions
                            workspace-dimensions)]
    (w/move-next! active-window-dimensions frame-dimensions placement-options)))

(comment

  (-main)

  )
