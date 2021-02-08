(ns wpad.wbottom
  (:require [wpad.core :as w]))

(defn get-placements-by-rate [ratio containing-screen workspace-height]
  (let [screen-width (:width containing-screen)
        screen-height (:height containing-screen)
        y-delta (- screen-height workspace-height)
        width (int (* screen-width ratio))
        height (int (/ workspace-height 2))
        x (int (/ (- screen-width width) 2))                ; todo use screen-x-offset
        y (+ height y-delta)]
    {:x      x
     :y      y
     :width  width
     :height height}))

(defn get-placement-options [active-window-dimensions
                             screen-dimensions
                             {workspace-height :height}]
  (let [containing-screen (w/get-containing-screen
                                active-window-dimensions
                                screen-dimensions)
        sizing-ratios [0.25 0.33 0.5 0.67 0.85 1.0]]
    (->> sizing-ratios
         (map #(get-placements-by-rate % containing-screen workspace-height)))))

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
