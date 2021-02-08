(ns wpad.wtopleft
  (:require [wpad.core :as w]))

(defn get-placements-by-rate [ratio screen-width workspace-height]
  (let [width (int (* screen-width ratio))
        height (int (/ workspace-height 2))
        x 0                                                 ; todo use screen-x-offset
        y 0]
    {:x      x
     :y      y
     :width  width
     :height height}))

(defn get-placement-options [active-window-dimensions
                             screen-dimensions
                             {workspace-height :height}]
  (let [{screen-width :width} (w/get-containing-screen
                                active-window-dimensions
                                screen-dimensions)
        sizing-ratios [0.25 0.33 0.5 0.67 0.75 0.85]]
    (map
      #(get-placements-by-rate % screen-width workspace-height)
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