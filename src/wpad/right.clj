(ns wpad.right
  (:require [wpad.core :as w]))

(def sizing-ratios [0.25 0.5 1.0])

(defn get-placements-by-rate [ratio containing-screen workspace-dimensions {:keys [left-extent right-extent top-extent bottom-extent] :as _frame-dimensions}]
  (let [screen-width (:width containing-screen)
        horizontal-extents (+ left-extent right-extent)
        vertical-extents (+ top-extent bottom-extent)
        desired-width (int (* ratio (:width workspace-dimensions)))
        desired-height (:height workspace-dimensions)
        width (+ desired-width horizontal-extents)
        height (+ desired-height vertical-extents)
        x (- screen-width desired-width)
        y 0]
    {:x      x
     :y      y
     :width  width
     :height height}))

(defn get-placement-options [active-window-dimensions screens-dimensions workspace-dimensions frame-dimensions]
  (let [containing-screen (w/get-containing-screen active-window-dimensions screens-dimensions)]
    (map
      #(get-placements-by-rate % containing-screen workspace-dimensions frame-dimensions)
      sizing-ratios)))

(defn -main []
  (w/log "")
  (try
    (let [active-window-dimensions (w/get-active-window-dimensions)
          frame-dimensions (w/get-frame-dimensions)
          screens-dimensions (w/get-screens-dims)
          workspace-dimensions (w/get-workspace-area)
          placement-options (get-placement-options
                              active-window-dimensions
                              screens-dimensions
                              workspace-dimensions
                              frame-dimensions)]
      (w/move-next! active-window-dimensions frame-dimensions placement-options))
    (catch Exception e
      (w/log e))))

(comment

  (-main)

  )
