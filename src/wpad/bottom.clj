(ns wpad.bottom
  (:require [wpad.core :as w]))

(def sizing-ratios [0.25 0.5 0.75 1.0])

(defn get-placements-by-rate
  [ratio
   {screen-height :height
    screen-width  :width
    :as           _containing-screen}
   {{workspace-width  :width
     workspace-height :height
     workspace-x      :x
     :as              _workspace-dimensions}   :workspace
    {{:keys [left-extent right-extent top-extent bottom-extent]
      :as   _frame-dimensions} :frame-extents} :window
    :as                                        _environment}]
  (let [horizontal-extents (+ left-extent right-extent)
        vertical-extents (+ top-extent bottom-extent)
        desired-width (int (* ratio workspace-width))
        desired-height (int (/ workspace-height 2))
        width (+ desired-width horizontal-extents)
        height (+ desired-height vertical-extents)
        x (-> screen-width
              (- width)
              (+ workspace-x)
              (/ 2))
        y (- screen-height desired-height)]
    {:x      x
     :y      y
     :width  width
     :height height}))

(defn get-placement-options
  [{{active-window-dimensions :dimensions} :window
    screens-dimensions                     :monitors
    :as                                    environment}]
  (let [containing-screen (w/get-containing-screen active-window-dimensions screens-dimensions)]
    (->> sizing-ratios
         (map #(get-placements-by-rate % containing-screen environment)))))

(defn -main []
  (w/log "")
  (try
    (let [environment (w/get-environment)
          placement-options (get-placement-options environment)]
      (w/move-next! environment placement-options))
    (catch Exception e
      (w/log e))))

(comment

  (-main)

  )
