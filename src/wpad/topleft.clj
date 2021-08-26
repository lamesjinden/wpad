(ns wpad.topleft
  (:require [wpad.core :as w]))

(def sizing-ratios [0.25 0.5 0.75 1.0])

(defn get-placements-by-rate
  [ratio
   _containing-screen
   {{workspace-width  :width
     workspace-height :height
     :as              _workspace-dimensions}   :workspace
    {{:keys [left-extent right-extent top-extent bottom-extent extents-type]
      :as   _frame-dimensions} :frame-extents} :window
    :as                                        _environment}]
  (let [horizontal-extents (+ left-extent right-extent)
        vertical-extents (+ top-extent bottom-extent)
        desired-width (int (* ratio workspace-width))
        desired-height (as-> (int (/ workspace-height 2)) $
                             (if (= extents-type :net)
                               (- $ (* 2 top-extent))
                               $))
        width (+ desired-width horizontal-extents)
        height (+ desired-height vertical-extents)
        x 0
        y 0]
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