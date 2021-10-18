(ns wpad.topright
  (:require [wpad.core :as w]))

(def sizing-ratios [0.15 0.25 0.33 0.5 0.66 0.75 0.85 1.0])

(defn get-placements-by-rate
  [ratio
   {screen-width :width
    :as          _containing-screen}
   {{workspace-height :height
     :as              _workspace-dimensions}   :workspace
    {{:keys [left-extent right-extent top-extent bottom-extent extents-type]
      :as   _frame-dimensions} :frame-extents} :window
    :as                                        _environment}]
  (let [width (if (= extents-type :gtk)
                (+ left-extent right-extent (int (* screen-width ratio)))
                (int (* screen-width ratio)))
        height (if (= extents-type :gtk)
                 (+ top-extent bottom-extent (int (/ workspace-height 2)))
                 (- (int (/ workspace-height 2)) top-extent bottom-extent))
        x (if (= extents-type :gtk)
            (+ (- screen-width width) left-extent)
            (- screen-width width))
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