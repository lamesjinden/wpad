(ns wpad.center
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
                 (+ top-extent bottom-extent (int workspace-height))
                 (- (int workspace-height) top-extent bottom-extent))
        x (int (/ (- screen-width width) 2))
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

(defn get-center-placement-option
  [{{workspace-x :x
     workspace-y :y
     :as         _workspace-dimensions}                       :workspace
    {{_window-x     :x
      _window-y     :y
      window-width  :width
      window-height :height
      :as           active-window-dimensions} :dimensions
     {:keys [top-extent]
      :as   _frame-dimensions}                :frame-extents} :window
    screens-dimensions                                        :monitors
    :as                                                       _environment}]
  (let [{screen-width  :width
         screen-height :height
         :as           _containing-screen}
        (w/get-containing-screen active-window-dimensions screens-dimensions)
        x (int (-> screen-width
                   (- window-width)
                   (+ workspace-x)
                   (/ 2)))
        y (int (-> screen-height
                   (- window-height)
                   (+ workspace-y)
                   (/ 2)
                   (- top-extent)))]
    {:x      x
     :y      y
     :width  window-width
     :height window-height}))

(def switch-center-only "--center-only")

(defn -main [& args]
  (w/log "")
  (try
    (let [environment (w/get-environment)
          center-only? (as-> (or args #{}) $
                             (into #{} $)
                             (contains? $ switch-center-only))]
      (if center-only?
        (let [center-placement (get-center-placement-option environment)]
          (w/move-active-window! center-placement (:frame-extents environment)))
        (let [placement-options (get-placement-options environment)]
          (w/move-next! environment placement-options))))
    (catch Exception e
      (w/log e))))

(comment

  (-main)

  )