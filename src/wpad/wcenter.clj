(ns wpad.wcenter
  (:require [wpad.core :as w]))

(defn get-placements-by-rate [ratio screen-width workspace-height]
  (let [width (int (* screen-width ratio))
        height (int workspace-height)
        x (int (/ (- screen-width width) 2))                ; todo use screen-x-offset
        y 0]
    {:x      x
     :y      y
     :width  width
     :height height}))

(defn get-placement-options [{window-x :x}
                             {monitor-descriptions :monitor-descriptions}
                             {workspace-height :height}]
  ; find which screen the current window's x coordinate lies within.
  ; use found screen to provide screen-width.
  ; assume that iteration order of screens is correct.
  ;   i.e. screen 0 begins at x = 0, screen 1 begins at x = 1 + <WIDTH-OF-SCREEN-0>
  (let [{screen-width    :width
         screen-x-offset :x-offset} (->> monitor-descriptions
                                         (reduce (fn [acc {width :width :as monitor}]
                                                   ; special case - when window-x is negative
                                                   (if (and (zero? acc)
                                                            (neg? window-x))
                                                     (reduced (assoc monitor :x-offset 0))
                                                     (if (<= acc window-x (+ acc width))
                                                       (reduced (assoc monitor :x-offset acc))
                                                       (+ acc width))))
                                                 0))
        sizing-ratios [0.5 0.67 0.85]]
    (->> sizing-ratios
         (map #(get-placements-by-rate % screen-width workspace-height)))))

(defn move-next! [{active-x :x active-width :width}
                  {:keys [left-extent right-extent] :as frame-dimensions}
                  centering-options]
  ; hack in alternate size
  ; * when current x is within twice the frame's x size
  ;   and suggested width is equal
  ; for background, see https://askubuntu.com/questions/576604/what-causes-the-deviation-in-the-wmctrl-window-move-command
  ; * though, under xfce 4.12 and through x2go client, not everything mentioned applied
  ;   * xdotool getactivewindow getwindoegeometry == xdotool getwindowgeometry $(xdotool selectwindow <click>)
  (let [nearest (->> centering-options
                     (partition 2 1)
                     (some (fn [[{:keys [x _y width _height] :as _option} next]]
                             (and (= width active-width)
                                  (<= (Math/abs ^Integer (- x active-x)) (+ left-extent right-extent))
                                  next))))
        selected (or nearest (first centering-options))]
    (w/move-active-window! selected frame-dimensions)))

(defn -main []
  (let [active-window-dimensions (w/get-active-window-dimensions)
        frame-dimensions (w/get-frame-dimensions)
        screens-dimensions (w/get-screens-dims)
        workspace-dimensions (w/get-workspace-area)
        placement-options (get-placement-options active-window-dimensions screens-dimensions workspace-dimensions)]
    (move-next! active-window-dimensions frame-dimensions placement-options)))

(comment

  (w/restore-active-window!)
  (w/xprop-root!)
  (w/parse-wmctrl-window-dimensions-line "0x01400b81  1 74   127  577  717  dev Terminal - james@dev: ~")
  (w/get-active-window-dimensions)
  (-main)

  )