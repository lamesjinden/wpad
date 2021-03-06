(ns wpad.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

;(def xprop-root! (memoize (fn [] (sh "xprop" "-root"))))
(def xprop-root! (fn [] (sh "xprop" "-root")))

(def xprop-active-window-pattern #"_NET_ACTIVE_WINDOW\(WINDOW\): window id # 0x([a-f0-9]+).*")

(defn parse-xprop-active-window-line [s]
  (when-let [match (re-matches xprop-active-window-pattern s)]
    (let [active-window-id (Integer/parseInt (get match 1) 16)
          padded (format "0x%08X" active-window-id)]
      padded)))

(defn parse-xprop-active-window-id [ss]
  (let [active-window-id (->> ss
                              (str/split-lines)
                              (filter (fn [s] (.startsWith s "_NET_ACTIVE_WINDOW(WINDOW)")))
                              (first)
                              (parse-xprop-active-window-line))]
    active-window-id))

(def wmctrl-lG-pattern #"(0x[a-f0-9]+)\s+\d+\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+).*")

(defn parse-wmctrl-window-dimensions-line [s]
  (when-let [match (re-matches wmctrl-lG-pattern s)]
    (let [x (Integer/parseInt (get match 2))
          y (Integer/parseInt (get match 3))
          width (Integer/parseInt (get match 4))
          height (Integer/parseInt (get match 5))]
      {:x      x
       :y      y
       :width  width
       :height height})))

(defn parse-wmctrl-window-dimensions [ss active-window-id]
  (let [active-window-dimensions (->> ss
                                      (str/split-lines)
                                      (filter (fn [s] (.startsWith (.toLowerCase s) (.toLowerCase active-window-id))))
                                      (first)
                                      (parse-wmctrl-window-dimensions-line))]
    (into {:window-id active-window-id} active-window-dimensions)))

(defn get-active-window-dimensions []
  (let [xprop-root-out (:out (xprop-root!))
        wmctrl-out (:out (sh "wmctrl" "-lG"))
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        active-window-dimensions (parse-wmctrl-window-dimensions wmctrl-out active-window-id)]
    active-window-dimensions))

(def xrandr-monitors-line-pattern #"^Monitors:\s+(\d+).*")

(defn parse-xrandr-monitors-line [s]
  (when-let [match (re-matches xrandr-monitors-line-pattern s)]
    (let [monitors-count (Integer/parseInt (get match 1))]
      monitors-count)))

(def xrandr-monitors-description-pattern #"^\s+(\d+):\s\+\*?.+\s(\d+)/\d+x(\d+)/\d+.*")

(defn parse-xrandr-monitors-descriptions [ss]
  (->> ss
       (map (fn [s]
              (when-let [match (re-matches xrandr-monitors-description-pattern s)]
                (let [monitor-id (get match 1)
                      width (Integer/parseInt (get match 2))
                      height (Integer/parseInt (get match 3))]
                  {:monitor-id monitor-id
                   :width      width
                   :height     height}))))
       (filter some?)))

(defn parse-xrandr [ss]
  (let [lines (str/split-lines ss)
        monitors-count (->> lines
                            (filter (fn [s] (.startsWith s "Monitors:")))
                            (first)
                            (parse-xrandr-monitors-line))
        monitor-descriptions (->> lines
                                  (filter (fn [s] (Character/isDigit (char (first (str/trim s))))))
                                  (parse-xrandr-monitors-descriptions))]
    {:monitors-count       monitors-count
     :monitor-descriptions monitor-descriptions}))

(defn get-screens-dims []
  (let [xrandr-out (:out (sh "xrandr" "--listmonitors"))
        parsed (parse-xrandr xrandr-out)]
    parsed))

(def xprop-workspace-pattern #"_NET_WORKAREA\(CARDINAL\)\s+=\s+(\d+), (\d+), (\d+), (\d+),?.*")

(defn parse-xprop-workarea-line [s]
  (when-let [match (re-matches xprop-workspace-pattern s)]
    (let [x (Integer/parseInt (get match 1))
          y (Integer/parseInt (get match 2))
          width (Integer/parseInt (get match 3))
          height (Integer/parseInt (get match 4))]
      {:x      x
       :y      y
       :width  width
       :height height})))

(defn parse-xprop-workspace-area [ss]
  (let [workspace-area (->> ss
                            (str/split-lines)
                            (filter (fn [s] (.startsWith s "_NET_WORKAREA(CARDINAL)")))
                            (first)
                            (parse-xprop-workarea-line))]
    workspace-area))

(defn get-workspace-area []
  (let [xprop-out (:out (sh "xprop" "-root"))
        workspace-area (parse-xprop-workspace-area xprop-out)]
    workspace-area))

(def xprop-frame-extents-pattern #"_NET_FRAME_EXTENTS\(CARDINAL\) = (\d+), (\d+), (\d+), (\d+)")

(defn parse-xprop-frame-extents-line [s]
  (when-let [match (re-matches xprop-frame-extents-pattern s)]
    (let [left-extent (Integer/parseInt (get match 1))
          right-extent (Integer/parseInt (get match 2))
          top-extent (Integer/parseInt (get match 3))
          bottom-extent (Integer/parseInt (get match 4))]
      {:left-extent   left-extent
       :right-extent  right-extent
       :top-extent    top-extent
       :bottom-extent bottom-extent})))

(defn parse-xprop-frame-extents [ss]
  (let [frame-extents (->> ss
                           (str/split-lines)
                           (filter (fn [s] (.startsWith s "_NET_FRAME_EXTENTS(CARDINAL)")))
                           (first)
                           (parse-xprop-frame-extents-line))]
    frame-extents))

(defn get-frame-dimensions []
  (let [xprop-root-out (:out (sh "xprop" "-root"))
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        xprop-id-out (:out (sh "xprop" "-id" active-window-id))
        frame-extents (parse-xprop-frame-extents xprop-id-out)]
    frame-extents))

(defn restore-active-window! []
  (as-> "wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz" $
        (str/split $ #" ")
        (apply sh $)))

(defn -adjust-dimensions [{:keys [x y width height] :as _window-dimensions}
                          {:keys [left-extent right-extent top-extent bottom-extent] :as _frame-dimensions}]
  {:x      x
   :y      y
   :width  (- width left-extent right-extent)
   :height (- height top-extent bottom-extent)})

(defn move-active-window! [{:keys [x y] :as window-dimensions} frame-extents]
  ; remove maximized flags, otherwise resizing has no effect
  (restore-active-window!)
  (let [{adjusted-width  :width
         adjusted-height :height} (-adjust-dimensions window-dimensions frame-extents)]
    (as-> (format "wmctrl -r :ACTIVE: -e 0,%s,%s,%s,%s" x y adjusted-width adjusted-height) $
          (str/split $ #" ")
          (apply sh $))))

(defn get-containing-screen [{window-x :x}
                             {monitor-descriptions :monitor-descriptions}]
  ; find which screen the current window's x coordinate lies within.
  ; use found screen to provide screen-width.
  ; assume that iteration order of screens is correct.
  ;   i.e. screen 0 begins at x = 0, screen 1 begins at x = 1 + <WIDTH-OF-SCREEN-0>
  (->> monitor-descriptions
       (reduce (fn [acc {width :width :as monitor}]
                 ; special case - when window-x is negative
                 (if (and (zero? acc)
                          (neg? window-x))
                   (reduced (assoc monitor :x-offset 0))
                   (if (<= acc window-x (+ acc width))
                     (reduced (assoc monitor :x-offset acc))
                     (+ acc width))))
               0)))

(defn -dimensions-match?
  "determines if the two provided dimension descriptions are equal-ish.

   a match is defined as follows:
   * when width is equal
   * and
   * when delta x is within twice the frame's x size

   for background, see https://askubuntu.com/questions/576604/what-causes-the-deviation-in-the-wmctrl-window-move-command
   * though, under xfce 4.12 and through x2go client, not everything mentioned applied
   * xdotool getactivewindow getwindoegeometry == xdotool getwindowgeometry $(xdotool selectwindow <click>)
   "
  [window-dimensions1 window-dimensions2 frame-dimensions]
  (let [{width1 :width x1 :x} window-dimensions1
        {width2 :width x2 :x} window-dimensions2
        {:keys [left-extent right-extent]} frame-dimensions]
    (and (= width1 width2)
         (<= (Math/abs ^Integer (- x1 x2))
             (* 2 (+ left-extent right-extent))))))

(defn move-next! [active-window-dimensions frame-dimensions centering-options]
  (let [nearest (->> centering-options
                     (partition 2 1)
                     (some (fn [[option next-option]]
                             (and (-dimensions-match?
                                    active-window-dimensions
                                    (-adjust-dimensions option frame-dimensions)
                                    frame-dimensions)
                                  next-option))))
        next-dimensions (or nearest (first centering-options))]
    (move-active-window! next-dimensions frame-dimensions)))

(comment
  ; required binaries:
  ;   * xprop
  ;     * provides dimensions of the virtual workspace - i.e. top panel height
  ;     * provides dimensions of active windoe frame extents i.e. thickness of title bar
  ;   * xrandr
  ;     * provides physical screen dimensions
  ;   * wmctrl
  ;     * provides sizing and movement of active window
  ;
  ; optional binaries:
  ;   * xdpyinfo (not used)
  ;     * provides virtual screen dimensions
  ;   * xdotool (not used)
  ;     * provides sizing and movement of active window
  ;   *xwininfo (not used)
  ;     * provides absolute positioning and measurements of specified window

  (restore-active-window!)
  (xprop-root!)
  (parse-wmctrl-window-dimensions-line "0x01400b81  1 74   127  577  717  dev Terminal - james@dev: ~")
  (get-active-window-dimensions)
  )
