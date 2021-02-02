(ns wpad.wcenter
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
  (let [lines (str/split-lines ss)
        active-window-id (->> lines
                              (filter (fn [s] (.startsWith s "_NET_ACTIVE_WINDOW(WINDOW)")))
                              (first)
                              (parse-xprop-active-window-line))]
    active-window-id))

(def wmctrl-lG-pattern #"(0x[a-f0-9]+)\s+\d+\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+).*")

(defn parse-wmctrl-window-dimensions-line [s]
  (when-let [match (re-matches wmctrl-lG-pattern s)]
    (let [x (get match 2)
          y (get match 3)
          width (get match 4)
          height (get match 5)]
      {:x      (Integer/parseInt x)
       :y      (Integer/parseInt y)
       :width  (Integer/parseInt width)
       :height (Integer/parseInt height)})))

(defn parse-wmctrl-window-dimensions [ss active-window-id]
  (let [lines (str/split-lines ss)
        active-window-dimensions (->> lines
                                      (filter (fn [s] (.startsWith (.toLowerCase s) (.toLowerCase active-window-id))))
                                      (first)
                                      (parse-wmctrl-window-dimensions-line))]
    active-window-dimensions))

(defn get-active-window-dimensions []
  (let [xprop-root-result (xprop-root!)
        xprop-root-out (:out xprop-root-result)
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        wmctrl-result (sh "wmctrl" "-lG")
        wmctrl-out (:out wmctrl-result)
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
  (let [xrandr-result (sh "xrandr" "--listmonitors")
        xrandr-out (:out xrandr-result)
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
  (let [lines (str/split-lines ss)
        workspace-area (->> lines
                            (filter (fn [s] (.startsWith s "_NET_WORKAREA(CARDINAL)")))
                            (first)
                            (parse-xprop-workarea-line))]
    workspace-area))

(defn get-workspace-area []
  (let [xprop-result (sh "xprop" "-root")
        xprop-out (:out xprop-result)
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
  (let [lines (str/split-lines ss)
        frame-extents (->> lines
                           (filter (fn [s] (.startsWith s "_NET_FRAME_EXTENTS(CARDINAL)")))
                           (first)
                           (parse-xprop-frame-extents-line))]
    frame-extents))

(defn get-frame-dimensions []
  (let [xprop-root-result (sh "xprop" "-root")
        xprop-root-out (:out xprop-root-result)
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        xprop-id-result (sh "xprop" "-id" active-window-id)
        xprop-id-out (:out xprop-id-result)
        frame-extents (parse-xprop-frame-extents xprop-id-out)]
    frame-extents))

(defn get-centered-coordinates [{window-x :x}
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
        new-x (+ (/ screen-width 4) screen-x-offset)
        new-y 0
        new-width (/ screen-width 2)
        new-height workspace-height]
    {:x      (int new-x)
     :y      (int new-y)
     :width  (int new-width)
     :height (int new-height)}))

(defn restore-active-window! []
  (as-> "wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz" $
        (str/split $ #" ")
        (apply sh $)))

(defn center-active-window! [{:keys [x y width height]} {:keys [top-extent bottom-extent]}]
  ; removed maximized flags, otherwise resizing does not work
  (restore-active-window!)
  (let [w width
        h (- height top-extent bottom-extent)]
    (as-> (format "wmctrl -r :ACTIVE: -e 0,%s,%s,%s,%s" x y w h) $
          (str/split $ #" ")
          (apply sh $))))

(defn -main []
  (let [active-window-dimensions (get-active-window-dimensions)
        screens-dimensions (get-screens-dims)
        workspace-dimensions (get-workspace-area)
        frame-dimensions (get-frame-dimensions)
        centered-coordinates (get-centered-coordinates active-window-dimensions screens-dimensions workspace-dimensions)]
    (center-active-window! centered-coordinates frame-dimensions)))

(-main)

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

  (restore-active-window!)
  (xprop-root!)
  (parse-wmctrl-window-dimensions-line "0x01400b81  1 74   127  577  717  dev Terminal - james@dev: ~")
  (get-active-window-dimensions)
  (-main)
  )