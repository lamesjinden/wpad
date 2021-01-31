(ns wpad.wcenter
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def active-window-pattern #"^(\w+)=(\d+)")

(defn parse-active-window-dimensions [ss]
  (->> ss
       (str/split-lines)
       (map (fn [l]
              (when-let [match (re-matches active-window-pattern l)]
                (let [name (get match 1)
                      value (Integer/parseInt (get match 2))]
                  [name value]))))))

(defn get-active-window-dimensions []
  (let [xdotool-result (sh "xdotool" "getactivewindow" "getwindowgeometry" "--shell")
        xdotool-out (:out xdotool-result)
        dimensions (into {} (parse-active-window-dimensions xdotool-out))]
    {:width  (get dimensions "WIDTH")
     :height (get dimensions "HEIGHT")
     :x      (get dimensions "X")
     :y      (get dimensions "Y")}))

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
                      width (get match 2)
                      height (get match 3)]
                  {:monitor-id monitor-id
                   :width      (Integer/parseInt width)
                   :height     (Integer/parseInt height)}))))
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
    (let [x (get match 1)
          y (get match 2)
          width (get match 3)
          height (get match 4)]
      {:x      (Integer/parseInt x)
       :y      (Integer/parseInt y)
       :width  (Integer/parseInt width)
       :height (Integer/parseInt height)})))

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

(def xprop-active-window-id-pattern #"_NET_ACTIVE_WINDOW\(WINDOW\): window id # (0x[a-f0-9]+),.*")

(defn parse-xprop-active-window-id-line [s]
  (when-let [match (re-matches xprop-active-window-id-pattern s)]
    (let [window-id (get match 1)]
      window-id)))

(defn parse-xprop-active-window-id [ss]
  (let [lines (str/split-lines ss)
        active-window-id (->> lines
                              (filter (fn [s] (.startsWith s "_NET_ACTIVE_WINDOW(WINDOW)")))
                              (first)
                              (parse-xprop-active-window-id-line))]
    active-window-id))

(def xprop-frame-extents-pattern #"_NET_FRAME_EXTENTS\(CARDINAL\) = (\d+), (\d+), (\d+), (\d+)")

(defn parse-xprop-frame-extents-line [s]
  (when-let [match (re-matches xprop-frame-extents-pattern s)]
    (let [left-extent (get match 1)
          right-extent (get match 2)
          top-extent (get match 3)
          bottom-extent (get match 4)]
      {:left-extent   (Integer/parseInt left-extent)
       :right-extent  (Integer/parseInt right-extent)
       :top-extent    (Integer/parseInt top-extent)
       :bottom-extent (Integer/parseInt bottom-extent)})))

(defn parse-xprop-frame-extents [ss]
  (let [lines (str/split-lines ss)
        frame-extents (->> lines
                           (filter (fn [s] (.startsWith s "_NET_FRAME_EXTENTS(CARDINAL)")))
                           (first)
                           (parse-xprop-frame-extents-line))]
    frame-extents))

(defn get-frame-dims []
  (let [xprop-root-result (sh "xprop" "-root")
        xprop-root-out (:out xprop-root-result)
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        xprop-id-result (sh "xprop" "-id" active-window-id)
        xprop-id-out (:out xprop-id-result)
        frame-extents (parse-xprop-frame-extents xprop-id-out)]
    frame-extents))

(defn get-centered-coords [{window-x :x}
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

(defn center-active-window [{:keys [x y width height]} {:keys [top-extent bottom-extent]}]
  (sh "xdotool"
      "getactivewindow"
      "windowsize" (str width) (str (- height top-extent bottom-extent))
      "windowmove" (str x) (str y)))

(defn -main []
  (let [active-window-dims (get-active-window-dimensions)
        screens-dims (get-screens-dims)
        workspace-dims (get-workspace-area)
        frame-dims (get-frame-dims)
        centered-coords (get-centered-coords active-window-dims screens-dims workspace-dims)]
    (center-active-window centered-coords frame-dims)))

(-main)

(comment
  ; required binaries:
  ;   * xprop
  ;     * provides dimensions of the virtual workspace - i.e. top panel height
  ;     * provides dimensions of active windoe frame extents i.e. thickness of title bar
  ;   * xdotool
  ;     * provides sizing and movement of active windowo
  ;   * xrandr
  ;     * provides physical screen dimensions
  ;
  ; optional binaries:
  ;   * xdpyinfo (not used)
  ;     * provides virtual screen dimensions
  )
