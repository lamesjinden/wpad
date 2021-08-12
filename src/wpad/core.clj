(ns wpad.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def logfile-path "/home/james/bin/wpad/log.txt")

(defn log [s]
  ;(spit logfile-path (str s "\n") :append true)
  )

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

; region replace get-active-window-dimensions

(def xwininfo-upper-left-x-token "Absolute upper-left X")
(def xwininfo-upper-left-y-token "Absolute upper-left Y")
(def xwininfo-width-token "Width")
(def xwininfo-height-token "Height")

(defn parse-xwininfo-window-dimensions [ss]
  (->> ss
       (str/split-lines)
       (map #(str/split % #":"))
       (filter #(= 2 (count %)))
       (reduce
         (fn [acc [left right]]
           (let [trim-left (str/trim left)
                 trim-right (str/trim right)]
             (cond
               (= trim-left xwininfo-upper-left-x-token) (assoc acc :x (Integer/parseInt trim-right))
               (= trim-left xwininfo-upper-left-y-token) (assoc acc :y (Integer/parseInt trim-right))
               (= trim-left xwininfo-width-token) (assoc acc :width (Integer/parseInt trim-right))
               (= trim-left xwininfo-height-token) (assoc acc :height (Integer/parseInt trim-right))
               :else acc)))
         {})))

(defn get-active-window-dimensions []
  (let [xprop-root-out (:out (xprop-root!))
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        xwininfo-out (:out (sh "xwininfo" "-id" active-window-id))
        active-window-dimensions (parse-xwininfo-window-dimensions xwininfo-out)]
    (assoc active-window-dimensions :window-id active-window-id)))

; endregion

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

;_GTK_FRAME_EXTENTS(CARDINAL) = 26, 26, 23, 29
;_NET_FRAME_EXTENTS(CARDINAL) = 0, 0, 37, 0
(def xprop-frame-extents-pattern #".*_FRAME_EXTENTS\(CARDINAL\) = (\d+), (\d+), (\d+), (\d+)")

(defn parse-xprop-frame-extents-line [s]
  (when s
    (when-let [match (re-matches xprop-frame-extents-pattern s)]
      (let [left-extent (Integer/parseInt (get match 1))
            right-extent (Integer/parseInt (get match 2))
            top-extent (Integer/parseInt (get match 3))
            bottom-extent (Integer/parseInt (get match 4))]
        {:left-extent   left-extent
         :right-extent  right-extent
         :top-extent    top-extent
         :bottom-extent bottom-extent}))))

; todo if _gtk_frame_extents is missing, try _net_frame_extents
(defn parse-xprop-frame-extents [ss]
  (->> ss
       (str/split-lines)
       (filter #(or (.startsWith % "_GTK_FRAME_EXTENTS(CARDINAL)")
                    (.startsWith % "_NET_FRAME_EXTENTS(CARDINAL)")))
       (first)
       (parse-xprop-frame-extents-line)))

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

(defn move-active-window! [{:keys [x y width height] :as _window-dimensions} _frame-extents]
  ; remove maximized flags, otherwise resizing has no effect
  (restore-active-window!)
  (as->
    (format "xdotool getactivewindow windowsize %s %s windowmove %s %s" width height x y) $
    (str/split $ #" ")
    (apply sh $)))

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

(defn move-next! [active-window-dimensions frame-dimensions placement-options]
  (let [nearest (->> placement-options
                     (partition 2 1)
                     (some (fn [[option next-option]]
                             (and
                               (>= (:width active-window-dimensions) (:width option))
                               (< (:width active-window-dimensions) (:width next-option))
                               next-option))))
        next-dimensions (or nearest (first placement-options))]
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
