(ns wpad.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(def logfile-path "/home/james/bin/wpad/log.txt")

(defn log [s]
  ;(spit logfile-path (str s "\n") :append true)
  )

(def get-xprop-root (fn [] (sh "xprop" "-root")))

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

(def frame-extents-gtk-prefix "_GTK_FRAME_EXTENTS(CARDINAL)")
(def frame-extents-net-prefix "_NET_FRAME_EXTENTS(CARDINAL)")

(defn parse-xprop-frame-extents [ss]
  (let [frame-extents-lines (->> ss
                                 (str/split-lines)
                                 (filter #(or (.startsWith % frame-extents-gtk-prefix)
                                              (.startsWith % frame-extents-net-prefix))))
        frame-extents-line (or (some #(and (.startsWith % frame-extents-gtk-prefix) %) frame-extents-lines)
                               (some #(and (.startsWith % frame-extents-net-prefix) %) frame-extents-lines))
        frame-extents (or (parse-xprop-frame-extents-line frame-extents-line)
                          {:left-extent   0
                           :right-extent  0
                           :top-extent    0
                           :bottom-extent 0})
        extents-type (cond
                       (nil? frame-extents-line) :none
                       (.startsWith frame-extents-line frame-extents-gtk-prefix) :gtk
                       (.startsWith frame-extents-line frame-extents-net-prefix) :net
                       :else :none)]
    (assoc frame-extents :extents-type extents-type)))

(defn restore-active-window! []
  (as-> "wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz" $
        (str/split $ #" ")
        (apply sh $)))

(defn move-active-window! [{:keys [x y width height] :as _window-dimensions} _frame-extents]
  ; remove maximized flags, otherwise resizing has no effect
  (restore-active-window!)
  (log (format "x: %s; y: %s; width: %s; height: %s" x y width height))
  (as-> (format "wmctrl -r :ACTIVE: -e 0,%s,%s,%s,%s" x y width height) $
        (str/split $ #" ")
        (apply sh $)))

(defn get-containing-screen [{_window-x :x}
                             {monitor-descriptions :monitor-descriptions}]
  ; find which screen the current window's x coordinate lies within.
  ; use found screen to provide screen-width.
  ; assume that iteration order of screens is correct.
  ;   i.e. screen 0 begins at x = 0, screen 1 begins at x = 1 + <WIDTH-OF-SCREEN-0>
  (-> monitor-descriptions
      (first)
      (assoc :x-offset 0)))

(defn parse-normal-hint-value [hint-value]
  ; examples:
  ;   program specified minimum size: 387 by 145
  ;   program specified resize increment: 6 by 13
  ;   program specified base size: 66 by 101

  (let [split (str/split hint-value #" by ")
        width (Integer/parseInt (str/trim (first split)))
        height (Integer/parseInt (str/trim (second split)))]
    {:hint-width  width
     :hint-height height}))

(def hint-minimum-size-token "program specified minimum size")
(def hint-resize-increment-token "program specified resize increment")
(def hint-base-size-token "program specified base size")
(defn parse-xprop-normal-hints [ss]
  (->> ss
       (str/split-lines)
       (map #(str/split % #":"))
       (filter #(= 2 (count %)))
       (reduce
         (fn [acc [left right]]
           (let [trim-left (str/trim left)
                 trim-right (str/trim right)]
             (cond
               (= trim-left hint-minimum-size-token) (assoc acc :minimum-size (parse-normal-hint-value trim-right))
               (= trim-left hint-resize-increment-token) (assoc acc :resize-increment (parse-normal-hint-value trim-right))
               (= trim-left hint-base-size-token) (assoc acc :width (parse-normal-hint-value trim-right))
               :else acc)))
         {})))

(defn get-environment []
  (let [xprop-root-out (:out (get-xprop-root))
        active-window-id (parse-xprop-active-window-id xprop-root-out)
        xwininfo-out (:out (sh "xwininfo" "-id" active-window-id))
        active-window-dimensions (parse-xwininfo-window-dimensions xwininfo-out)
        xprop-id-out (:out (sh "xprop" "-id" active-window-id))
        frame-extents (parse-xprop-frame-extents xprop-id-out)
        xrandr-out (:out (sh "xrandr" "--listmonitors"))
        monitors (parse-xrandr xrandr-out)
        workspace-area (parse-xprop-workspace-area xprop-root-out)
        normal-hints (parse-xprop-normal-hints xprop-id-out)]
    {:window    {:id            active-window-id
                 :dimensions    active-window-dimensions
                 :frame-extents frame-extents
                 :hints         normal-hints}
     :workspace workspace-area
     :monitors  monitors}))

(defn resize-dimensions [{width  :width
                          height :height
                          :as    dimensions}
                         {:keys [hint-width hint-height] :as _resize-increments}
                         {left-extent  :left-extent
                          right-extent :right-extent
                          :as          _frame-dimensions}
                         {workspace-width :width
                          :as             _workspace-dimensions}]
  (let [total-width (+ workspace-width left-extent right-extent)
        next-width (as-> (+ (- width (mod width hint-width)) hint-width) $
                         ; resize down if adjusted size exceeds screen dimensions
                         (if (> $ total-width)
                           (- $ hint-width)
                           $))
        next-height (+ (- height (mod height hint-height)) hint-height)
        next-dimensions (-> dimensions
                            (assoc :width next-width)
                            (assoc :height next-height))]
    next-dimensions))

(defn move-next!
  [{{{active-window-width :width
      :as                 _active-window-dimensions} :dimensions
     frame-dimensions                                :frame-extents
     {resize-increments :resize-increment
      :as               _active-window-hints}        :hints} :window
    workspace-dimensions                                     :workspace
    :as                                                      _environment}
   placement-options]
  (let [resized-placements (if resize-increments
                             (->> placement-options
                                  (map #(resize-dimensions % resize-increments frame-dimensions workspace-dimensions)))
                             placement-options)
        nearest (->> resized-placements
                     (partition 2 1)
                     (some (fn [[{option-width :width :as _option}
                                 {next-width :width :as next-option}]]
                             (and
                               (>= active-window-width option-width)
                               (< active-window-width next-width)
                               next-option))))
        next-dimensions (or nearest (first resized-placements))]
    (move-active-window! next-dimensions frame-dimensions)))

(comment
  ; required binaries:
  ;   * xprop
  ;     * provides dimensions of the virtual workspace - i.e. top panel height
  ;     * provides dimensions of active window frame extents i.e. thickness of title bar
  ;   * xrandr
  ;     * provides physical screen dimensions
  ;   * wmctrl
  ;     * used to un-maximize
  ;     * provides sizing and movement of active window
  ;   * xwininfo
  ;     * provides absolute positioning and measurements of specified window
  ;     * used to get active window geometry
  ;
  ; optional binaries:
  ;   * xdpyinfo (not used)
  ;     * provides virtual screen dimensions
  ;   * xdotool (not used)
  ;     * provides sizing and movement of active window
  )
