(ns carbon-rect.core
  (:require [carbon.rx :as rx :include-macros true]
            [carbon.vdom :as vdom]))

(enable-console-print!)

(def initial-state
  {:vertices []
   :edges {}
   :count 100})

(defonce state (rx/cell initial-state))

(defn random-point [n]
  [(rand n) (rand n)])

(defn generate-points [n size]
  (repeatedly n #(random-point size)))

(defn sqr [x] (* x x))

(defn dist
  ([[x y]]
   (+ (sqr x) (sqr y)))
  ([[x1 y1] [x2 y2]]
   (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

(defn connect-vertices1 [vs]
  (let [vs (sort-by dist vs)]
    (loop [edges {} vs vs]
      (let [[v1 v2] vs]
        (if v2
          (recur (assoc edges v1 v2) (rest vs))
          edges)))))

(defn connect-vertices [vs]
  (let [vs (sort-by #(dist [(rand-int 800) (rand-int 800)]) vs)]
    (loop [edges {} vs vs]
      (let [[v1 v2] vs]
        (if v2
          (recur (assoc edges v1 v2) (sort-by #(dist v2 %) (rest vs)))
          edges)))))

(defn bezier [{[sx sy] :start [ex ey] :end :as props}]
  [:path
   (merge
    {:d (str "M" sx "," sy
             " C" (- sx (rand-int 20)) "," (- sy (rand-int 20))
             " "  (+ ex (rand-int 20)) "," (+ ey (rand-int 20))
             " "  ex "," ey)}
    (dissoc props :start :end))])

(defn app []
  [:div.flex
   [:div
    [:input
     {:type "number"
      :style {:width "6em"}
      :min 10
      :max 1000
      :value (get @state :count)
      :on-input #(swap! state assoc :count (-> % .-target .-value (js/parseInt 10)))}]
    [:button
     {:on-click #(rx/dosync
                  (swap! state assoc :vertices (generate-points (get @state :count) 800))
                  (swap! state assoc :edges {}))}
     "Generate"]
    [:button
     {:on-click #(swap! state assoc :edges (connect-vertices (get @state :vertices)))}
     "Connect"]]
   [:svg
    {:style {:width "800px"
             :height "800px"}
     :xmlns "http://www.w3.org/2000/svg"
     :width 800
     :height 800}
    (for [[i [start end]] (map-indexed vector (get @state :edges))]
      ^{:key i}
      [bezier
       {:start start
        :end end
        :fill "#000000"
        :stroke-width 1
        :stroke "orange"}])
    (for [[i [x y]] (map-indexed vector (get @state :vertices))]
      ^{:key i}
      [:circle
       {:cx x
        :cy y
        :r (+ 3 (rand-int 7))
        :fill (rand-nth ["#000000" "#ffaa00" "#dd00ff"])
        }])]])

(vdom/mount [app] (js/document.getElementById "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
