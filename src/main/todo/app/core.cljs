(ns todo.app.core
  (:require [reagent.core :as r] 
            [reagent.dom :as rdom]
            [clojure.string :as str]
            [cljs.pprint :as pp]
            [cljs.reader :as reader]))




;;--- App State ----

(defonce todos (r/atom (sorted-map)))




;;--- Local Storage ----

(def local-store-key "todo-app")

(defn todos->local-store []
(.setItem js/localStorage local-store-key (str @todos)))

(defn local-store->todos []
   (let [edn-map-todos (.getItem js/localStorage local-store-key)
   unsorted-todos (some->> edn-map-todos reader/read-string)
   sorted-todos (into (sorted-map) unsorted-todos)]
   (reset! todos sorted-todos)))

   

;;--- Watch the State ----

(add-watch todos :todos
    (fn[key _atom _old-state new-state]
    (todos->local-store)
    (println "---" key "atom changed ---")
    (pp/pprint new-state)))

;;--- Utilities ----

(defn allocate-next-id [todos]
      ((fnil inc 0) (last (keys todos))))


(defn add-todo [text]
  (let [id (allocate-next-id @todos)
  new-todo {:id id, :title text, :done false }]
  (swap! todos assoc id new-todo)))

  (defn toggle-done [id]
    (swap! todos update-in [id :done] not))

    (defn save-todo [id title]
    (swap! todos assoc-in [id :title] title))


  (defn delete-todo [id]
  (swap! todos dissoc id))



;;--- Initialize App with sample Data ----

#_(defonce init (do
      (add-todo "Do laundry")
       (add-todo "Learn Clojure")
        (add-todo "Buy book")))

;;--- Views ----

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [input-text (r/atom title)
      update-text #(reset! input-text %)
      stop #(do (reset! input-text "")
      (when on-stop (on-stop)))
      save #(let [trimmed-text (-> @input-text str str/trim)]
         (if-not (empty? trimmed-text) (on-save trimmed-text))
         (stop))
         key-pressed #(case %
                          "Enter" (save)
                          "Esc" (stop)
                          "Escape" (stop)
                          nil)] 
    (fn [{:keys [class placeholder]}]  
 [:input {:class class
          :placeholder placeholder
          :type "text"
          :value @input-text
          :on-blur save
          :on-change #(update-text (.. % -target -value))
          :on-key-down #(key-pressed (.. % -key))}])))

  (defn todo-item [_props-map]
    (let [editing (r/atom false)]
    (fn [{:keys [id title done]}]
    [:li {:class (str (when done "completed")
                  (when @editing "editing"))}
    [:div.view
    [:input {:class "toggle"
            :type "checkbox"
            :checked done
            :on-change #(toggle-done id)}]
     [:label {:on-double-click #(reset! editing true)} title]
     [:button.destroy {:on-click #(delete-todo id)}]]
     (when @editing 
     [todo-input {:class "edit"
                  :title title
                  :on-save (fn [text] (save-todo id text))
                  :on-stop #(reset! editing false)}])])))


  (defn task-list []
    (let [items (vals @todos)]
    [:section.main
    [:ul.todo-list
      (for [todo items]
        ^{:key (:id todo)} [todo-item todo])]]))

  (defn footer-controls []
  [:footer.footer
    [:div "Footer Controls"]
  ]
  )

(defn task-entry []
  [:header.header
  [:h1 "todos"]
  [todo-input {:class "new-todo"
                :placeholder "What needs to be done?"
                :on-save add-todo}]])


(defn todo-app []
  [:div
    [:section.todoapp
        [task-entry]
        (when (seq @todos) 
        [:div
          [task-list]
          [footer-controls]])]
        [:footer.info
        [:p "Double Click to edit a todo"]]])


  ;;--- Render ----

(defn render []
  (rdom/render [todo-app] (.getElementById js/document "root")))

(defn ^:export main []
(local-store->todos)
  (render))

(defn ^:dev/after-load reload! []
  (render))
