(ns github-client.core
  (:require [clj-http.client :as client])
  (:use [clojure.pprint])
  (:use [cheshire.core])
  (:use [clojure.data.json :only (read-json json-str)]))

(defmulti event-str (fn [type & _] type))

(defn -main []
  (let [resp (:body (client/get "https://api.github.com/users/xumingming/received_events"))
	resp (read-json resp)]
    (doseq [event resp]
      (let [actor (:login (:actor event))
	    type (:type event)
	    repo (:repo event)
	    ts   (:created_at event)
	    payload (:payload event)]
        (println (event-str type actor repo ts payload))))))

(defmethod event-str "WatchEvent" [type actor repo ts payload]
	   (println actor (:action payload) "watching repo: " (:name repo)))

(defmethod event-str "IssueCommentEvent" [type actor repo ts payload]
	   (println "IssueCommentEvent"))

(defmethod event-str :default [type actor repo ts payload]
	   (println "default: " type))

