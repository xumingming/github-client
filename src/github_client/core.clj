(ns github-client.core
  (:use [clojure.string :only (replace)])
  (:require [clj-http.client :as client])
  (:use [cheshire.core])
  (:use [clj-time.core])
  (:use [clj-time.format])
  (:use [clojure.data.json :only (read-json json-str)]))

(defmulti event-str (fn [type & _] type))
(def date-formatter (formatter "yyyy-MM-dd HH:mm:ss"))

(defn parse-date [msg]
  (let [msg-date (parse date-formatter msg)
	now (now)
        minutes (in-minutes (interval msg-date now))]
    (cond
     (> minutes (* 60 24)) (format "%s days ago" (int (/ minutes (* 60 24))))
     (> minutes 60) (format "%s hours ago" (int (/ minutes 60)))
     :else (format "%s minutes ago" minutes))))

(defn -main []
  (let [resp (:body (client/get "https://api.github.com/users/xumingming/received_events?page=1&per_page=10"))
	resp (read-json resp)]
    (doseq [event resp]
      (let [actor (:login (:actor event))
	    type (:type event)
	    repo (:repo event)
	    ts   (replace (replace (:created_at event) "T" " ") "Z" "")
	    ts   (parse-date ts)
	    payload (:payload event)
	    msg (event-str type actor repo ts payload)
	    ]
	(if (not (nil? msg))
	  (println msg))))))

(defmethod event-str "WatchEvent" [type actor repo ts payload]
	   (format "%s %s watching repo: %s %s" actor (:action payload)  (:name repo) ts))

(defmethod event-str "IssueCommentEvent" [type actor repo ts payload]
	   (let [issue (:issue payload)
		 comment (:comment payload)
		 issue-num (:number issue)
		 repo-name (:name repo)]
	     (format "%s commented on issue %s on %s %s" actor issue-num repo-name ts)))

(defmethod event-str "PushEvent" [type actor repo ts payload]
	   (let [ref (:ref payload)
		 repo-name (:name repo)]
	     (format "%s pushed to %s at %s %s" actor ref repo-name ts)))

	   
(defmethod event-str :default [type actor repo ts payload]
	   nil)

