(ns github-client.core
  (:require [clojure.string :as string])
  (:require [clj-http.client :as client])
  (:require [clj-time.core :as time])
  (:use [clj-time.format])
  (:use [clojure.data.json :only (read-json json-str)])
  (:use [clojure.java.io]))

(defmulti event-str (fn [type & _] type))
(def date-formatter (formatter "yyyy-MM-dd HH:mm:ss"))

(defn parse-date [msg]
  (let [msg-date (parse date-formatter msg)
	now (time/now)
        minutes (time/in-minutes (time/interval msg-date now))]
    (cond
     (> minutes (* 60 24)) (format "%3s days    ago" (int (/ minutes (* 60 24))))
     (> minutes 60) (format "%3s hours   ago" (int (/ minutes 60)))
     :else (format "%3s minutes ago" minutes))))

(defn usage []
  (println "github-cleint [event|gist]"))

(defn event []
  (let [resp (:body (client/get "https://api.github.com/users/xumingming/received_events?page=1&per_page=10"))
	resp (read-json resp)]
    (doseq [event resp]
      (let [actor (:login (:actor event))
	    type (:type event)
	    repo (:repo event)
	    ts   (string/replace (string/replace (:created_at event) "T" " ") "Z" "")
	    ts   (parse-date ts)
	    payload (:payload event)
	    msg (event-str type actor repo ts payload)
	    ]
	(if (not (nil? msg))
	  (println msg))))))

(defmethod event-str "WatchEvent" [type actor repo ts payload]
	   (format "%-12s %-12s %s watching repo: %s" ts actor (:action payload)  (:name repo)))

(defmethod event-str "IssueCommentEvent" [type actor repo ts payload]
	   (let [issue (:issue payload)
		 comment (:comment payload)
		 issue-num (:number issue)
		 repo-name (:name repo)]
	     (format "%-12s %-12s commented on issue %s on %s" ts actor issue-num repo-name)))

(defmethod event-str "PushEvent" [type actor repo ts payload]
	   (let [ref (:ref payload)
		 repo-name (:name repo)]
	     (format "%-12s %-12s pushed to %s at %s" ts actor ref repo-name)))

	   
(defmethod event-str :default [type actor repo ts payload]
		   nil)
(defn gist-create []
  (let [gist {:description "test gist"
			 :public :true
			  :files {"file1.txt" {:content "just a test"}}}]
	(client/post "https://api.github.com/gists"
				 {:body (json-str gist)
				  :content-type :json})))

(defn test-it [file-path]
  (with-open [rdr (reader file-path)]
	(loop [line (.readLine rdr)]
	  (if (not (nil? line))
	    (println line)
	    (recur (.readLine rdr))))))  
(defn -main [& args]
  (let [command (nth args 0)]
    (println "command is" command)
    (condp = command
	  "event" (event)
	  "gist"  (gist-create)
	  "test"  (let [file-path (nth args 1)]
				(test-it file-path))
	   (usage))))
