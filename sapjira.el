;;; sapjira.el --- Log time in SAP Jira.

;; Copyright (C) 2015 Vincent Zhang

;; Author: Vincent.Zhang <vincent.zhang@sap.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Org-mode for SAPJIRA is used to log work time for issues or tasks.
;; There are 5 interactive commands in total:
;; 1. sapjira-login: Login SAPJIRA with your global SAP account & password
;; 2. sapjira-fetch: Fetch all the open sprints and issues. Closed ones are
;;    filterred. The sprints and issues will be converted to a fix org-mode
;;    format. You must first have a local ".org" file with following lines
;;    in the page head:
;;      #+STARTUP: align
;;      #+TODO: TODO | OPEN DONE
;;      #+PROJECT: <Your project name>
;;    sapjira-fetch only fetch Sprints and Issues defination information.
;;    Those worklog information is not downloaded. For example: If you
;;    have logged your work through SAPJIRA web UI, then these changes will
;;    not be downloaded and merged.
;; 3. sapjira-push: Push all your un-logged work log items to SAPJIRA. If
;;    Your set your task to status DONE, it will also change the task status
;;    on SAPJIRA. According to the http callback response, sapjira-push will
;;    also sync the status in local org-mode file.
;; 4. sapjira-set-done: You can manually set a task to status DONE.
;;    The SAPJIRA status and local org-mode file status will be changed to
;;    DONE correspondingly.
;; 5. sapjira-re-open: Reverse operation of sapjira-set-done. If you
;;    occursionly what to add some work logs to an already done task, then
;;    you should first reopen it. The SAPJIRA status and local org-mode file
;;    status will be changed correspondingly.
;;
;; SAPJIRA depends on sapwiki, so you should sapwiki.el in your load path. 

;;; Code:
(require 'sapwiki)
(require 'json)

(defcustom dk-sapjira-main-url
  "https://sapjira.wdf.sap.corp/"
  "The main URL of SAPWIKI"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-login-url
  (concat dk-sapjira-main-url "login.jsp" )
  "The url used to enter username and password"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-project-name
   "REALTIMECONSOLIDATION"
  "Jira project name"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-worklog-url
  (concat dk-sapjira-main-url "secure/CreateWorklog.jspa")
  "Jira browse issues in the project"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-workflow-dispatch-url
  (concat dk-sapjira-main-url "secure/WorkflowUIDispatcher.jspa")
  "Jira set the issue to DONE status"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-sprints-fetch-url
  (concat dk-sapjira-main-url
	  "rest/greenhopper/1.0/sprintquery/2442")
  "Jira fetch all the sprints"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-issues-fetch-url
  (concat dk-sapjira-main-url
	  "sr/jira.issueviews:searchrequest-xml/temp/SearchRequest.xml")
  "Jira browse issues in XML format"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defvar dk-sapjira-atl-token nil)
(defvar dk-sapjira-issue-log nil)
(defvar dk-sapjira-issue-logs nil)
(defvar dk-sapjira-work-buffer nil)
(defvar dk-sapjira-total-hours nil)

(defun sapjira-login (&optional callback)
  (interactive)
  (unless dk-sapwiki-pwd
    (setq dk-sapwiki-pwd (read-passwd "Enter your password: ")))
  (dk-url-http-post dk-sapjira-login-url
		    (list (cons "os_username" dk-sapwiki-user)
			  (cons "os_password" dk-sapwiki-pwd)
			  (cons "login" "Log In"))
		    'dk-sapjira-process-login))

(defun dk-sapjira-process-login (status &optional callback)
  (if (dk-sapjira-check-login-successfully)
      (progn
	(message "Login successfully")
	(dk-sapjira-get-token)
        (and (functionp callback)
	     (apply callback)))
    (setq dk-sapwiki-pwd nil)
    (error "Login failed")))

(defun dk-sapjira-check-login-successfully ()
  (set-buffer (current-buffer))
  (goto-char 1)
  (if (re-search-forward "Sorry, your username and password are incorrect - please try again." nil t)
      nil
    t))

(defun dk-sapjira-get-token ()
  "Get the SAPJIRA security token for further actions"
  (and (boundp 'url-cookie-secure-storage)
       (assoc "sapjira.wdf.sap.corp" url-cookie-secure-storage)
       (setq dk-sapjira-atl-token
	     (aref (nth 0 (cdr (assoc "sapjira.wdf.sap.corp" url-cookie-secure-storage))) 2))))

(defun sapjira-push ()
  "The function will first iterate all the open sprints, then the open tasks under the sprints. Finally, find all the non-logged work log and post to SAPJIRA. Afterwards, status will be changed."
  (interactive)
  (let ((project (dk-sapwiki-get-attribute-value "PROJECT")))
    (if (not project)
	(message "Please make sure you are in a SAPJIRA org file!")
      (setq dk-sapjira-project-name project)
      (dk-sapjira-do-open-sprints))))
	   
(defun dk-sapjira-logwork-internal (issue-id adjustEstimate &optional issue-num)
  "[(\"<2016-01-18 Mon>\" \"4h\" \"comments\").....]"
  (mapc (lambda (log-item)
	  (let ((startDate  (nth 0 log-item))
		(timeLogged (nth 1 log-item))
		(comment (nth 2 log-item)))
	    (dk-sapjira-logwork-internal-single
	     issue-id startDate timeLogged
	     adjustEstimate comment issue-num)))
	dk-sapjira-issue-logs))  

(defun dk-sapjira-logwork-internal-single
    (issue-id startDate timeLogged adjustEstimate comment &optional issue-num)
  (dk-url-http-post
   dk-sapjira-worklog-url
   (list (cons "inline" "true")
	 (cons "decorator" "dialog")
	 (cons "id" issue-id)
	 (cons "timeLogged" (concat timeLogged "h"))
	 (cons "startDate" (format-time-string
			    "%d/%b/%y %I:%M %p"
			    (date-to-time startDate)))
	 (cons "adjustEstimate" adjustEstimate)
	 (cons "comment" (concat comment " (via Emacs)"))
	 (cons "atl_token" dk-sapjira-atl-token))
   'dk-sapjira-process-logwork
   (list (or issue-num issue-id) startDate)))

(defun dk-sapjira-process-logwork (status issue-num startDate)
  (set-buffer (current-buffer))
  (goto-char 1)
  (if (re-search-forward "<div class=\"dialog-title hidden\"></div>" nil t)
      (progn
	(dk-sapjira-set-logged-flag startDate)
	(message "Work log of issue:%s on date:%s is successfully logged!"
		 issue-num
		 startDate))
    (message "Work logging failed for issue:%s on date:%s"
	     issue-num
	     startDate)))

(defun dk-sapjira-set-logged-flag (date)
  (mapcar (lambda (item)
	    (when (string= (car item) date)
	      (setcdr (cdr (cdr item)) '("X"))))
	  dk-sapjira-issue-logs))

(defun sapjira-set-done ()
  "Set task to the status DONE"
  (interactive)
  (let ((issue-id (org-entry-get nil "IssueID"))
	(issue-num (org-entry-get nil "IssueNum")))
    (if (not issue-id)
	(message "Please move your cursor to a Task!")
      (setq dk-sapjira-work-buffer (current-buffer))
      (dk-sapjira-set-done issue-id issue-num t))))

(defun dk-sapjira-set-done (issue-id issue-num &optional interactive)
  (dk-sapjira-dispatch-workflow
   "811"
   issue-id
   issue-num
   (lambda (status issue-num interactive)
     (if (dk-sapjira-check-session-expired)
	 (message "Session is expired, please re-logon!")
       (when interactive
	 (with-current-buffer dk-sapjira-work-buffer
	   (org-entry-put nil "TODO" "DONE")
	   (org-map-entries
	    (lambda ()
	      (unless (org-map-entries
		       (lambda ())
		       "+Type=\"Task\"/OPEN"
		       'tree)
		(org-entry-put nil "TODO" "DONE")))
	    (concat "+id=\""
		    (org-entry-get nil "Sprint")
		    "\"")
	    nil)))
       (message "Issue:%s is set to DONE!" issue-num)))
   t))

(defun sapjira-re-open ()
  (interactive)
  (let ((issue-id (org-entry-get nil "IssueID"))
	(issue-num (org-entry-get nil "IssueNum")))
    (if (not issue-id)
	(message "Please move your cursor to a Task!")
      (setq dk-sapjira-work-buffer (current-buffer))
      (dk-sapjira-re-open issue-id issue-num t))))

(defun dk-sapjira-re-open (issue-id issue-num &optional interactive)
  (dk-sapjira-dispatch-workflow
   "831"
   issue-id
   issue-num
   (lambda (status issue-num interactive)
     ;; (switch-to-buffer (current-buffer)))
     (if (dk-sapjira-check-session-expired)
	 (message "Session is expired, please re-logon!")
       (when interactive
	 (with-current-buffer dk-sapjira-work-buffer
	   (org-entry-put nil "TODO" "OPEN")
	   (org-map-entries
	    (lambda ()
	      (org-entry-put nil "TODO" "OPEN"))
	    (concat "+id=\""
		    (org-entry-get nil "Sprint")
		    "\"")
	    nil)))
       (message "Issue:%s is REOPENED!" issue-num)))
   t))

(defun dk-sapjira-dispatch-workflow
    (action issue-id issue-num callback &optional interactive)
  (dk-sapjira-http-get
   dk-sapjira-workflow-dispatch-url
   (list (cons "id" issue-id)
	 (cons "action" action)
	 (cons "atl_token" dk-sapjira-atl-token)
	 (cons "decorator" "dialog")
	 (cons "inline" "true")
	 (cons "_"  (format-time-string "%s%3N")))
   callback
   (list issue-num interactive)))

(defun dk-sapjira-check-session-expired ()
  (set-buffer (current-buffer))
  (goto-char 1)
  (if (re-search-forward
       "<title>.*Session.*Expired.*</title>" nil t)
      t
    nil))

(defun dk-sapjira-http-get (url args callback &optional cbargs)
  (let ((dk-sapwiki-block-auth t)
	(url-request-method "GET")
	(url-request-extra-headers
	 '(("X-Requested-With" . "XMLHttpRequest")
	   ("X-SITEMESH-OFF" . "off")))
	(query-string
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg))
			      "="
			      (url-hexify-string (cdr arg))))
		    args
		    "&")))
    (url-retrieve (concat url "?" query-string)
		  callback cbargs)))

(defun sapjira-fetch ()
  "Fetch the open sprints and issues, and convert them into a fix org-mode format."
  (interactive)
  (let ((project (dk-sapwiki-get-attribute-value "PROJECT")))
    (if (not project)
	(message "Please make sure you are in a SAPJIRA org file!")
      (setq dk-sapjira-project-name project)
      (setq dk-sapjira-work-buffer (current-buffer))
      (dk-sapjira-http-get
       dk-sapjira-sprints-fetch-url
       (list (cons "includeHistoricSprints" "false")
	     (cons "includeFutureSprints" "false")
	     (cons "_"  (format-time-string "%s%3N")))
       'dk-sapjira-process-sprints))))

(defun dk-sapjira-process-sprints (status)
  (let* ((json-object-type 'plist)
	 (json-plist
	  (json-read-from-string
	   (dk-sapjira-get-resp-json (current-buffer))))
	 (sprints
	  (plist-get json-plist :sprints)))
    (if (not sprints)
	(message "Session is timeout, please re-login!")
      (message "Sprints and Issues are fetched!")
      (dk-sapjira-process-sprints-internal sprints)
      (dk-sapjira-fetch-open-issues))))

(defun dk-sapjira-get-resp-json (response-buffer)
  (with-current-buffer response-buffer
    (goto-char 1)
    (re-search-forward "{.*}" nil t)
    (buffer-substring-no-properties
     (match-beginning 0)
     (match-end 0))))

(defun dk-sapjira-process-sprints-internal (sprints)
  (with-current-buffer dk-sapjira-work-buffer
    (mapc
     (lambda (sprint)
       (when (string= (plist-get sprint :state) "ACTIVE")
	 (unless (org-map-entries
		  (lambda ()
		    (message "Sprint: %s already exists!"
			     (plist-get sprint :name)))
		  (concat "+id=\"" (number-to-string 
				    (plist-get sprint :id))
			  "\"")
		  nil)
	   (goto-char (point-max))
	   (insert ?\n ?\n)
	   (insert "* OPEN " (plist-get sprint :name) ?\n)
	   (insert ":PROPERTIES:" ?\n)
	   (insert ":id: " (number-to-string
			    (plist-get sprint :id)))
	   (insert ?\n)
	   (insert ":Type: " "Sprint" ?\n)
	   (insert ":END:" ?\n))))
     sprints)))

(defun dk-sapjira-fetch-open-issues ()
  (dk-sapjira-http-get
   dk-sapjira-issues-fetch-url
   (list (cons "jqlQuery"
	       "assignee = currentUser() AND resolution = Unresolved ORDER BY updatedDate DESC")
	 (cons "tempMax" "2500"))
   'dk-sapjira-process-open-issues))

(defun dk-sapjira-process-open-issues (status)
  (set-buffer (current-buffer))
  (goto-char 1)
  (when (re-search-forward "<rss " nil t)
    (let* ((issue-list (libxml-parse-xml-region
			(match-beginning 0)
			(point-max)))
	   (channel (nth 2 issue-list)))
      (mapc 'dk-sapjira-process-open-issue-internal
	    channel))))

(defun dk-sapjira-process-open-issue-internal (issue)
  "The issues information is downloaded an XML format. It will be parsed and extract here."
  (when (and (listp issue)
	     (eq (car issue) 'item))
    (let (link key-id key-num summary type priority
	  status assignee reporter created updated
	  resolved due estimate1 estimate2 spent sprint)
      (mapc
       (lambda (item)
	 (when (listp item)
	   (pcase (car item)
	     ('link (setq link (nth 2 item)))
	     ('key (setq key-id (cdr (car (nth 1 item))))
		   (setq key-num (nth 1 (split-string (nth 2 item) "-"))))
	     ('summary (setq summary (nth 2 item)))
	     ('type (setq type (nth 2 item)))
	     ('priority (setq priority (nth 2 item)))
	     ('status (setq status (nth 2 item)))
	     ('assignee (setq assignee (nth 2 item)))
	     ('reporter (setq reporter (nth 2 item)))
	     ('created (setq created (nth 2 item)))
	     ('updated (setq updated (nth 2 item)))
	     ('resolved (setq resolved (nth 2 item)))
	     ('due (setq due (nth 2 item)))
	     ('timeoriginalestimate
	      (setq estimate1 (nth 2 item)))
	     ('aggregatetimeoriginalestimate
	      (setq estimate1 (nth 2 item)))
	     ('timeestimate (setq estimate2 (nth 2 item)))
	     ('aggregatetimeremainingestimate
	      (setq estimate2 (nth 2 item)))
	     ('timespent (setq spent (nth 2 item)))
	     ('customfields
	      (setq sprint
		    (dk-sapjira-get-customfield-value item "Sprint"))))))
       issue)
      (with-current-buffer dk-sapjira-work-buffer
	(unless (org-map-entries
		 (lambda ()
		   (message "Issue %s is already there!" key-num))
		 (concat "+IssueNum=\"" key-num "\"")
		 nil)
	  (org-element-map (org-element-parse-buffer)
	      'headline
	    (lambda (headline)
	      (when (string= (org-element-property :ID headline)
			     sprint)
		(let ((contents-begin
		       (org-element-property :contents-begin headline))
		      (contents-end
		       (org-element-property :contents-end headline)))
		  (goto-char contents-end)
		  (when (> (- contents-end contents-begin) 60)
		    (insert ?\n ?\n))
		  (insert "** OPEN " summary ?\n)
		  (insert ":PROPERTIES:" ?\n)
		  (insert ":IssueNum: " key-num ?\n)
		  (insert ":IssueID: " key-id ?\n)
		  (insert ":Sprint: " sprint ?\n)
		  (insert ":Type: " type ?\n)
		  (insert ":Priority: " priority ?\n)
		  (insert ":Estimate: " estimate1 ?\n)
		  (insert ":END:" ?\n)
		  (insert "[[" link "][" dk-sapjira-project-name
			  "-" key-num "]]" ?\n)
		  (dk-sapjira-insert-sprint-table created))))))))))		
;;       (message "key-id=%s\nkey-num=%s\nsummary=%s
;; type=%s\npriority=%s\nstatus=%s\nassignee=%s\nreporter=%s
;; created=%s\nupdated=%s\ndue=%s\nresolved=%s\nestimate1=%s
;; estimate2=%s\nspent=%s\nsprint=%s" key-id key-num summary type priority status assignee reporter created updated due resolved estimate1 estimate2 spent sprint))))

(defun dk-sapjira-get-customfield-value (customfields fieldname)
  (let ((result))
    (mapc
     (lambda (customfield)
       (when (listp customfield)
	 (when (string= (nth 2 (nth 2 customfield)) fieldname)
	   (dolist (fieldvalue (nth 3 customfield))
	     (when (listp fieldvalue)
	       (setq result (nth 2 fieldvalue)))))))
     customfields)
    result))

(defun dk-sapjira-insert-sprint-table (create-time)
  (insert "| | |<4>|<25>| |" ?\n)
  (insert "| | Day | LT: Log Time | Comments | L |" ?\n)
  (insert "|- " ?\n)
  (let* ((timestamp (format-time-string
		    (car org-time-stamp-formats)
		    (date-to-time create-time)))
	 (weekday (substring timestamp 12 15))
	 thisMonday)
    (pcase weekday
      ("Mon"
       (setq thisMonday timestamp))  
      ("Tue"
       (setq thisMonday (dk-sapjira-calc-day timestamp -1)))
      ("Wed"
       (setq thisMonday (dk-sapjira-calc-day timestamp -2)))
      ("Thu"
       (setq thisMonday (dk-sapjira-calc-day timestamp -3)))
      ("Fri"
       (setq thisMonday (dk-sapjira-calc-day timestamp -4)))
      ("Sat"
       (setq thisMonday (dk-sapjira-calc-day timestamp -5)))
      ("Sun"
       (setq thisMonday (dk-sapjira-calc-day timestamp -6))))
    (insert "|#|" thisMonday "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 1) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 2) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 3) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 4) "| | | |" ?\n)
    (insert "|- " ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 7) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 8) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 9) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 10) "| | | |" ?\n)
    (insert "|#|" (dk-sapjira-calc-day thisMonday 11) "| | | |" ?\n)
    (insert "|- " ?\n)
    (insert "|#|*TotalHours*| | | |" ?\n)
    (org-table-align)
    (insert "#+TBLFM: @13$3=vsum(@3$3..@12$3)" ?\n)))
    
(defun dk-sapjira-calc-day (timestamp days)
  (format-time-string (car org-time-stamp-formats)
		      (time-add (date-to-time timestamp)
				(seconds-to-time
				 (* 86400 days)))))
		       
(defun dk-sapjira-http-post (url args callback &optional cbargs)
  "Send ARGS to URL as a POST request."
  (let ((dk-sapwiki-block-auth t)
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")
	   ("X-Atlassian-Token" . "nocheck")
	   ("X-Requested-With" . "XMLHttpRequest")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg))
			      "="
			      (url-hexify-string (cdr arg))))
		    args
		    "&")))
    (url-retrieve url callback cbargs)))

;;------------------------------------------------------
;; Parse the fix org-mode format and extract open issues
;; to be posted to SAPJIRA. Only new work logs are extracted.
;;------------------------------------------------------
(defun dk-sapjira-do-open-sprints ()
  "Interate sprints with status OPEN."
  (org-map-entries
   'dk-sapjira-process-sprint
   "+Type=\"Sprint\"/OPEN"
   nil))

(defun dk-sapjira-process-sprint ()
  "Parse the sprint properties and tasks under it."
  (message "Sprint: %s" (org-entry-get nil "ITEM"))
  (dk-sapjira-do-open-tasks)
  (dk-sapjira-sprint-post-process))

(defun dk-sapjira-sprint-post-process ()
  "If all the tasks under the sprint are DONE, 
set the sprint DONE."
  (unless (org-map-entries
	   (lambda ())
	   "+Type=\"Task\"/OPEN"
	   'tree)
    (org-entry-put nil "TODO" "DONE")))

(defun dk-sapjira-do-open-tasks ()
  "Interate tasks with OPEN status."
  (org-map-entries
   'dk-sapjira-process-task
   "+Type={Task\\|Backlog Item}"
   'tree))
  
(defun dk-sapjira-process-task ()
  "Parse the task properties and the work log table under it.
Then call the SAPJIRA log work web service to post the results. Waiting callback give response."
  (setq dk-sapjira-issue-logs nil)
  (setq dk-sapjira-issue-log nil)
  ;;You must call "org-map-entries" here again to narrow the
  ;;buffer region, or org-element-map will iterate all the
  ;;table-row under current sprint.
  (org-map-entries
   (lambda ()
     (org-element-map (org-element-parse-buffer) 'table-row
       'dk-sapjira-process-table)
     ;; (message "Logged Issues: %s" dk-sapjira-issue-logs))
     (let* ((org-trust-scanner-tags t)
	    (issue-id (org-entry-get nil "IssueID"))
	    (issue-num (org-entry-get nil "IssueNum"))
	    (estimate
	     (string-to-number
	      (substring
	       (org-entry-get nil "Estimate") 0 -1)))
	    (todo (org-entry-get nil "TODO")))
       (when dk-sapjira-issue-logs
	 (message "Log work for task: %s"
		  (org-entry-get nil "ITEM"))
	 (if (and (string= todo "DONE")
		  dk-sapjira-total-hours
		  (> estimate dk-sapjira-total-hours))
	     (dk-sapjira-logwork-internal issue-id "leave" issue-num)
	   (dk-sapjira-logwork-internal issue-id "auto" issue-num))
	 (setq dk-sapjira-total-hours nil)
	 (dk-sapjira-waiting-for-success)
	 (org-element-map (org-element-parse-buffer) 'table-row
	   'dk-sapjira-table-post-process))
       (if (and (string= todo "DONE")
		(dk-sapjira-check-work-log-success))
	   (dk-sapjira-set-done issue-id issue-num))))  
   "+Type={Task\\|Backlog Item}"
   'tree))

(defun dk-sapjira-waiting-for-success ()
  "I don't know if there is an elegant way to do async-2-sync in emacs.
I use this shallow method to block emacs to waiting for parallel asynch
http calls finish."
  (sleep-for 0 200)
  (if (dk-sapjira-check-work-log-success)
      t
    (sleep-for 0 500)
    (if (dk-sapjira-check-work-log-success)
	t
      (sleep-for 1)
      (if (dk-sapjira-check-work-log-success)
	  t
	(sleep-for 2)
	(if (dk-sapjira-check-work-log-success)
	    t
	  nil)))))

(defun dk-sapjira-check-work-log-success ()
  "Check if all the work log items are successfully logged!"
  (if (not dk-sapjira-issue-logs)
      nil
    (catch 'Exit
      (dolist (issue-log dk-sapjira-issue-logs)
	(unless (nth 3 issue-log)
	  (throw 'Exit nil)))
      t)))

(defun dk-sapjira-process-table (table-row)
  "Parse the work log table row by row, cell by cell, to extract a list of un-logged work items. Meanwhile, the TotalHours is also extracted."
  (unless (eq (org-element-property :type table-row)
	      'rule)
    (org-element-map table-row 'table-cell
      (lambda (table-cell)
	(let* ((cell-contents (org-element-contents table-cell))
	       (current-cell (car cell-contents)))
	  (if (and current-cell
		   (eq (org-element-type current-cell)
		       'timestamp))
	      (push (org-element-property :raw-value
					  current-cell)
		    dk-sapjira-issue-log)
	    (when dk-sapjira-issue-log
	      (push current-cell dk-sapjira-issue-log))
	    (when (and (eq dk-sapjira-total-hours 'TotalHours)
		       current-cell)        
	      (setq dk-sapjira-total-hours
		    (string-to-number current-cell)))
	    (when (and current-cell
		       (eq (org-element-type current-cell) 'bold)
		       (string= (car (org-element-contents current-cell))
				"TotalHours"))
	      (setq dk-sapjira-total-hours 'TotalHours))))))
    (when dk-sapjira-issue-log
      (setq dk-sapjira-issue-log (nreverse dk-sapjira-issue-log))
      (when (and (car (cdr dk-sapjira-issue-log))
		 (not (nth 3 dk-sapjira-issue-log)))	
	(add-to-list 'dk-sapjira-issue-logs
		     dk-sapjira-issue-log t))
      (setq dk-sapjira-issue-log nil))))

(defun dk-sapjira-table-post-process (table-row)
  "After logged work in SAPJIRA, the org table should be updated according to the callback result. If a work log item is successfully logged, then set the last column to X, else nil."
  (unless (eq (org-element-property :type table-row)
	      'rule) 
    (goto-char (org-element-property :begin table-row))
    (let ((day (org-table-get nil 2)))
      (when (string-match
	     "<[0-9]\\{4\\}-[0-1][0-9]-[0-3][0-9] [[:alpha:]]\\{3\\}>"
	     day)
	(catch 'exit
	  (dolist (issue-log dk-sapjira-issue-logs)
	    (when (and (string= (car issue-log) day)
		       (nth 3 issue-log))
	      (org-table-put nil 5 (nth 3 issue-log) t)
	      (throw 'exit issue-log))))))))

(provide 'sapjira)
;;sapjira.el ends here
