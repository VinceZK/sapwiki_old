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

(defcustom dk-sapjira-browse-url
  (concat dk-sapjira-main-url "browse/" dk-sapjira-project-name)
  "Jira browse issues in the project"
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

(defcustom dk-sapjira-issues-fetch-url
  (concat dk-sapjira-main-url "rest/issueNav/1/issueTable")
  "Jira browse issues in the project"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-issues-fetch-open-url
  (concat dk-sapjira-main-url "rest/issueNav/1/issueTable/stable")
  "Jira browse issues in the project"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defcustom dk-sapjira-issue-detail-url
  (concat dk-sapjira-main-url "secure/AjaxIssueAction!default.jspa")
  "Jira browse issues in the project"
  :group 'sapjira
  :version "1.0"
  :package-version '(sapjira . "1.0")
  :type 'string)

(defvar dk-sapjira-atl-token nil)
(defvar dk-sapjira-issue-id nil)
(defvar dk-sapjira-issue-log nil)
(defvar dk-sapjira-issue-logs nil)

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
    (user-error "Login failed")))

(defun dk-sapjira-check-login-successfully ()
  (set-buffer (current-buffer))
  (goto-char 1)
  (if (re-search-forward "Sorry, your username and password are incorrect - please try again." nil t)
      nil
    t))

(defun dk-sapjira-get-token ()
  (and (boundp 'url-cookie-secure-storage)
       (assoc "sapjira.wdf.sap.corp" url-cookie-secure-storage)
       (setq dk-sapjira-atl-token
	     (aref (nth 0 (cdr (assoc "sapjira.wdf.sap.corp" url-cookie-secure-storage))) 2))))

(defun sapjira-logwork ()
  "The function will first iterate all the open sprints, then the open tasks under the sprints. Finally, find all the non-logged work log and post to SAPJIRA. Afterwards, status will be changed."
  (interactive)
  (dk-sapjira-do-open-sprints))

(defun dk-sapjira-browse-issue (issue-num callback &optional cbargs)
  (dk-sapjira-http-get
   (concat dk-sapjira-browse-url "-" issue-num)
   '()
   (lambda (status callback issue-num cbargs)
     (set-buffer (current-buffer))
     (goto-char 1)
     (if (not (re-search-forward "\\(<input name=\"id\" type=\"hidden\" value=\"\\)\\([^\"]+\\)" nil t))
	 (message "Issue:%s cannot be retreived. Could be not logged in, or %s does not exist!" issue-num issue-num)
       (setq dk-sapjira-issue-id (buffer-substring (match-beginning 2) (match-end 2)))
       (apply callback issue-num cbargs)))
   (list callback issue-num cbargs)))
	   
(defun dk-sapjira-logwork-internal (issue-num)
"[(\"<2016-01-18 Mon>\" \"4h\" \"comments\").....]"
(mapc (lambda (log-item)
	(let ((startDate  (nth 0 log-item))
	      (timeLogged (nth 1 log-item))
	      (comment (nth 2 log-item)))
	  (dk-sapjira-logwork-internal-single
	   issue-num startDate timeLogged comment)))
      dk-sapjira-issue-logs))  

(defun dk-sapjira-logwork-internal-single (issue-num startDate timeLogged comment)
  (dk-url-http-post
   dk-sapjira-worklog-url
   (list (cons "inline" "true")
	 (cons "decorator" "dialog")
	 (cons "id" dk-sapjira-issue-id)
	 (cons "timeLogged" timeLogged)
	 (cons "startDate" (format-time-string
			    "%d/%b/%y %I:%M %p"
			    (date-to-time startDate)))
	 (cons "adjustEstimate" "auto")
	 (cons "comment" (concat comment "(via Emacs)"))
	 (cons "atl_token" dk-sapjira-atl-token))
   'dk-sapjira-process-logwork
   (list issue-num startDate)))

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
	      
    
(defun dk-sapjira-set-done (issue-num)
  (dk-sapjira-dispatch-workflow
   "811"
   issue-num
   (lambda (status issue-num)
     (message "Issue:%s is set to DONE!" issue-num))))

(defun dk-sapjira-re-open (issue-num)
  (dk-sapjira-dispatch-workflow
   "831"
   issue-num
   (lambda (status issue-num)
     (message "Issue:%s is REOPENED!" issue-num))))

(defun dk-sapjira-dispatch-workflow (action issue-num callback)
  (message "Browse the issue:%s ......" issue-num)
  (dk-sapjira-browse-issue
   issue-num
   (lambda (issue-num action callback)
     (dk-sapjira-http-get
      dk-sapjira-workflow-dispatch-url
      (list (cons "id" dk-sapjira-issue-id)
	    (cons "action" action)
	    (cons "atl_token" dk-sapjira-atl-token)
	    (cons "decorator" "dialog")
	    (cons "inline" "true")
	    (cons "_"  (format-time-string "%s%3N")))
     ;; 'dk-switch-to-url-buffer))))
      callback
      (list issue-num)))
   (list action callback)))

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

(defun sapjira-fetch (filter)
  (dk-sapjira-http-post
   dk-sapjira-issues-fetch-url
   (list (cons "startIndex" "0")
	 (cons "filterId" filter)
	 (cons "jql"
	       (format "project = %s AND assignee in (%s)"
		       dk-sapjira-project-name
		       dk-sapwiki-user))
	 (cons "layoutKey" "split-view"))
   'dk-sapjira-process-issue-table))
;;'dk-switch-to-url-buffer))

(defun sapjira-fetch-open ()
  (dk-sapjira-http-post
   dk-sapjira-issues-fetch-url
   (list (cons "startIndex" "0")
	 (cons "filterId" "-1")
	 (cons "jql" "assignee = currentUser() AND resolution = Unresolved ORDER BY updatedDate DESC")
	 (cons "layoutKey" "split-view"))
   'dk-sapjira-process-issue-table))

(defun dk-sapjira-process-issue-table (status)
  (let* ((json-object-type 'plist)
	 (json-plist
	  (json-read-from-string
	   (dk-sapjira-get-resp-json (current-buffer))))
	 (errorMessages
	  (plist-get json-plist :errorMessages))
	 (issue-table
	  (plist-get json-plist :issueTable))
	 (table
	  (plist-get issue-table :table)))
    (if errorMessages
	(message "Session is timeout, please re-login!")
      (mapc
       (lambda (issue)
	 (message "Issue id is %s, key is %s"
		  (plist-get issue :id)
		  (plist-get issue :key)))
       table))))

(defun dk-sapjira-get-resp-json (response-buffer)
  (with-current-buffer response-buffer
    (goto-char 1)
    (re-search-forward "{.*}" nil t)
    (buffer-substring-no-properties
     (match-beginning 0)
     (match-end 0))))

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

(defun dk-sapjira-get-issue-detail (issue-key)
  (dk-sapjira-http-get
   dk-sapjira-issue-detail-url
   (list (cons "issueKey" (concat dk-sapjira-project-name
				  "-"
				  issue-key))
	 (cons "decorator" "none")
	 (cons "prefetch" "false")
	 (cons "shouldUpdateCurrentProject" "false")
	 (cons "loadFields" "false")
	 (cons "_"  (format-time-string "%s%3N")))
    'dk-sapjira-process-issue-detail))

(defun dk-sapjira-process-issue-detail (status)
  (let* ((json-object-type 'plist)
	 (json-plist
	  (json-read-from-string
	   (dk-sapjira-get-resp-json (current-buffer))))
	 (errorMessages
	  (plist-get
	   (plist-get json-plist :errorCollection)
	   :errorMessages))
	 (panels
	  (plist-get json-plist :panels))
	 (leftPanels
	  (plist-get panels :leftPanels))
	 (rightPanels
	  (plist-get panels :rightPanels))
	 (detail-html
	  (plist-get (elt leftPanels 0) :html))
	 (worklog-html
	  (plist-get (elt leftPanels 1) :html))
	 (people-html
	  (plist-get (elt rightPanels 0) :html))
	 (dates-html
	  (plist-get (elt rightPanels 1) :html))
	 (timetracking-html
	  (plist-get (elt rightPanels 2) :html)))
    (if (equal (length errorMessages) 1)
	(message "Session is timeout, please re-login!")
      (message "timetracking html: %s" timetracking-html))))

;; (libxml-parse-xml-region 0 20)

;;------------------------------------------------------
;; Define a org export backend for sapjira
;;------------------------------------------------------

(defun dk-sapjira-do-open-sprints ()
  (org-map-entries
   'dk-sapjira-process-sprint
   "/+OPEN"
   nil
   'dk-sapjira-skip-non-sprint))

(defun dk-sapjira-process-sprint ()
  (dk-sapjira-do-open-tasks)
  (dk-sapjira-sprint-post-process))

(defun dk-sapjira-sprint-post-process ()
  (message "Sprint: %s" (org-entry-get nil "TITLE"))
  (let ((all-done t))
    (org-map-entries
     (lambda ()
       (message "Task: %s;  Status: %s"
		(org-entry-get nil "TITLE")
		(org-entry-get nil "TODO"))
       (unless (string= (org-entry-get (point) "TODO")
			"DONE")
	 (setq all-done nil)))
     "+Type=\"Task\""
     'tree)
    (when all-done
      (org-entry-put (point) "TODO" "DONE"))))
     
(defun dk-sapjira-skip-non-sprint ()
  (if (string-match "CF_RTC-Sprint"
		    (dk-sapjira-current-headline))
      nil
    (point)))

(defun dk-sapjira-current-headline ()
  (let ((org-trust-scanner-tags t))
    (org-entry-get nil "ITEM")))

(defun dk-sapjira-do-open-tasks ()
  (org-map-entries
   'dk-sapjira-process-task
   "/+OPEN"
   'tree
   'dk-sapjira-skip-non-task))

(defun dk-sapjira-process-task ()
  (setq dk-sapjira-issue-logs nil)
  (setq dk-sapjira-issue-log nil)
  ;;(message "Task Tags: %s" org-scanner-tags)
  ;;(org-entry-put (point) "Remaining" "4h")
  (org-map-entries
   (lambda ()
     ;; (org-element-map (org-element-parse-buffer) 'link
     ;;   (lambda (link)
     ;; 	 (message "Link is: %s"
     ;; 		  (org-element-contents link))))
     
     (org-element-map (org-element-parse-buffer) 'table-row
       'dk-sapjira-process-table)
     
     (message "Logged Issues: %s" dk-sapjira-issue-logs)
     ;; (when dk-sapjira-issue-logs
     ;;   (dk-sapjira-browse-issue
     ;; 	(org-entry-get nil "IssueNum")
     ;; 	(lambda (issue-num)
     ;; 	  (message "Begin logging work for issue: %s...." issue-num)
     ;; 	  (dk-sapjira-logwork-internal issue-num))))
     
     (org-element-map (org-element-parse-buffer) 'table-row
       'dk-sapjira-table-post-process))
   "+Type=\"Task\""
   'tree)
  (if (and (string= (org-entry-get nil "TODO") "DONE")
	   (dk-sapjira-check-work-log-success))
      (dk-sapjira-set-done (org-entry-get nil "IssueNum"))))
      
(defun dk-sapjira-check-work-log-success ()
  (if (not dk-sapjira-issue-logs)
      nil
    (catch 'Exit
      (dolist (issue-log dk-sapjira-issue-logs)
	(unless (nth 3 issue-log)
	  (throw 'Exit nil)))
      t)))

(defun dk-sapjira-process-table (table-row)
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
	      (push current-cell dk-sapjira-issue-log))))))
    (when dk-sapjira-issue-log
      (setq dk-sapjira-issue-log (nreverse dk-sapjira-issue-log))
      (when (and (car (cdr dk-sapjira-issue-log))
		 (not (nth 3 dk-sapjira-issue-log)))	
	(add-to-list 'dk-sapjira-issue-logs
		     dk-sapjira-issue-log t))
      (setq dk-sapjira-issue-log nil))))
 
(defun dk-sapjira-skip-non-task ()
  (if (string-match "CF_RTC-Sprint"
		    (dk-sapjira-current-headline))
      (point)
    nil))

(defun dk-sapjira-setter-test ()
  (org-element-map (org-element-parse-buffer) 'table-row
    (lambda (table-row)
      (let ((begin-point (org-element-property :begin table-row))
	    (end-point (org-element-property :end table-row)))
	(goto-char begin-point)
	(message "Current Line: %s"
		 (org-table-get nil 1))
	(org-table-put nil 2 "X" t)))))

(defun dk-sapjira-table-post-process (table-row)
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
  
