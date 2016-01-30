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

(defun sapjira-logwork (issue-num)
  "TODO: Fill/check the list: dk-sapjira-issue-logs here"
  (dk-sapjira-browse-issue
   issue-num
   (lambda (issue-num)
     (message "Begin logging work for issue: %s...." issue-num)
     (dk-sapjira-logwork-internal issue-num))))

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
	(let ((startDate (format-time-string
			  "%d/%b/%y %I:%M %p"
			  (date-to-time (nth 0 log-item))))
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
	 (cons "startDate" startDate)
	 (cons "adjustEstimate" "auto")
	 (cons "comment" (concat comment "(via Emacs)"))
	 (cons "atl_token" dk-sapjira-atl-token))
   'dk-sapjira-process-logwork
   (list issue-num startDate)))

(defun dk-sapjira-process-logwork (status issue-num startDate)
  (set-buffer (current-buffer))
  (goto-char 1)
  (if (re-search-forward "<div class=\"dialog-title hidden\"></div>" nil t)
      (message "Work log of issue:%s on date:%s is successfully logged!"
	       issue-num
	       startDate)
    (message "Log work failed for issue:%s on date:%s"
	     issue-num
	     startDate)))

(defun sapjira-set-done (issue-num)
  (dk-sapjira-dispatch-workflow
   "811"
   issue-num
   (lambda (status issue-num)
     (message "Issue:%s is set to DONE!" issue-num))))

(defun sapjira-re-open (issue-num)
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
(require 'ox)
(eval-when-compile (require 'cl) (require 'table nil 'noerror))

;;; Define Back-End

(org-export-define-backend 'sapjira
  '((headline . dk-sapjira-headline)
    (node-property . org-html-node-property)
    (drawer . org-html-drawer)
    (inner-template . dk-sapjira-inner-template)
    (template . dk-sapjira-template)
    (plain-text . dk-sapjira-plain-text)
    (property-drawer . org-html-property-drawer)
    (table . dk-sapjira-html-table)
    (table-cell . dk-sapjira-table-cell)
    (table-row . dk-sapjira-table-row)
    (timestamp . dk-sapjira-timestamp))
  :options-alist
  '(
    (:sapjira-mode "SAPJIRA_MODE" nil "Slacker")))

(defun dk-sapjira-headline (headline contents info)
  (let* ((todo (and (plist-get info :with-todo-keywords)
		   (let ((todo (org-element-property :todo-keyword headline)))
		     (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (text (org-export-data (org-element-property :title headline) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info))))
    (format "todo:%s, todo-type:%s, text:%s, tags:%s"
	    todo todo-type text tags)))

(defun dk-sapjira-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun dk-sapjira-template (contents info)
  (org-export-collect-headlines info))

(defun dk-sapjira-plain-text (text info)
  text)  

(defun dk-sapjira-table (table contents info)
  (format "[%s]" contents))

(defun dk-sapjira-table-cell (table-cell contents info)
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell)))
    (if (and (org-export-table-has-header-p table info)
	     (= 1 (org-export-table-row-group table-row info)))
	"HEADLINE"
      contents)))

(defun dk-sapjira-table-row (table-row contents info)
  (let* ((row-number (org-export-table-row-number table-row info)))
    contents))

(defun org-html-timestamp (timestamp contents info)
  timestamp)

(defun dk-sapjira-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))

(defun dk-sapjira-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (format "<pre class=\"example\">\n%s</pre>" contents)))

(defun dk-org-sapjira-export-as-json ()
  (org-export-to-buffer 'sapjira "*SAPJIRA Export*"))
   ;; (org-export-as 'sapjira))
