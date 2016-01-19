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

(defcustom dk-sapjira-issues-fetch-url
  (concat dk-sapjira-main-url "rest/issueNav/1/issueTable")
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

(defun sapjira-login (&optional callback)
  (interactive)
  (unless dk-sapwiki-pwd
    (setq dk-sapwiki-pwd (read-passwd "Enter your password: ")))
  (dk-url-http-post dk-sapjira-login-url
		    (list (cons "os_username" dk-sapwiki-user)
			  (cons "os_password" dk-sapwiki-pwd)
			  (cons "login" "Log In"))
		    'dk-sapjira-process-login))
		    ;; 'dk-switch-to-url-buffer))
		   ;; (list callback)))

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

(defun sapjira-logwork (issue_num)
  (message "Get the issue...")
  (dk-sapjira-get-issue-info issue_num))

(defun dk-sapjira-get-issue-info (issue_num)
  (dk-url-http-get
   (concat dk-sapjira-browse-url "-" issue_num)
   '()
   (lambda (status)
     (message "Issue information got!")
     (set-buffer (current-buffer))
     (goto-char 1)
     (when
	 (re-search-forward "\\(<input name=\"id\" type=\"hidden\" value=\"\\)\\([^\"]+\\)" nil t)
       (setq dk-sapjira-issue-id (buffer-substring (match-beginning 2) (match-end 2))))
     (message "begin log work....")
     (dk-sapjira-logwork-internal))))

(defun dk-sapjira-logwork-internal ()
  (dk-url-http-post
   dk-sapjira-worklog-url
   (list (cons "inline" "true")
	 (cons "decorator" "dialog")
	 (cons "id" dk-sapjira-issue-id)
	 (cons "timeLogged" "2h")
	 (cons "startDate" "15/Jan/16 4:38 PM")
	 ;; (cons "startDate"
	 ;;       (format-time-string "%d/%b/%y %I:%M %p"))
	 (cons "adjustEstimate" "auto")
	 (cons "comment" "via emacs")
	 (cons "atl_token" dk-sapjira-atl-token))
   'dk-sapjira-process-logwork))


(defun dk-sapjira-process-logwork (status)
  (set-buffer (current-buffer))
  (goto-char 1)
  (if (re-search-forward "<div class=\"dialog-title hidden\"></div>" nil t)
      (message "log work successfully!")
    (message "log work failed")))

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
  (let ((url-request-method "POST")
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

(defun dk-sapjira-http-get (url args callback &optional cbargs)
  (let ((url-request-method "GET")
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
