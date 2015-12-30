;;; sapwiki.el --- Write SAP wiki using orgmode.

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

;;; Commentary (How it works):
;; 1. Download a wiki from SAP internal wiki site. When downloading, the wiki should be locked to prevent other downlodings.
;; 2. Convert the wiki html to orgmode format.
;; 3. Compare the difference to the local copy if exist.Do the merge.
;; 4. Write/Modify the wiki using orgmode.
;; 5. When finish the editing:
;; 5.1. Save a orgmode local copy;
;; 5.2. Download the latest version (lock the wiki), and convert to orgmode format;
;; 5.3. Compare the difference and do the merge;
;; 5.4. Convert the merged orgmode version to the wiki html;
;; 5.5. Upload the wiki html to the sap wiki site (unlock the wiki).

;;; Code:
;;------------------------------------------------------
;;Begin of 1. Connect to SAP wiki, uploading/downloading
;;------------------------------------------------------
(defcustom dk-sapwiki-main-url
  "https://wiki.wdf.sap.corp/wiki/"
  "The main URL of SAPWIKI"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-login-url
  (concat dk-sapwiki-main-url "dologin.action" )
  "The url used to enter username and password"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-fetch-url
  (concat dk-sapwiki-main-url "plugins/viewstorage/viewpagestorage.action")
  "The url used to fetch content from sapwiki"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-info-url
  (concat dk-sapwiki-main-url "pages/viewinfo.action")
  "The url used to fetch content from sapwiki"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-lock-url
  (concat dk-sapwiki-main-url "pages/editpage.action")
  "The url used to lock the wikipage"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-push-url
  (concat dk-sapwiki-main-url "pages/doeditpage.action")
  "The url used to updated the wikipage"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-upload-url
  (concat dk-sapwiki-main-url "pages/doattachfile.action")
  "The url used to updated the wikipage"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-user
  "i046147" "SAP i<number>"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defcustom dk-sapwiki-pwd nil
  "SAP main password"
  :group 'sapwiki
  :version "1.0"
  :package-version '(sapwiki . "1.0")
  :type 'string)

(defvar dk-sapwiki-pageID nil)
(defvar dk-sapwiki-title nil)
(defvar dk-sapwiki-version-comment nil)
(defvar dk-sapwiki-attachments nil)
(defvar dk-sapwiki-attachment-comments nil)
(defvar dk-sapwiki-current-page-version nil)
(defvar dk-sapwiki-latest-page-version nil)
  
(defun dk-sapwiki-login ()
  (interactive)
  (unless dk-sapwiki-pwd
    (setq dk-sapwiki-pwd (read-passwd "Enter your password: ")))
  (dk-url-http-post dk-sapwiki-login-url
		    (list (cons "os_username" dk-sapwiki-user)
			  (cons "os_password" dk-sapwiki-pwd))
		    'dk-sapwiki-process-login))

(defun dk-sapwiki-fetch ()
  (interactive)
  (unless (dk-sapwiki-check-login-successfully)
    (dk-sapwiki-login))
  (setq dk-sapwiki-pageID (dk-sapwiki-get-attribute-value "PAGEID"))
  (setq dk-sapwiki-title (dk-sapwiki-get-attribute-value "TITLE"))

  (message "Get the lastest version...")
  (dk-url-http-get
   dk-sapwiki-fetch-url 
   (list (cons "pageId" dk-sapwiki-pageID))
   'dk-sapwiki-fetch-internal))

(defun dk-sapwiki-pull ()
  (interactive)
  (unless (dk-sapwiki-check-login-successfully)
    (dk-sapwiki-login))
  (setq dk-sapwiki-pageID (dk-sapwiki-get-attribute-value "PAGEID"))
  (setq dk-sapwiki-title (dk-sapwiki-get-attribute-value "TITLE"))
  (setq dk-sapwiki-current-page-version (dk-sapwiki-get-attribute-value "VERSION"))
  
  (message "Get the lastest version number...")
  (dk-sapwiki-get-pageinfo
   (lambda (work-buffer)
     (if (equal dk-sapwiki-current-page-version
		dk-sapwiki-latest-page-version)
	 (message "Current version is the latest!")
       (message "Fetch the lastest version content...")  
       (if (not dk-sapwiki-current-page-version)
	   (dk-url-http-get
	    dk-sapwiki-fetch-url 
	    (list (cons "pageId" dk-sapwiki-pageID))
	    'dk-sapwiki-pull-internal (list work-buffer))	
	 (dk-url-http-get
	  dk-sapwiki-fetch-url 
	  (list (cons "pageId" dk-sapwiki-pageID))
	  'dk-sapwiki-ediff (list work-buffer))))
     (list (current-buffer)))))

(defun dk-sapwiki-push (arg versionComment)
  (interactive "P\nsVersion Comment: ")
  (setq dk-sapwiki-version-comment versionComment)

  (unless (dk-sapwiki-check-login-successfully)
    (dk-sapwiki-login))
  
  (setq dk-sapwiki-pageID (dk-sapwiki-get-attribute-value "PAGEID"))
  (setq dk-sapwiki-title (dk-sapwiki-get-attribute-value "TITLE"))
  (setq dk-sapwiki-current-page-version (dk-sapwiki-get-attribute-value "VERSION"))

  (message "Get the lastest version...")
  (dk-sapwiki-get-pageinfo
   (lambda (work-buffer)
     (if (equal dk-sapwiki-current-page-version
		dk-sapwiki-latest-page-version)
	 (progn
	   (message "Lock the wiki for uploading.")  
	   (dk-url-http-get
	    dk-sapwiki-lock-url 
	    (list (cons "pageId"  dk-sapwiki-pageID))
	    'dk-extract-hidden-token (list work-buffer)))
       (message "Fetch the lastest version content, and do the comparing...")  
       (dk-url-http-get
	dk-sapwiki-fetch-url 
	(list (cons "pageId" dk-sapwiki-pageID))
	'dk-sapwiki-ediff (list work-buffer)))))
  (list (current-buffer)))

(defun dk-sapwiki-get-pageinfo (callback &optional cbargs)
  (dk-url-http-get
   dk-sapwiki-info-url 
   (list (cons "pageId" dk-sapwiki-pageID))
   (lambda (status)
     (set-buffer (current-buffer))
     (goto-char 1)
     (re-search-forward "\\(<meta name=\"page-version\" content=\"\\)\\([^\"]+\\)" nil t)
     (setq dk-sapwiki-latest-page-version (buffer-substring (match-beginning 2) (match-end 2)))
     (apply callback cbargs))))

(defun dk-url-http-get (url args callback &optional cbargs)
  (let ((url-request-method "GET")
	(query-string
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg))
			      "="
			      (url-hexify-string (cdr arg))))
		    args
		    "&")))
    (url-retrieve (concat url "?" query-string)
		  callback cbargs)))

(defun dk-url-http-post (url args callback &optional cbargs)
      "Send ARGS to URL as a POST request."
      (let ((url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data
             (mapconcat (lambda (arg)
                          (concat (url-hexify-string (car arg))
                                  "="
                                  (url-hexify-string (cdr arg))))
                        args
                        "&")))
        (url-retrieve url callback cbargs)))

(defun dk-url-http-post-multipart (url args fields files callback &optional charset cbargs)
      "Send FIELDS and FILES to URL as a multipart HTTP POST.
       fields is an alist, eg ((field-name . \"value\")); 
       files  is an a list of \(fieldname \"filename\" \"file MIME type\" \"file data\")*"
      (let ((url-request-method "POST")
	    (query-string
	     (mapconcat (lambda (arg)
			  (concat (url-hexify-string (car arg))
				  "="
				  (url-hexify-string (cdr arg))))
			args
			"&"))
	    (url-request-extra-headers
	     (list (cons "Content-Type"  (format "multipart/form-data; boundary=%S"
						 (dk-http-post-multipart-boundary)))))
	    (url-request-data (dk-http-post-encode-multipart-data fields files charset)))	
        (url-retrieve (concat url "?" query-string)
		      callback cbargs)))

(defun dk-http-post-encode-multipart-data (fields files charset)
  "Return FIELDS and FILES encoded for use as the data for a multipart HTTP POST request.
   FIELDS is an alist, eg ((field-name . \"value\")); 
   FILES is an alist of \(fieldname \"filename\" \"file MIME type\" \"file data\")*"
  (dk-http-post-join-lines
   (mapcar (lambda (field)
             (dk-http-post-bound-field
              (format "Content-Disposition: form-data; name=%S" (symbol-name (car field)))
              ""
              (cdr field)))
   	   fields)
   (mapcar (lambda (file)
                (destructuring-bind (fieldname filename mime-type data) file
                  (dk-http-post-bound-field
                   (format "Content-Disposition: form-data; name=%S; filename=%S" fieldname filename)
                   (format "Content-type: %s" (dk-http-post-content-type mime-type charset))
                   ""
                   data)))
              files)
   (format "--%s--" (dk-http-post-multipart-boundary))))

(defun dk-http-post-join-lines (&rest bits)
  (let ((sep "\r\n"))
    (mapconcat (lambda (bit)
		 (if (listp bit)
		     (apply 'dk-http-post-join-lines bit)
                   bit))
	       bits sep)))

(defun dk-http-post-bound-field (&rest parts)
  (let ((boundary (format "--%s" (dk-http-post-multipart-boundary))))
    (dk-http-post-join-lines  boundary parts)))

(defun dk-http-post-multipart-boundary ()
  "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")

(defun dk-http-post-content-type (content-type &optional charset)
  (if charset
      (format "%s; charset=%s" content-type charset)
    content-type))

(defun dk-sapwiki-check-login-successfully ( )
  (and (boundp 'url-cookie-secure-storage)
       (assoc "wiki.wdf.sap.corp" url-cookie-secure-storage)
       (aref (nth 1 (cdr (assoc "wiki.wdf.sap.corp" url-cookie-secure-storage))) 2)))

(defun dk-sapwiki-process-login (status)
  (if (dk-sapwiki-check-login-successfully)
      (message "Login successfully")
    (user-error "Login failed")))

(defun dk-sapwiki-get-attribute-value (attribute-name)
  (with-current-buffer (current-buffer)
    (goto-char 1)
    (let ((search-string
	   (concat "\\(+" attribute-name ": \\)\\([^\n\r]+\\)"))) 
      (if (re-search-forward search-string nil t)
	  (buffer-substring (match-beginning 2) (match-end 2))
	nil))))
 
(defun dk-process-tr-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun dk-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(defun dk-sapwiki-fetch-internal (status)
  (message "Fetch Successfully, converting to org-mode...")
  (set-buffer (current-buffer))
  (goto-char 1)
  (dk-iterate-html-tag)
  (switch-to-buffer result-org-buffer))

(defun dk-sapwiki-pull-internal (status work-buffer)
  (message "Fetch Successfully, converting to org-mode...")
  (set-buffer (current-buffer))
  (goto-char 1)
  (dk-iterate-html-tag)
  (set-buffer result-org-buffer)
  (copy-to-buffer work-buffer 1 (point-max))
  (dk-merged-with-latest-version work-buffer)
  (switch-to-buffer work-buffer))

(defun dk-get-buffer-as-string (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun dk-sapwiki-ediff (status work-buffer)
  (set-buffer (current-buffer))
  (goto-char 1)
  (dk-iterate-html-tag)
  (ediff-buffers work-buffer result-org-buffer))
  
(defun dk-extract-hidden-token (status work-buffer)
  (message "Push the wiki...")
  (set-buffer (current-buffer))
  (goto-char 1)
  (let* ((parentPageString
	  (progn
	    (re-search-forward "\\(<meta name=\"ajs-parent-page-title\" content=\"\\)\\([^\"]+\\)" nil t)
	    (buffer-substring (match-beginning 2) (match-end 2))))
	 (newSpaceKey
	  (progn
	    (re-search-forward "\\(<meta name=\"ajs-space-key\" content=\"\\)\\([^\"]+\\)" nil t)
	    (buffer-substring (match-beginning 2) (match-end 2))))
	 (atl-token
	  (progn 
	    (re-search-forward "\\(<meta name=\"ajs-atl-token\" content=\"\\)\\([^\"]+\\)" nil t)
	    (buffer-substring (match-beginning 2) (match-end 2))))
	 (originalVersion
	  (progn		     
	    (re-search-forward "\\(<meta name=\"page-version\" content=\"\\)\\([^\"]+\\)" nil t)
	    (buffer-substring (match-beginning 2) (match-end 2)))))
    (set-buffer work-buffer)
    (dk-sapwiki-export-as-html)
    (dk-url-http-post
     dk-sapwiki-push-url
     (list (cons "pageId"  dk-sapwiki-pageID)
	   (cons "atl_token" atl-token)
	   (cons "originalVersion" originalVersion)
	   (cons "title" dk-sapwiki-title)
	   (cons "wysiwygContent" (dk-get-buffer-as-string "*sapwiki export*"))
	   (cons "watchPageAfterComment" "true")
	   (cons "versionComment" dk-sapwiki-version-comment)
	   (cons "notifyWatchers" "on")
	   (cons "confirm" "Save")
	   (cons "parentPageString" parentPageString)
	   (cons "moveHierarchy" "true")
	   (cons "draftId" "0")
	   (cons "entityId"  dk-sapwiki-pageID)
	   (cons "newSpaceKey" newSpaceKey))
     'dk-sapwiki-process-push (list work-buffer))
    
    (add-to-list 'dk-sapwiki-attachment-comments
		 (cons 'atl_token atl-token))
    (add-to-list 'dk-sapwiki-attachment-comments
		 (cons 'confirm "Attach") t)
    (dk-url-http-post-multipart
     dk-sapwiki-upload-url
     (list (cons "pageId"  dk-sapwiki-pageID))
     dk-sapwiki-attachment-comments
     dk-sapwiki-attachments
     'dk-sapwiki-process-attchments)
    
    (dk-increase-page-version work-buffer)
    (message "Pushed!")))

(defun dk-increase-page-version (work-buffer)
  (setq dk-sapwiki-current-page-version
	(+ dk-sapwiki-current-page-version 1))
  (setq dk-sapwiki-latest-page-version
	dk-sapwiki-current-page-version)
  (dk-update-page-version work-buffer))

(defun dk-merged-with-latest-version (work-buffer)
  (with-current-buffer result-org-buffer (erase-buffer))
  (setq dk-sapwiki-current-page-version dk-sapwiki-latest-page-version)
  (dk-update-page-version work-buffer))

(add-hook 'ediff-after-quit-hook-internal 'dk-merged-with-latest-version)

(defun dk-update-page-version (work-buffer) 
  (with-current-buffer work-buffer
    (goto-char 1)
    (if (re-search-forward "\\(+VERSION: \\)\\([^\n\r]+\\)" nil t)
	(replace-match dk-sapwiki-current-page-version nil nil nil 2)
      (goto-char 1)
      (re-search-forward "+PAGEID: [^\n\r]+")
      (insert ?\n)
      (insert "#+VERSION: " dk-sapwiki-current-page-version ?\n))))

(defun dk-update-page-title (work-buffer)
  (with-current-buffer work-buffer
    (goto-char 1)
    (if (re-search-forward "\\(+TITLE: \\)\\([^\n\r]+\\)" nil t)
	(replace-match dk-sapwiki-title nil nil nil 2)
      (goto-char 1)
      (re-search-forward "+VERSION: [^\n\r]+")
      (insert ?\n)
      (insert "#+TITLE: " dk-sapwiki-title ?\n))))  

(defun dk-sapwiki-process-push (status work-buffer)
  "The function is called only if post is not successfully"
  (switch-to-buffer (current-buffer))
  (message "Push Failed!"))

(defun dk-sapwiki-process-attchments (status work-buffer)
  "The function is called only if post is not successfully"
  (switch-to-buffer (current-buffer))
  (message "Upload Attachments Failed!"))

;;------------------------------------------------------
;;End of 1. Connect to SAP wiki, uploading/downloading
;;------------------------------------------------------

;;------------------------------------------------------
;;2. Convert the wiki html to orgmode format
;;------------------------------------------------------
(defconst dk-wiki-html5-tags
  '("<div>" "<h1>" "<h2>" "<h3>" "<h4>" "<h5>" "<p>"
    "<figure>" "<img>" "<figcaption>" "<span>" "<em>"
    "<i>" "<b>" "<code>" "<u>" "<s>" "<strong>" "<table>"
    "<colgroup>" "<col>" "<thead>" "<tr>" "<th>"
    "<tbody>" "<td>" "<ul>" "<li>" "<ol>" "<a>"
    "<ac:image>" "<sub>" "<sup>"))

(defconst dk-wiki-html5-close-tags
  '("<ri:attachment/>" "<br/>" "<hr/>" "<col/>" "<p/>"))

(defvar begin-tag-list ())
(defvar result-org-buffer (get-buffer-create "result-org-buffer"))

(defun dk-search-html-tag ()
  "Search for html tags <xxx> in current buffer.
 It retruns a list (<tag> start-pos end-pos),
    <tag> is the html tag found.
    beg-pos is the position before the first '<' in the buffer.
    end-pos is the position after the last '>' in the buffer."
  (when (re-search-forward "<[^>]+[/]*>" nil t)
    (cons
     (downcase
      (replace-regexp-in-string
       "[\n\s\t\r][^>/]*" ""
       (replace-regexp-in-string
	"\".*\"" ""
	(buffer-substring-no-properties
	 (match-beginning 0) (match-end 0)))))
     (cons (match-beginning 0) (match-end 0)))))

(defun dk-check-valid-html-tag (tag)
  (if (member tag dk-wiki-html5-tags)
      t
    (member tag dk-wiki-html5-close-tags)))

(defun dk-check-begin-html-tag (tag)
  (and (not (equal (substring tag 1 2 ) "/"))
       (not (member tag dk-wiki-html5-close-tags))))

(defun dk-check-end-html-tag (tag)
  (equal (substring tag 1 2 ) "/"))

(defun dk-check-close-html-tag (tag)
  (member tag dk-wiki-html5-close-tags))

(defun dk-get-html-end-tag (begin-tag)
  (concat "<" (store-substring begin-tag 0 ?/)))

(defun dk-add-tag-to-begin-tag-list (begin-tag) 
  (push (cons begin-tag (generate-new-buffer (car begin-tag)))
	 begin-tag-list))    

(defsubst dk-replace-html-placeholder ()
  (goto-char 1)
  (while (re-search-forward "&[^;]+;" nil t)
    (pcase (buffer-substring
	    (match-beginning 0)
	    (match-end 0))
      ("&gt;" (replace-match ">"))
      ("&lt;" (replace-match "<"))
      ("&nbsp;" (replace-match " "))
      ("&amp;" (replace-match "&")))))
      
(defun dk-process-in-line-ele ()
  (goto-char (point-min))
  (let ((line-num 0))
    ;; First, search and replace emphasis in the paragraph
    (while (re-search-forward "<[^/]+/>\\|<[^>]+>[^>]*</[^>]+>" nil t 1)
      (save-excursion
	(goto-char 1)
	(forward-line line-num)     
	(replace-match (buffer-substring
			(line-beginning-position)
			(line-end-position)))
	(setq line-num (+ 1 line-num))))
    ;; Then, remove the template emphasis above the paragraph
    (when (> line-num 0)
      (goto-char 1)
      (forward-line (- line-num 1))
      (delete-region 1 (line-end-position))))
  ;; replace placeholders
  (dk-replace-html-placeholder)
  ;; remove whitesapces
  (goto-char (point-min))
  (while (re-search-forward "[\t\r\n]+" nil t)
    (replace-match "" nil nil))
  ;; re-add line break for //
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\{2\\}" nil t)
    (insert ?\n)))

(defun dk-process-head-line (stars)
  (dk-process-in-line-ele)
  (goto-char 1)
  ;; Remove headline numberring, org-mode doesn't need it
  (when
      (re-search-forward "\\([0-9]+[.]*\\)+\s" nil t)
    (replace-match "" nil nil))
  (goto-char 1)
  (insert stars)
  (goto-char (point-max))
  (insert ?\n)
  (insert ?\n))

(defsubst dk-process-h1 ()
  (dk-process-head-line "* "))

(defsubst dk-process-h2 ()
  (dk-process-head-line "** "))

(defsubst dk-process-h3 ()
  (dk-process-head-line "*** "))

(defsubst dk-process-h4 ()
  (dk-process-head-line "**** "))

(defun dk-process-emphasis (decorate)
  (goto-char  (point-min))
  (insert decorate)
  (goto-char (point-max))
  (insert decorate)
  (insert ?\n))

(defsubst dk-process-em ()
 (dk-process-emphasis "="))

(defsubst dk-process-code ()
 (dk-process-emphasis "~"))

(defsubst dk-process-italic ()
  (dk-process-emphasis "/"))

(defsubst dk-process-bold ()
  (dk-process-emphasis "*"))

(defsubst dk-process-underline ()
  (dk-process-emphasis "_"))

(defsubst dk-process-strike-through ()
  (dk-process-emphasis "+"))

(defsubst dk-process-span ()
  (insert ?\n))

(defsubst dk-process-sub ()
  (goto-char 1)
  (insert "_{")
  (goto-char (point-max))
  (insert "}")
  (insert ?\n))

(defsubst dk-process-sup ()
  (goto-char 1)
  (insert "^{")
  (goto-char (point-max))
  (insert "}")
  (insert ?\n))

(defsubst dk-process-p ()
  (dk-process-in-line-ele)
  (goto-char (point-max))
  (insert ?\n)
  (insert ?\n))

(defsubst dk-process-table ()
  ;(org-table-align)
  (goto-char (point-max)) 
  (insert ?\n))

(defsubst dk-process-colgroup ()
  (goto-char 1)
  (re-search-forward "\\(| <l> \\)+" nil t)
  (if (and (equal (match-beginning 0) 1)
	   (equal (match-end 0) (point-max)))
      (replace-match "" nil nil)
    (progn
      (goto-char (point-max))
      (insert "|")
      (insert ?\n))))

(defsubst dk-process-thead ()
  (insert "|-")
  (insert ?\n))

(defsubst dk-process-tbody ()
  )

(defsubst dk-process-tr ()
  ;(goto-char (point-max))
  (insert " |")
  (insert ?\n))

(defsubst dk-process-td ()
  "remove those block-tag(<ol>,<ul>, including <p>) contents, as org-mode table does not support complex table cells. Org-mode table will also remove all the newlines among paragrahs"
  (goto-char 1)
  (while (re-search-forward "<[oup]l?[^>]*>[[:ascii:]]*</[oup]l?>\\|<[^/]+/>" nil t)
    (replace-match "" nil nil))
  (dk-process-in-line-ele)
  (goto-char 1)
  (insert "| ")
  ;; (while (re-search-forward "[\n]+" nil t)
  ;;   (replace-match "" nil nil))
  (goto-char (point-max))
  (insert " "))

(defsubst dk-process-ul ()
  (goto-char 0)
  (let ((total-lines (count-lines 1 (point-max)))
   	(current-line 0))     
    (while (< current-line total-lines)
      (insert "- ")
      (forward-line)
      (setq current-line (+ 1 current-line))))
  (goto-char (point-max))
  (insert ?\n)
  (insert ?\n))

(defsubst dk-process-ol ()
   (goto-char 0)
   (let ((total-lines (count-lines 1 (point-max)))
   	(current-line 0))     
     (while (< current-line total-lines)
       (setq current-line (+ 1 current-line))
       (insert (concat
		(number-to-string current-line)
		". "))
       (forward-line)))
   (goto-char (point-max))
   (insert ?\n)
   (insert ?\n))

(defsubst dk-process-li ()
  (dk-process-in-line-ele)
  (goto-char (point-max))
  (insert ?\n))

(defsubst dk-process-begin-a (tag-string)
  (insert "[[")
  (string-match "\\( href=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (match-string 2 tag-string))
  (insert "]["))

(defsubst dk-process-end-a ()
  (goto-char 1)
  (while (re-search-forward "[\t\r\n]+" nil t)
    (replace-match "" nil nil)
    )
  (goto-char (point-max))
  (insert "]]")
  (insert ?\n)
  (insert ?\n))

(defsubst dk-process-br ()
  (insert ?\\)
  (insert ?\\)
  (insert ?\\)
  (insert ?\\)
  (insert ?\n))

(defsubst dk-process-hr ()
  (insert "------")
  (insert ?\n))

(defsubst dk-process-col (tag-string)
  (insert "| <")
  (string-match "\\( align=\"\\)\\([^\"]+\\)"
		tag-string)
  (pcase (downcase (match-string 2 tag-string))
    ("left" (insert "l"))
    ("right" (insert "r"))
    ("center" (insert "c")))
  (if (string-match "\\( width=\"\\)\\([^\"]+\\)" tag-string)
      (insert (number-to-string (/ (string-to-number (match-string 2 tag-string)) 20))))
  (insert "> "))

(defsubst dk-process-riattachment (tag-string)
  (insert "[[")
  (string-match "\\( ri:filename=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (concat "../image/" (match-string 2 tag-string))))

(defsubst dk-process-begin-acimage (tag-string)
  (insert "#+CAPTION: ")
  (string-match "\\( ac:title=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (match-string 2 tag-string))
  (insert ?\n))

(defsubst dk-process-end-acimage ()  
  (insert "]]")
  (insert ?\n)
  (insert ?\n))

(defun dk-get-parent-buffer ()
  (let* ((parent-node (car begin-tag-list)))
    (if parent-node
        (cdr parent-node)
      result-org-buffer)))
     
(defsubst dk-process-html-begin-tag (begin-tag)
  (unless (dk-check-valid-html-tag (car begin-tag))
    (user-error "html tag: %s is not supported!" (car begin-tag)))
  (dk-add-tag-to-begin-tag-list begin-tag)
  (let ((tag-string  (buffer-substring-no-properties
		      (car (cdr begin-tag))
		      (cdr (cdr begin-tag)))))
    (with-current-buffer (cdr (car begin-tag-list))
      (pcase (car begin-tag)
	("<a>" (dk-process-begin-a tag-string))
	("<ac:image>" (dk-process-begin-acimage tag-string))))))

(defsubst dk-process-html-end-tag (end-tag)
  (let ((nearest-tag (pop begin-tag-list)))
    ;Get the nearest tag and remove it from the global list.
    (unless (equal (dk-get-html-end-tag (car (car nearest-tag)))
		   (car end-tag))
      (user-error "Parsing order error! end-tag: %s" (car end-tag)))

    (unless (or (equal "</table>" (car end-tag))
		(equal "</colgroup>" (car end-tag))
		(equal "</thead>" (car end-tag))
		(equal "</tbody>" (car end-tag))
		(equal "</div>" (car end-tag))
		(equal "</tr>" (car end-tag))
		;(equal "</td>" (car end-tag))
		;(equal "</th>" (car end-tag))
		(equal "</ul>" (car end-tag))	      
		(equal "</ol>" (car end-tag))
		(equal "</ac:image>" (car end-tag)))
      (append-to-buffer (cdr nearest-tag)
			(cdr (cdr (car nearest-tag)))
			(car (cdr end-tag))))
      
    (with-current-buffer (cdr nearest-tag)
      (pcase (car end-tag)
	("</h1>" (dk-process-h1))
	("</h2>" (dk-process-h2))
	("</h3>" (dk-process-h3))
	("</h4>" (dk-process-h4))
	("</em>" (dk-process-em))
	("</i>" (dk-process-italic))
	("</code>" (dk-process-code))
	("</b>" (dk-process-bold))
	("</strong>" (dk-process-bold))
	("</u>" (dk-process-underline))
	("</s>" (dk-process-strike-through))
	("</span>" (dk-process-span))
	("</sub>" (dk-process-sub))
	("</sup>" (dk-process-sup))
	("</p>" (dk-process-p))
	("</table>" (dk-process-table))
	("</colgroup>" (dk-process-colgroup))
	("</thead>" (dk-process-thead))
	("</tbody>" (dk-process-tbody))
	("</tr>" (dk-process-tr))
	("</th>" (dk-process-td))
	("</td>" (dk-process-td))
	("</ul>" (dk-process-ul))
	("</ol>" (dk-process-ol))
	("</li>" (dk-process-li))
	("</a>" (dk-process-end-a))
	("</ac:image>" (dk-process-end-acimage)))

      (append-to-buffer (dk-get-parent-buffer)
			1 (point-max))
      ;)))
      (kill-buffer))))

(defsubst dk-process-html-close-tag (close-tag)
  (let ((tag-string  (buffer-substring-no-properties
		      (car (cdr close-tag))
		      (cdr (cdr close-tag)))))
    (with-current-buffer (generate-new-buffer
			  (car close-tag))
      (pcase (car close-tag)
	("<br/>" (dk-process-br))
	("<hr/>" (dk-process-hr))
	("<col/>" (dk-process-col tag-string))
	("<ri:attachment/>"
	 (dk-process-riattachment tag-string)))
      (append-to-buffer (dk-get-parent-buffer)
			1 (point-max))
      (kill-buffer))))

(defsubst dk-add-org-head-properties ()
  (with-current-buffer result-org-buffer
    (when dk-sapwiki-pageID
      (insert "#+PAGEID: " dk-sapwiki-pageID)
      (insert ?\n))
    (when dk-sapwiki-title
      (insert "#+TITLE: " dk-sapwiki-title)
      (insert ?\n))
    (insert "#+STARTUP: align")
    (insert ?\n)))

(defun dk-iterate-html-tag ()  
  (dk-add-org-head-properties)
  ;; If Table of Content is needed?
  (goto-char 1)
  (if (re-search-forward
	 "\\(ac:name=\"toc\"[^>]*\\)\\(/>[^<]*</\\)"
	 nil t)
      (with-current-buffer result-org-buffer
	(insert "#+OPTIONS: toc:1")
	(insert ?\n))
    (with-current-buffer result-org-buffer
	(insert "#+OPTIONS: toc:nil")
	(insert ?\n)))
  
  (setq begin-tag-list ())
  (let ((this-tag))
    (catch 'exit
      (while t
	(setq this-tag (dk-search-html-tag))
	(unless this-tag (throw 'exit t))
	(cond ((dk-check-begin-html-tag (car this-tag))
	       (dk-process-html-begin-tag this-tag))
	      ((dk-check-end-html-tag (car this-tag))
	       (dk-process-html-end-tag this-tag))
	      ((dk-check-close-html-tag (car this-tag))
	       (dk-process-html-close-tag this-tag))
	      (t (user-error "html parse error!"))))))
  (with-current-buffer result-org-buffer
    (org-mode)))
;;------------------------------------------------------
;;End of 2. Convert the wiki html to orgmode format
;;------------------------------------------------------

;;------------------------------------------------------
;;5.4. Convert the orgmode to wiki html
;;------------------------------------------------------
(require 'ox-html)
(eval-when-compile (require 'cl))

(org-export-define-derived-backend 'sapwiki 'html
  :translate-alist '((template . dk-sapwiki-template)
		     (inner-template . dk-sapwiki-inner-template)
		     (keyword . dk-sapwiki-keyword)
		     (headline . dk-sapwiki-headline)
		     (section . dk-sapwiki-section)
		     (paragraph . dk-sapwiki-paragraph)
		     (link . dk-sapwiki-link)
		     (plain-list . dk-sapwiki-plain-list)
		     (table . dk-sapwiki-table)
		     (table-cell . dk-sapwiki-table-cell)
		     (clock . dk-sapwiki-clock)
		     (timestamp . dk-sapwiki-timestamp)
		     (planning . dk-sapwiki-planning)
		     ))

(defun dk-sapwiki-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist holding export options."
  contents)

(defun dk-sapwiki-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat   
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (dk-sapwiki-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))

(defun dk-sapwiki-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "HTML") value)
     ((string= key "TOC")
      (let ((case-fold-search t))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (and (string-match "\\<[0-9]+\\>" value)
			    (string-to-number (match-string 0 value))))
		(localp (org-string-match-p "\\<local\\>" value)))
	    (dk-sapwiki-toc depth info (and localp keyword))))
	 ((string= "listings" value) (org-html-list-of-listings info))
	 ((string= "tables" value) (org-html-list-of-tables info))))))))

(defun dk-sapwiki-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      "<h1><img class=\"editor-inline-macro\" src=\"https://wiki.wdf.sap.corp/wiki/plugins/servlet/confluence/placeholder/macro?definition=e3RvY30&amp;locale=en_GB&amp;version=2\" data-macro-name=\"toc\" data-macro-id=\"d3fd2cf9-db86-4ae0-95b0-bc542e8a1cfe\" data-macro-schema-version=\"1\"></h1>")))

(defun dk-sapwiki-headline (headline contents info)
  "Derive function org-html-headline"
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (section-number (and numbers
				(mapconcat #'number-to-string numbers "-")))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (ids (delq nil
                      (list (org-element-property :CUSTOM_ID headline)
                            (org-export-get-reference headline info)
                            (org-element-property :ID headline))))
           (preferred-id (car ids))
           (extra-ids
	    (mapconcat
	     (lambda (id)
	       (org-html--anchor
		(if (org-uuidgen-p id) (concat "ID-" id) id)
		nil nil info))
	     (cdr ids) "")))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((type (if numberedp 'ordered 'unordered))
                 (itemized-body
                  (org-html-format-list-item
                   contents type nil info nil
                   (concat (org-html--anchor preferred-id nil nil info)
                           extra-ids
                           full-text))))
            (concat (and (org-export-first-sibling-p headline info)
                         (org-html-begin-plain-list type))
                    itemized-body
                    (and (org-export-last-sibling-p headline info)
                         (org-html-end-plain-list type))))
        (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
              (first-content (car (org-element-contents headline))))
          ;; Standard headline.  Export it as a section.
          (format "%s%s\n"
                  (format "\n<h%d>%s%s</h%d>\n"
                          level
                          extra-ids
                          (concat
                           (and numberedp
                                (format
                                 "<span>%s</span> "
                                 (mapconcat #'number-to-string numbers ".")))
                           full-text)
                          level)

                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (dk-sapwiki-section first-content "" info) contents))
                  ))))))

(defun dk-sapwiki-section (section contents info)
  ( or contents "" ))

(defun dk-html-begin-plain-list (type &optional arg1)
  (case type
    (ordered  "<ol>")
    (unordered "<ul>")
    (descriptive "<dl>")))

(defun dk-sapwiki-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to HTML.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
	 (type (org-element-property :type plain-list)))
    (format "%s\n%s%s"
	    (dk-html-begin-plain-list type)
	    contents (org-html-end-plain-list type))))

(defun dk-html--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.INFO is a plist used as a communication channel.  When optional arguments CAPTION and LABEL are given, use them for caption and \"id\" attribute."
  (dk-collect-attachment-comments (format "%s (via emacs)" caption))
  (format "<p>%s</p>\n<p align=\"center\">%s</p>"
	  contents caption))

(defun dk-collect-attachment-comments (caption)
  " (fieldname . \"value\")*"
  (add-to-list 'dk-sapwiki-attachment-comments
	       (cons (dk-get-next-comment-symbol) caption)))

(defun dk-get-next-comment-symbol ()
  (if dk-sapwiki-attachment-comments
      (make-symbol
       (concat "comment_"
	       (number-to-string
		(+ (string-to-number
		    (nth 1
			 (split-string
			  (symbol-name
			   (car (car dk-sapwiki-attachment-comments)))
			  "_")))
		  1))))
    (make-symbol "comment_0")))

(defun dk-html--format-image (source attributes info)
  (dk-collect-attachments source)
  (org-html-close-tag
   "img"
   (org-html--make-attribute-string
    (org-combine-plists
     (list :class "confluence-embedded-image"
	   :src (concat "/wiki/download/attachments/"
			dk-sapwiki-pageID "/"
			(file-name-nondirectory source))
	   :alt (file-name-nondirectory source))
     attributes))
   info))

(defun dk-collect-attachments (source)
  " (fieldname \"filename\" \"MIME type\" \"file data\")*"
  (let* ((filename (file-name-nondirectory source))
	 (mimetype (dk-get-mime-type filename)))
      (add-to-list 'dk-sapwiki-attachments
      		   (list (dk-get-next-fieldname-symbol)
      			 filename
			 mimetype
			 (dk-get-attachment-rawdata source)))))

(defun dk-get-next-fieldname-symbol ()
  (if dk-sapwiki-attachments
      (make-symbol
       (concat "file_"
	       (number-to-string
		(+ (string-to-number
		    (nth 1
			 (split-string
			  (symbol-name
			   (car (car dk-sapwiki-attachments)))
			  "_")))
		  1))))
    (make-symbol "file_0")))
	     

(defun dk-get-mime-type (filename)
  "Currently, only image is allowed! "
  (concat "image/" (file-name-extension filename)))

(defun dk-get-attachment-rawdata (filename)
  "Return the raw data of the attachment
   TODO: try to differenciate the relative path and absolute path"
  (with-temp-buffer
    (insert-file-contents (concat "../image/" filename))
    (buffer-substring-no-properties (point-min) (point-max))))    
  
(defun dk-sapwiki-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((home (when (plist-get info :html-link-home)
		 (org-trim (plist-get info :html-link-home))))
	 (use-abs-url (plist-get info :html-link-use-abs-url))
	 (link-org-files-as-html-maybe
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
	    ;; needed.  See `org-html-link-org-files-as-html'.
	    (cond
	     ((and (plist-get info :html-link-org-files-as-html)
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) "."
		      (plist-get info :html-extension)))
	     (t raw-path))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto"))
	    (org-link-escape-browser
	     (org-link-unescape (concat type ":" raw-path))))
	   ((string= type "file")
	    ;; Treat links to ".org" files as ".html", if needed.
	    (setq raw-path
		  (funcall link-org-files-as-html-maybe raw-path info))
	    ;; If file path is absolute, prepend it with protocol
	    ;; component - "file://".
	    (cond
	     ((file-name-absolute-p raw-path)
	      (setq raw-path (org-export-file-uri raw-path)))
	     ((and home use-abs-url)
	      (setq raw-path (concat (file-name-as-directory home) raw-path))))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id, a headline title a name,
	    ;; a target or a radio-target.
	    (let ((option (org-element-property :search-option link)))
	      (if (not option) raw-path
		(concat raw-path
			"#"
			(org-publish-resolve-external-link
			 option
			 (org-element-property :path link))))))
	   (t raw-path)))
	 ;; Extract attributes from parent's paragraph.  HACK: Only do
	 ;; this for the first link in parent (inner image link for
	 ;; inline images).  This is needed as long as attributes
	 ;; cannot be set on a per link basis.
	 (attributes-plist
	  (let* ((parent (org-export-get-parent-element link))
		 (link (let ((container (org-export-get-parent link)))
			 (if (and (eq (org-element-type container) 'link)
				  (org-html-inline-image-p link info))
			     container
			   link))))
	    (and (eq (org-element-map parent 'link 'identity info t) link)
		 (org-export-read-attribute :attr_html parent))))
	 (attributes
	  (let ((attr (org-html--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	   (org-export-inline-image-p
	    link (plist-get info :html-inline-image-rules)))
      (dk-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (org-export-get-reference destination info)
		  attributes
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; ID link points to an external file.
	  (plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  ((nil)
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (headline
	   (let ((href (or (org-element-property :CUSTOM_ID destination)
			   (org-export-get-reference destination info)))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat #'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc
			(org-export-data
			 (org-element-property :title destination) info)))))
	     (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (t
	   (let* ((ref (org-export-get-reference destination info))
		  (org-html-standalone-image-predicate
		   #'org-html--has-caption-p)
		  (number (cond
			   (desc nil)
			   ((org-html-standalone-image-p destination info)
			    (org-export-get-ordinal
			     (org-element-map destination 'link
			       #'identity info t)
			     info 'link 'org-html-standalone-image-p))
			   (t (org-export-get-ordinal
			       destination info nil 'org-html--has-caption-p))))
		  (desc (cond (desc)
			      ((not number) "No description for this link")
			      ((numberp number) (number-to-string number))
			      (t (mapconcat #'number-to-string number ".")))))
	     (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
	(format "<a href=\"#%s\"%s%s>%s</a>"
		fragment
		(format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			fragment fragment)
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\"%s>%s</a>"
			      (org-html-encode-plain-text path)
			      attributes
			      desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\"%s>%s</a>"
		   (org-html-encode-plain-text path)
		   attributes
		   path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

(defun dk-sapwiki-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (style '((footnote-definition " class=\"footpara\"")
		  (org-data " class=\"footpara\"")))
	 (attributes (org-html--make-attribute-string
		      (org-export-read-attribute :attr_html paragraph)))
	 (extra (or (cadr (assq parent-type style)) "")))
    (cond
     ((and (eq parent-type 'item)
	   (not (org-export-get-previous-element paragraph info))
	   (let ((followers (org-export-get-next-element paragraph info 2)))
	     (and (not (cdr followers))
		  (memq (org-element-type (car followers)) '(nil plain-list)))))
      ;; First paragraph in an item has no tag if it is alone or
      ;; followed, at most, by a sub-list.
      contents)
     ((org-html-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption (org-export-data
		      (org-export-get-caption paragraph) info))		 
	    (label (and (org-element-property :name paragraph)
			(org-export-get-reference paragraph info))))
	(dk-html--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s%s>\n%s</p>"
		(if (org-string-nw-p attributes)
		    (concat " " attributes) "")
		extra contents)))))

(defun dk-sapwiki-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-row (org-export-get-parent table-cell))
	 (table (org-export-get-parent-table table-cell))
	 (cell-attrs
	  (if (not (plist-get info :html-table-align-individual-fields))
	      ""
	    (format " align=\"%s\""
		    (org-export-table-cell-alignment table-cell info)))))
    ;; (when (or (not contents) (string= "" (org-trim contents)))
    ;;   (setq contents "&#xa0;"))
    (cond
     ((and (org-export-table-has-header-p table info)
	   (= 1 (org-export-table-row-group table-row info)))
      (let ((header-tags (plist-get info :html-table-header-tags)))
	(concat "\n" (format (car header-tags) "col" cell-attrs)
		contents
		(cdr header-tags))))
     ((and (plist-get info :html-table-use-header-tags-for-first-column)
	   (zerop (cdr (org-export-table-cell-address table-cell info))))
      (let ((header-tags (plist-get info :html-table-header-tags)))
	(concat "\n" (format (car header-tags) "row" cell-attrs)
		contents
		(cdr header-tags))))
     (t (let ((data-tags (plist-get info :html-table-data-tags)))
	  (concat "\n" (format (car data-tags) cell-attrs)
		  contents
		  (cdr data-tags)))))))

(defun dk-sapwiki-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (case (org-element-property :type table)
    ;; Case 1: table.el table.  Convert it using appropriate tools.
    (table.el (org-html-table--table.el-table table info))
    ;; Case 2: Standard table.
    (t
     (let* ((caption (org-export-get-caption table))
	    (number (org-export-get-ordinal
		     table info nil #'org-html--has-caption-p))
	    (attributes
	     (org-html--make-attribute-string
	      (org-combine-plists
	       (and (org-element-property :name table)
		    (list :id (org-export-get-reference table info)))
	       (and (not (org-html-html5-p info))
		    (plist-get info :html-table-attributes))
	       (org-export-read-attribute :attr_html table))))
	    (table-column-specs
	     (function
	      (lambda (table info)
		(mapconcat
		 (lambda (table-cell)
		   (let* ((alignment (org-export-table-cell-alignment
				      table-cell info))
			  (cellwidth (org-export-table-cell-width
				      table-cell info)))
		     (concat
		      ;; Begin a colgroup?
		      (when (org-export-table-cell-starts-colgroup-p
			     table-cell info)
			"\n<colgroup>")
		      ;; Add a column.  Also specify it's alignment.
		      (format "\n%s"
			      (org-html-close-tag
			       "col" (concat " "
					     (if cellwidth
						 (format "align=\"%s\" width=\"%d\""
							 alignment
							 (* 20 cellwidth))
					       (format "align=\"%s\"" alignment))) info))
		      ;; End a colgroup?
		      (when (org-export-table-cell-ends-colgroup-p
			     table-cell info)
			"\n</colgroup>"))))
		 (org-html-table-first-row-data-cells table info) "\n")))))
       (format "<table%s>\n%s\n%s\n%s</table>"
	       (if (equal attributes "") "" (concat " " attributes))
	       (if (not caption) ""
		 (format (if (plist-get info :html-table-caption-above)
			     "<caption class=\"t-above\">%s</caption>"
			   "<caption class=\"t-bottom\">%s</caption>")
			 (concat
			  "<span class=\"table-number\">"
                          (format (org-html--translate "Table %d:" info) number)
			  "</span> " (org-export-data caption info))))
	       contents
	       (funcall table-column-specs table info))))))
	      ; contents)))))

(defun dk-sapwiki-clock (clock contents info)
  "Transcode a CLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "%s %s %s"
	  org-clock-string
	  (org-timestamp-translate (org-element-property :value clock))
	  (let ((time (org-element-property :duration clock)))
	    (and time (format "(%s)" time)))))

(defun dk-sapwiki-planning (planning contents info)
  "Transcode a PLANNING element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((span-fmt "<span>%s</span> <span>%s</span>"))
    (format
     "<p>%s</p>"
     (mapconcat
      'identity
      (delq nil
	    (list
	     (let ((closed (org-element-property :closed planning)))
	       (when closed
		 (format span-fmt org-closed-string
			 (org-timestamp-translate closed))))
	     (let ((deadline (org-element-property :deadline planning)))
	       (when deadline
		 (format span-fmt org-deadline-string
			 (org-timestamp-translate deadline))))
	     (let ((scheduled (org-element-property :scheduled planning)))
	       (when scheduled
		 (format span-fmt org-scheduled-string
			 (org-timestamp-translate scheduled))))))
      " "))))

(defun dk-sapwiki-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "<span>%s</span>"
	    (replace-regexp-in-string "--" "&#x2013;" value))))

;;;###autoload
(defun dk-sapwiki-export-as-html
  (&optional async subtreep visible-only body-only exp-plist)
  "Export current buffer to an HTML buffer."
  (interactive)
  (setq dk-sapwiki-attachments ())
  (setq dk-sapwiki-attachment-comments ())
  (org-export-to-buffer 'sapwiki "*sapwiki export*"
    async subtreep visible-only body-only
    (cdr (assoc "sapwiki" org-publish-project-alist))
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun dk-html-publish-to-sapwiki (plist filename pub-dir)
  "Publish an org file to pd custom HTML.FILENAME is the filename of the Org file to be published.  PLIST is the property list for the given project.  PUB-DIR is the publishing directory. Return output file name."
  (org-publish-org-to 'sapwiki filename ".html" plist pub-dir))

;;------------------------------------------------------
;;End of 5.4. Convert the orgmode to wiki html
;;------------------------------------------------------

(provide 'sapwiki)
;; sapwiki.el ends here
