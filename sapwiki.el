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
(defconst dk-wiki-html5-tags
  '("<div>" "<h1>" "<h2>" "<h3>" "<h4>" "<h5>" "<p>"
    "<figure>" "<img>" "<figcaption>" "<span>" "<em>"
    "<i>" "<b>" "<code>" "<u>" "<table>" "<colgroup>"
    "<col>" "<thead>" "<tr>" "<th>" "<tbody>" "<td>"
    "<ul>" "<li>" "<ol>" "<a>"))

(defvar begin-tag-list ())
(defvar result-org-buffer (get-buffer-create "result-org-buffer"))

(defun dk-search-html-tag ()
  "Search for html tags <xxx> in current buffer.
 It retruns a list (<tag> start-pos end-pos),
    <tag> is the html tag found.
    beg-pos is the position before the first '<' in the buffer.
    end-pos is the position after the last '>' in the buffer."
  (let* ((start (search-forward "<" nil t))
	 (end (search-forward ">" nil t)))
	 (if (and start end)
	     (cons (buffer-substring-no-properties
		    (- start 1) end)
		   (cons (- start 1) end))
	   nil)))

(defun dk-check-valid-html-tag (tag)
  (-contains-p dk-wiki-html5-tags tag))

(defun dk-check-begin-html-tag (tag)
  (not (equal (substring tag 1 2 ) "/")))

(defun dk-check-end-html-tag (tag)
  (equal (substring tag 1 2 ) "/"))

(defun dk-get-html-end-tag (begin-tag)
  (concat "<" (store-substring begin-tag 0 ?/)))

(defun dk-add-tag-to-begin-tag-list (begin-tag) 
  (push (cons begin-tag (generate-new-buffer (car begin-tag)))
	 begin-tag-list))    

(defsubst dk-process-html-begin-tag (begin-tag)
  (unless (dk-check-valid-html-tag (car begin-tag))
    (user-error "html tag: %s is not valid!" (car begin-tag)))
  (dk-add-tag-to-begin-tag-list begin-tag))

(defsubst dk-process-html-end-tag (end-tag)
  (let ((nearest-tag (pop begin-tag-list)))
    ;; Get the nearest tag and remove it from the global list.
    (unless (equal (dk-get-html-end-tag (car (car nearest-tag)))
		   (car end-tag))
      (user-error "Parsing order error!"))
    (append-to-buffer (cdr nearest-tag)
		      (cdr (cdr (car nearest-tag)))
		      (car (cdr end-tag)))

    (cond ((equal "</h2>" (car end-tag))
	   (with-current-buffer (cdr nearest-tag)
	     (goto-char  (point-min))
	     (insert "** ")
	     (goto-char (point-max))
	     (insert ?\n)))
	  ((equal "</h3>" (car end-tag))
	   (with-current-buffer (cdr nearest-tag)
	     (goto-char (point-min))
	     (insert "*** ")
	     (goto-char (point-max))
	     (insert ?\n)))
	  ((equal "</p>" (car end-tag))
	   (with-current-buffer (cdr nearest-tag))))
    
    (with-current-buffer (cdr nearest-tag)
      (let* ((parent-node (car begin-tag-list))
	     (parent-node-buffer))
	(if parent-node
	    (setq parent-node-buffer (cdr parent-node))
	  (setq parent-node-buffer result-org-buffer))
	(append-to-buffer parent-node-buffer 1 (point-max))
	(kill-buffer)))))
    
(defun dk-iterate-html-tag ( )
  (setq begin-tag-list ())
  (let ((this-tag))
    (catch 'exit
      (while t
	(setq this-tag (dk-search-html-tag))
	(unless this-tag
	  (throw 'exit ))
	(cond ((dk-check-begin-html-tag (car this-tag))
	       (dk-process-html-begin-tag this-tag))
	      ((dk-check-end-html-tag (car this-tag))
	       (dk-process-html-end-tag this-tag))
	      (t (user-error "html parse error!")))))))

(defun dk-kill-dummy-buffers ()
  (kill-buffer "<h2>")
  (kill-buffer "<h2><2>")
  )
    
(defun dk-convert-wiki-to-org (wiki-html-buffer)
  "Convert SAP wiki html to orgmode"
  (interactive)
  (set-buffer wiki-html-buffer)
  (goto-char point-min)

  (search-html-tag-bracket)
)










