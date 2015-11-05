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
    "<i>" "<b>" "<code>" "<u>" "<s>" "<strong>" "<table>"
    "<colgroup>" "<col>" "<thead>" "<tr>" "<th>"
    "<tbody>" "<td>" "<ul>" "<li>" "<ol>" "<a>"
    "<ac:image>" "<ri:attachment>" "<br>"))

(defconst dk-wiki-html5-uni-tags
  `("<ri:attachment>" "<br>"))

(defvar begin-tag-list ())
(defvar result-org-buffer (get-buffer-create "result-org-buffer"))

(defun dk-search-html-tag ()
  "Search for html tags <xxx> in current buffer.
 It retruns a list (<tag> start-pos end-pos),
    <tag> is the html tag found.
    beg-pos is the position before the first '<' in the buffer.
    end-pos is the position after the last '>' in the buffer."
  (when (re-search-forward "<[^>]+>" nil t)
    (cons
     (downcase
      (replace-regexp-in-string "[\s-][^>]+" "" (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
     (cons (match-beginning 0) (match-end 0)))))

(defun dk-check-valid-html-tag (tag)
  (member tag dk-wiki-html5-tags))

(defun dk-check-begin-html-tag (tag)
  (and (not (equal (substring tag 1 2 ) "/"))
       (not (member tag dk-wiki-html5-uni-tags))))

(defun dk-check-end-html-tag (tag)
  (equal (substring tag 1 2 ) "/"))

(defun dk-check-uni-html-tag (tag)
  (member tag dk-wiki-html5-uni-tags))

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
      ("&nbsp;" (replace-match ?\s))
      ("&amp;" (replace-match "&")))))
      
(defun dk-process-in-line-ele ()
  (goto-char (point-min))
  (let ((line-num 0))
    ;; First, search and replace emphasis in the paragraph
    (while (re-search-forward "<[^>]*>[^>]*</[^>]*>" nil t 1)
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
    (replace-match "" nil nil)
    ))

(defun dk-process-head-line (stars)
  (dk-process-in-line-ele)
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
  (dk-process-emphasis "-"))

(defsubst dk-process-span ()
  (insert ?\n))

(defsubst dk-process-p ()
  (dk-process-in-line-ele)
  (insert ?\n)
  (insert ?\n))

(defsubst dk-process-table ()
  (goto-char (point-max))
  (insert ?\n))

(defsubst dk-process-thead ()
  (insert "|-")
  (insert ?\n))

(defsubst dk-process-tbody ()
  )

(defsubst dk-process-tr ()
  (goto-char (point-max))
  (insert " |")
  (insert ?\n))

(defsubst dk-process-td ()
  (goto-char 1)
  (insert "| ")
  (while (re-search-forward "[\n]+" nil t)
    (replace-match "" nil nil))
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
       (forward-line))))

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
  (insert "]]")
  (insert ?\n)
  (insert ?\n))

(defsubst dk-process-br ()
  (insert "------")
  (insert ?\n))

(defsubst dk-process-begin-acimage (tag-string)
  (insert "#+CAPTION: ")
  (string-match "\\( ac:title=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (match-string 2 tag-string))
  (insert ?\n))

(defsubst dk-process-riattachment (tag-string)
  (insert "[[")
  (string-match "\\( ri:filename=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (match-string 2 tag-string)))

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
		(equal "</thead>" (car end-tag))
		(equal "</tbody>" (car end-tag))
		(equal "</tr>" (car end-tag))
		(equal "</td>" (car end-tag))
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
	("</p>" (dk-process-p))
	("</table>" (dk-process-table))
	("</thead>" (dk-process-thead))
	("</tbody>" (dk-process-tbody))
	("</tr>" (dk-process-tr))
	("</th>" (dk-process-td))
	("</td>" (dk-process-td))
	("</ul>" (dk-process-ul))
	("</ol>" (dk-process-ol))
	("</li>" (dk-process-li))
	("</a>" (dk-process-end-a))
	("</ac:image>" (dk-process-end-acimage))
	)

      (append-to-buffer (dk-get-parent-buffer)
			1 (point-max))
      (kill-buffer))))

(defsubst dk-process-html-uni-tag (uni-tag)
  (let ((tag-string  (buffer-substring-no-properties
		      (car (cdr uni-tag))
		      (cdr (cdr uni-tag)))))
    (with-current-buffer (generate-new-buffer
			  (car uni-tag))
      (pcase (car uni-tag)
	("<br>" (dk-process-br))
	("<ri:attachment>"
	 (dk-process-riattachment tag-string)))
      (append-to-buffer (dk-get-parent-buffer)
			1 (point-max)))))
	
(defun dk-iterate-html-tag ()
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
	      ((dk-check-uni-html-tag (car this-tag))
	       (dk-process-html-uni-tag this-tag))
	      (t (user-error "html parse error!")))))))
