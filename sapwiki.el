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
;;2. Convert the wiki html to orgmode format
;;------------------------------------------------------
(defconst dk-wiki-html5-tags
  '("<div>" "<h1>" "<h2>" "<h3>" "<h4>" "<h5>" "<p>"
    "<figure>" "<img>" "<figcaption>" "<span>" "<em>"
    "<i>" "<b>" "<code>" "<u>" "<s>" "<strong>" "<table>"
    "<colgroup>" "<col>" "<thead>" "<tr>" "<th>"
    "<tbody>" "<td>" "<ul>" "<li>" "<ol>" "<a>"
    "<ac:image>" "<ri:attachment>" "<sub>" "<sup>"
    "<br>" "<hr>"))

(defconst dk-wiki-html5-uni-tags
  `("<ri:attachment>" "<br>" "<hr>" "<col>"))

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
      (replace-regexp-in-string "[\n\s\t\r][^>]*" "" (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
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
      ("&nbsp;" (replace-match " "))
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
  ;; Remove headline numberring, org-mode doesn't need it
  (when
      (re-search-forward "\\([0-9]+[.]+?\\)+\s" nil t)
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
  (org-table-align)
  (goto-char (point-max)) 
  (insert ?\n))

(defsubst dk-process-colgroup ()
  ;(goto-char (point-max))
  (insert " |")
  (insert ?\n))

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
  (insert ?\n))

(defsubst dk-process-hr ()
  (insert "------")
  (insert ?\n))

(defsubst dk-process-col (tag-string)
  (insert "| <")
  (string-match "\\( org_width=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (match-string 2 tag-string))
  (insert "> "))

(defsubst dk-process-riattachment (tag-string)
  (insert "[[")
  (string-match "\\( ri:filename=\"\\)\\([^\"]+\\)"
		tag-string)
  (insert (match-string 2 tag-string)))

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
		(equal "</td>" (car end-tag))
		(equal "</th>" (car end-tag))
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
	("<hr>" (dk-process-hr))
	("<col>" (dk-process-col tag-string))
	("<ri:attachment>"
	 (dk-process-riattachment tag-string)))
      (append-to-buffer (dk-get-parent-buffer)
			1 (point-max))
      (kill-buffer))))

(defsubst dk-add-org-head-properties ()
  (with-current-buffer result-org-buffer
    (insert "#+STARTUP: align")
    (insert ?\n)))

(defun dk-iterate-html-tag ()
  (dk-add-org-head-properties)
  (goto-char 1)
  (when (re-search-forward
	 "\\(<ac:macro[^>]+\\)\\(ac:name=\"toc\"[^>]*\\)\\(/>[^<]*</\\)"
	 nil t)
    (with-current-buffer result-org-buffer
      (insert "#+OPTIONS: toc")
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
	      ((dk-check-uni-html-tag (car this-tag))
	       (dk-process-html-uni-tag this-tag))
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
		     (headline . dk-sapwiki-headline)
		     (section . dk-sapwiki-section)
		     (paragraph . dk-sapwiki-paragraph)
		    ; (subscript . dk-sapwiki-subscript)
		     ))

(defun dk-sapwiki-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist holding export options."
  contents)

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
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (mapconcat #'number-to-string numbers ".")))
                           full-text)
                          level)

                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (dk-html-section first-content "" info) contents))
                  ))))))

(defun dk-sapwiki-section (section contents info)
  ( or contents "" ))

(defun dk-sapwiki-subscript (subscript contents info)
  (format "_%s" contents))

(defun org-html-begin-plain-list (type &optional arg1)
  "Insert the beginning of the HTML list depending on TYPE. When ARG1 is a string, use it as the start parameter for ordered lists.The function is overwriten only because it is convient to do so. It should be changed in a more tender way!"
  (case type
    (ordered  "<ol>")
    (unordered "<ul>")
    (descriptive "<dl>")))

(defun org-html--wrap-image (contents info &optional caption label)
  "Wrap CONTENTS string within an appropriate environment for images.INFO is a plist used as a communication channel.  When optional arguments CAPTION and LABEL are given, use them for caption and \"id\" attribute."
  (let ((html5-fancy (org-html--html5-fancy-p info)))
    (format (if html5-fancy "\n<figure>\n%s%s\n</figure>"
	      "\n<div%s class=\"figure\">%s%s\n</div>")
	    ;; Contents.
	     contents
	    ;; Caption.
	    (if (not (org-string-nw-p caption)) ""
	      (format (if html5-fancy "\n<figcaption>%s</figcaption>"
			"\n<p>%s</p>")
		      caption)))))

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
	(org-html--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s%s>\n%s</p>"
		(if (org-string-nw-p attributes)
		    (concat " " attributes) "")
		extra contents)))))

(defun dk-html-publish-to-sapwiki (plist filename pub-dir)
  "Publish an org file to pd custom HTML.FILENAME is the filename of the Org file to be published.  PLIST is the property list for the given project.  PUB-DIR is the publishing directory. Return output file name."
  (org-publish-org-to 'sapwiki filename ".html" plist pub-dir))

;;------------------------------------------------------
;;End of 5.4. Convert the orgmode to wiki html
;;------------------------------------------------------

(provide 'sapwiki)
;; sapwiki.el ends here
