(ert-deftest dk-html-tag-test ()
  (set-buffer "example.html")
  (goto-char (point-min))
  (should (equal (dk-search-html-tag) '("<h2>" 1 . 5)))
  (should (equal (dk-search-html-tag) '("</h2>" 23 . 28)))
  (let ((beg-tag (dk-search-html-tag))
	(end-tag (dk-search-html-tag)))
    (should (equal (buffer-substring-no-properties
		    (cdr (cdr beg-tag))
		    (car (cdr end-tag)))
		   "Second level"))))

(ert-deftest dk-html-begin-tag-test ()
  (should (dk-check-valid-html-tag "<h2>"))
  (should (dk-check-begin-html-tag "<table>")))

(ert-deftest dk-html-end-tag-test ()
  (should (dk-check-end-html-tag "</p>"))
  (should (equal (dk-get-html-end-tag "<p>") "</p>"))
  (should (equal (dk-get-html-end-tag "<div>") "</div>")))

(ert-deftest dk-add-tag-to-begin-tag-list ()
  (let ((html-tag '("<h2>" 1 . 5)))
    (dk-process-html-begin-tag html-tag)
  (should (equal (car (nth 0 begin-tag-list))
		 html-tag))
  (should (eq (cdr (nth 0 begin-tag-list))
	      (get-buffer "<h2>"))))
  (kill-buffer "<h2>")) 

(ert-deftest dk-process-html-begin-tag ()
  (dk-kill-dummy-buffers)
  (let ((html-tag '("<h2>" 1 . 5)))
    (dk-process-html-begin-tag html-tag)
    (should (equal (car (nth 0 begin-tag-list))
		   html-tag))
    (should (eq (cdr (nth 0 begin-tag-list))
		(get-buffer "<h2>"))))
  (kill-buffer "<h2>"))
  
(ert-deftest dk-iterate-html-tag ()
  (set-buffer "example.html")
  (goto-char (point-min))
  (dk-iterate-html-tag))






