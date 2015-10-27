(ert-deftest dk-html-tag-test ()
  (set-buffer "firstOrgNote.html")
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
  (should (equal (dk-get-html-end-tag "<div>") "</div>")))









