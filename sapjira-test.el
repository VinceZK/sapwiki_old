(ert-deftest dk-sapjira-get-token ()
  (dk-sapjira-get-token)
  (should (string= "BL21-PM8G-VRX3-HE7F"
		   (substring dk-sapjira-atl-token 0 19))))

(ert-deftest sapjira-logwork ()
  (setq dk-sapjira-issue-logs
	'(("<2016-01-19 Tue>" "4h" "Refine the overall architecture diagram")
	  ("<2016-01-20 Wed>" "4h" "Rehearsal and integrate with the team for the Validation Engine demo")
	  ("<2016-01-25 Mon>" "3h" "Presentation on the overall architecture to Helmut")
	  ("<2016-01-26 Tue>" "2h" "Rehearsal for the demo")
	  ("<2016-01-27 Wed>" "4h" "Demonstration the validation Engine to Helmut and the team")))
  (dk-sapjira-browse-issue
   "254"
   (lambda (issue-num)
     (message "Begin logging work for issue: %s...." issue-num)
     (dk-sapjira-logwork-internal issue-num))))

(ert-deftest sapjira-set-done ()
  (dk-sapjira-set-done "254"))

(ert-deftest sapjira-re-open ()
  (dk-sapjira-re-open "254"))

(ert-deftest sapjira-fetch ()
  (sapjira-fetch "68070"))

(ert-deftest sapjira-fetch-open ()
  (sapjira-fetch-open))

(ert-deftest dk-sapjira-get-issue-detail ()
  (dk-sapjira-get-issue-detail "118"))

(ert-deftest dk-sapjira-get-working-sprint ()
  (find-file "sapjira.org")
  (dk-sapjira-get-working-sprint))

(ert-deftest dk-sapjira-setter-test ()
  (find-file "sapjira-table.org")
  (dk-sapjira-setter-test))

(ert-deftest dk-sapjira-check-work-log-success ()
  (let ((dk-sapjira-issue-logs
	 '(("<2016-01-19 Tue>" "4h" "Refine the overall architecture diagram" "X")
	   ("<2016-01-20 Wed>" "4h" "Rehearsal and integrate with the team for the Validation Engine demo" "X")
	   ("<2016-01-25 Mon>" "3h" "Presentation on the overall architecture to Helmut" "X"))))
    (should (dk-sapjira-check-work-log-success)))
  (let ((dk-sapjira-issue-logs
	 '(("<2016-01-19 Tue>" "4h" "Refine the overall architecture diagram" "X")
	   ("<2016-01-20 Wed>" "4h" "Rehearsal and integrate with the team for the Validation Engine demo" nil)
	   ("<2016-01-25 Mon>" "3h" "Presentation on the overall architecture to Helmut" "X"))))
    (should-not (dk-sapjira-check-work-log-success)))
  (let ((dk-sapjira-issue-logs nil))
    (should-not (dk-sapjira-check-work-log-success))))

(ert-deftest dk-sapjira-sprint-post-process ()
  (find-file "sapjira.org")
  (org-map-entries
   'dk-sapjira-sprint-post-process
   "/+OPEN"
   nil
   'dk-sapjira-skip-non-sprint))
