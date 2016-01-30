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
    (sapjira-logwork "254"))

(ert-deftest sapjira-set-done ()
  (sapjira-set-done "254"))

(ert-deftest sapjira-re-open ()
  (sapjira-re-open "254"))

(ert-deftest sapjira-fetch ()
  (sapjira-fetch "68070"))

(ert-deftest sapjira-fetch-open ()
  (sapjira-fetch-open))

(ert-deftest dk-sapjira-get-issue-detail ()
  (dk-sapjira-get-issue-detail "118"))

(ert-deftest dk-sapjira-export-to-string ()
  (find-file "../sapjira.org")
  (switch-to-buffer (dk-org-sapjira-export-as-json)))

