(ert-deftest dk-sapjira-get-token ()
  (dk-sapjira-get-token)
  (should (string= "BL21-PM8G-VRX3-HE7F"
		   (substring dk-sapjira-atl-token 0 19))))

(ert-deftest sapjira-logwork ()
  (sapjira-logwork "140"))
 ;; (dk-sapjira-logwork-internal))
  
(ert-deftest sapjira-fetch ()
  (sapjira-fetch "68070"))

(ert-deftest dk-sapjira-get-issue-detail ()
  (setq dk-sapjira-block-auth t)
  (dk-sapjira-get-issue-detail "118"))
