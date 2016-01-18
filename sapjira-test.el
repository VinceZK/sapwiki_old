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
  (require 'url)
  (let* ((my-credetial '(("sapjira.wdf.sap.corp:443" ("i046147" . "Zkle@2016"))))
	 (url-basic-auth-storage 'my-credetial))
    (dk-sapjira-get-issue-detail "118")))
