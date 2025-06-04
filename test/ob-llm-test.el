;;; ob-llm-test.el --- Tests for ob-llm -*- lexical-binding: t; -*-

(require 'ob-llm)
(require 'ert)

(ert-deftest ob-llm-test-process-header-args-empty ()
  "Test processing empty parameter list."
  (let ((result (ob-llm--process-header-args '())))
    (should (equal (plist-get result :llm-flags) '()))
    (should (equal (plist-get result :org-code-block-header-args) '()))
    (should (equal (plist-get result :custom-params) '()))))

(ert-deftest ob-llm-test-process-header-args-org-code-block-header-args ()
  "Test that all standard org babel parameters are recognized."
  (let* ((params '((:results . "output") (:exports . "results") (:cache . "no")
                   (:noweb . "yes") (:session . "test") (:tangle . "file.txt")
                   (:hlines . "yes") (:colname-names . "yes") (:rowname-names . "yes")
                   (:result-type . "value") (:result-params . "replace")))
         (result (ob-llm--process-header-args params)))
    (should (equal (plist-get result :org-code-block-header-args) (reverse params)))
    (should (equal (plist-get result :llm-flags) '()))
    (should (equal (plist-get result :custom-params) '()))))

(ert-deftest ob-llm-test-process-header-args-llm-flags ()
  "Test that non-org parameters become llm flags."
  (let* ((params '((:model . "gpt-4") (:temperature . "0.7") (:continue . nil)))
         (result (ob-llm--process-header-args params)))
    (should (equal (plist-get result :llm-flags) (reverse params)))
    (should (equal (plist-get result :org-code-block-header-args) '()))
    (should (equal (plist-get result :custom-params) '()))))

(ert-deftest ob-llm-test-process-header-args-custom-params ()
  "Test that custom parameters are categorized correctly."
  (let* ((params '((:database . "/path/to/db") (:no-conversion)))
         (result (ob-llm--process-header-args params)))
    (should (equal (plist-get result :custom-params) (reverse params)))
    (should (equal (plist-get result :llm-flags) '()))
    (should (equal (plist-get result :org-code-block-header-args) '()))))

(ert-deftest ob-llm-test-process-header-args-mixed ()
  "Test processing mixed parameter types."
  (let* ((params '((:results . "raw") (:model . "claude") (:database . "/tmp/db") 
                   (:exports . "both") (:temperature . "0.5") (:no-conversion)))
         (result (ob-llm--process-header-args params))
         (org-code-block-header-args (plist-get result :org-code-block-header-args))
         (llm-flags (plist-get result :llm-flags))
         (custom-params (plist-get result :custom-params)))
    ;; Check org params
    (should (member '(:results . "raw") org-code-block-header-args))
    (should (member '(:exports . "both") org-code-block-header-args))
    ;; Check llm flags
    (should (member '(:model . "claude") llm-flags))
    (should (member '(:temperature . "0.5") llm-flags))
    ;; Check custom params
    (should (member '(:database . "/tmp/db") custom-params))
    (should (member '(:no-conversion) custom-params))))

(provide 'ob-llm-test)

;;; ob-llm-test.el ends here
