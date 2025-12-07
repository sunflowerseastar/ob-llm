;;; ob-llm-test.el --- Tests for ob-llm -*- lexical-binding: t; -*-

(require 'ob-llm)
(require 'ert)

(ert-deftest ob-llm--shell-quote-a-string-but-keep-spaces ()
  (should (equal (ob-llm--shell-quote-a-string-but-keep-spaces "myfile.org text/plain")
                 "myfile.org text/plain"))
  (should (equal (ob-llm--shell-quote-a-string-but-keep-spaces  "$cr@zy word|2")
                 "\\$cr\\@zy word\\|2")))

(ert-deftest ob-llm-test-process-header-args-empty ()
  "Test processing empty parameter list."
  (let ((result (ob-llm--process-header-args '())))
    (should (equal (plist-get result :llm-params) '()))
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
    (should (equal (plist-get result :llm-params) '()))
    (should (equal (plist-get result :custom-params) '()))))

(ert-deftest ob-llm-test-process-header-args-llm-params ()
  "Test that non-org parameters become llm flags."
  (let* ((params '((:model . "gpt-4") (:temperature . "0.7") (:continue . nil) (:database "myfile.db")))
         (result (ob-llm--process-header-args params)))
    (should (equal (plist-get result :llm-params) (reverse params)))
    (should (equal (plist-get result :org-code-block-header-args) '()))
    (should (equal (plist-get result :custom-params) '()))))

(ert-deftest ob-llm-test-process-header-args-custom-params ()
  "Test that custom parameters are categorized correctly."
  (let* ((params '((:no-conversion)))
         (result (ob-llm--process-header-args params)))
    (should (equal (plist-get result :custom-params) params))
    (should (equal (plist-get result :llm-params) '()))
    (should (equal (plist-get result :org-code-block-header-args) '()))))

(ert-deftest ob-llm-test-process-header-args-mixed ()
  "Test processing mixed parameter types."
  (let* ((params '((:results . "raw") (:model . "claude") (:database . "/tmp/db/test-db.db")
                   (:exports . "both") (:temperature . "0.5") (:no-conversion)))
         (result (ob-llm--process-header-args params))
         (org-code-block-header-args (plist-get result :org-code-block-header-args))
         (llm-params (plist-get result :llm-params))
         (custom-params (plist-get result :custom-params)))
    ;; Check org params
    (should (member '(:results . "raw") org-code-block-header-args))
    (should (member '(:exports . "both") org-code-block-header-args))
    ;; Check llm flags
    (should (member '(:model . "claude") llm-params))
    (should (member '(:temperature . "0.5") llm-params))
    (should (member '(:database . "/tmp/db/test-db.db") llm-params))
    ;; Check custom params
    (should (member '(:no-conversion) custom-params))))

(ert-deftest ob-llm-test-llm-params->llm-flags ()
  (let ((llm-params '((:temperature . "0.7") (:model . "4o") (:no-log)
                      (:s . "emit gfm; brief"))))
    (should (equal (ob-llm--llm-params->llm-flags llm-params)
                   "--temperature 0.7 --model 4o --no-log -s emit\\ gfm\\;\\ brief "))))

(ert-deftest ob-llm-test-construct-llm-shell-command ()
  (let ((body "hello")
        (raw-params '((:model . "4o") (:user-path . "/tmp/db") (:database . "/tmp/db/test-db-2.db")
                      (:temperature . "0.7") (:no-conversion) (:no-log))))
    (should (equal (ob-llm--construct-llm-shell-command body raw-params)
                   "LLM_USER_PATH=/tmp/db llm hello --no-log --temperature 0.7 --database /tmp/db/test-db-2.db --model 4o "))))

(ert-deftest ob-llm-test-convert-markdown-preserves-nested-lists ()
  "Test that nested bullet lists are preserved during markdown->org conversion."
  (skip-unless (executable-find "pandoc"))
  (let ((input "
- Parent item:
  - Nested item 1
  - Nested item 2
- Another parent"))
    (let ((result (ob-llm--convert-markdown-response-to-org-mode input)))
      ;; Nested items should have leading whitespace (indentation preserved)
      (should (string-match-p "^  - Nested item 1" result))
      (should (string-match-p "^  - Nested item 2" result))
      ;; Parent items should not have leading whitespace
      (should (string-match-p "^- Parent item" result))
      (should (string-match-p "^- Another parent" result)))))

(provide 'ob-llm-test)

;;; ob-llm-test.el ends here
