(defmacro ert-deftest-matches-expected-file (testname testfile &rest body)
  "TODO"
  `(ert-deftest ,testname ()
     (should (string-equal
              (with-temp-buffer
                (org-mode)
                (insert-file-contents ,testfile)
                (goto-char (point-min))
                ,@body
                (buffer-string))
              (with-temp-buffer
                (insert-file-contents (concat ,testfile ".expected"))
                (buffer-string))))))

(ert-deftest org-custom-cookies--timestamp-to-duration-seconds-test ()
  (should (= (org-custom-cookies--org-timestamp-string-to-duration-seconds nil) 0))
  (should (= (org-custom-cookies--org-timestamp-string-to-duration-seconds "") 0))
  (should (= (org-custom-cookies--org-timestamp-string-to-duration-seconds "<2021-11-04 Thu 14:30-15:00>") 1800))
  (should (= (org-custom-cookies--org-timestamp-string-to-duration-seconds "<2021-11-04 Thu 14:30>") 0))
  (should (= (org-custom-cookies--org-timestamp-string-to-duration-seconds "<2021-11-04 Thu 14:30-14:30>") 0))
  (should (= (org-custom-cookies--org-timestamp-string-to-duration-seconds "<2021-11-04 Thu>") 0)))

(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-custom-cookies-update))

(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (goto-char 208)
 (org-custom-cookies-update))
