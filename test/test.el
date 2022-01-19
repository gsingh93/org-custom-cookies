;; TODO: Rename tests
;; TODO: Move .org headings to different test files

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

;; -current-heading while on the heading with a scheduled time cookie
(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-custom-cookies-update-current-heading))

;; -current-heading while inside the heading with a scheduled time cookie
(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-next-visible-heading 1)
 (org-custom-cookies-update-current-heading))

;; -current-heading while inside a subtree scheduled time cookie
(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-next-visible-heading 5)
 (org-custom-cookies-update-current-heading))

;; -current-heading while on the heading with an effort cookie
(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-forward-heading-same-level)
 (org-custom-cookies-update-current-heading))

;; -current-heading while on the heading with a clocked time cookie
(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-forward-heading-same-level 2)
 (org-custom-cookies-update-nearest-heading))

;; -subtree while on subtree with different combinations of cookies
(ert-deftest-matches-expected-file org-custom-cookies-update-on-heading
 "schedule.org"
 (org-forward-heading-same-level 3)
 (org-custom-cookies-update-current-heading))

;; -nearest-heading
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 4)
 (org-next-visible-heading)
 (org-custom-cookies-update-nearest-heading))

;; -nearest-heading
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 4)
 (org-next-visible-heading 2)
 (org-custom-cookies-update-nearest-heading))

;; -nearest-heading
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 4)
 (org-next-visible-heading 3)
 (org-custom-cookies-update-nearest-heading))

;; -nearest-heading
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 4)
 (org-next-visible-heading 4)
 (org-custom-cookies-update-nearest-heading))

;; -containing-subtree
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 5)
 (org-next-visible-heading)
 (org-custom-cookies-update-nearest-heading))

;; -containing-subtree
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 5)
 (org-next-visible-heading 2)
 (org-custom-cookies-update-nearest-heading))

;; -containing-subtree
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 5)
 (org-next-visible-heading 3)
 (org-custom-cookies-update-nearest-heading))

;; -containing-subtree
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 5)
 (org-next-visible-heading 4)
 (org-custom-cookies-update-nearest-heading))

;; -containing-subtree
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (org-forward-heading-same-level 5)
 (org-next-visible-heading 5)
 (org-custom-cookies-update-nearest-heading))

;; -all
(ert-deftest-matches-expected-file org-custom-cookies-update-inside-heading
 "schedule.org"
 (goto-char 208)
 (org-custom-cookies-update-all))
