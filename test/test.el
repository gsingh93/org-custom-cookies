;; TODO: Rename tests
;; TODO: Move .org headings to different test files


(defmacro ert-deftest-matches-expected-file (testname testfile &rest body)
  "TODO"
  `(ert-deftest ,testname ()
     (should (string-equal
              (with-temp-buffer
                (org-mode)
                (insert-file-contents (concat ,testfile))
                (goto-char (point-min))
                ,@body
                (buffer-substring-no-properties (point-min) (point-max)))
              (with-temp-buffer
                (insert-file-contents (concat (symbol-name ',testname) ".expected"))
                (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest org-custom-cookies--timestamp-to-duration-seconds-test ()
  (let ((func 'org-custom-cookies--org-timestamp-string-to-duration-seconds))
    (should (= (funcall func nil) 0))
    (should (= (funcall func "") 0))
    (should (= (funcall func "<2021-11-04 Thu 14:30-15:00>") 1800))
    (should (= (funcall func "<2021-11-04 Thu 14:30>") 0))
    (should (= (funcall func "<2021-11-04 Thu 14:30-14:30>") 0))
    (should (= (funcall func "<2021-11-04 Thu>") 0))))

;;; `org-custom-cookies-update-current-heading' tests

;; On the heading with a scheduled time cookie
(ert-deftest-matches-expected-file update-on-heading
                                   "schedule.org"
                                   (org-custom-cookies-update-current-heading))

;; Inside the heading with a scheduled time cookie
(ert-deftest-matches-expected-file update-inside-heading
                                   "schedule.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-current-heading))

;; Inside a subtree with a scheduled time cookie
(ert-deftest-matches-expected-file update-inside-subtree
                                   "schedule.org"
                                   (org-next-visible-heading 5)
                                   (org-custom-cookies-update-current-heading))

;; On the heading with an effort cookie
(ert-deftest-matches-expected-file effort-update-on-heading
                                   "effort.org"
                                   (org-custom-cookies-update-current-heading))

;; On the heading with a clocked time cookie
(ert-deftest-matches-expected-file clocked-update-on-heading
                                   "clocked.org"
                                   (org-custom-cookies-update-current-heading))

;; On heading with different combinations of cookies
(ert-deftest-matches-expected-file multiple-update-on-heading
                                   "multiple.org"
                                   (org-custom-cookies-update-current-heading))

;;; `org-custom-cookies-update-subtree' tests

;; On heading with different combinations of cookies
(ert-deftest-matches-expected-file multiple-update-subtree-on-heading
                                   "multiple.org"
                                   (org-custom-cookies-update-subtree))

(ert-deftest-matches-expected-file multiple-update-subtree-inside-heading
                                   "multiple.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-subtree))

(ert-deftest-matches-expected-file subtree-update-subtree-above-heading
                                   "subtree.org"
                                   (org-custom-cookies-update-subtree))

(ert-deftest-matches-expected-file subtree-update-subtree-on-heading
                                   "subtree.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-subtree))

(ert-deftest-matches-expected-file subtree-update-subtree-inside-heading
                                   "subtree.org"
                                   (org-next-visible-heading 2)
                                   (org-custom-cookies-update-subtree))

;;; `org-custom-cookies-update-nearest-heading' tests


(ert-deftest-matches-expected-file multiple-update-nearest-on-heading
                                   "multiple.org"
                                   (org-custom-cookies-update-nearest-heading))

(ert-deftest-matches-expected-file multiple-update-nearest-inside-heading
                                   "multiple.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-nearest-heading))

(ert-deftest-matches-expected-file subtree-update-nearest-above-heading
                                   "subtree.org"
                                   (org-custom-cookies-update-nearest-heading))

(ert-deftest-matches-expected-file subtree-update-nearest-on-heading
                                   "subtree.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-nearest-heading))

(ert-deftest-matches-expected-file subtree-update-nearest-inside-heading
                                   "subtree.org"
                                   (org-next-visible-heading 2)
                                   (org-custom-cookies-update-nearest-heading))

;;; `org-custom-cookies-update-containing-subtree' tests

(ert-deftest-matches-expected-file multiple-update-containing-subtree-on-heading
                                   "multiple.org"
                                   (org-custom-cookies-update-containing-subtree))

(ert-deftest-matches-expected-file multiple-update-containing-subtree-inside-heading
                                   "multiple.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-containing-subtree))

(ert-deftest-matches-expected-file subtree-update-containing-subtree-above-heading
                                   "subtree.org"
                                   (org-custom-cookies-update-containing-subtree))

(ert-deftest-matches-expected-file subtree-update-containing-subtree-on-heading
                                   "subtree.org"
                                   (org-next-visible-heading 1)
                                   (org-custom-cookies-update-nearest-heading))

(ert-deftest-matches-expected-file subtree-update-containing-subtree-inside-heading
                                   "subtree.org"
                                   (org-next-visible-heading 2)
                                   (org-custom-cookies-update-nearest-heading))

;;; `org-custom-cookies-update-all' test

(ert-deftest-matches-expected-file multiple-update-all-on-heading
                                   "multiple.org"
                                   (org-custom-cookies-update-all))

(ert-deftest-matches-expected-file subtree-update-all-on-heading
                                   "subtree.org"
                                   (org-custom-cookies-update-all))
