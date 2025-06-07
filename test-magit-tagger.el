;;; test-magit-tagger.el --- Tests for magit-tagger -*- lexical-binding: t; -*-

;; Simple test functions to verify magit-tagger functionality

;;; Code:

(require 'magit-tagger)

(defun test-magit-tagger-version-parsing ()
  "Test version parsing functionality."
  (interactive)
  (let ((test-cases '(("v1.2.3" . (1 2 3))
                      ("v0.0.1" . (0 0 1))
                      ("v10.20.30" . (10 20 30))
                      ("1.2.3" . (1 2 3)))))
    (dolist (test-case test-cases)
      (let ((input (car test-case))
            (expected (cdr test-case))
            (actual (magit-tagger--parse-version (car test-case))))
        (if (equal actual expected)
            (message "✓ PASS: %s -> %s" input actual)
          (message "✗ FAIL: %s -> %s (expected %s)" input actual expected))))))

(defun test-magit-tagger-version-increment ()
  "Test version increment functionality."
  (interactive)
  (let ((test-cases '(((1 2 3) patch . (1 2 4))
                      ((1 2 3) minor . (1 3 0))
                      ((1 2 3) major . (2 0 0))
                      ((0 0 1) patch . (0 0 2)))))
    (dolist (test-case test-cases)
      (let* ((input-version (car test-case))
             (increment-type (cadr test-case))
             (expected (cddr test-case))
             (actual (magit-tagger--increment-version input-version increment-type)))
        (if (equal actual expected)
            (message "✓ PASS: %s %s -> %s" input-version increment-type actual)
          (message "✗ FAIL: %s %s -> %s (expected %s)"
                   input-version increment-type actual expected))))))

(defun test-magit-tagger-version-formatting ()
  "Test version formatting functionality."
  (interactive)
  (let ((test-cases '(((1 2 3) . "v1.2.3")
                      ((0 0 1) . "v0.0.1")
                      ((10 20 30) . "v10.20.30"))))
    (dolist (test-case test-cases)
      (let ((input (car test-case))
            (expected (cdr test-case))
            (actual (magit-tagger--format-version (car test-case))))
        (if (equal actual expected)
            (message "✓ PASS: %s -> %s" input actual)
          (message "✗ FAIL: %s -> %s (expected %s)" input actual expected))))))

(defun test-magit-tagger-validation ()
  "Test version validation functionality."
  (interactive)
  (let ((test-cases '(("v1.2.3" . t)
                      ("v0.0.1" . t)
                      ("v10.20.30" . t)
                      ("1.2.3" . t)
                      ("v1.2" . nil)
                      ("v1.2.3.4" . nil)
                      ("vx.y.z" . nil)
                      ("invalid" . nil))))
    (dolist (test-case test-cases)
      (let ((input (car test-case))
            (expected (cdr test-case))
            (actual (magit-tagger--validate-version-format (car test-case))))
        (if (eq (not (not actual)) expected)
            (message "✓ PASS: %s -> %s" input actual)
          (message "✗ FAIL: %s -> %s (expected %s)" input actual expected))))))

(defun test-magit-tagger-all ()
  "Run all magit-tagger tests."
  (interactive)
  (message "Running magit-tagger tests...")
  (message "")
  (message "=== Version Parsing Tests ===")
  (test-magit-tagger-version-parsing)
  (message "")
  (message "=== Version Increment Tests ===")
  (test-magit-tagger-version-increment)
  (message "")
  (message "=== Version Formatting Tests ===")
  (test-magit-tagger-version-formatting)
  (message "")
  (message "=== Version Validation Tests ===")
  (test-magit-tagger-validation)
  (message "")
  (message "All tests completed!"))

(provide 'test-magit-tagger)

;;; test-magit-tagger.el ends here
