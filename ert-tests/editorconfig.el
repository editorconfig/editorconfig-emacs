(require 'editorconfig)

;;; interactive

(ert-deftest interactive-test-01 nil
  "This test should not run on Travis"
  :tags '(:interactive)
  (should t))

;;; noninteractive, will run on Travis

(ert-deftest has-feature-01 nil
  "minimally working - provides 'editorconfig"
  (should (featurep 'editorconfig)))

(ert-deftest test-apply-when-find-file ()
  "Test if editorconfig-apply is called when visiting file."
  (editorconfig-mode 1)
  (prefer-coding-system 'utf-8-unix)
  (let ((files-dir (concat default-directory
                     "ert-tests/editorconfig-plugin-tests/test_files/")))
    (dolist (target-charset '(
                               ("latin1.txt" . iso-latin-1-unix)
                               ("utf-8.txt" . utf-8-unix)))
      (let ((target (concat files-dir
                      (car target-charset))))
        (with-current-buffer (find-file-noselect target)
          (should (eq buffer-file-coding-system
                    (cdr target-charset))))))

  ))
