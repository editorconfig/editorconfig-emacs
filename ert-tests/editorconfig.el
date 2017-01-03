(require 'editorconfig)

(defmacro with-visit-file (path &rest body)
  "Visit PATH and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((buf (find-file-noselect ,path)))
     (with-current-buffer buf
       ,@body)
     (kill-buffer buf)))

;;; interactive

(ert-deftest interactive-test-01 nil
  "This test should not run on Travis"
  :tags '(:interactive)
  (should t))

;;; noninteractive, will run on Travis

(ert-deftest has-feature-01 nil
  "minimally working - provides 'editorconfig"
  (should (featurep 'editorconfig)))

(defvar editorconfig-ert-dir
  (concat default-directory
          "ert-tests/plugin-tests/test_files/"))

(ert-deftest test-editorconfig nil
  "Check if properties are applied."
  (editorconfig-mode 1)

  (with-visit-file (concat editorconfig-ert-dir
                           "3_space.txt")
    (should (eq tab-width 3))
    (should (eq indent-tabs-mode nil)))

  (with-visit-file (concat editorconfig-ert-dir
                           "4_space.py")
    (should (eq python-indent-offset 4))
    (should (eq tab-width 8))
    (should (eq indent-tabs-mode nil)))
  (editorconfig-mode -1))
