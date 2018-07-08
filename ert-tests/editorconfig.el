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

(defvar editorconfig-secondary-ert-dir
  (concat default-directory
          "ert-tests/test_files_secondary/"))

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

(ert-deftest test-lisp-use-default-indent nil
  (editorconfig-mode 1)

  (with-visit-file (concat editorconfig-secondary-ert-dir
                     "2_space.el")
    (should (eq lisp-indent-offset 2)))

  (let ((editorconfig-lisp-use-default-indent t))
    (with-visit-file (concat editorconfig-secondary-ert-dir
                       "2_space.el")
      (should (eq lisp-indent-offset nil))))

  (let ((editorconfig-lisp-use-default-indent 2))
    (with-visit-file (concat editorconfig-secondary-ert-dir
                       "2_space.el")
      (should (eq lisp-indent-offset nil))))

  (let ((editorconfig-lisp-use-default-indent 4))
    (with-visit-file (concat editorconfig-secondary-ert-dir
                       "2_space.el")
      (should (eq lisp-indent-offset 2))))
  (editorconfig-mode -1))

(ert-deftest test-trim-trailing-ws nil
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-ert-dir
                           "trim.txt")
    (should (memq 'delete-trailing-whitespace
                  write-file-functions)))
  (with-visit-file (concat editorconfig-ert-dir
                           "trim.txt")
    (read-only-mode 1)
    (should (not (memq 'delete-trailing-whitespace
                       write-file-functions))))
  (editorconfig-mode -1))
