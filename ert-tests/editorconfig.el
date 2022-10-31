(require 'editorconfig)

(set-variable 'vc-handled-backends nil)

(defun display-warning (type message &optional level buffer-name)
  "When testing overwrite this function to throw error when called."
  (unless (eq level :debug)
    (error "display-warning called: %S %S %S %S"
           type
           message
           level
           buffer-name)))

(defmacro with-visit-file (path &rest body)
  "Visit PATH and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((buf (find-file-noselect ,path)))
     (unwind-protect
         (with-current-buffer buf
           ,@body)
       (kill-buffer buf))))

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

  ;; Check that directory can be opened with editorconfig enabled
  (with-visit-file editorconfig-ert-dir
    ())

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

(ert-deftest test-file-type-emacs nil
  :expected-result t  ;; Ignore failure
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-secondary-ert-dir
                           "c.txt")
    (should (eq major-mode 'conf-unix-mode)))
  (editorconfig-mode -1))

(ert-deftest test-file-type-ext nil
  :expected-result t  ;; Ignore failure
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-secondary-ert-dir
                           "a.txt")
    (should (eq major-mode 'conf-unix-mode)))

  (with-visit-file (concat editorconfig-secondary-ert-dir
                           "bin/perlscript")
    (should (eq major-mode 'perl-mode))
    (should (eq perl-indent-level 5)))
  (editorconfig-mode -1))

(ert-deftest test-hack-properties-functions nil
  (editorconfig-mode 1)
  (add-hook 'editorconfig-hack-properties-functions
            (lambda (props)
              (puthash 'indent_size "5" props)))
  (with-visit-file (concat editorconfig-ert-dir
                           "4_space.py")
    (should (eq python-indent-offset 5)))
  (setq editorconfig-hack-properties-functions nil)
  (editorconfig-mode -1))
