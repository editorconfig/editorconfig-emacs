;;; editorconfig.el --- Tests editorconfig  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2022 EditorConfig Team

;; This file is part of EditorConfig Emacs Plugin.

;; EditorConfig Emacs Plugin is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; EditorConfig Emacs Plugin is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; EditorConfig Emacs Plugin. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests editorconfig

;;; Code:

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
         (with-current-buffer buf ,@body)
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
  (concat default-directory "ert-tests/plugin-tests/test_files/"))

(defvar editorconfig-secondary-ert-dir
  (concat default-directory "ert-tests/test_files_secondary/"))

(defvar editorconfig-local-variables-ert-dir
  (concat default-directory "ert-tests/local_variables/"))

(ert-deftest test-editorconfig nil
  "Check if properties are applied."
  (editorconfig-mode 1)

  (with-visit-file (concat editorconfig-ert-dir "3_space.txt")
    (should (eq tab-width 3))
    (should (eq indent-tabs-mode nil)))

  (with-visit-file (concat editorconfig-ert-dir "4_space.py")
    (should (eq (bound-and-true-p python-indent-offset) 4))
    (should (eq tab-width 8))
    (should (eq indent-tabs-mode nil)))
  (editorconfig-mode -1))

(ert-deftest test-lisp-use-default-indent nil
  (editorconfig-mode 1)

  (with-visit-file (concat editorconfig-secondary-ert-dir "2_space.el")
    (should (eq lisp-indent-offset 2)))

  (let ((editorconfig-lisp-use-default-indent t))
    (with-visit-file (concat editorconfig-secondary-ert-dir "2_space.el")
      (should (eq lisp-indent-offset nil))))

  (let ((editorconfig-lisp-use-default-indent 2))
    (with-visit-file (concat editorconfig-secondary-ert-dir "2_space.el")
      (should (eq lisp-indent-offset nil))))

  (let ((editorconfig-lisp-use-default-indent 4))
    (with-visit-file (concat editorconfig-secondary-ert-dir "2_space.el")
      (should (eq lisp-indent-offset 2))))
  (editorconfig-mode -1))

(ert-deftest test-trim-trailing-ws nil
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-ert-dir "trim.txt")
    (should (memq #'editorconfig--delete-trailing-whitespace
                  before-save-hook)))
  ;; FIXME: We don't hook into `read-only-mode', so instead we should
  ;; make a more thorough test that saves the file after making the buffer
  ;; read-only and makes sure it does not trim the trailing-ws of the
  ;; buffer/file.
  ;;(with-visit-file (concat editorconfig-ert-dir "trim.txt")
  ;;  (read-only-mode 1)
  ;;  (should (not (memq #'editorconfig--delete-trailing-whitespace
  ;;                     before-save-hook))))
  (editorconfig-mode -1))

(ert-deftest test-charset nil
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-ert-dir "latin1.txt")
    (set-buffer-file-coding-system 'undecided-unix)
    (should (eq buffer-file-coding-system
                'iso-latin-1-unix)))
  (with-visit-file (concat editorconfig-ert-dir "utf-16be.txt")
    (set-buffer-file-coding-system 'undecided-unix)
    (unwind-protect
        (progn
          (save-buffer)
          (should (eq buffer-file-coding-system
                      'utf-16be-with-signature-unix)))
      (delete-file (expand-file-name "utf-16be.txt" editorconfig-ert-dir))))
  (editorconfig-mode -1))


(ert-deftest test-local-variables nil
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-local-variables-ert-dir "file_locals.rb")
    (should (eq tab-width 9))
    (should (eq (bound-and-true-p ruby-indent-level) 7)))

  (with-visit-file (concat editorconfig-local-variables-ert-dir "dir_locals.c")
    (should (eq tab-width 9))
    (should (eq (bound-and-true-p c-basic-offset) 7)))

  (let ((editorconfig-override-file-local-variables nil))
    (with-visit-file (concat editorconfig-local-variables-ert-dir "file_locals.rb")
      (should (eq tab-width 5))
      (should (eq (bound-and-true-p ruby-indent-level) 3))))

  (let ((editorconfig-override-dir-local-variables nil))
    (with-visit-file (concat editorconfig-local-variables-ert-dir "dir_locals.c")
      (should (eq tab-width 5))
      (should (eq (bound-and-true-p c-basic-offset) 3))))
  (editorconfig-mode -1))

(ert-deftest test-file-type-emacs nil
  :expected-result t  ;; Ignore failure
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-secondary-ert-dir "c.txt")
    (should (eq major-mode 'conf-unix-mode)))
  (editorconfig-mode -1))

(ert-deftest test-file-type-ext nil
  :expected-result t  ;; Ignore failure
  (editorconfig-mode 1)
  (with-visit-file (concat editorconfig-secondary-ert-dir "a.txt")
    (should (eq major-mode 'conf-unix-mode)))

  (with-visit-file (concat editorconfig-secondary-ert-dir "bin/perlscript")
    (should (eq major-mode 'perl-mode))
    (should (eq perl-indent-level 5)))
  (editorconfig-mode -1))

(ert-deftest test-hack-properties-functions nil
  (editorconfig-mode 1)
  (add-hook 'editorconfig-hack-properties-functions
            (lambda (props)
              (puthash 'indent_size "5" props)))
  (with-visit-file (concat editorconfig-ert-dir "4_space.py")
    (should (eq (bound-and-true-p python-indent-offset) 5)))
  (setq editorconfig-hack-properties-functions nil)
  (editorconfig-mode -1))

;;; editorconfig.el ends here
