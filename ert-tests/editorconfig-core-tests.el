;;; editorconfig-core.el --- Tests editorconfig-core  -*- lexical-binding: t -*-

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

;; Tests editorconfig-core

;;; Code:

(require 'editorconfig-core)

(set-variable 'vc-handled-backends nil)

(ert-deftest test-editorconfig-core--get-handles ()
  (let* ((fixtures (concat default-directory "/ert-tests/fixtures/"))
         (dir (concat fixtures "dir1"))
         (confname "parent.ini")
         (handles (editorconfig-core--get-handles dir confname)))
    (should (= 2 (length handles)))
    (should (editorconfig-core-handle-p (car handles)))
    (should (editorconfig-core-handle-p (cadr handles)))))

;;; editorconfig-core.el ends here
