;;; metadata.el --- Metadata before ert-tests  -*- lexical-binding: t -*-

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

;; Metadata before ert-tests

;;; Code:

(require 'package)

(set-variable 'vc-handled-backends nil)

(defvar metadata-el-files nil)

(ert-deftest test-metadata ()
  (dolist (el metadata-el-files)
    (message "Loading info: %s" el)
    (with-temp-buffer
      (insert-file-contents el)
      (message "%S" (package-buffer-info)))))

;;; metadata.el ends here
