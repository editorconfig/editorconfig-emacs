;;; editorconfig-core.el --- EditorConfig Core library in Emacs Lisp

;; Copyright (C) 2011-2016 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

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

;; This library is one implementation of EditorConfig Core, which parses
;; .editorconfig files and returns properties for given files.
;; This can be used in place of, for example, editorconfig-core-c.


;; Use from EditorConfig Emacs Plugin

;; Emacs plugin (v0.5 or later) can utilize this implementation.
;; By default, the plugin first search for any EditorConfig executable,
;; and fallback to this library if not found.
;; If you always want to use this library, add following lines to your init.el:

;;     (setq editorconfig-get-properties-function
;;           'editorconfig-core-get-properties-hash)


;; Functions

;; editorconfig-core-get-properties (&optional file confname confversion)

;; Get EditorConfig properties for FILE.

;; If FILE is not given, use currently visiting file.
;; Give CONFNAME for basename of config file other than .editorconfig.
;; If need to specify config format version, give CONFVERSION.

;; This functions returns alist of properties.  Each element will look like
;; (KEY . VALUE) .


;; editorconfig-core-get-properties-hash (&optional file confname confversion)

;; Get EditorConfig properties for FILE.

;; This function is almost same as `editorconfig-core-get-properties', but
;; returns hash object instead.

;;; Code:

(require 'editorconfig-core-handle)


(defconst editorconfig-core-version
  "0.7.8"
  "EditorConfig core version.")

(defun editorconfig-core--remove-duplicate (alist)
  "Remove duplicated keys in ALIST.

If same keys are found in ALIST multiple times, the latter ones take precedence.
For example, when ALIST is

    '((a 1) (b 2) (c 3) (b 4))

then the result will be

    '((a 1) (b 4) (c 3)) ."
  (let ((result ()))
    (dolist (e alist)
      (let ((pair (assoc (car e)
                    result)))
        (if pair
          (setcdr pair
            (cdr e))
          (setq result
            `(,@result ,e)))))
    result))

(defun editorconfig-core--get-handles (dir confname &optional result)
  "Get list of EditorConfig handlers for DIR from CONFNAME.

In the resulting list, the handle for root config file comes first, and the
nearest comes last.
The list may contains nil when no file was found for directories.
RESULT is used internally and normally should not be used."
  (setq dir (expand-file-name dir))
  (let ((handle (editorconfig-core-handle (concat (file-name-as-directory dir)
                                            confname)))
         (parent (file-name-directory (directory-file-name dir))))
    (if (or (string= parent
              dir)
          (and handle
            (editorconfig-core-handle-root-p handle)))
      (cons handle result)
      (editorconfig-core--get-handles parent
        confname
        (cons handle
          result)))))


;;;###autoload
(defun editorconfig-core-get-properties (&optional file confname confversion)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This functions returns alist of properties.  Each element will look like
'(KEY . VALUE) ."
  (setq file (expand-file-name (or file
                                 buffer-file-name
                                 (error "FILE is not given and `buffer-file-name' is nil"))))
  (setq confname (or confname
                   ".editorconfig"))
  (setq confversion (or confversion
                      "0.12.0"))
  (let ((result (editorconfig-core--remove-duplicate
                  (apply 'append
                    (mapcar (lambda (handle)
                              (apply 'append
                                (editorconfig-core-handle-get-properties handle
                                  file)))
                      (editorconfig-core--get-handles (file-name-directory file)
                        confname))))))
    (dolist (key '("end_of_line" "indent_style" "indent_size"
                    "insert_final_newline" "trim_trailing_whitespace" "charset"))
      (let ((pair (assoc key
                    result)))
        (when pair
          (setcdr pair
            (downcase (cdr pair))))))

    ;; Add indent_size property
    (let ((p-indent-size (assoc "indent_size" result))
           (p-indent-style (assoc "indent_style" result)))
      (when (and (not p-indent-size)
              (string= (cdr p-indent-style) "tab")
              ;; If VERSION < 0.9.0, indent_size should have no default value
              (version<= "0.9.0"
                confversion))
        (setq result
          `(,@result ("indent_size" . "tab")))))
    ;; Add tab_width property
    (let ((p-indent-size (assoc "indent_size" result))
           (p-tab-width (assoc "tab_width" result)))
      (when (and p-indent-size
              (not p-tab-width)
              (not (string= (cdr p-indent-size) "tab")))
        (setq result
          `(,@result ("tab_width" . ,(cdr p-indent-size))))))
    ;; Update indent-size property
    (let ((p-indent-size (assoc "indent_size" result))
           (p-tab-width (assoc "tab_width" result)))
      (when (and p-indent-size
              p-tab-width
              (string= (cdr p-indent-size) "tab"))
        (setcdr p-indent-size (cdr p-tab-width))))

    result))

;;;###autoload
(defun editorconfig-core-get-properties-hash (&optional file confname confversion)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead."
  (let ((result (editorconfig-core-get-properties file
                  confname
                  confversion))
         (hash (make-hash-table :test 'equal)))
    (dolist (prop result)
      (puthash (intern (car prop))
        (cdr prop)
        hash))
    hash))

(provide 'editorconfig-core)

;;; editorconfig-core.el ends here
