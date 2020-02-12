;;; editorconfig.el --- EditorConfig Emacs Plugin  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2020 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.8.1
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

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

;; EditorConfig helps developers define and maintain consistent
;; coding styles between different editors and IDEs.

;; The EditorConfig project consists of a file format for defining
;; coding styles and a collection of text editor plugins that enable
;; editors to read the file format and adhere to defined styles.
;; EditorConfig files are easily readable and they work nicely with
;; version control systems.

;;; Code:
(require 'cl-lib)
(eval-when-compile
  (require 'rx)
  (defvar tex-indent-basic)
  (defvar tex-indent-item)
  (defvar tex-indent-arg)
  (defvar evil-shift-width))

(declare-function editorconfig-core-get-properties-hash
                  "editorconfig-core"
                  nil)

(defgroup editorconfig nil
  "EditorConfig Emacs Plugin.

EditorConfig helps developers define and maintain consistent
coding styles between different editors and IDEs."
  :tag "EditorConfig"
  :prefix "editorconfig-"
  :group 'tools)

(defcustom editorconfig-exec-path
  "editorconfig"
  "Path to EditorConfig executable.

Used by `editorconfig-call-editorconfig-exec'."
  :type 'string
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-exec-path
  'editorconfig-exec-path
  "0.5")

(defcustom editorconfig-get-properties-function
  'editorconfig-get-properties
  "A function which gets EditorConofig properties for current buffer.

This function will be called with no argument and should return a
hash object containing properties, or nil if any core program is
not available.  Keys of this hash should be symbols of properties, and values
should be strings of their values.


For example, if you always want to use built-in core library instead
of any EditorConfig executable to get properties, add following to
your init.el:

  (set-variable 'editorconfig-get-properties-function
                #'editorconfig-core-get-properties-hash)

Possible known values are:

* `editorconfig-get-properties' (default)
  * Use `editorconfig-get-properties-from-exec' when
    `editorconfig-exec-path' executable executable is found, otherwise
    use `editorconfig-core-get-properties-hash'
* `editorconfig-get-properties-from-exec'
  * Get properties by executing EditorConfig executable
* `editorconfig-core-get-properties-hash'
  * Always use built-in Emacs-Lisp implementation to get properties"
  :type 'function
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-get-properties-function
  'editorconfig-get-properties-function
  "0.5")

(defcustom editorconfig-mode-lighter " EditorConfig"
  "`editorconfig-mode' lighter string."
  :type 'string
  :group 'editorconfig)

(defcustom editorconfig-after-apply-functions ()
  "A list of functions after loading common EditorConfig settings.

Each element in this list is a hook function.  This hook function
takes one parameter, which is a property hash table.  The value
of properties can be obtained through gethash function.

The hook does not have to be coding style related; you can add
whatever functionality you want.  For example, the following is
an example to add a new property emacs_linum to decide whether to
show line numbers on the left:

  (add-hook 'editorconfig-after-apply-functions
    '(lambda (props)
       (let ((show-line-num (gethash 'emacs_linum props)))
         (cond ((equal show-line-num \"true\") (linum-mode 1))
           ((equal show-line-num \"false\") (linum-mode 0))))))

This hook will be run even when there are no matching sections in
\".editorconfig\", or no \".editorconfig\" file was found at all."
  :type 'hook
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-custom-hooks
  'editorconfig-after-apply-functions
  "0.5")
(define-obsolete-variable-alias
  'editorconfig-custom-hooks
  'editorconfig-after-apply-functions
  "0.7.14")

(defcustom editorconfig-hack-properties-functions ()
  "A list of function to alter property values before applying them.

These functions will be run after loading \".editorconfig\" files and before
applying them to current buffer, so that you can alter some properties from
\".editorconfig\" before they take effect.

For example, Makefiles always use tab characters for indentation: you can
overwrite \"indent_style\" property when current `major-mode' is a
`makefile-mode' with following code:

  (add-hook 'editorconfig-hack-properties-functions
            '(lambda (props)
               (when (derived-mode-p 'makefile-mode)
                 (puthash 'indent_style \"tab\" props))))

This hook will be run even when there are no matching sections in
\".editorconfig\", or no \".editorconfig\" file was found at all."
  :type 'hook
  :group 'editorconfig)

(defcustom editorconfig-indentation-alist
  ;; For contributors: Sort modes in alphabetical order
  '((apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (bpftrace-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (css-mode css-indent-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (enh-ruby-mode enh-ruby-indent-level)
    (erlang-mode erlang-indent-level)
    (ess-mode ess-indent-offset)
    (f90-mode f90-associate-indent
              f90-continuation-indent
              f90-critical-indent
              f90-do-indent
              f90-if-indent
              f90-program-indent
              f90-type-indent)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (fsharp-mode fsharp-continuation-offset
                 fsharp-indent-level
                 fsharp-indent-offset)
    (groovy-mode groovy-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  haskell-indentation-layout-offset
                  haskell-indentation-left-offset
                  haskell-indentation-starter-offset
                  haskell-indentation-where-post-offset
                  haskell-indentation-where-pre-offset
                  shm-indent-spaces)
    (haxor-mode haxor-tab-width)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js-jsx-mode js-indent-level sgml-basic-offset)
    (js2-mode js2-basic-offset)
    (js2-jsx-mode js2-basic-offset sgml-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (julia-mode julia-indent-offset)
    (kotlin-mode kotlin-tab-width)
    (latex-mode . editorconfig-set-indentation-latex-mode)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (lua-mode lua-indent-level)
    (matlab-mode matlab-indent-level)
    (mips-mode mips-tab-width)
    (mustache-mode mustache-basic-offset)
    (nasm-mode nasm-basic-offset)
    (nginx-mode nginx-indent-level)
    (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
    (objc-mode c-basic-offset)
    (octave-mode octave-block-offset)
    (perl-mode perl-indent-level)
    ;; No need to change `php-mode-coding-style' value for php-mode
    ;; since we run editorconfig later than it resets `c-basic-offset'.
    ;; See https://github.com/editorconfig/editorconfig-emacs/issues/116
    ;; for details.
    (php-mode c-basic-offset)
    (pike-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (puppet-mode puppet-indent-level)
    (python-mode . editorconfig-set-indentation-python-mode)
    (ruby-mode ruby-indent-level)
    (rust-mode rust-indent-offset)
    (rustic-mode rustic-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (slim-mode slim-indent-offset)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (terra-mode terra-indent-level)
    (typescript-mode typescript-indent-level)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode (web-mode-indent-style . (lambda (size) 2))
              web-mode-attr-indent-offset
              web-mode-attr-value-indent-offset
              web-mode-code-indent-offset
              web-mode-css-indent-offset
              web-mode-markup-indent-offset
              web-mode-sql-indent-offset
              web-mode-block-padding
              web-mode-script-padding
              web-mode-style-padding)
    (yaml-mode yaml-indent-offset))
  "Alist of indentation setting methods by modes.

Each element looks like (MODE . FUNCTION) or (MODE . INDENT-SPEC-LIST).

If FUNCTION is provided, it will be called when setting the
indentation.  The indent size will be passed.

If INDENT-SPEC-LIST is provided, each element of it must have one of the
following forms:

 1. VARIABLE
    It means (VARIABLE . 1).

 2. (VARIABLE . SPEC)
    Setting VARIABLE according to the type of SPEC:

      - Integer
        The value is (* SPEC INDENT-SIZE);

      - Function
        The value is (funcall SPEC INDENT-SIZE);

      - Any other type.
        The value is SPEC.

NOTE: Only the **buffer local** value of VARIABLE will be set."
  :type '(alist :key-type symbol :value-type sexp)
  :risky t
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-indentation-alist
  'editorconfig-indentation-alist
  "0.5")

(defcustom editorconfig-exclude-modes ()
  "Modes in which `editorconfig-mode-apply' will not run."
  :type '(repeat (symbol :tag "Major Mode"))
  :group 'editorconfig)

(defcustom editorconfig-exclude-regexps
  (list (eval-when-compile
          (rx string-start (or "http" "https" "ftp" "sftp" "rsync") ":")))
  "List of regexp for buffer filenames `editorconfig-mode-apply' will not run.

When variable `buffer-file-name' matches any of the regexps, then
`editorconfig-mode-apply' will not do its work."
  :type '(repeat string)
  :group 'editorconfig)

(defcustom editorconfig-trim-whitespaces-mode nil
  "Buffer local minor-mode to use to trim trailing whitespaces.

If set, enable that mode when `trim_trailing_whitespace` is set to true.
Otherwise, use `delete-trailing-whitespace'."
  :type 'symbol
  :group 'editorconfig)

(defvar editorconfig-properties-hash nil
  "Hash object of EditorConfig properties that was enabled for current buffer.
Set by `editorconfig-apply' and nil if that is not invoked in
current buffer yet.")
(make-variable-buffer-local 'editorconfig-properties-hash)
(put 'editorconfig-properties-hash
     'permanent-local
     t)

(defvar editorconfig-lisp-use-default-indent nil
  "Selectively ignore the value of indent_sizefor Lisp files.
Prevents `lisp-indent-offset' from being set selectively.

nil - `lisp-indent-offset' is always set normally.
t   - `lisp-indent-offset' is never set normally
       (always use default indent for lisps).
number - `lisp-indent-offset' is not set only if indent_size is
         equal to this number.  For example, if this is set to 2,
         `lisp-indent-offset'will not be set only if indent_size is 2.")

(defconst editorconfig-unset-value "unset"
  "String of value used to unset properties in .editorconfig .")

(defun editorconfig-string-integer-p (string)
  "Return non-nil if STRING represents integer."
  (and (stringp string)
       (string-match-p "\\`[0-9]+\\'" string)))

(defun editorconfig-set-indentation-python-mode (size)
  "Set `python-mode' indent size to SIZE."
  (set (make-local-variable (if (or (> emacs-major-version 24)
                                    (and (= emacs-major-version 24)
                                         (>= emacs-minor-version 3)))
                                'python-indent-offset
                              'python-indent))
       size)
  ;; For https://launchpad.net/python-mode
  (when (boundp 'py-indent-offset)
    (set (make-local-variable 'py-indent-offset) size)))

(defun editorconfig-set-indentation-latex-mode (size)
  "Set `latex-mode' indent size to SIZE."
  (set (make-local-variable 'tex-indent-basic) size)
  (set (make-local-variable 'tex-indent-item) size)
  (set (make-local-variable 'tex-indent-arg) (* 2 size))
  ;; For AUCTeX
  (when (boundp 'TeX-brace-indent-level)
    (set (make-local-variable 'TeX-brace-indent-level) size))
  (when (boundp 'LaTeX-indent-level)
    (set (make-local-variable 'LaTeX-indent-level) size))
  (when (boundp 'LaTeX-item-indent)
    (set (make-local-variable 'LaTeX-item-indent) (- size))))

(defun editorconfig--should-set (size symbol)
  "Determines if editorconfig should set SYMBOL using SIZE."
  (if (eq symbol 'lisp-indent-offset)
      (cond
       ((eql nil editorconfig-lisp-use-default-indent)
        t)
       ((eql t editorconfig-lisp-use-default-indent)
        nil)
       ((numberp editorconfig-lisp-use-default-indent)
        (not (eql size editorconfig-lisp-use-default-indent)))
       (t t))
    t))

(defun editorconfig-set-indentation (style &optional size tab_width)
  "Set indentation type from STYLE, SIZE and TAB_WIDTH."
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'tab-width)
  (if (editorconfig-string-integer-p size)
      (setq size (string-to-number size))
    (when (not (equal size "tab")) (setq size nil)))
  (setq tab-width (cond (tab_width (string-to-number tab_width))
                        ((numberp size) size)
                        (t tab-width)))
  (when (equal size "tab")
    (setq size tab-width))
  (cond ((equal style "space")
         (setq indent-tabs-mode nil))
        ((equal style "tab")
         (setq indent-tabs-mode t)))
  (when size
    (when (featurep 'evil)
      (set (make-local-variable 'evil-shift-width) size))
    (let ((parent major-mode)
          entry)
      ;; Find the closet parent mode of `major-mode' in
      ;; `editorconfig-indentation-alist'.
      (while (and (not (setq entry (assoc parent editorconfig-indentation-alist)))
                  (setq parent (get parent 'derived-mode-parent))))
      (when entry
        (let ((fn-or-list (cdr entry)))
          (cond ((functionp fn-or-list) (funcall fn-or-list size))
                ((listp fn-or-list)
                 (dolist (elem fn-or-list)
                   (cond ((and (symbolp elem)
                               (editorconfig--should-set size elem))
                          (set (make-local-variable elem) size))
                         ((and (consp elem)
                               (editorconfig--should-set size (car elem)))
                          (let ((spec (cdr elem)))
                            (set (make-local-variable (car elem))
                                 (cond ((functionp spec) (funcall spec size))
                                       ((integerp spec) (* spec size))
                                       (t spec))))))))))))))

(defun editorconfig-set-coding-system (end-of-line charset)
  "Set buffer coding system by END-OF-LINE and CHARSET."
  (let ((eol (cond
              ((equal end-of-line "lf") 'undecided-unix)
              ((equal end-of-line "cr") 'undecided-mac)
              ((equal end-of-line "crlf") 'undecided-dos)
              (t 'undecided)))
        (cs (cond
             ((equal charset "latin1") 'iso-latin-1)
             ((equal charset "utf-8") 'utf-8)
             ((equal charset "utf-8-bom") 'utf-8-with-signature)
             ((equal charset "utf-16be") 'utf-16be-with-signature)
             ((equal charset "utf-16le") 'utf-16le-with-signature)
             (t 'undecided))))
    (unless (and (eq eol 'undecided)
                 (eq cs 'undecided))
      (set-buffer-file-coding-system (merge-coding-systems
                                      cs
                                      eol)
                                     nil t))))

(defun editorconfig-set-trailing-nl (final-newline)
  "Set up requiring final newline by FINAL-NEWLINE.

This function will set `require-final-newline' and `mode-require-final-newline'
to non-nil when FINAL-NEWLINE is true."
  (cond
   ((equal final-newline "true")
    ;; keep prefs around how/when the nl is added, if set - otherwise add on save
    (set      (make-local-variable 'require-final-newline)      (or require-final-newline t))
    (set      (make-local-variable 'mode-require-final-newline) (or mode-require-final-newline t)))
   ((equal final-newline "false")
    ;; FIXME: Add functionality for actually REMOVING any trailing newlines here!
    ;;        (rather than just making sure we don't automagically ADD a new one)
    (set      (make-local-variable 'require-final-newline) nil)
    (set      (make-local-variable 'mode-require-final-newline) nil))))

(defun editorconfig-set-trailing-ws (trim-trailing-ws)
  "Set up trimming of trailing whitespace at end of lines by TRIM-TRAILING-WS."
  (make-local-variable 'write-file-functions) ;; just current buffer
  (when (and (equal trim-trailing-ws "true")
             (not buffer-read-only))
    ;; when true we push delete-trailing-whitespace (emacs > 21)
    ;; to write-file-functions
    (if editorconfig-trim-whitespaces-mode
        (funcall editorconfig-trim-whitespaces-mode 1)
      (add-to-list
       'write-file-functions
       'delete-trailing-whitespace)))
  (when (or (equal trim-trailing-ws "false")
            buffer-read-only)
    ;; when false we remove every delete-trailing-whitespace
    ;; from write-file-functions
    (when editorconfig-trim-whitespaces-mode
      (funcall editorconfig-trim-whitespaces-mode 0))
    (setq
     write-file-functions
     (delete
      'delete-trailing-whitespace
      write-file-functions))))

(defun editorconfig-set-line-length (length)
  "Set the max line length (`fill-column') to LENGTH."
  (when (and (editorconfig-string-integer-p length)
             (> (string-to-number length) 0))
    (setq fill-column (string-to-number length))))

(defvar editorconfig-file-type-emacs-whitelist
  (append (mapcar 'car
                  editorconfig-indentation-alist)
          '(conf-mode))
  "List of known `major-mode' that can be used for file_type_emacs value.")

;; Emacs<26 does not have provided-mode-derived-p
(defun editorconfig--provided-mode-derived-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
  (if (fboundp 'provided-mode-derived-p)
      (apply 'provided-mode-derived-p mode modes)
    (while (and (not (memq mode modes))
                (setq mode (get mode 'derived-mode-parent))))
    mode))


(defun editorconfig-set-major-mode-from-name (filetype)
  "Set buffer `major-mode' by FILETYPE.

FILETYPE should be s string like `\"ini\"`, if not nil or empty string."
  (let ((mode (and filetype
                   (not (string= filetype
                                 ""))
                   (intern (concat filetype
                                   "-mode")))))
    (when mode
      (if (fboundp mode)
          (if (apply 'editorconfig--provided-mode-derived-p mode
                     editorconfig-file-type-emacs-whitelist)
              (editorconfig-apply-major-mode-safely mode)
            (display-warning :error (format "Major-mode `%S' is not listed in `%S'"
                                            mode
                                            'editorconfig-file-type-emacs-whitelist)))
        (display-warning :error (format "Major-mode `%S' not found"
                                        mode))
        nil))))

(defvar editorconfig--apply-major-mode-currently nil
  "Used internally.")
(make-variable-buffer-local 'editorconfig--apply-major-mode-currently)
(put 'editorconfig--apply-major-mode-currently
     'permanent-local
     t)

(defun editorconfig-apply-major-mode-safely (mode)
  "Set `major-mode' to MODE.
Normally `editorconfig-apply' will be hooked so that it runs when changing
`major-mode', so there is a possibility that MODE is called infinitely if
MODE is called naively from inside of `editorconfig-apply'.
This function will avoid such cases and set `major-mode' safely.

Just checking current `major-mode' value is not enough, because it can be
different from MODE value (for example, `conf-mode' will set `major-mode' to
`conf-unix-mode' or another conf mode)."
  (cl-assert mode)
  (when (and (not (eq mode
                      editorconfig--apply-major-mode-currently))
             (not (eq mode
                      major-mode))
             (not (derived-mode-p mode)))
    (unwind-protect
        (progn
          (setq editorconfig--apply-major-mode-currently
                mode)
          (funcall mode))
      (setq editorconfig--apply-major-mode-currently
            nil))))

(defun editorconfig--find-mode-from-ext (ext &optional filename)
  "Get suitable `major-mode' from EXT and FILENAME.
If FILENAME is omitted filename of current buffer is used."
  (cl-assert ext)
  (cl-assert (not (string= ext "")))
  (let* ((name (concat (or filename
                           buffer-file-name)
                       "."
                       ext)))
    (assoc-default name
                   auto-mode-alist
                   'string-match)))

(defun editorconfig-set-major-mode-from-ext (ext)
  "Set buffer `major-mode' by EXT.

EXT should be a string like `\"ini\"`, if not nil or empty string."
  (cl-assert buffer-file-name)
  (when (and ext
             (not (string= ext ""))
             (not (string= ext editorconfig-unset-value)))

    (let ((mode (editorconfig--find-mode-from-ext ext
                                                  buffer-file-name)))
      (if mode
          (editorconfig-apply-major-mode-safely mode)
        (display-warning :error (format "Major-mode for `%s' not found"
                                        ext))
        nil))))

(defun editorconfig-call-editorconfig-exec ()
  "Call EditorConfig core and return output."
  (let* ((filename (buffer-file-name))
         (filename (and filename (expand-file-name filename))))
    (if filename
        (with-temp-buffer
          (setq default-directory "/")
          (if (eq 0
                  (call-process editorconfig-exec-path nil t nil filename))
              (buffer-string)
            (error (buffer-string))))
      "")))

(defun editorconfig-parse-properties (props-string)
  "Create properties hash table from PROPS-STRING."
  (let (props-list properties)
    (setq props-list (split-string props-string "\n")
          properties (make-hash-table :test 'equal))
    (dolist (prop props-list properties)
      (let ((key-val (split-string prop " *= *")))
        (when (> (length key-val) 1)
          (let ((key (intern (car key-val)))
                (val (mapconcat 'identity (cdr key-val) "")))
            (puthash key val properties)))))))

(defun editorconfig-get-properties-from-exec ()
  "Get EditorConfig properties of current buffer.

This function uses value of `editorconfig-exec-path' to get properties."
  (if (executable-find editorconfig-exec-path)
      (editorconfig-parse-properties (editorconfig-call-editorconfig-exec))
    (error "Unable to find editorconfig executable")))

(defun editorconfig-get-properties ()
  "Get EditorConfig properties of current buffer.

It calls `editorconfig-get-properties-from-exec' if
`editorconfig-exec-path' is found, otherwise
`editorconfig-core-get-properties-hash'."
  (if (and (executable-find editorconfig-exec-path)
           (not (file-remote-p buffer-file-name)))
      (editorconfig-get-properties-from-exec)
    (require 'editorconfig-core)
    (editorconfig-core-get-properties-hash)))

;;;###autoload
(defun editorconfig-apply ()
  "Get and apply EditorConfig properties to current buffer.

This function does not respect the values of `editorconfig-exclude-modes' and
`editorconfig-exclude-regexps' and always applies available properties.
Use `editorconfig-mode-apply' instead to make use of these variables."
  (interactive)
  (when buffer-file-name
    (condition-case err
        (progn
          (unless (functionp editorconfig-get-properties-function)
            (error "Invalid editorconfig-get-properties-function value"))
          (let ((props (funcall editorconfig-get-properties-function)))
            (progn
              (condition-case err
                  (run-hook-with-args 'editorconfig-hack-properties-functions props)
                (error
                 (display-warning 'editorconfig-hack-properties-functions
                                  (concat (error-message-string err)
                                          ". Abort running hook.")
                                  :warning)))
              (setq editorconfig-properties-hash props)
              (editorconfig-set-indentation (gethash 'indent_style props)
                                            (gethash 'indent_size props)
                                            (gethash 'tab_width props))
              (editorconfig-set-coding-system
               (gethash 'end_of_line props)
               (gethash 'charset props))
              (editorconfig-set-trailing-nl (gethash 'insert_final_newline props))
              (editorconfig-set-trailing-ws (gethash 'trim_trailing_whitespace props))
              (editorconfig-set-line-length (gethash 'max_line_length props))
              (editorconfig-set-major-mode-from-name (gethash 'file_type_emacs props))
              (editorconfig-set-major-mode-from-ext (gethash 'file_type_ext props))
              (condition-case err
                  (run-hook-with-args 'editorconfig-after-apply-functions props)
                (error
                 (display-warning 'editorconfig-after-apply-functions
                                  (concat (error-message-string err)
                                          ". Stop running hook.")
                                  :warning))))))
      (error
       (display-warning 'editorconfig
                        (concat (error-message-string err)
                                ".  Styles will not be applied.")
                        :error)))))

(defun editorconfig-mode-apply ()
  "Get and apply EditorConfig properties to current buffer.

This function does nothing when the major mode is listed in
`editorconfig-exclude-modes', or variable `buffer-file-name' matches
any of regexps in `editorconfig-exclude-regexps'."
  (interactive)
  (when (and major-mode
             (not (memq major-mode
                        editorconfig-exclude-modes))
             buffer-file-name
             (not (cl-loop for regexp in editorconfig-exclude-regexps
                           if (string-match regexp buffer-file-name) return t
                           finally return nil)))
    (editorconfig-apply)))

;;;###autoload
(define-minor-mode editorconfig-mode
  "Toggle EditorConfig feature.

To disable EditorConfig in some buffers, modify
`editorconfig-exclude-modes' or `editorconfig-exclude-regexps'."
  :global t
  :lighter editorconfig-mode-lighter
  ;; See https://github.com/editorconfig/editorconfig-emacs/issues/141 for why
  ;; not `after-change-major-mode-hook'
  (dolist (hook '(change-major-mode-after-body-hook
                  read-only-mode-hook
                  ;; Some modes call `kill-all-local-variables' in their init
                  ;; code, which clears some values set by editorconfig.
                  ;; For those modes, editorconfig-apply need to be called
                  ;; explicitly through their hooks.
                  rpm-spec-mode-hook
                  ))
    (if editorconfig-mode
        (add-hook hook 'editorconfig-mode-apply)
      (remove-hook hook 'editorconfig-mode-apply))))


;; Tools
;; Some useful commands for users, not required for EditorConfig to work

;;;###autoload
(defun editorconfig-find-current-editorconfig ()
  "Find the closest .editorconfig file for current file."
  (interactive)
  (eval-and-compile (require 'editorconfig-core))
  (let ((file (editorconfig-core-get-nearest-editorconfig
               default-directory)))
    (when file
      (find-file file))))

;;;###autoload
(defun editorconfig-display-current-properties ()
  "Display EditorConfig properties extracted for current buffer."
  (interactive)
  (if editorconfig-properties-hash
      (let (
            (buf (get-buffer-create "*EditorConfig Properties*"))
            (file buffer-file-name)
            (props editorconfig-properties-hash))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "# EditorConfig for %s\n" file))
          (maphash (lambda (k v)
                     (insert (format "%S = %s\n" k v)))
                   props))
        (display-buffer buf))
    (message "Properties are not applied to current buffer yet.")
    nil))
;;;###autoload
(defalias 'describe-editorconfig-properties
  'editorconfig-display-current-properties)

;;;###autoload
(defun editorconfig-format-buffer()
  "Format buffer according to .editorconfig indent_style and indent_width."
  (interactive)
  (if (string= (gethash 'indent_style editorconfig-properties-hash) "tab")
      (tabify (point-min) (point-max)))
  (if (string= (gethash 'indent_style editorconfig-properties-hash) "space")
      (untabify (point-min) (point-max)))
  (indent-region (point-min) (point-max)))



;; (defconst editorconfig--version
;;   (eval-when-compile
;;     (require 'lisp-mnt)
;;     (declare-function lm-version "lisp-mnt" nil)
;;     (lm-version))
;;   "EditorConfig version.")

(declare-function find-library-name "find-func" (library))
(declare-function lm-version "lisp-mnt" nil)

;;;###autoload
(defun  editorconfig-version (&optional show-version)
  "Get EditorConfig version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer."
  (interactive (list t))
  (let* ((version
          (with-temp-buffer
            (require 'find-func)
            (insert-file-contents (find-library-name "editorconfig"))
            (require 'lisp-mnt)
            (lm-version)))
         (pkg
          (and (require 'package nil t)
               (cadr (assq 'editorconfig
                           package-alist))))
         (pkg-version
          (and pkg
               (package-version-join (package-desc-version pkg))))
         (version-full (if (and pkg-version
                                (not (string= version
                                              pkg-version)))
                           (concat version "-" pkg-version)
                         version)))
    (when show-version
      (message "EditorConfig Emacs v%s"
               version-full))
    version-full))

(provide 'editorconfig)

;;; editorconfig.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
