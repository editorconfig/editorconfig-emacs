;;; editorconfig.el --- EditorConfig Emacs Plugin  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.11.0
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme
;; Package-Requires: ((emacs "27.2"))
;; Keywords: convenience editorconfig

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

(eval-when-compile (require 'subr-x))
(defvar tex-indent-basic)
(defvar tex-indent-item)
(defvar tex-indent-arg)
(defvar evil-shift-width)

(require 'editorconfig-core)

(defgroup editorconfig nil
  "EditorConfig Emacs Plugin.

EditorConfig helps developers define and maintain consistent
coding styles between different editors and IDEs."
  :tag "EditorConfig"
  :prefix "editorconfig-"
  :group 'tools)

(define-obsolete-variable-alias
  'edconf-exec-path
  'editorconfig-exec-path
  "0.5")
(defcustom editorconfig-exec-path
  "editorconfig"
  "Path to EditorConfig executable.

Used by `editorconfig--execute-editorconfig-exec'."
  :type 'string)

(define-obsolete-variable-alias
  'edconf-get-properties-function
  'editorconfig-get-properties-function
  "0.5")
(defcustom editorconfig-get-properties-function
  'editorconfig-core-get-properties-hash
  "A function which gets EditorConfig properties for specified file.

This function will be called with one argument, full path of the target file,
and should return a hash object containing properties, or nil if any core
program is not available.  Keys of this hash should be symbols of properties,
and values should be strings of their values.


For example, if you always want to use built-in core library instead
of any EditorConfig executable to get properties, add following to
your init.el:

  (set-variable \\='editorconfig-get-properties-function
                #\\='editorconfig-core-get-properties-hash)

Possible known values are:

* `editorconfig-core-get-properties-hash' (default)
  * Always use built-in Emacs-Lisp implementation to get properties
* `editorconfig-get-properties'
  * Use `editorconfig-get-properties-from-exec' when
    `editorconfig-exec-path' executable is found, otherwise
    use `editorconfig-core-get-properties-hash'
* `editorconfig-get-properties-from-exec'
  * Get properties by executing EditorConfig executable"
  :type 'function)

(defcustom editorconfig-mode-lighter " EditorConfig"
  "Command `editorconfig-mode' lighter string."
  :type 'string)

(define-obsolete-variable-alias
  'edconf-custom-hooks
  'editorconfig-after-apply-functions
  "0.5")
(define-obsolete-variable-alias
  'editorconfig-custom-hooks
  'editorconfig-after-apply-functions
  "0.7.14")
(defcustom editorconfig-after-apply-functions ()
  "A list of functions after loading common EditorConfig settings.

Each element in this list is a hook function.  This hook function
takes one parameter, which is a property hash table.  The value
of properties can be obtained through gethash function.

The hook does not have to be coding style related; you can add
whatever functionality you want.  For example, the following is
an example to add a new property emacs_linum to decide whether to
show line numbers on the left:

  (add-hook \\='editorconfig-after-apply-functions
    \\='(lambda (props)
       (let ((show-line-num (gethash \\='emacs_linum props)))
         (cond ((equal show-line-num \"true\") (linum-mode 1))
           ((equal show-line-num \"false\") (linum-mode 0))))))

This hook will be run even when there are no matching sections in
\".editorconfig\", or no \".editorconfig\" file was found at all."
  :type 'hook)

(defcustom editorconfig-hack-properties-functions ()
  "A list of function to alter property values before applying them.

These functions will be run after loading \".editorconfig\" files and before
applying them to current buffer, so that you can alter some properties from
\".editorconfig\" before they take effect.
\(Since 2021/08/30 (v0.9.0): Buffer coding-systems are set before running
this functions, so this variable cannot be used to change coding-systems.)

For example, Makefiles always use tab characters for indentation: you can
overwrite \"indent_style\" property when current `major-mode' is a
`makefile-mode' with following code:

  (add-hook \\='editorconfig-hack-properties-functions
            \\='(lambda (props)
               (when (derived-mode-p \\='makefile-mode)
                 (puthash \\='indent_style \"tab\" props))))

This hook will be run even when there are no matching sections in
\".editorconfig\", or no \".editorconfig\" file was found at all."
  :type 'hook)
(make-obsolete-variable 'editorconfig-hack-properties-functions
                        "Using `editorconfig-after-apply-functions' instead is recommended,
    because since 2021/08/30 (v0.9.0) this variable cannot support all properties:
    charset values will be referenced before running this hook."
                        "v0.9.0")

(define-obsolete-variable-alias
  'edconf-indentation-alist
  'editorconfig-indentation-alist
  "0.5")
(defcustom editorconfig-indentation-alist
  ;; For contributors: Sort modes in alphabetical order
  '((ada-mode ada-indent)
    (ada-ts-mode ada-ts-mode-indent-offset)
    (apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (bash-ts-mode sh-basic-offset
                  sh-indentation)
    (bpftrace-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c++-ts-mode c-basic-offset
                 c-ts-mode-indent-offset)
    (c-mode c-basic-offset)
    (c-ts-mode c-basic-offset
               c-ts-mode-indent-offset)
    (cmake-mode cmake-tab-width)
    (cmake-ts-mode cmake-tab-width
                   cmake-ts-mode-indent-offset)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (csharp-ts-mode c-basic-offset
                    csharp-ts-mode-indent-offset)
    (css-mode css-indent-offset)
    (css-ts-mode css-indent-offset)
    (d-mode c-basic-offset)
    (elixir-ts-mode elixir-ts-indent-offset)
    (emacs-lisp-mode . editorconfig-set-indentation-lisp-mode)
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
    (gdscript-mode gdscript-indent-offset)
    (go-ts-mode go-ts-mode-indent-offset)
    (gpr-mode gpr-indent)
    (gpr-ts-mode gpr-ts-mode-indent-offset)
    (graphql-mode graphql-indent-level)
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
    (hcl-mode hcl-indent-level)
    (html-ts-mode html-ts-mode-indent-offset)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (java-ts-mode c-basic-offset
                  java-ts-mode-indent-offset)
    (js-mode js-indent-level)
    (js-ts-mode js-indent-level)
    (js-jsx-mode js-indent-level sgml-basic-offset)
    (js2-mode js2-basic-offset)
    (js2-jsx-mode js2-basic-offset sgml-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (json-ts-mode json-ts-mode-indent-offset)
    (jsonian-mode jsonian-default-indentation)
    (julia-mode julia-indent-offset)
    (kotlin-mode kotlin-tab-width)
    (kotlin-ts-mode kotlin-ts-mode-indent-offset)
    (latex-mode . editorconfig-set-indentation-latex-mode)
    (lisp-mode . editorconfig-set-indentation-lisp-mode)
    (livescript-mode livescript-tab-width)
    (lua-mode lua-indent-level)
    (lua-ts-mode lua-ts-indent-offset)
    (magik-mode magik-indent-level)
    (magik-ts-mode magik-indent-level)
    (matlab-mode matlab-indent-level)
    (meson-mode meson-indent-basic)
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
    (php-ts-mode php-ts-mode-indent-offset)
    (pike-mode c-basic-offset)
    (protobuf-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (puppet-mode puppet-indent-level)
    (python-mode . editorconfig-set-indentation-python-mode)
    (python-ts-mode . editorconfig-set-indentation-python-mode)
    (rjsx-mode js-indent-level sgml-basic-offset)
    (ruby-mode ruby-indent-level)
    (ruby-ts-mode ruby-indent-level)
    (rust-mode rust-indent-offset)
    (rust-ts-mode rust-indent-offset
                  rust-ts-mode-indent-offset)
    (rustic-mode rustic-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (slim-mode slim-indent-offset)
    (sml-mode sml-indent-level)
    (svelte-mode svelte-basic-offset)
    (swift-mode swift-mode:basic-offset)
    (terra-mode terra-indent-level)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (templ-ts-mode go-ts-mode-indent-offset js-indent-level)
    (toml-ts-mode toml-ts-mode-indent-offset)
    (typescript-mode typescript-indent-level)
    (typescript-ts-base-mode typescript-ts-mode-indent-offset)
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
    (yaml-mode yaml-indent-offset)
    (yaml-ts-mode yaml-indent-offset)
    (zig-mode zig-indent-offset))
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
  :risky t)

(defcustom editorconfig-exclude-modes ()
  "Modes in which `editorconfig-mode-apply' will not run."
  :type '(repeat (symbol :tag "Major Mode")))

(defcustom editorconfig-exclude-regexps ()
  "List of regexp for buffer filenames `editorconfig-mode-apply' will not run.

When variable `buffer-file-name' matches any of the regexps, then
`editorconfig-mode-apply' will not do its work."
  :type '(repeat string))
(with-eval-after-load 'recentf
  (add-to-list 'editorconfig-exclude-regexps
               (rx-to-string '(seq string-start
                                   (eval (file-truename (expand-file-name recentf-save-file))))
                             t)))

(defcustom editorconfig-trim-whitespaces-mode nil
  "Buffer local minor-mode to use to trim trailing whitespaces.

If set, enable that mode when `trim_trailing_whitespace` is set to true.
Otherwise, use `delete-trailing-whitespace'."
  :type 'symbol)

(defvar editorconfig-properties-hash nil
  "Hash object of EditorConfig properties that was enabled for current buffer.
Set by `editorconfig-apply' and nil if that is not invoked in
current buffer yet.")
(make-variable-buffer-local 'editorconfig-properties-hash)
(put 'editorconfig-properties-hash
     'permanent-local
     t)

(defvar editorconfig-lisp-use-default-indent nil
  "Selectively ignore the value of indent_size for Lisp files.
Prevents `lisp-indent-offset' from being set selectively.

nil - `lisp-indent-offset' is always set normally.
t   - `lisp-indent-offset' is never set normally
       (always use default indent for lisps).
number - `lisp-indent-offset' is not set only if indent_size is
         equal to this number.  For example, if this is set to 2,
         `lisp-indent-offset' will not be set only if indent_size is 2.")

(defcustom editorconfig-override-file-local-variables t
  "Non-nil means editorconfig will override file local variable values."
  :type 'boolean)

(defcustom editorconfig-override-dir-local-variables t
  "Non-nil means editorconfig will override values defined in dir-locals.el ."
  :type 'boolean)

(define-error 'editorconfig-error
              "Error thrown from editorconfig lib")

(defun editorconfig-error (&rest args)
  "Signal an `editorconfig-error'.
Make a message by passing ARGS to `format-message'."
  (signal 'editorconfig-error (list (apply #'format-message args))))

(defun editorconfig--disabled-for-filename (filename)
  "Return non-nil when EditorConfig is disabled for FILENAME."
  (cl-assert (stringp filename))
  (cl-loop for regexp in editorconfig-exclude-regexps
           if (string-match regexp filename) return t
           finally return nil))

(defun editorconfig--disabled-for-majormode (majormode)
  "Return non-nil when Editorconfig is disabled for MAJORMODE."
  (cl-assert majormode)
  (or (provided-mode-derived-p majormode 'special-mode)
      ;; Some special modes (like `archive-mode') are not derived from
      ;; `special-mode'
      (eq (get majormode 'mode-class) 'special)
      (memq majormode
            editorconfig-exclude-modes)))

(defun editorconfig-string-integer-p (string)
  "Return non-nil if STRING represents integer."
  (and (stringp string)
       (string-match-p "\\`[0-9]+\\'" string)))

(defun editorconfig-set-indentation-python-mode (size)
  "Set `python-mode' indent size to SIZE."
  (when (boundp 'python-indent-offset)
    (setq-local python-indent-offset size))
  ;; For https://gitlab.com/python-mode-devs/python-mode
  (when (boundp 'py-indent-offset)
    (setq-local py-indent-offset size)))

(defun editorconfig-set-indentation-latex-mode (size)
  "Set `latex-mode' indent size to SIZE."
  (setq-local tex-indent-basic size)
  (setq-local tex-indent-item size)
  (setq-local tex-indent-arg (* 2 size))
  ;; For AUCTeX
  (when (boundp 'TeX-brace-indent-level)
    (setq-local TeX-brace-indent-level size))
  (when (boundp 'LaTeX-indent-level)
    (setq-local LaTeX-indent-level size))
  (when (boundp 'LaTeX-item-indent)
    (setq-local LaTeX-item-indent (- size))))

(defun editorconfig-set-indentation-lisp-mode (size)
 "Set indent size to SIZE for Lisp mode(s)."
 (when (cond ((null editorconfig-lisp-use-default-indent)  t)
             ((eql t editorconfig-lisp-use-default-indent) nil)
             ((numberp editorconfig-lisp-use-default-indent)
              (not (eql size editorconfig-lisp-use-default-indent)))
             (t t))
   (setq-local lisp-indent-offset size)))

(cl-defun editorconfig--should-set (symbol)
  "Determine if editorconfig should set SYMBOL."
  (display-warning '(editorconfig editorconfig--should-set)
                   (format "symbol: %S"
                           symbol)
                   :debug)
  (when (and (not editorconfig-override-file-local-variables)
             (assq symbol file-local-variables-alist))
    (cl-return-from editorconfig--should-set
      nil))

  (when (and (not editorconfig-override-dir-local-variables)
             (assq symbol dir-local-variables-alist))
    (cl-return-from editorconfig--should-set
      nil))

  t)

(defun editorconfig-set-indentation (style &optional size tab_width)
  "Set indentation type from STYLE, SIZE and TAB_WIDTH."
  (setq size
        (cond ((editorconfig-string-integer-p size)
               (string-to-number size))
              ((equal size "tab")
               "tab")
              (t
               nil)))

  (cond ((not (editorconfig--should-set 'tab-width))
         nil)
        (tab_width
         (setq tab-width (string-to-number tab_width)))
        ((numberp size)
         (setq tab-width size)))

  (when (equal size "tab")
    (setq size tab-width))

  (cond ((not (editorconfig--should-set 'indent-tabs-mode))
         nil)
        ((equal style "space")
         (setq indent-tabs-mode nil))
        ((equal style "tab")
         (setq indent-tabs-mode t)))

  (when size
    (when (and (featurep 'evil)
               (editorconfig--should-set 'evil-shift-width))
      (setq-local evil-shift-width size))
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
                               (editorconfig--should-set elem))
                          (set (make-local-variable elem) size))
                         ((and (consp elem)
                               (editorconfig--should-set (car elem)))
                          (let ((spec (cdr elem)))
                            (set (make-local-variable (car elem))
                                 (cond ((functionp spec) (funcall spec size))
                                       ((integerp spec) (* spec size))
                                       (t spec))))))))))))))

(defvar-local editorconfig--apply-coding-system-currently nil
  "Used internally.")
(put 'editorconfig--apply-coding-system-currently
     'permanent-local
     t)

(defun editorconfig-merge-coding-systems (end-of-line charset)
  "Return merged coding system symbol of END-OF-LINE and CHARSET."
  (let ((eol (cond
              ((equal end-of-line "lf") 'undecided-unix)
              ((equal end-of-line "cr") 'undecided-mac)
              ((equal end-of-line "crlf") 'undecided-dos)))
        (cs (cond
             ((equal charset "latin1") 'iso-latin-1)
             ((equal charset "utf-8") 'utf-8)
             ((equal charset "utf-8-bom") 'utf-8-with-signature)
             ((equal charset "utf-16be") 'utf-16be-with-signature)
             ((equal charset "utf-16le") 'utf-16le-with-signature))))
    (if (and eol cs)
        (merge-coding-systems cs eol)
      (or eol cs))))

(cl-defun editorconfig-set-coding-system-revert (end-of-line charset)
  "Set buffer coding system by END-OF-LINE and CHARSET.

This function will revert buffer when the coding-system has been changed."
  ;; `editorconfig--advice-find-file-noselect' does not use this function
  (let ((coding-system (editorconfig-merge-coding-systems end-of-line
                                                          charset)))
    (display-warning '(editorconfig editorconfig-set-coding-system-revert)
                     (format "editorconfig-set-coding-system-revert: buffer-file-name: %S | buffer-file-coding-system: %S | coding-system: %S | apply-currently: %S"
                             buffer-file-name
                             buffer-file-coding-system
                             coding-system
                             editorconfig--apply-coding-system-currently)
                     :debug)
    (when (memq coding-system '(nil undecided))
      (cl-return-from editorconfig-set-coding-system-revert))
    (when (and buffer-file-coding-system
               (memq buffer-file-coding-system
                     (coding-system-aliases (merge-coding-systems coding-system
                                                                  buffer-file-coding-system))))
      (cl-return-from editorconfig-set-coding-system-revert))
    (unless (file-readable-p buffer-file-name)
      (set-buffer-file-coding-system coding-system)
      (cl-return-from editorconfig-set-coding-system-revert))
    (unless (memq coding-system
                  (coding-system-aliases editorconfig--apply-coding-system-currently))
      ;; Revert functions might call `editorconfig-apply' again
      ;; FIXME: I suspect `editorconfig--apply-coding-system-currently'
      ;; gymnastics is not needed now that we hook into `find-auto-coding'.
      (unwind-protect
          (progn
            (setq editorconfig--apply-coding-system-currently coding-system)
            ;; Revert without query if buffer is not modified
            (let ((revert-without-query '(".")))
              (revert-buffer-with-coding-system coding-system)))
        (setq editorconfig--apply-coding-system-currently nil)))))

(defun editorconfig--delete-final-newline ()
  "Ensure the buffer does not end with a newline.
Does nothing if the buffer is read-only."
  (unless buffer-read-only
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (while (looking-at-p "^$")
          (delete-char -1))))))

(defun editorconfig-set-trailing-nl (final-newline)
  "Set up requiring final newline by FINAL-NEWLINE.

This function will set `require-final-newline' and `mode-require-final-newline'
to non-nil when FINAL-NEWLINE is true."
  (pcase final-newline
    ("true"
     ;; keep prefs around how/when the nl is added, if set - otherwise add on save
     (setq-local require-final-newline      (or require-final-newline t))
     (setq-local mode-require-final-newline (or mode-require-final-newline t))
     (remove-hook 'before-save-hook
                  #'editorconfig--delete-final-newline t))

    ("false"
     (setq-local require-final-newline      nil)
     (setq-local mode-require-final-newline nil)
     (add-hook 'before-save-hook
               #'editorconfig--delete-final-newline nil t))))

(defun editorconfig--delete-trailing-whitespace ()
  "Call `delete-trailing-whitespace' unless the buffer is read-only."
  (unless buffer-read-only (delete-trailing-whitespace)))

(defun editorconfig-set-trailing-ws (trim-trailing-ws)
  (pcase trim-trailing-ws
    ("true"
     (if editorconfig-trim-whitespaces-mode
         (funcall editorconfig-trim-whitespaces-mode 1)
       (add-hook 'before-save-hook
                 #'editorconfig--delete-trailing-whitespace nil t)))
    ("false"
     (when editorconfig-trim-whitespaces-mode
       (funcall editorconfig-trim-whitespaces-mode 0))
     (remove-hook 'before-save-hook
                  #'editorconfig--delete-trailing-whitespace t))))


(defun editorconfig-set-line-length (length)
  "Set the max line length (`fill-column') to LENGTH."
  (when (and (editorconfig-string-integer-p length)
             (> (string-to-number length) 0))
    (setq fill-column (string-to-number length))))


(defun editorconfig--execute-editorconfig-exec (filename)
  "Execute EditorConfig core with FILENAME and return output."
  (if filename
      (with-temp-buffer
        (let ((remote (file-remote-p filename))
              (remote-localname (file-remote-p filename
                                               'localname)))
          (display-warning '(editorconfig editorconfig--execute-editorconfig-exec)
                           (format "editorconfig--execute-editorconfig-exec: filename: %S | remote: %S | remote-localname: %S"
                                   filename
                                   remote
                                   remote-localname)
                           :debug)
          (if remote
              (progn
                (cd (concat remote "/"))
                (setq filename remote-localname))
            (cd "/")))
        (display-warning '(editorconfig editorconfig--execute-editorconfig-exec)
                         (format "editorconfig--execute-editorconfig-exec: default-directory: %S | filename: %S"
                                 default-directory
                                 filename
                                 )
                         :debug)
        (if (eq 0
                (process-file editorconfig-exec-path nil t nil filename))
            (buffer-string)
          (editorconfig-error (buffer-string))))
    ""))

(defun editorconfig--parse-properties (props-string)
  "Create properties hash table from PROPS-STRING."
  (let ((props-list (split-string props-string "\n"))
        (properties (make-hash-table)))
    (dolist (prop props-list properties)
      (let ((key-val (split-string prop " *= *")))
        (when (> (length key-val) 1)
          (let ((key (intern (car key-val)))
                (val (mapconcat #'identity (cdr key-val) "")))
            (puthash key val properties)))))))

(defun editorconfig-get-properties-from-exec (filename)
  "Get EditorConfig properties of file FILENAME.

This function uses value of `editorconfig-exec-path' to get properties."
  (if (executable-find editorconfig-exec-path)
      (editorconfig--parse-properties (editorconfig--execute-editorconfig-exec filename))
    (editorconfig-error "Unable to find editorconfig executable")))

(defun editorconfig-get-properties (filename)
  "Get EditorConfig properties for file FILENAME.

It calls `editorconfig-get-properties-from-exec' if
`editorconfig-exec-path' is found, otherwise
`editorconfig-core-get-properties-hash'."
  (if (and (executable-find editorconfig-exec-path)
           (not (file-remote-p filename)))
      (editorconfig-get-properties-from-exec filename)
    (require 'editorconfig-core)
    (editorconfig-core-get-properties-hash filename)))

(defun editorconfig-call-get-properties-function (filename)
  "Call `editorconfig-get-properties-function' with FILENAME and return result.

This function also removes `unset' properties and calls
`editorconfig-hack-properties-functions'."
  (unless (functionp editorconfig-get-properties-function)
    (editorconfig-error "Invalid editorconfig-get-properties-function value"))
  (if (stringp filename)
      (setq filename (expand-file-name filename))
    (editorconfig-error "Invalid argument: %S" filename))
  (let ((props nil))
    (condition-case err
        (setq props (funcall editorconfig-get-properties-function
                             filename))
      (error
       (editorconfig-error "Error from editorconfig-get-properties-function: %S"
                           err)))
    (cl-loop for k being the hash-keys of props using (hash-values v)
             when (equal v "unset") do (remhash k props))
    props))

(defun editorconfig-set-local-variables (props)
  "Set buffer variables according to EditorConfig PROPS."
  (editorconfig-set-indentation (gethash 'indent_style props)
                                (gethash 'indent_size props)
                                (gethash 'tab_width props))
  (editorconfig-set-trailing-nl (gethash 'insert_final_newline props))
  (editorconfig-set-trailing-ws (gethash 'trim_trailing_whitespace props))
  (editorconfig-set-line-length (gethash 'max_line_length props)))


(defun editorconfig-major-mode-hook ()
  "Function to run when `major-mode' has been changed.

This functions does not reload .editorconfig file, just sets local variables
again.  Changing major mode can reset these variables.

This function also executes `editorconfig-after-apply-functions' functions."
  (display-warning '(editorconfig editorconfig-major-mode-hook)
                   (format "editorconfig-major-mode-hook: editorconfig-mode: %S, major-mode: %S, -properties-hash: %S"
                           (and (boundp 'editorconfig-mode)
                                editorconfig-mode)
                           major-mode
                           editorconfig-properties-hash)
                   :debug)
  (when (and (bound-and-true-p editorconfig-mode)
             editorconfig-properties-hash)
    (editorconfig-set-local-variables editorconfig-properties-hash)
    (condition-case err
        (run-hook-with-args 'editorconfig-after-apply-functions editorconfig-properties-hash)
      (error
       (display-warning '(editorconfig editorconfig-major-mode-hook)
                        (format "Error while running `editorconfig-after-apply-functions': %S"
                                err))))))

;; This is a new var in Emacs≥30.
;; We dyn-bind it in our `editorconfig--advice-find-auto-coding' advice
;; to mimic the Emacs-30 behavior.
(defvar auto-coding-file-name)

(defun editorconfig--advice-find-auto-coding (filename &rest _args)
  "Consult `charset' setting of EditorConfig."
  (let ((cs (let ((auto-coding-file-name filename))
              (editorconfig--get-coding-system))))
    (when cs (cons cs 'EditorConfig))))

(defun editorconfig--advice-find-file-noselect (f filename &rest args)
  "Get EditorConfig properties and apply them to buffer to be visited.

This function should be added as an advice function to `find-file-noselect'.
F is that function, and FILENAME and ARGS are arguments passed to F."
  (let ((props nil)
        (ret nil))
    (condition-case err
        (when (stringp filename)
          (setq props (editorconfig-call-get-properties-function filename)))
      (error
       (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                        (format "Failed to get properties, styles will not be applied: %S"
                                err)
                        :warning)))

    (setq ret (apply f filename args))

    (condition-case err
        (with-current-buffer ret
          (when props

            ;; NOTE: hack-properties-functions cannot affect coding-system value,
            ;; because it has to be set before initializing buffers.
            (condition-case err
                (run-hook-with-args 'editorconfig-hack-properties-functions props)
              (error
               (display-warning '(editorconfig editorconfig-hack-properties-functions)
                                (format "Error while running editorconfig-hack-properties-functions, abort running hook: %S"
                                        err)
                                :warning)))
            (setq editorconfig-properties-hash props)
            ;; When initializing buffer, `editorconfig-major-mode-hook'
            ;; will be called before setting `editorconfig-properties-hash', so
            ;; execute this explicitly here.
            (editorconfig-set-local-variables props)

            (condition-case err
                (run-hook-with-args 'editorconfig-after-apply-functions props)
              (error
               (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                                (format "Error while running `editorconfig-after-apply-functions': %S"
                                        err))))))
      (error
       (display-warning '(editorconfig editorconfig--advice-find-file-noselect)
                        (format "Error while setting variables from EditorConfig: %S" err))))
    ret))

(defvar editorconfig--getting-coding-system nil)

(defun editorconfig--get-coding-system (&optional _size)
  "Return the coding system to use according to EditorConfig.
Meant to be used on `auto-coding-functions'."
  (when (and (stringp auto-coding-file-name)
             (file-name-absolute-p auto-coding-file-name)
	     ;; Don't recurse infinitely.
	     (not (member auto-coding-file-name
	                  editorconfig--getting-coding-system)))
    (let* ((editorconfig--getting-coding-system
            (cons auto-coding-file-name editorconfig--getting-coding-system))
           (props (editorconfig-call-get-properties-function
                   auto-coding-file-name)))
      (editorconfig-merge-coding-systems (gethash 'end_of_line props)
                                         (gethash 'charset props)))))

;;;###autoload
(define-minor-mode editorconfig-mode
  "Toggle EditorConfig feature.

To disable EditorConfig in some buffers, modify
`editorconfig-exclude-modes' or `editorconfig-exclude-regexps'."
  :global t
  :lighter editorconfig-mode-lighter
  (let ((modehooks '(prog-mode-hook
                     text-mode-hook
                     ;; Some modes call `kill-all-local-variables' in their init
                     ;; code, which clears some values set by editorconfig.
                     ;; For those modes, editorconfig-apply need to be called
                     ;; explicitly through their hooks.
                     rpm-spec-mode-hook)))
    (if editorconfig-mode
        (progn
          (advice-add 'find-file-noselect :around #'editorconfig--advice-find-file-noselect)
          (advice-add 'find-auto-coding :after-until
                      #'editorconfig--advice-find-auto-coding)
          (dolist (hook modehooks)
            (add-hook hook
                      #'editorconfig-major-mode-hook
                      t)))
      (advice-remove 'find-file-noselect #'editorconfig--advice-find-file-noselect)
      (advice-remove 'find-auto-coding
                     #'editorconfig--advice-find-auto-coding)
      (dolist (hook modehooks)
        (remove-hook hook #'editorconfig-major-mode-hook)))))


;; (defconst editorconfig--version
;;   (eval-when-compile
;;     (require 'lisp-mnt)
;;     (declare-function lm-version "lisp-mnt" nil)
;;     (lm-version))
;;   "EditorConfig version.")

;;;###autoload
(defun editorconfig-version (&optional show-version)
  "Get EditorConfig version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer."
  (interactive (list t))
  (let ((version-full
         (if (fboundp 'package-get-version) ;Emacs≥27
             (package-get-version)
           (let* ((version
                   (with-temp-buffer
                     (require 'find-func)
                     (declare-function find-library-name "find-func" (library))
                     (insert-file-contents (find-library-name "editorconfig"))
                     (require 'lisp-mnt)
                     (declare-function lm-version "lisp-mnt" nil)
                     (lm-version)))
                  (pkg (and (eval-and-compile (require 'package nil t))
                            (cadr (assq 'editorconfig
                                        package-alist))))
                  (pkg-version (and pkg (package-version-join
                                         (package-desc-version pkg)))))
             (if (and pkg-version
                      (not (string= version pkg-version)))
                 (concat version "-" pkg-version)
               version)))))
    (when show-version
      (message "EditorConfig Emacs v%s" version-full))
    version-full))

(provide 'editorconfig)
;;; editorconfig.el ends here
