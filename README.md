[![Build Status](https://travis-ci.org/editorconfig/editorconfig-emacs.svg?branch=master)](https://travis-ci.org/editorconfig/editorconfig-emacs)
[![MELPA](http://melpa.org/packages/editorconfig-badge.svg)](http://melpa.org/#/editorconfig)
[![MELPA Stable](https://stable.melpa.org/packages/editorconfig-badge.svg)](https://stable.melpa.org/#/editorconfig)


# EditorConfig Emacs Plugin

This is an [EditorConfig][] plugin for [Emacs](https://www.gnu.org/software/emacs/).

## Installation

Download the [EditorConfig C Core][] and follow the instructions in the README
and INSTALL files to install it.

This plugin also has a built-in core library implemented in Emacs-Lisp, and
fallback to it when no core executable is found.

In either case, copy `.el` files in this repository to `~/.emacs.d/lisp`
and add the following to your `~/.emacs` file:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editorconfig)
(editorconfig-mode 1)
```

Alternatively, you can find the package available on
[MELPA](https://melpa.org/#/editorconfig) and [MELPA Stable](https://stable.melpa.org/#/editorconfig)
([The Marmalade package](http://marmalade-repo.org/packages/editorconfig) is deprecated).

Or if you use [**use-package**](https://www.emacswiki.org/emacs/UsePackage):

```emacs-lisp
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
```

## Supported properties

Current Emacs plugin coverage for EditorConfig's [properties][]:

* `indent_style`
* `indent_size`
* `tab_width`
* `end_of_line`
* `charset`
* `trim_trailing_whitespace`
* `insert_final_newline = true` is supported
* <del>`insert_final_newline = false`</del> is not enforced
  (as in trailing newlines actually being removed automagically),
  we just buffer-locally override any preferences that would auto-add them
  to files `.editorconfig` marks as trailing-newline-free
* `max_line_length`
* `file_type_ext` (Experimental)
* `file_type_emacs` (Experimental)
* `root` (only used by EditorConfig core)

Not yet covered properties marked with <del>over-strike</del>
– pull requests implementing missing features warmly welcomed!
Typically, you will want to tie these to native functionality,
or the configuration of existing packages handling the feature.

As several packages have their own handling of, say, indention,
we might not yet cover some mode you use, but we try to add the
ones that show up on our radar. Similarly, we don't yet hook
in to all different packages for whitespace trimming to inform
them about editorconfig settings, but aim for better coverage
of things like
[ws-trim](ftp://ftp.lysator.liu.se/pub/emacs/ws-trim.el).


### File Type

This plugin has experimental supports for `file_type_ext` and
`file_type_emacs`, which specify "file types" for files.
As for Emacs, it means `major-mode` can be set.

**file_type_ext** When it is set to `md` for `a.txt`, for example,
`major-mode` will be decided as if the file name would be `a.txt.md`
(and thus `markdown-mode` is likely to be used).

**file_type_emacs** When it is set to `markdown` for `a.txt`,
`markdown-mode`  will be enabled when opening `a.txt`.

These property are experimental and their meanings might change in the
future updates. When both are specified, `file_type_ext` takes precedence.


## Customize

### `editorconfig-after-apply-functions`

(Formerly `editorconfig-custom-hooks`)

A list of functions after loading common EditorConfig settings, where you can
set some custom variables or overwrite existing properties.

For example, `web-mode` has several variables for indentation offset size and
EditorConfig sets them at once by `indent_size`. You may want to stop indenting
only blocks of `web-mode`: it can be achieved by adding following to your init.el:

```emacs-lisp
(add-hook 'editorconfig-after-apply-functions
  (lambda (hash) (setq web-mode-block-padding 0)))
```

You can also define your own custom properties and enable them here.

### `editorconfig-hack-properties-functions`

A list of function to alter property values before applying them.

These functions will be run after loading \".editorconfig\" files and before
applying them to current buffer, so that you can alter some properties from
\".editorconfig\" before they take effect.

For example, Makefiles always use tab characters for indentation: you can
overwrite \"indent_style\" property when current `major-mode` is a
`makefile-mode` with following code:

``` emacs-lisp
(add-hook 'editorconfig-hack-properties-functions
          '(lambda (props)
             (when (derived-mode-p 'makefile-mode)
               (puthash 'indent_style "tab" props))))

```

### `editorconfig-indentation-alist`

Alist of indentation setting methods by modes.

For the easiest case to add a new support for a major-mode, you just need to
add a pair of major-mode symbol and its indentation variables:

```emacs-lisp
(add-to-list 'editorconfig-indentation-alist
  ;; Just an example, of course EditorConfig has already included this setting!
  '(c-mode c-basic-offset))
```

You can also modify this variable with the command
<kbd>M-x customize-variable [RET] editorconfig-indentation-alist [RET]</kbd>.
For a bit more complicated cases please take a look at the docstring of this variable.

### `editorconfig-exec-path`

String of `editorconfig` executable name (command name or full path to
the executable).


### `editorconfig-get-properties-function`

Function to use to get EditorConfig properties.

For example, if you always want to use built-in core library instead
of any EditorConfig executable to get properties, add following to
your init.el:

``` emacs-lisp
(set-variable 'editorconfig-get-properties-function
              #'editorconfig-core-get-properties-hash)
```

Possible known values are:

* `editorconfig-get-properties` (default)
  * Use `editorconfig-get-properties-from-exec` when
    `editorconfig-exec-path` executable is found, otherwise use
    `editorconfig-core-get-properties-hash`
* `editorconfig-get-properties-from-exec`
  * Get properties by executing EditorConfig executable specified in
    `editorconfig-exec-path`
* `editorconfig-core-get-properties-hash`
  * Always use built-in Emacs-Lisp implementation to get properties


### `editorconfig-trim-whitespaces-mode`

Buffer local minor-mode to use to trim trailing whitespaces.

If set, enable that mode when `trim_trailing_whitespace` is set to true.
Otherwise, use `delete-trailing-whitespace`.

One possible value is
[`ws-butler-mode`](https://github.com/lewang/ws-butler), with which
only lines touched get trimmed. To use it, add following to yo
init.el:

``` emacs-lisp
(setq editorconfig-trim-whitespaces-mode
      'ws-butler-mode)
```

## Testing

Make and [CMake](https://cmake.org) must be installed to run the tests.

To run the tests:

    $ make test

## Submitting Bugs and Feature Requests

Bugs, feature requests, and other issues should be submitted to the issue
tracker: https://github.com/editorconfig/editorconfig-emacs/issues

## License

EditorConfig Emacs Plugin is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program.  If not, see <https://www.gnu.org/licenses/>.

[EditorConfig]: https://editorconfig.org
[EditorConfig C Core]: https://github.com/editorconfig/editorconfig-core-c
[properties]: https://editorconfig.org/#supported-properties
