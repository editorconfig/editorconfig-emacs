![build](https://github.com/editorconfig/editorconfig-emacs/workflows/build/badge.svg)
[![MELPA](https://melpa.org/packages/editorconfig-badge.svg)](http://melpa.org/#/editorconfig)
[![MELPA Stable](https://stable.melpa.org/packages/editorconfig-badge.svg)](https://stable.melpa.org/#/editorconfig)


# EditorConfig Emacs Plugin

This is an [EditorConfig][] plugin for [Emacs][].


## Getting Started


### Install from MELPA

This package is available from [MELPA][] and [MELPA Stable][].
Install from these repositories and enable global minor-mode `editorconfig-mode`:

```emacs-lisp
(editorconfig-mode 1)
```

Normally, enabling `editorconfig-mode` should be enough for this plugin to work:
all other configurations are optional.
This mode sets up hooks so that EditorConfig properties will be
loaded and applied to the new buffers automatically when visiting files.


### use-package

If you use [**use-package**][use-package], add the following to your
`init.el` file:

```emacs-lisp
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
```


### Manual installation

Copy all `.el` files in this repository to `~/.emacs.d/lisp` and add the
following:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editorconfig)
(editorconfig-mode 1)
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
* <del>`file_type_ext` (Experimental)</del> (See below)
* <del>`file_type_emacs` (Experimental)</del> (See below)
* `root` (only used by EditorConfig core)

Not yet covered properties marked with <del>over-strike</del>
– pull requests implementing missing features warmly welcomed!
Typically, you will want to tie these to native functionality,
or the configuration of existing packages handling the feature.

As several packages have their own handling of, say, indentation,
we might not yet cover some mode you use, but we try to add the
ones that show up on our radar.



### <del>File Type (file_type_ext, file_type_emacs)</del>

This feature is currently disabled.  For those who want this functionality,
please consider using [editorconfig-custom-majormode](https://github.com/10sr/editorconfig-custom-majormode-el).


## Customize

`editorconfig-emacs` provides some customize variables.

Here are some of these variables: for the full list of available variables,
type <kbd>M-x customize-group [RET] editorconfig [RET]</kbd>.


### `editorconfig-trim-whitespaces-mode`

Buffer local minor-mode to use to trim trailing whitespaces.

If set, editorconfig will enable/disable this mode in accord with
`trim_trailing_whitespace` property in `.editorconfig`.
Otherwise, use Emacs built-in `delete-trailing-whitespace` function.

One possible value is
[`ws-butler-mode`](https://github.com/lewang/ws-butler), with which
only lines touched get trimmed. To use it, add following to your
init.el:

``` emacs-lisp
(setq editorconfig-trim-whitespaces-mode
      'ws-butler-mode)
```


### `editorconfig-after-apply-functions`

(Formerly `editorconfig-custom-hooks`)

A list of functions which will be called after loading common EditorConfig settings,
when you can set some custom variables.

For example, `web-mode` has several variables for indentation offset size and
EditorConfig sets them at once by `indent_size`. You can stop indenting
only blocks of `web-mode` by adding following to your init.el:

```emacs-lisp
(add-hook 'editorconfig-after-apply-functions
  (lambda (props) (setq web-mode-block-padding 0)))
```


### `editorconfig-hack-properties-functions`

A list of functions to alter property values before applying them.

These functions will be run after loading \".editorconfig\" files and before
applying them to current buffer, so that you can alter some properties from
\".editorconfig\" before they take effect.

For example, Makefile files always use tab characters for indentation: you can
overwrite \"indent_style\" property when current `major-mode` is
`makefile-mode`:

``` emacs-lisp
(add-hook 'editorconfig-hack-properties-functions
          '(lambda (props)
             (when (derived-mode-p 'makefile-mode)
               (puthash 'indent_style "tab" props))))

```


## Troubleshooting

Enabling `editorconfig-mode` should be enough for normal cases.

When EditorConfig properties are not effective for unknown reason, we recommend
first trying `M-x editorconfig-display-current-properties`.

This command will open a new buffer and display the EditorConfig properties
loaded for current buffer.
You can check if EditorConfig properties were not read for buffers at all,
or they were loaded but did not take effect for some other reasons.



### Indentation for new major-modes

Because most Emacs major-modes have their own indentation settings, this plugin
requires explicit support for each major-mode for `indent_size` property.

By default this plugin ships with settings for many major-modes, but,
sorry to say, it cannot be perfect. Especially it is difficult to support
brand-new major-modes.
Please feel free to submit issue or pull-request for such major-mode!

Supported major-modes and their indentation configs are defined in the variable
`editorconfig-indentation-alist`.


### Not work at all for FOO-mode!

Most cases properties are loaded just after visiting files when
`editorconfig-mode` is enabled.
But it is known that there are major-modes that this mechanism does not work
for and require explicit call of `editorconfig-apply`.

Typically it will occur when the major-mode is not defined using
`define-derived-mode` (`rpm-spec-mode` is an example for this).
Please feel free to submit issues if you find such modes!




## Submitting Bugs and Feature Requests

Bugs, feature requests, and other issues should be submitted to the issue
tracker: https://github.com/editorconfig/editorconfig-emacs/issues


### Development

Make and [CMake][] must be installed to run the tests
locally:

    $ make test

To start a new Emacs process with current `*.el` and without loading user init
file, run:

    $ make sandbox




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



[Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: https://melpa.org/#/editorconfig
[MELPA Stable]: https://stable.melpa.org/#/editorconfig
[use-package]: https://www.emacswiki.org/emacs/UsePackage
[EditorConfig]: https://editorconfig.org
[EditorConfig C Core]: https://github.com/editorconfig/editorconfig-core-c
[properties]: https://editorconfig.org/#supported-properties
[CMake]: https://cmake.org
