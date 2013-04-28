# EditorConfig Emacs Plugin

This is an [EditorConfig][] plugin for Emacs.

## Installation

Download the [EditorConfig core][] and follow the instructions in the README
and INSTALL files to install it.

Once EditorConfig core is installed, copy [`editorconfig.el`][] to `~/.emacs.d/`
and add the following to your `~/.emacs` file:

    (add-to-list 'load-path "~/.emacs.d/")
    (load "editorconfig")

## Supported properties

Current Emacs plugin coverage for EditorConfig's [properties][]:

* `indent_style`
* `indent_size`
* `tab_width`
* `end_of_line`
* <del>`charset`</del>
* <del>`trim_trailing_whitespace`</del>
* `insert_final_newline = true` is supported
* <del>`insert_final_newline = false`</del> is not enforced
  (as in trailing newlines actually being removed automagically),
  we just buffer-locally override preferences that would auto-add them
  to files noted as trailing newline free via `.editorconfig`
* `root` (only used by EditorConfig core)

Not yet covered properties marked with <del>over-strike</del>
– pull requests implementin missing features warmly welcomed!
(Typically, you will want to tie these to native functionality,
or the configuration of existing packages handling the feature.)

## Submitting Bugs and Feature Requests

Bugs, feature requests, and other issues should be submitted to the main
EditorConfig issue tracker: https://github.com/editorconfig/editorconfig/issues

[EditorConfig]: http://editorconfig.org
[EditorConfig core]: https://github.com/editorconfig/editorconfig-core
[properties]: http://editorconfig.org/#supported-properties
[`editorconfig.el`]: https://github.com/editorconfig/editorconfig-emacs/raw/master/editorconfig.el
