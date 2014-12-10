# EditorConfig Emacs Plugin

This is an [EditorConfig][] plugin for Emacs.

## Installation

Download the [EditorConfig core][] and follow the instructions in the README
and INSTALL files to install it.

Once EditorConfig core is installed, copy [`editorconfig.el`][] to `~/.emacs.d/lisp`
and add the following to your `~/.emacs` file:

    (add-to-list 'load-path "~/.emacs.d/lisp")
    (load "editorconfig")

Alternatively, you can find the package available on [Marmalade](http://marmalade-repo.org/packages/editorconfig).

## Supported properties

Current Emacs plugin coverage for EditorConfig's [properties][]:

* `indent_style`
* `indent_size`
* `tab_width`
* `end_of_line`
* <del>`charset`</del>
* `trim_trailing_whitespace`
* `insert_final_newline = true` is supported
* <del>`insert_final_newline = false`</del> is not enforced
  (as in trailing newlines actually being removed automagically),
  we just buffer-locally override any preferences that would auto-add them
  to files `.editorconfig` marks as trailing-newline-free
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
of things like [ws-trim](ftp://ftp.lysator.liu.se/pub/emacs/ws-trim.el).

## Submitting Bugs and Feature Requests

Bugs, feature requests, and other issues should be submitted to the main
EditorConfig issue tracker: https://github.com/editorconfig/editorconfig/issues

[EditorConfig]: http://editorconfig.org
[EditorConfig core]: https://github.com/editorconfig/editorconfig-core
[properties]: http://editorconfig.org/#supported-properties
[`editorconfig.el`]: https://github.com/editorconfig/editorconfig-emacs/raw/master/editorconfig.el
