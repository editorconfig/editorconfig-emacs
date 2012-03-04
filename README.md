# EditorConfig Emacs Plugin

This is an [EditorConfig][] plugin for Emacs.

## Installation

Download the [EditorConfig core][] and follow the instructions in the README
and INSTALL files to install it.

Once EditorConfig core is installed, copy `editorconfig.el` to `~/.emacs.d/`
and add the following to your `~/.emacs` file:

    (add-to-list 'load-path "~/.emacs.d/")
    (load "editorconfig")

## Supported properties

The EditorConfig Emacs plugin supports the following EditorConfig [properties][]:

* indent_style
* indent_size
* tab_width
* end_of_line
* root (only used by EditorConfig core)

[EditorConfig]: http://editorconfig.org
[EditorConfig core]: https://github.com/editorconfig/editorconfig-core
[properties]: http://editorconfig.org/#supported-properties
