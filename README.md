# EditorConfig Emacs Plugin

This is an [EditorConfig][] plugin for Emacs.

## Installation

Download the [EditorConfig core][] and follow the instructions in the README
and INSTALL files to install it.

Once EditorConfig core is installed, copy `editorconfig.el` to `~/.emacs.d/`
and add the following to your `~/.emacs` file:

    (add-to-list 'load-path "~/.emacs.d/")
    (load "editorconfig")

[EditorConfig]: http://editorconfig.org
[EditorConfig core]: https://github.com/editorconfig/editorconfig
