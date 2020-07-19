# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Added

- Add rustic-mode to editorconfig-indentation-alist (#208)
- Add conf-mode abbrev-table definitions (#220)

### Fixed

- Fix so that "?" does not match "/" (#211)
- Fix document typo (#213)

### Changed

- Define -mode-apply as an interactive command (#216)
- Use elisp core by default (#209)
- User functions in the hooks `editorconfig-hack-properties-functions` and
  `editorconfig-after-apply-functions` can no longer distinguish explicitly
  unset properties from ones that were never set in the first place.  (#222)


## [0.8.1] - 2019-10-10

### Added

- Add indentation support
  - #196
    - enh-ruby-mode
    - haxor-mode
    - mips-mode
    - nasm-mode
    - terra-mode
    - kotlin-mode
  - bpftrace-mode (#199)
  - f90-mode (#200)
- Add explicit support for rpm-spec-mode (efc1ff4b1c3422d6e231b1c01138becab4b9eded, see #197 )
- Add whitelist for file_type_emacs value (#204)


## [0.8.0] - 2019-03-26

### Fixed

- Allow library forget properties order (#187)
- Use API to get version info (#193)
  - `editorconfig-version()` was added and `editorconfig-core-version` removed
- Update docs and metadata to follow MELPA guidelines (#189)
- Refactor (#188, #191)


## [0.7.14] - 2018-12-25

### Added

- Add feature to decide major-mode from file_type_ext [EXPERIMENTAL] #175 (#178) (#179) (#180)
- Add feature to hack properties before applying #182
- Add variable editorconfig-trim-whitespaces-mode #183
  - Useful when you want to use non-default mode like `ws-butler` to trim spaces

### Fixed

- Make conf-mode used when a file has .editorconfig extension 01a064015ed8d00f2853f966f07d2be5b97bfe5e
- Fix tests
- Fix docs

### Changed

- Change hook name -custom-hooks -> -after-apply-functions bb4bc4497783e6607480cd0b761f974136784fdd


## [0.7.13] - 2018-08-23

### Fixed

- Check editorconfig configs when read only state changes (#168)
- use CURDIR instead of PWD in Makefile (#170)
- Refactor fnmatch-p (#171)
- Update tests


## [0.7.12] - 2018-06-20

### Added

- Add /Fix major-mode support
  - pug-mode #149
  - csharp-mode #154
- Add variable to disable lisp-indent-offset sometimes #155
- Add texinfo doc #159

### Fixed

- Avoid passing a non-absolute file path to editorconfig(1)  #151
- Use "-with-signature" coding systems for all UTF-16 charsets #158
- Allow normal whitespace when reading EditorConfig settings file #162
- Add some fixes to tests


## [0.7.11] - 2017-11-07

### Added

- Add /Fix major-mode support
  - apache-mode
  - groovy-mode
  - web-mode
- Add support for a custom lighter
- Add editorconfig-format-buffer function
- Add experimental file_type_emacs support

### Changed

- Change hook editorconfig is applied on


## [0.7.10] - 2017-06-07

*Undocumented*

## [0.7.9] - 2017-02-22

*Undocumented*

## [0.7.8] - 2016-08-09

*Undocumented*

## [0.7.7] - 2016-07-19

*Undocumented*

## [0.7.6] - 2016-05-05

*Undocumented*

## [0.7.5] - 2016-04-22

*Undocumented*

## [0.7.4] - 2016-03-31

*Undocumented*

## [0.7.3] - 2016-02-12

*Undocumented*

## [0.7.2] - 2016-01-27

*Undocumented*

## [0.7.1] - 2016-01-24

*Undocumented*

## [0.7.0] - 2016-01-17

*Undocumented*

## [0.6.2] - 2016-01-15

*Undocumented*

## [0.6.1] - 2015-12-09

*Undocumented*

## [0.6] - 2015-12-04

*Undocumented*

## [0.5] - 2015-11-04

*Undocumented*

## [0.4] - 2014-12-20

*Undocumented*

## [0.3] - 2014-05-07

*Undocumented*

## [0.2] - 2013-06-06

*Undocumented*

## [0.1] - 2012-02-07

*Undocumented*


[Unreleased]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.8.1...HEAD
[0.8.1]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.14...v0.8.0
[0.7.14]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.13...v0.7.14
[0.7.13]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.12...v0.7.13
[0.7.12]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.11...v0.7.12
[0.7.11]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.10...v0.7.11
[0.7.10]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.9...v0.7.10
[0.7.9]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.8...v0.7.9
[0.7.8]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.7...v0.7.8
[0.7.7]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.6...v0.7.7
[0.7.6]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.5...v0.7.6
[0.7.5]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.4...v0.7.5
[0.7.4]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.3...v0.7.4
[0.7.3]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.2...v0.7.3
[0.7.2]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.6.2...v0.7.0
[0.6.2]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.6...v0.6.1
[0.6]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.5...v0.6
[0.5]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.4...v0.5
[0.4]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.3...v0.4
[0.3]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.2...v0.3
[0.2]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.1...v0.2
[0.1]: https://github.com/editorconfig/editorconfig-emacs/releases/tag/v0.1
