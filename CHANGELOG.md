# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

### Changed

- Remove editorconfig-mode legacy version ([#304])
  - Remove flag `editorconfig--legacy-version`, which was defined in [#263]
- Separate some utility commands to new file ([#330])
  - Following commands are now defined in `editoroconfig-tools.el`, not `editorconfig.el`
    - editorconfig-apply
    - editorconfig-mode-apply
    - editorconfig-find-current-editorconfig
    - editorconfig-display-current-properties (and its alias describe-editorconfig-properties)
    - editorconfig-format-buffer
  - These commands are configured to be autoloaded functions, except for `editorconfig-mode-apply`

### Deprecated

### Removed

### Fixed

### Security


## [0.10.1]

### Fixed

- Fix when-let (again) ([#305])
- Fix compile warning of python-mode offset ([#306])


## [0.10.0] - 2023-05-07

### Added

- Enable indentation for tree-sitter based typescript mode ([#282])
- Add support for json-ts-mode ([#283])
- Add support for some treesit modes ([#287])
- Add indent variable associations for numerous tree-sitter modes ([#290])
- Add js-ts-mode' spec to editorconfig-indentation-alist' ([#293])
- Add bash-ts-mode to editorconfig-indentation-alist ([#296])
- Add support for gdscript-mode ([#300])

### Changed

- Drop Emacs 24.x and 25.x ([#286])

### Fixed

- Fix write-file-functions default value ([#295])
- Check mode-class property for special modes ([#301])
- Load subr-x when compiling ([#302])


## [0.9.1] - 2022-11-07

### Fixed

-  Check filename rather than buffer-file-name for consistency ([#280])


## [0.9.0] - 2022-10-23

### Changed

- Use new implementation by default ([#263])
  - Set `(setq editorconfig--legacy-version t)` to use previous one


## [0.8.2] - 2021-08-13

### Added

- Add rustic-mode to editorconfig-indentation-alist ([#208])
- Add conf-mode abbrev-table definitions ([#220])
- Add meson-mode indentation rule ([#253])
- Add support for rjsx-mode ([#254])
- Update README for NonGNU ELPA repository ([#259])
- Add new implementation of editorconfig-mode ([#248], [#250], [#251], [#255], [#258], [#260])
  - By default this is disabled: set `(setq editorconfig--enable-20210221-testing t)` to use this

### Fixed

- Fix so that "?" does not match "/" ([#211])
- Fix document typo ([#213])
- Don't make unchanged vars buffer-local ([#222])
- Silence byte-compiler warnings ([#235])
- Use revert-buffer-with-coding-system to set coding system ([#236])
- Do not run editorconfig-apply on recentf-save-file ([#241])
- Skip special-mode buffers when applying ([#247])
- Stop excluding remote files by default ([#234], [#245])
- Fix editorconfig execution for remote hosts via tramp ([#249])
- Add minor fixes to tests ([#252])
- Fix excluding the recentf-save-file when in a symlinked directory ([#256])

### Changed

- Define -mode-apply as an interactive command ([#216])
- Use elisp core by default ([#209])
- User functions in the hooks `editorconfig-hack-properties-functions` and
  `editorconfig-after-apply-functions` can no longer distinguish explicitly
  unset properties from ones that were never set in the first place.  ([#222])


## [0.8.1] - 2019-10-10

### Added

- Add indentation support
  - [#196]
    - enh-ruby-mode
    - haxor-mode
    - mips-mode
    - nasm-mode
    - terra-mode
    - kotlin-mode
  - bpftrace-mode ([#199])
  - f90-mode ([#200])
- Add explicit support for rpm-spec-mode ([efc1ff4], see [#197] )
- Add whitelist for file_type_emacs value ([#204])


## [0.8.0] - 2019-03-26

### Fixed

- Allow library forget properties order ([#187])
- Use API to get version info ([#193])
  - `editorconfig-version()` was added and `editorconfig-core-version` removed
- Update docs and metadata to follow MELPA guidelines ([#189])
- Refactor ([#188], [#191])


## [0.7.14] - 2018-12-25

### Added

- Add feature to decide major-mode from file_type_ext [EXPERIMENTAL] [#175] ([#178]) ([#179]) ([#180])
- Add feature to hack properties before applying [#182]
- Add variable editorconfig-trim-whitespaces-mode [#183]
  - Useful when you want to use non-default mode like `ws-butler` to trim spaces

### Fixed

- Make conf-mode used when a file has .editorconfig extension [01a0640]
- Fix tests
- Fix docs

### Changed

- Change hook name -custom-hooks -> -after-apply-functions [bb4bc44]


## [0.7.13] - 2018-08-23

### Fixed

- Check editorconfig configs when read only state changes ([#168])
- use CURDIR instead of PWD in Makefile ([#170])
- Refactor fnmatch-p ([#171])
- Update tests


## [0.7.12] - 2018-06-20

### Added

- Add /Fix major-mode support
  - pug-mode [#149]
  - csharp-mode [#154]
- Add variable to disable lisp-indent-offset sometimes [#155]
- Add texinfo doc [#159]

### Fixed

- Avoid passing a non-absolute file path to editorconfig(1)  [#151]
- Use "-with-signature" coding systems for all UTF-16 charsets [#158]
- Allow normal whitespace when reading EditorConfig settings file [#162]
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


[Unreleased]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.10.1...HEAD
[0.10.1]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.10.0...v0.10.1
[0.10.0]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.9.1...v0.10.0
[0.9.1]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.8.2...v0.9.0
[0.8.2]: https://github.com/editorconfig/editorconfig-emacs/compare/v0.8.1...v0.8.2
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
[#304]: https://github.com/editorconfig/editorconfig-emacs/issues/304
[#306]: https://github.com/editorconfig/editorconfig-emacs/issues/306
[#305]: https://github.com/editorconfig/editorconfig-emacs/issues/305
[#302]: https://github.com/editorconfig/editorconfig-emacs/issues/302
[#301]: https://github.com/editorconfig/editorconfig-emacs/issues/301
[#300]: https://github.com/editorconfig/editorconfig-emacs/issues/300
[#296]: https://github.com/editorconfig/editorconfig-emacs/issues/296
[#295]: https://github.com/editorconfig/editorconfig-emacs/issues/295
[#293]: https://github.com/editorconfig/editorconfig-emacs/issues/293
[#290]: https://github.com/editorconfig/editorconfig-emacs/issues/290
[#287]: https://github.com/editorconfig/editorconfig-emacs/issues/287
[#286]: https://github.com/editorconfig/editorconfig-emacs/issues/286
[#283]: https://github.com/editorconfig/editorconfig-emacs/issues/283
[#282]: https://github.com/editorconfig/editorconfig-emacs/issues/282
[#280]: https://github.com/editorconfig/editorconfig-emacs/issues/280
[#263]: https://github.com/editorconfig/editorconfig-emacs/issues/263
[#260]: https://github.com/editorconfig/editorconfig-emacs/issues/260
[#258]: https://github.com/editorconfig/editorconfig-emacs/issues/258
[#255]: https://github.com/editorconfig/editorconfig-emacs/issues/255
[#251]: https://github.com/editorconfig/editorconfig-emacs/issues/251
[#250]: https://github.com/editorconfig/editorconfig-emacs/issues/250
[#248]: https://github.com/editorconfig/editorconfig-emacs/issues/248
[#259]: https://github.com/editorconfig/editorconfig-emacs/issues/259
[#256]: https://github.com/editorconfig/editorconfig-emacs/issues/256
[#252]: https://github.com/editorconfig/editorconfig-emacs/issues/252
[#249]: https://github.com/editorconfig/editorconfig-emacs/issues/249
[#245]: https://github.com/editorconfig/editorconfig-emacs/issues/245
[#234]: https://github.com/editorconfig/editorconfig-emacs/issues/234
[#241]: https://github.com/editorconfig/editorconfig-emacs/issues/241
[#236]: https://github.com/editorconfig/editorconfig-emacs/issues/236
[#235]: https://github.com/editorconfig/editorconfig-emacs/issues/235
[#222]: https://github.com/editorconfig/editorconfig-emacs/issues/222
[#222]: https://github.com/editorconfig/editorconfig-emacs/issues/222
[#220]: https://github.com/editorconfig/editorconfig-emacs/issues/220
[#216]: https://github.com/editorconfig/editorconfig-emacs/issues/216
[#213]: https://github.com/editorconfig/editorconfig-emacs/issues/213
[#211]: https://github.com/editorconfig/editorconfig-emacs/issues/211
[#209]: https://github.com/editorconfig/editorconfig-emacs/issues/209
[#208]: https://github.com/editorconfig/editorconfig-emacs/issues/208
[#204]: https://github.com/editorconfig/editorconfig-emacs/issues/204
[#200]: https://github.com/editorconfig/editorconfig-emacs/issues/200
[#199]: https://github.com/editorconfig/editorconfig-emacs/issues/199
[#197]: https://github.com/editorconfig/editorconfig-emacs/issues/197
[#196]: https://github.com/editorconfig/editorconfig-emacs/issues/196
[#193]: https://github.com/editorconfig/editorconfig-emacs/issues/193
[#191]: https://github.com/editorconfig/editorconfig-emacs/issues/191
[#189]: https://github.com/editorconfig/editorconfig-emacs/issues/189
[#188]: https://github.com/editorconfig/editorconfig-emacs/issues/188
[#187]: https://github.com/editorconfig/editorconfig-emacs/issues/187
[#183]: https://github.com/editorconfig/editorconfig-emacs/issues/183
[#182]: https://github.com/editorconfig/editorconfig-emacs/issues/182
[#180]: https://github.com/editorconfig/editorconfig-emacs/issues/180
[#179]: https://github.com/editorconfig/editorconfig-emacs/issues/179
[#178]: https://github.com/editorconfig/editorconfig-emacs/issues/178
[#175]: https://github.com/editorconfig/editorconfig-emacs/issues/175
[#171]: https://github.com/editorconfig/editorconfig-emacs/issues/171
[#170]: https://github.com/editorconfig/editorconfig-emacs/issues/170
[#168]: https://github.com/editorconfig/editorconfig-emacs/issues/168
[#162]: https://github.com/editorconfig/editorconfig-emacs/issues/162
[#159]: https://github.com/editorconfig/editorconfig-emacs/issues/159
[#158]: https://github.com/editorconfig/editorconfig-emacs/issues/158
[#155]: https://github.com/editorconfig/editorconfig-emacs/issues/155
[#154]: https://github.com/editorconfig/editorconfig-emacs/issues/154
[#151]: https://github.com/editorconfig/editorconfig-emacs/issues/151
[#149]: https://github.com/editorconfig/editorconfig-emacs/issues/149
[01a0640]: https://github.com/editorconfig/editorconfig-emacs/commit/01a064015ed8d00f2853f966f07d2be5b97bfe5e
[efc1ff4]: https://github.com/editorconfig/editorconfig-emacs/commit/efc1ff4b1c3422d6e231b1c01138becab4b9eded
[bb4bc44]: https://github.com/editorconfig/editorconfig-emacs/commit/bb4bc4497783e6607480cd0b761f974136784fdd
