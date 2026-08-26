# Changelog
All notable changes to this project will be documented in this file.

## 0.5.0.0 (in development)
- [#87] FIXED: Misleading error message when YAML syntax of .headroom.yaml is invalid
- [#88] Add support for _Python_
- [#89] Support GHC 9.2
- [#92] Switch to fourmolu source code formatter
- [#93] FIXED: Haddock module header detection is faulty
- [#94] Add support for _XML_
- [#100] FIXED: Block comment terminator followed by source code isn't detected
- [#101] FIXED: `init` command overwrites already existing configuration and template files
- [#102] FIXED: `builtin-templates` set in YAML configuration is ignored when the CLI option is absent
- [#104] FIXED: Source files are written non-atomically, so concurrent changes can be lost
- [#106] FIXED: Missing dotted Mustache variable renders as empty text instead of being reported
- Bump _LTS Haskell_ to `24.56`

## 0.4.3.0 (released 2022-01-20)
- [#78] Check automatically for Headroom updates
- [#81] Add support for _PHP_ language
- [#83] FIXED: _Update Copyright_ post-processor incorrectly changes years in some cases
- [#84] Add support for _Dart_ language
- [#85] Add support for _Kotlin_ language
- Bump _LTS Haskell_ to `18.22`

## 0.4.2.0 (released 2021-07-01)
- [#42] Add option to exclude source paths ignored by VCS
- [#66] Allow loading templates from HTTP(S) resource
- Bump _LTS Haskell_ to `18.0`

## 0.4.1.0 (released 2021-03-14)
- [#69] Improve console output
- [#71] Add support for _Go_ language
- [#72] FIXED: Headroom doctest not passing with hashable-1.3.1.0

## 0.4.0.0 (released 2021-03-04)
- [#57] FIXED: put-before/put-after in commented code breaks header detection
- [#58] BREAKING CHANGE: Extend configuration for defining header margins
- [#59] Check compatibility of loaded YAML configuration
- [#61] BREAKING CHANGE: Use regular expressions to specify headers start/end
- [#62] Sanitize block comment headers with line prefixes
- [#64] Add command line option for displaying version
- Bump _LTS Haskell_ to `17.5`

## 0.3.2.0 (released 2020-11-26)
- [#56] Move CI from Travis to GitHub Actions
- Minor improvements & bugfixes
- Bump _LTS Haskell_ to `16.23`

## 0.3.1.0 (released 2020-08-18)
- [#48] Decouple IO actions for easier testing
- [#53] Add support for _PureScript_ source code files
- [#54] Extract _dynamic variables_ from _Java_ source code files
- Bump _LTS Haskell_ to `16.10`

## 0.3.0.0 (released 2020-06-28)
- [#25] Extract _dynamic variables_ from Haskell source code files
- [#29] Add _dynamic variable_ for current year and add support for updating years
- [#30] Allow workflow without configuration and template files
- [#49] Microsite for documentation ([https://doc.norcane.com/headroom/latest/](https://doc.norcane.com/headroom/latest/))
- [#51] Allow template variable values to be templates itself
- [#52] Post-processing functions for _license headers_
- Various bugfixes and performance improvements
- Bump _LTS Haskell_ to `16.2`

## 0.2.2.1 (released 2020-05-08)
- Minor improves in generated `.headroom.yaml` in `init` mode

## 0.2.2.0 (released 2020-05-04)
- [#45] Add `-c|--check-headers` command line option
- Bump _LTS Haskell_ to `15.11`

## 0.2.1.1 (released 2020-04-30)
- [#47] Make possible to build Headroom with GHC 8.10
- Remove unused dependency on `text` package.

## 0.2.1.0 (released 2020-04-29)
- [#41] Add `--dry-run` option to allow test run without changing files.
- [#44] Don't touch files whose contents have not changed.
- [#46] Add `-e|--excluded-path=REGEX` option to exclude source paths.
- Bump _LTS Haskell_ to `15.10`.
- Remove unused dependencies.

## 0.2.0.0 (released 2020-04-25)
- [#28] Allow license headers to be anywhere in the file, not only at the very beginning.
- [#31] Render templates for each source file instead of once (blocker for [#25])
- [#32] Allow custom user configuration for license headers.
- [#34] Support for [Rust](https://www.rust-lang.org/)
- [#35] Support for [Bash](https://www.gnu.org/software/bash/)
- [#36] Support for _C/C++_
- [#38] Add `-a|--add-headers` command-line option
- bump _LTS Haskell_ to `15.9`

## 0.1.3.0 (released 2020-03-23)
- [#24] Added _Init_ command that automatically creates initial _Headroom_ configuration and set of templates.
- bump _LTS Haskell_ to `15.5`

## 0.1.2.0 (released 2020-03-06)
- FIXED: missing `test-data/` folder in dist tarball prevents tests execution
- bump _LTS Haskell_ to `15.2`

## 0.1.1.0 (released 2020-02-26)
- FIXED: missing `embedded/` folder in dist tarball for _Hackage_ prevents successful build using _Cabal_
- bump _LTS Haskell_ to `15.1`

## 0.1.0.0 (released 2020-02-20)
- initial release
