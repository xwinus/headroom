<p align="center"><img src="https://github.com/xwinus/headroom/blob/master/doc/assets/logo.png?raw=true" alt="Headroom" width="200" /></p>

<p align="center">Keep license and copyright headers consistent across your source code.</p>

<p align="center">
  <a href="https://github.com/xwinus/headroom/actions/workflows/ci.yml"><img src="https://github.com/xwinus/headroom/actions/workflows/ci.yml/badge.svg" alt="CI" /></a>
  <a href="https://hackage.haskell.org/package/headroom"><img src="https://img.shields.io/hackage/v/headroom.svg" alt="Hackage version" /></a>
  <a href="https://www.stackage.org/package/headroom"><img src="https://www.stackage.org/package/headroom/badge/lts?label=stackage%20LTS" alt="Stackage version" /></a>
  <a href="LICENSE"><img src="https://img.shields.io/github/license/xwinus/headroom" alt="BSD 3-Clause license" /></a>
</p>

**Headroom** adds, updates, checks, and removes license headers in source code files. Define a header once as a [Mustache][web:mustache] template, keep its variables in a [YAML][wiki:yaml] configuration file, and apply it consistently across your project.

Headroom includes templates for popular open source licenses, understands the structure of selected source file types, and conservatively recognizes legacy headers from their license content.

## Quick Start

Install Headroom with [Homebrew][web:homebrew], then initialize it in your project:

```shell
brew install norcane/tools/headroom
headroom init --license-type mit --source-path src
```

Review the variables generated in `.headroom.yaml`, preview the changes, and apply them:

```shell
headroom run --dry-run
headroom run
```

You can also verify that all headers are up to date without modifying any files, which is useful in CI:

```shell
headroom run --check-headers
```

See the [Project Setup Guide][doc:project-setup] for custom templates, multiple source directories, and other setup options.

## How It Works

Given a template such as `headroom-templates/python.mustache`:

```mustache
# Copyright (c) {{ year }} {{ author }}
# SPDX-License-Identifier: MIT
```

And variables in `.headroom.yaml`:

```yaml
variables:
  author: "Jane Doe"
  year: "2026"
```

Headroom renders the header in each matching source file:

```python
# Copyright (c) 2026 Jane Doe
# SPDX-License-Identifier: MIT
```

[![asciicast](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8.svg)](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8)

## Main Features

- **Safe header management** — Add, replace, check, or remove owned headers while preserving unrelated source documentation and comments.
- **Built-in OSS licenses** — Generate ready-to-use templates for popular open source licenses.
- **Flexible configuration** — Control source paths, exclusions, margins, comment styles, and header placement through YAML.
- **Content-aware templates** — Use information extracted from the processed file, such as a Java package name or Haskell module metadata.
- **Copyright year updates** — Keep years and year ranges in copyright notices current.
- **Safe workflows** — Preview changes with `--dry-run`, validate them in CI, and initialize projects without overwriting existing files.

## Supported Licenses and File Types

Headroom includes templates for these licenses:

| License | Configuration value |
| --- | --- |
| Apache License 2.0 | `apache2` |
| BSD 3-Clause | `bsd3` |
| GNU GPL v2 | `gpl2` |
| GNU GPL v3 | `gpl3` |
| MIT | `mit` |
| Mozilla Public License 2.0 | `mpl2` |

Supported file types include C, C++, CSS, Dart, Go, Haskell, HTML, Java, JavaScript, Kotlin, Nix, PHP, PureScript, Python, Rust, Scala, Shell, and XML. See the [configuration reference][doc:configuration] for extensions and file-type-specific options.

## Installation

### Homebrew

```shell
brew install norcane/tools/headroom
```

### Cabal

```shell
cabal install headroom
```

### Pre-built Binaries

Pre-built x64 binaries for GNU/Linux and macOS are available on the [releases page][meta:releases].

### From Source

Headroom can be built with Cabal or Stack. See the [installation guide][doc:installation] for requirements and detailed instructions.

The `master` branch may contain changes that have not yet been published to Hackage. See the [changelog](CHANGELOG.md) for version details.

## Documentation

- [Project Setup Guide][doc:project-setup]
- [Command-line reference][doc:running-headroom]
- [Configuration reference][doc:configuration]
- [Template documentation][doc:templates]
- [Post-processing functions][doc:post-processing]
- [Haskell API on Hackage][hackage:headroom]

## Contributing

Bug reports and pull requests are welcome. You can [open an issue][meta:new-issue] or [submit a pull request][meta:pulls] on GitHub.

To preview the documentation locally, run:

```shell
cd doc/microsite/
mkdocs serve
```

The microsite will be available at <http://127.0.0.1:8000>.

## Adopters

- [kowainik/hit-on](https://github.com/kowainik/hit-on) — Kowainik Git Workflow Helper Tool
- [kowainik/summoner](https://github.com/kowainik/summoner) — Tool for scaffolding batteries-included production-level Haskell projects
- [wireapp/wire-server](https://github.com/wireapp/wire-server) — Wire back-end services (<https://wire.com>)

Using Headroom in your project? Feel free to [open an issue][meta:new-issue] or [submit a pull request][meta:pulls] to add it to this list.

## Mentions

- [Issue #2 of Bind the Gap magazine](https://bindthegap.news/issues/02dec2020.html) includes a chapter dedicated to Headroom (pages 17–18).

## License

Headroom is distributed under the [BSD 3-Clause License](LICENSE).

[hackage:headroom]: https://hackage.haskell.org/package/headroom
[meta:new-issue]: https://github.com/xwinus/headroom/issues/new
[meta:pulls]: https://github.com/xwinus/headroom/pulls
[meta:releases]: https://github.com/xwinus/headroom/releases
[web:homebrew]: https://brew.sh
[doc:configuration]: https://doc.norcane.com/headroom/latest/documentation/configuration/
[doc:installation]: https://doc.norcane.com/headroom/latest/documentation/installation/
[doc:post-processing]: https://doc.norcane.com/headroom/latest/documentation/post-processing/
[doc:project-setup]: https://doc.norcane.com/headroom/latest/documentation/project-setup-guide/
[doc:running-headroom]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/
[doc:templates]: https://doc.norcane.com/headroom/latest/documentation/templates/
[web:mustache]: https://mustache.github.io
[wiki:yaml]: https://en.wikipedia.org/wiki/YAML
