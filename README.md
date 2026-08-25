<p align="center"><img src="https://github.com/vaclavsvejcar/headroom/blob/master/doc/assets/logo.png?raw=true" width="200" /></p>

![CI](https://github.com/vaclavsvejcar/headroom/workflows/CI/badge.svg)
[![Hackage version](http://img.shields.io/hackage/v/headroom.svg)](https://hackage.haskell.org/package/headroom)
[![Stackage version](https://www.stackage.org/package/headroom/badge/lts?label=stackage%20LTS)](https://www.stackage.org/package/headroom)

Would you like to have clean, up-to-date license and copyright headers in your source code files but hate managing them manually? Then **Headroom** is the right tool for you. Define your license header as a [Mustache][web:mustache] template, put any template variables in a [YAML][wiki:yaml] configuration file, and you're ready to go!

**Headroom** offers much more than simply [adding, replacing, or removing][doc:running-headroom] license headers. It can also [update copyright years][doc:post-processing], provide [content-aware templates][doc:templates] for selected source code file types, and much more!

[![asciicast](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8.svg)](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8)

## Main Features

- **License Header Management** — [Add, replace, or remove license headers][doc:running-headroom] in your source code files with a single command. Unlike many similar tools, **Headroom** can also replace or remove headers that it did not generate by using smart header auto-detection.
- **Powerful Customization** — The default [configuration][doc:configuration] should cover most use cases. However, if you need blank lines before or after the generated header or want to use a different comment style, you can customize the configuration to match your needs exactly.
- **Built-in OSS License Headers** — If you want to use a license header for a popular OSS license, **Headroom** can [generate it for you][doc:running-headroom#gen-command]—no need to search for it on the web.
- **Automatic Initialization for OSS Projects** — Setting up external tools such as **Headroom** for your project can be tedious. Fortunately, **Headroom** can [initialize your project][doc:running-headroom#init-command] by generating a configuration file and template files.
- **Content-aware Templates** — For selected file types, **Headroom** [exposes additional information][doc:templates] about the file being processed through template variables, allowing you to include details such as a Java package name in your templates.
- **Copyright Year Updater** — **Headroom** is useful not only for basic license header management but also for further processing of generated headers. Do you need to [update years in your copyright notices][doc:post-processing]? No problem!

## Installation

You can get **Headroom** via one of the following options:

1. Download a pre-built binary for GNU/Linux or macOS (x64) from the [releases page][meta:releases].
1. Install it using [Homebrew][web:homebrew]: `brew install norcane/tools/headroom`.
1. Build it from source; see the [project microsite][web:headroom] for more details.

## Adopters

Here is a list of projects using **Headroom**. If you're using **Headroom** and aren't on the list, feel free to [submit a new issue][meta:new-issue] or [pull request][meta:pulls].

- [kowainik/hit-on](https://github.com/kowainik/hit-on) - Kowainik Git Workflow Helper Tool
- [kowainik/summoner](https://github.com/kowainik/summoner) - Tool for scaffolding batteries-included production-level Haskell projects
- [wireapp/wire-server](https://github.com/wireapp/wire-server) - Wire back-end services (https://wire.com)

## Mentions

- [Issue #2 of Bind the Gap magazine](https://bindthegap.news/issues/02dec2020.html) includes a chapter dedicated to **Headroom** (pages 17–18).

## Documentation

- For end-user documentation, see the [official project microsite][web:headroom].
- For Haskell API documentation, see [Headroom on Hackage][hackage:headroom].

### Running microsite locally

If you need to preview the microsite documentation for an unreleased version of **Headroom** (such as the current `master` branch), you can run it locally using [MkDocs][web:mkdocs]:

```shell
cd doc/microsite/
mkdocs serve
```

The documentation will then be available at <http://127.0.0.1:8000>.

[hackage:headroom]: https://hackage.haskell.org/package/headroom
[meta:new-issue]: https://github.com/vaclavsvejcar/headroom/issues/new
[meta:pulls]: https://github.com/vaclavsvejcar/headroom/pulls
[meta:releases]: https://github.com/vaclavsvejcar/headroom/releases
[web:headroom]: https://doc.norcane.com/headroom/latest/
[web:homebrew]: https://brew.sh
[doc:configuration]: https://doc.norcane.com/headroom/latest/documentation/configuration/
[doc:templates]: https://doc.norcane.com/headroom/latest/documentation/templates/
[doc:post-processing]: https://doc.norcane.com/headroom/latest/documentation/post-processing/
[doc:running-headroom]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/
[doc:running-headroom#gen-command]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/#gen-command
[doc:running-headroom#init-command]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/#init-command
[web:mkdocs]: https://www.mkdocs.org
[web:mustache]: https://mustache.github.io
[wiki:yaml]: https://en.wikipedia.org/wiki/YAML
