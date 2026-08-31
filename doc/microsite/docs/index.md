<p align="center"><img src ="images/logo.png" width="200" /></p>

Would you like to have nice, up-to-date license/copyright headers in your source code files but hate to manage them manually? Then __Headroom__ is the right tool for you. Define your license header as [Mustache][web:mustache] template, put any template variables into [YAML][wiki:yaml] configuarion file, and you're ready to go!

__Headroom__ also offers much more than just adding, replacing or dropping license headers. It can also update years in copyrights for you, provides specialized support for some source code file types, and much more!

[![asciicast](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8.svg)](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8)


# Main Features
- __License Header Management__ - [Add, replace or drop license headers][rel:running-headroom] in your source code files with a single command. Generated headers carry ownership markers; legacy headers are managed only when they exactly match the configured template, so unrelated comments are preserved.
- __Powerful Customization__ - Default [configuration][rel:configuration] should cover most use-cases, but if you need to put empty lines before/after generated header, use different comment style of headers, you can customize the configuration to match exactly your needs.
- __Built-in OSS License Headers__ - If you want to use license header for one of the popular OSS licenses, then _Headroom_ can [generate them for you][rel:running-headroom#gen-command]: documentation/running-headroom.md - no need to search for them on web.
- __Automatic Initialization for OSS Projects__ - Setting up external tools like _Headroom_ for your project can be boring. Fortunately, _Headroom_ can [initialize itself][rel:running-headroom#init-command] for your project, by generating configuration file and template files.
- __Content-aware Templates__ - for selected file types, _Headroom_ [exposes additional info][[rel:templates] about processed file using template variables, so you can use info like _Java_ package name in your templates.
- __Copyright Year Updater__ - _Headroom_ is good not only for basic license header management, but also for further processing of generated headers. Do you need to [update years in your copyrights][rel:post-processing]? No problem!


# Adopters
Here is the list of projects using _Headroom_. If you're using _Headroom_ and aren't on the list, feel free to [submit new issue][meta:new-issue] or [pull request][meta:pulls].

- [kowainik/hit-on](https://github.com/kowainik/hit-on) - Kowainik Git Workflow Helper Tool
- [kowainik/summoner](https://github.com/kowainik/summoner) - Tool for scaffolding batteries-included production-level Haskell projects
- [wireapp/wire-server](https://github.com/wireapp/wire-server) - Wire back-end services ([https://wire.com](https://wire.com))


[meta:new-issue]: https://github.com/xwinus/headroom/issues/new
[meta:pulls]: https://github.com/xwinus/headroom/pulls
[rel:configuration]: documentation/configuration.md
[rel:templates]: documentation/templates.md
[rel:post-processing]: documentation/post-processing.md
[rel:running-headroom]: documentation/running-headroom.md
[rel:running-headroom#gen-command]: documentation/running-headroom.md#gen-command
[rel:running-headroom#init-command]: documentation/running-headroom.md#init-command
[web:mustache]: https://mustache.github.io
[wiki:yaml]: https://en.wikipedia.org/wiki/YAML
