Version `0.5.0.0` is minor release that brings some improvements and bugfixes, without any breaking changes in _CLI_ interface or configuration.

## New & Noteworthy

### Support for Python language
This version adds support for managing license/copyright headers in _Python_ source code files. Default configuration assumes that license/copyright header is the very first comment present in _Python_ file, such as:

```python
#!/usr/bin/env python3

# This is
# header

# This is not

print("This line will be printed.")
```

At this moment, only basic support is present (i.e. no _dynamic variables_ are extracted from the header), but this might change in future releases.

### Global Configuration
Since this release, apart from usual per-project configuration that _Headroom_ uses (`.headroom.yaml` files), it also adds _global configuration_, stored in user's home directory (`~/.headroom`), where some global config values can be changed, such as how often (and if) _Headroom_ should check for updates. See [Configuration Overview][doc:configuration] chapter for more details.

### Automatically check for updates
This release adds functionality that automatically checks whether new version of _Headroom_ is available and if yes, it notifies user in command line. By default, this happens once a week and it can be changed (or disabled) in _global configuration_. See [Configuration Overview][doc:configuration] chapter for more details.

!!! note
    Info about new version is retrieved using _GitHub REST API_ from project releases page, no personal info is collected and no tracking is done. Your privacy is priority for us and always will be respected.

## Other bugfixes and improvements
- [[#87]][github/issue/87] - FIXED: Misleading error message when YAML syntax of .headroom.yaml is invalid

[doc:configuration]: documentation/configuration.md
[doc:migration-guide]: migration-guide.md
[doc:running-headroom]: documentation/running-headroom.md
[doc:templates]: documentation/templates.md
[github:vcs-ignore]: https://github.com/vaclavsvejcar/vcs-ignore
[github/issue/87]: https://github.com/vaclavsvejcar/headroom/issues/87