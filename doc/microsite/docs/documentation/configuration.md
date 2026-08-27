Headroom uses two main sources of configuration, _global configuration_, shared between all _Headroom_ instances, and _application configuration_, that is specific for project in which _Headroom_ is executed.

## Global Configuration
This configuration is used to configure some global rules that are applied for all instances of _Headroom_, for example whether _Headroom_ should automatically check for updates, etc. It's located in directory in user's home directory (`~/.headroom`) and has following structure:

```
~/.headroom
├── cache.sqlite
└── global-config.yaml
```

Where:

- `cache.sqlite` is key-value cache _(SQLite database)_ used by _Headroom_ to store some values, such as when was the last attemt to check updates done
- `global-config.yaml` is the global configuration _YAML_ file

### YAML File Structure
Below is the structure of `global-config.yaml` configuration file:

```yaml
## This is the global configuration file for Headroom.
## See https://github.com/xwinus/headroom for more details.

## Configuration for Headroom Updater.
updates:

  ## Whether Headroom should automatically check for available updates. 
  check-for-updates: true

  ## How often (in days) should Headroom check for updates.
  update-interval-days: 1
```

At this moment, only configuration of automatic updates is present.

## Application Configuration
Headroom uses three different sources of configuration, where the next one eventually overrides the previous one:

1. default configuration in [embedded/default-config.yaml][file:embedded/default-config.yaml]
1. custom configuration in `.headroom.yaml`
1. command line arguments

The _YAML_ configuration file, located in `.headroom.yaml`, is the main and most capable configuration source for _Headroom_. Don't forget to check the _default YAML configuration_ in [embedded/default-config.yaml][file:embedded/default-config.yaml], both to know more about all possible configuration keys (it's well documented there), but also to know which value you need to override when in your custom `.headroom.yaml`.

### Configuration Version
When _Headroom_ is loading your `.headroom.yaml` file, it first checks whether your configuration is compatible with your version of _Headroom_. This is done by checking the `version` field. If you generated your configuration using _Headroom's_ `init` or `gen` command, then this field was automatically set to correct value. You don't need to care about this field unless you upgrade your _Headroom_ version, in case that this field will need to be updated, you'll be always informed by corresponding [migration guide][doc:migration-guide].

### Top Level Overview
From top-level perspective, the _YAML_ configuration has the following structure:

```yaml
version: 1.2.3.4            ## Version of Headroom the configuration is compatible with
run-mode: ...               ## Default mode of the 'run' command
excluded-paths: []          ## Path(s) to exclude from 'source-paths'
exclude-ignored-paths: true ## Whether to exclude source paths ignored by GIT
source-paths: []            ## Path(s) to source code files to process headers in
template-paths: []          ## Path(s) to license header templates
variables: {}               ## Value(s) of variable(s) to replace in template(s)
license-headers: {}         ## License header detection & positioning
post-process: {}            ## Post-processing functions
```

#### Relation between Command-line Arguments and YAML Configuration
Not all configuration options can be set/overridden using the command line arguments, but below is the list of matching _YAML_ options and command line options:

| YAML option             | Command Line Option            |		
|-------------------------|--------------------------------|		
| `run-mode: add`         | `-a`, `--add-headers`          |		
| `run-mode: drop`        | `-d`, `--drop-headers`         |		
| `run-mode: replace`     | `-r`, `--replace-headers`      |	
| `run-mode: check`       | `-c`, `--check-headers`        |		
| `source-paths`          | `-s`, `--source-path PATH`, or positional arguments |	
| `excluded-paths`        | `-e`, `--excluded-path REGEX`  |	
| `exclude-ignored-paths` | `--exclude-ignored-paths`      |	
| `template-paths`        | `-t`, `--template-path PATH`   |
| `variables`             | `-v`, `--variable "KEY=VALUE"` |

Where `source-path`, `template-path` and `variable` command line arguments can be used multiple times to set more values. Source paths can be also passed to the `run` command as positional arguments, in which case they're combined with any `-s|--source-path` options.

### `run-mode` key
This configuration changes default behaviour of the `run` command. Possible values are `add`, `drop`, `replace` or `check`. For more details, see the [Running Headroom][doc:running-headroom#run-command] chapter.

### `excluded-paths` key
If you need to exclude selected paths from the list of paths defined in `source-paths`, you can use this option. The value of this configuration is list of `regexes`, so be aware of that when you exclude path with dot, as you need to escape it, like:

```yaml
excluded-paths:
    - '\.stack-work'
```

### `exclude-ignored-paths` key
If some paths within your source paths list are ignored by _Git_, you can either manually exclude them using the above key, but better way is to use this option. Using this configuration, _Headroom_ will scan Git's ignore rules for current project and will automatically exclude appropriate source paths.

### `source-paths` key
Defines list of paths to source code files to process. Note that given path, if directory, is __recursively__ traversed for all types of source code files knowh to _Headroom_.

### `template-paths` key
Defines list of _template_ paths. Path can be either relative or absolute path of template stored on local file system, or _URL_ pointing to template stored on Internet (note that only `HTTP` or `HTTPS` protocols are supported).

### `variables` key
This is the main point where you define values for variables present in template files. Variables in _Headroom_ can be split into two main categories:

- __static variables__ - Variables that are defined by user and which values the same for all processed source code files (e.g. `author` variable that holds author's name).
- __dynamic variables__ - Variables that are defined by _Headroom_ and which values can vary file from file. Convention is that these variables are prefixed by _underscore_, so you can spot them easily. Below is the list of general _dynamic variables_, provided by _Headroom_:
    - `_current_year` - value of the current year (e.g. `2020`)
More _dynamic variables_ might be exposed for individual processed files by the [content-aware templates][doc:templates] for selected types of source code files.

Also note that you can use __reference other variables in variables__, as long as you
- don't call them recursively
- don't define cyclic dependencies between them

Example of such nested reference is below:

```yaml
variables:
    author: John Smith
    copyright: Copyright (c) {{ _current_year }} {{ author }}
```

### `license-headers` key
This configuration allows to modify the behaviour how _license headers_ are detected and/or rendered for each supported file type. As example, this is how default configuration for _Haskell_ source code files looks like:

```yaml
## Haskell configuration
haskell:
  file-extensions: ["hs"]
  margin-top-code: 0
  margin-top-file: 0
  margin-bottom-code: 0
  margin-bottom-file: 0
  put-after: []
  put-before: ["^module"]
  block-comment:
    starts-with: ^{-\|
    ends-with: (?<!#)-}$
```

If the default configuration for selected _file type_ doesn't suit your needs, you can override following settings:

1. __list of file extensions__ - if you need to match selected _file type_ with different set of file extensions, override the `file-extensions` option.
1. __margin around license header__ - if you want to add blank lines before and/or after generated license header, you can use one of the following options:
    - `margin-top-code` - defines margin (in no. of empty lines) between generated header and code above it, but only **IF** header is **NOT** the first thing in file.
    - `margin-top-file` - defines margin (in no. of empty lines) between generated header the very top of the file, but only **IF** header **IS** the first thing in file.
    - `margin-bottom-code` - defines margin (in no. of empty lines) between generated header and code below it, but only **IF** header is **NOT** the last thing in file.
    - `margin-bottom-file` - defines margin (in no. of empty lines) between generated header the very end of the file, but only **IF** header **IS** the last thing in file.
1. __put header before/after pattern__ - you can define before and/or after which pattern _Headroom_ will put the license header (such as before the line starting with `module` in _Haskell_ code).
1. __ syntax of header comment__ - license header can be represented by either single block comment (such as `{- -}` in Haskell) or bunch of single line comments (such as `//` in _C/C++_ or `--` in _Haskell_). Block comment can be defined by using the use the `block-comment` option with `starts-with` and `ends-with` sub-options, line comment by `line-comment` option with `prefixed-by` sub-option. Note that all these options expects valid regular expressions.

### `post-process` key
This is the configuration for _post-processing functions_. See more details in [Post-processing functions][doc:post-processing] chapter.

## Supported License Types
_Headroom_ provides built-it license templates for major _OSS_ licenses. Whenever you need to specify the _license type_ in `.headroom.yaml` or command line arguments, use one of the values below:

| License        | Name in configuration |
|----------------|-----------------------|
| _Apache 2.0_   | `apache2`             |
| _BSD 3-Clause_ | `bsd3`                |
| _GPLv2_        | `gpl2`                |
| _GPLv3_        | `gpl3`                |
| _MIT_          | `mit`                 |
| _MPL2_         | `mpl2`                |

If you miss support for license type you use, feel free to [open new issue][meta:new-issue].

## Supported File Types
_Headroom_ can manage license headers only for supported types of source code files. Whenever you need to specify the _license type_ in `.headroom.yaml` or command line arguments, use one of the values below:

| Language     | Name in configuration | Default extensions |
|--------------|-----------------------|--------------------|
| _C_          | `c`                   | `.c`               |
| _C++_        | `cpp`                 | `.cpp`             |
| _CSS_        | `css`                 | `.css`             |
| _Dart_       | `dart`                | `.dart`            |
| _Go_         | `go`                  | `.go`              |
| _Haskell_    | `haskell`             | `.hs`              |
| _HTML_       | `html`                | `.html`, `.htm`    |
| _Java_       | `java`                | `.java`            |
| _JavaScript_ | `js`                  | `.js`              |
| _Nix_        | `nix`                 | `.nix`             |
| _PHP_        | `php`                 | `.php`             |
| _PureScript_ | `purescript`          | `.purs`            |
| _Python_     | `python`              | `.py`              |
| _Rust_       | `rust`                | `.rs`              |
| _Scala_      | `scala`               | `.scala`           |
| _Shell_      | `shell`               | `.sh`              |
| _XML_        | `xml`               | `.xml`              |

If you miss support for file type you use, feel free to [open new issue][meta:new-issue].


[doc:templates]: templates.md
[doc:migration-guide]: ../migration-guide.md
[doc:running-headroom#run-command]: running-headroom.md#run-command
[doc:post-processing]: post-processing.md
[file:embedded/default-config.yaml]: https://github.com/xwinus/headroom/blob/master/embedded/default-config.yaml
[meta:new-issue]: https://github.com/xwinus/headroom/issues/new