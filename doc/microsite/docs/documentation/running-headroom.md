After you set up _Headroom_ for your project, you should be ready to run it. Below is the overview of the _command line interface_ that _Headroom provides_.

## Commands
_Headroom_ provides three different commands, you might already familiar with the `gen` or `init` command from the [Project Setup Guide][rel:project-setup-guide] chapter:

1. __run__ - this is the main command and the one you'll use the most. It allows you to add, replace, drop or check license headers in source code files.
1. __gen__ - this command allows to generate files stub required by _Headroom_, such as _YAML_ configuration file or license templates for selected _OOS_ license.
1. __init__ - this command simplifies _Headroom_ setup by automatically scanning your project and generating proper `.headroom.yaml` and template files. 

### Run Command
Let's start with the most useful one. This command allows you to manipulate license headers in source code files, and provides four different modes to do so:

1. __add mode__ _(default one)_ - will only add missing license headers, but won't touch existing ones.
1. __replace mode__ - will add missing license headers, but also replace any already existing ones (if differs).
1. __check mode__ - checks that all your source code files have up-to-date license headers, without actually touching any files. When any file with outdated license header is detected, Headroom execution will result in return code `1`, so you can easily use this mode in your CI pipelines, etc. 
1. __drop mode__ - will drop any existing license headers, without replacement.

Changed source files are written through a temporary file in the same directory and replaced atomically. Headroom verifies the original content immediately before replacement; if another process changed the file while it was being processed, the command aborts without overwriting that newer content. Check mode and `--dry-run` never write source files.

#### Command Line Interface
Below is the overview of command line interface of `run` command:

```
$ headroom run --help
Usage: headroom run [-s|--source-path PATH] [PATHS...] 
                    [-e|--excluded-path REGEX] [--exclude-ignored-paths] 
                    [--builtin-templates licenseType] [-t|--template-path PATH] 
                    [-v|--variable KEY=VALUE] 
                    [(-a|--add-headers) | (-c|--check-headers) | 
                      (-r|--replace-headers) | (-d|--drop-headers)] [--debug] 
                    [--dry-run]
  add, replace, drop or check source code headers

Available options:
  -s,--source-path PATH    path to source code file/directory
  PATHS...                 path to source code file/directory
  -e,--excluded-path REGEX path to exclude from source code file paths
  --exclude-ignored-paths  exclude paths ignored by used VCS
  --builtin-templates licenseType
                           use built-in templates of selected license type
  -t,--template-path PATH  path to template, can be either local file or
                           directory or URL
  -v,--variable KEY=VALUE  value for template variable
  -a,--add-headers         only adds missing license headers
  -c,--check-headers       check whether existing headers are up-to-date
  -r,--replace-headers     force replace existing license headers
  -d,--drop-headers        drop existing license headers only
  --debug                  produce more verbose output
  --dry-run                execute dry run (no changes to files)
  -h,--help                Show this help text
```

This command requires you to specify three key parameters, either using command line option, or in `.headroom.yaml` file:

1. __paths to source code files__ - this needs to be specified either by using the `-s|--source-path PATH` option (can be repeated), by passing the paths as positional arguments (e.g. `headroom run src/Foo.hs src/Bar.hs`, which plays nicely with shell expansion), or by using the `source-paths` option in `.headroom.yaml`. Paths from options and positional arguments are combined together.
1. __paths to templates__ - you can freely combine all three supported sources of templates: First are built-in templates for one of supported _OSS_ licenses, which can be enabled by `--builtin-templates TYPE`. Second option is to use either templates stored on local file system or URI-based template. For local stored template, if path to directory is provided, _Headroom_ will recursively search for all suitable templates in all subdirectories.
1. __paths to template files__ - you have two options here. Either specify path(s) to template files using `-t|--template-path PATH` (can be repeated) or `template-paths` option in `.headroom.yaml`, __or__ use the built-in templates using the `--builtin-templates TYPE`, where `TYPE` is one of the supported _OSS_ license types.
1. __variables__ - if your templates use any variables, you need to specify their values using the `-v|--variable "KEY=VALUE"` option (can be repeated) or using the `variable` option in `.headroom.yaml`.
1. __run mode__ _(optional)_ - if you don't specify this, the _add mode_ will be used as default, but you can set the run mode using one of the `-a,--add-headers`, `-c,--check-headers`, `-r,--replace-headers` or `-d,--drop-headers` options, or using the `run-mode` option in `.headroom.yaml`.

For regular work, it's more comfortable to define as much options in `.headroom.yaml` and then you can just run _Headroom_ like `headroom run` or just override the _run mode_ like `headroom run -r`, but it's also completely fine to run _Headroom_ without the need to prepare any files (templates, `.headroom.yaml`) and define all the required stuff using command line options, as shown below:

#### Using Headroom with treefmt
Because the `run` command accepts source paths as positional arguments, _Headroom_ satisfies the [treefmt formatter specification][web:treefmt-spec] and can be used as a formatter without any wrapper script:

```toml
[formatter.headroom]
command = "headroom"
options = ["run"]
includes = ["*.hs"]
```

Note that _Headroom_ still applies the `excluded-paths` rules from `.headroom.yaml` to the files passed on the command line.

### Gen Command
This command allows to generate stubs for files required by _Headroom_, such as `.headroom.yaml` and template files. You'll likely need this command only once for manual initialization of _Headroom_ for your project. For more info how to use it, see the [Project Setup Guide][rel:project-setup-guide] chapter.

#### Command Line Interface
Below is the overview of command line interface of `gen` command:

```
$ headroom gen --help
Usage: headroom gen [-c|--config-file] [-l|--license licenseType:fileType]
  generate stub configuration and template files

Available options:
  -c,--config-file         generate stub YAML config file to stdout
  -l,--license licenseType:fileType
                           generate template for license and file type
  -h,--help                Show this help text
```

Main things you can do with this command is to generate:

1. __configuration file__ - using the `headroom gen -c >.headroom.yaml`, you'll generate stub for the main configuration file
1. __template files__ - using the `headroom gen -l haskell:bsd3 >templates/haskell.mustache`, you'll generate stub license template file for _Haskell_ file type and _BSD-3-Clause_ license. See all supported file types and license types in [Configuration][rel:configuration] chapter.

### Init Command
This command is used to initialize _Headroom_ for your project by generating the `.headroom.yaml` configuration file and template files for selected _OSS_ license.

#### Command Line Interface
Below is the overview of command line interface of `init` command:

```
$ headroom init --help
Usage: headroom init (-l|--license-type TYPE) (-s|--source-path PATH)
  initialize current project for Headroom

Available options:
  -l,--license-type TYPE   type of open source license, available options:
                           apache2, bsd3, gpl2, gpl3, mit, mpl2
  -s,--source-path PATH    path to source code file/directory
  -h,--help                Show this help text
```

With this command, you need to define two required options:

1. __license type__ - type of the _OSS_ license your project uses. See [Configuration][rel:configuration] chapter for list of all supported licenses.
1. __path to source code files__ - using the `-s|--source-path PATH` (can be repeated), you need to define path(s) to source code files which license headers will be managed by _Headroom_.


[rel:configuration]: configuration.md
[rel:project-setup-guide]: project-setup-guide.md
[web:treefmt-spec]: https://treefmt.com/latest/reference/formatter-spec/
