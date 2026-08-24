_Headroom_ needs to know where to search for configuration, template and source code files. If you plan to use _Headroom_ regularly for your project, you might want to keep template files and configuration together with your project, but there's also option to run _Headroom_ completely without any need to generate any files.

## Running Without Initialization
If you only plan to use _Headroom_ occasionally or just don't want have any additional files in your project, you can completely skip initialization and specify all required things using command line options, like this:

```
headroom run --builtin-templates bsd3 -s src -v "author=John Smith" -v "email=john.smith@tardis.co" -v "project=Project Name" -v "year=2020"
```

See the [Running Headroom][rel:running-headroom] chapter for more details.

## Automatic Initialization (for OSS projects)
Easiest and fastest way how to initialize Headroom for your project is to use the `init` command, which generates all the boilerplate for you. The only drawback is that you can use it only in case that you use any supported open source license for which Headroom contains license header templates:

```shell
cd project/
headroom init -l bsd3 -s src/
```

This command will automatically scan source code directories for supported file types and will generate:
1. `.headroom.yaml` configuration file with correctly set path to template files, source codes and will contain dummy values for variables.
1. `headroom-templates/` directory which contains template files for all known file types you use in your project and for open source license you choose.

Initialization never overwrites existing configuration or template files. Before writing anything, Headroom checks every target file and aborts if any of them already exists. Files are staged first, so a later write failure is rolled back instead of leaving a partially initialized project.

Now the project structure will be following:

```
project/
  ├── src/
  │   ├── scala/
  │   │   ├── Foo.scala
  │   │   └── Bar.scala
  │   └── html/
  │       └── template1.html
  ├── headroom-templates/
  │   ├── html.mustache
  │   └── scala.mustache
  └── .headroom.yaml
```

## Manual Initialization
If you for some reason don't want to use the automatic initialization using the steps above, you can either create all the required files (_YAML_ configuration and template files) by hand, or you can use the `gen` command to do that in semi-automatic way:

```shell
cd project/
headroom gen -c >./.headroom.yaml

mkdir headroom-templates/
cd headroom-templates/

headroom gen -l bsd3:css >./css.mustache
headroom gen -l bsd3:html >./html.mustache
headroom gen -l bsd3:scala >./scala.mustache
```

After these steps, make sure you set correctly the paths to template files and source code files in the _YAML_ configuration file.


[rel:running-headroom]: running-headroom.md
