# plan-convert

Reads a .plan file from stdin, and transform it using Djula (Django) templates

## Changelog

v0.0.1 (2020-08-12)

- Initial release

```
> bin/plan-convert --help
Reads a .plan file from stdin, and pass its content to the specified Djula template

Available options:
  -h, --help                               print the help text and exit
  --template-path TEMPLATE-PATH (Required)
                                           use TEMPLATE-PATH as Djula template to convert the .plan file
  -v, --version                            print the version and exit
```
