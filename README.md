# Python AST code formatter

Currently I'm only experimenting/testing with this idea and using this package as base for my vim plugin... If you want something really stable check codegen, PythonTidy or autopep8.


## Puprose

This project contains basic code formatters which operates on python AST tree (for more info check `ast` package). It can be used as AST pretty printer or simple code formatter, __but__ you have to know that there as serious limitiations and it shouldn't be considered as fully "automatic" tool for project code validation/correction:

* it can be used only for complete and correct python statements (

* comments are skiped in ast (not docstrings but literal comments) so if you parse and format big 

* as far as I know ast can be generated only for "current" version of Python (for example if you are running python 3.3 you can't process/format statements from python 2.7 which contains incompatibile constructs like "print")

* I'm not sure if all default formatters


## Usage

### Basics


### Configuration


### Custom formatters


### Simple formatters

### Compound fomratters


## Contributing

### Hacking
I'm TDD fanatic so if you are going to provide some custom formatters please provide apropriate tests for them.

### Bug reporting

If you found a bug please fill a ticket on project page on github.

