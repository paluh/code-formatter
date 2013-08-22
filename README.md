# Python AST code formatter

Currently I'm only experimenting/testing this code/idea and using it as a base for my Vim plugin. Be prepared for API changes. If you want something really stable check: codegen, PythonTidy or autopep8.

I'm trying to cover Python 2.7 at first, but some language constructs are still missing. If you want to check what has been already implemented I'm documenting each test with section number from "The Python Language Reference" (version 2.7): http://docs.python.org/2/reference/index.html.


## Puprose

This project contains basic code formatter which operates on Python AST tree (for more info check `ast` package). It can be used as AST pretty printer or simple code formatter, __but__ you have to know that there as serious limitiations and it shouldn't be considered as fully "automatic" tool for project code validation/correction:

* it can be used only for complete and correct Python statements

* comments are skiped in ast (not docstrings but literal comments)

* as far as I know AST can be generated only for "current" version of Python (for example if you are running Python 3.3 you can't process/format statements from Python 2.7 which contains incompatibile constructs)


One of main principles of this project is to make this library easily extensible (everybody should be able to customize single formatter), so if you see any obstacles in current design just fill an github issue.


## Usage

### Python API


### Example of very ugly Vim plugin


## Hacking

### Contributing

I'm TDD fanatic so if you are going to provide some custom formatters please provide apropriate tests for them.

### Bug reporting

If you found a bug please fill a ticket on project page on github.
