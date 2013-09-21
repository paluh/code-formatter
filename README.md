Python AST/code formatter [![Build Status](https://travis-ci.org/paluh/code-formatter.png?branch=master)](https://travis-ci.org/paluh/code-formatter) [![Coverage Status](https://coveralls.io/repos/paluh/code-formatter/badge.png?branch=master)](https://coveralls.io/r/paluh/code-formatter?branch=master)
==========================

Currently I'm only experimenting with this code/idea and using it as a base for my Vim plugin. Be prepared for API changes. If you want something really stable check: codegen, PythonTidy or autopep8.

I'm trying to cover Python 2.7 at first, but some language constructs are still missing. If you want to check what has been already implemented I'm documenting each test with section number from "The Python Language Reference" (version 2.7): http://docs.python.org/2/reference/index.html.


# Puprose

This project contains basic code formatter which operates on Python AST tree (for more info check `ast` package). It can be used as AST pretty printer or simple code formatter, __but__ you have to know that there are serious limitations of this approach and it shouldn't be considered as fully "automatic" tool for project code validation/correction:

* it can be used only for complete and correct Python statements

* comments are skiped in AST (not docstrings but literal comments), so they are missing in formatted results

* as far as I know AST can be generated only for "current" version of Python (for example if you are running Python 3.3 you can't process/format statements from Python 2.7 which contains incompatible constructs)

* it can be quite inefficient for long chunks of code as it really tries to find appropriate formatting for every statement

One of main principles of this project is to make this library easily extensible (everybody should be able to customize single formatter), so if you see any obstacles in current design just fill an github issue.


# Usage

## Simple API example

    >>> import code_formatter
    >>> print code_formatter.format_code('foo(f=8, s=bar(x=9, y=10, z=20))', width=10)
    Traceback (most recent call last):
    ...
    code_formatter.NotEnoughSpace
    >>> print code_formatter.format_code('foo(f=8, s=bar(x=9, y=10, z=20))', width=10, force=True)
    foo(f=8,
        s=bar(x=9,
              y=10,
              z=20))

## Customizing formatters

You can easily customize single or bunch of formatters - subclass given formatter and override it's `_format_code` method. Lets use some extra formatter (`code_formatter.extra.UnbreakableTupleFormatter`) as an example:

    >>> from code_formatter.base import formatters, format_code
    >>> from code_formatter.extra import UnbreakableTupleFormatter
    >>> my_formatters = dict(formatters,
                             **{UnbreakableTupleFormatter.ast_type: UnbreakableTupleFormatter})
    >>> format_code(code, formatters_register=my_formatters)

For more examples check `code_formatter.extra` package (especially `tests` module there).

## Extra formatters
By default this package provides basic formatters (`code_formatter.base`) which I'm trying to keep as simple/straightforward as possible. I'm also want to provide one and exactly one formatter for givent `ast` node type. All additional formatters (usually more funny :-P) goes into `code_formatter.extra` package, so don't hesitate and check them.

# Hacking

## Contributing

I'm TDD fanatic so if you are going to provide some custom formatters please provide appropriate tests for them.

## Testing

To run project test suite (`code_formatter.tests.test_suite`) just type:

    $ python setup.py test

## Bug reporting

If you found a bug please fill a ticket on project page on github.

# Examples

## Very ugly Vim plugin

In `examples` directory you can find simple (and ugly) plugin which allows you to format single python statement in Vim. If you want to check this script drop it into your vim plugins directory for python ($VIMHOME/ftplugins/python/code\_formatter.vim)

To use it place your cursor on a first line of a statement which you want to format and call `:python format_code(80)` (or make some convenient mapping for this action - you can check my proposition below).

My mappings for this plugin allow to change width dynamically (you can easily try different formattings):

    map <leader>fds :python format_code(60)<CR>
    map <leader>fd :python format_code(70)<CR>
    map <leader>f :python format_code(80)<CR>
    map <leader>F :python format_code(90)<CR>
    map <leader>FD :python format_code(100)<CR>
    map <leader>FDS :python format_code(110)<CR>
