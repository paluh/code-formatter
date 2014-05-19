Python AST/code formatter [![Build Status](https://travis-ci.org/paluh/code-formatter.png?branch=master)](https://travis-ci.org/paluh/code-formatter) [![Coverage Status](https://coveralls.io/repos/paluh/code-formatter/badge.png?branch=master)](https://coveralls.io/r/paluh/code-formatter?branch=master)
==========================

This project contains basic and advanced code formatters which operates on Python AST tree (for more info check `ast` package). It can be used as AST pretty printer or code formatter, __but__ you have to know that there are serious limitations of this approach and it shouldn't be considered as fully "automatic" tool for project code validation/correction:

* it can be used only for complete and correct Python statements

* comments are skiped in AST (not docstrings but literal comments), so they are missing in formatted results

* as far as I know AST can be generated only for "current" version of Python (for example if you are running Python 3.3 you can't process/format statements from Python 2.7 which contains incompatible constructs)

* it is currently only tested against Python 2.7

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

### Base formatters

Default formatters (`code_formatter.base`) follow simple logic:

* use as much space as possible - try to find formatting with maximal width, which is lower or equal to given value

* if you use `force` parameter in `format_code` helper it uses above strategy, but in case of failure (desired width is to small for given statement) it tries to find smallest possible width which allows formatting

Above algorithm generates really compact formatting and is quite easy to follow and test. And what is really important, it is quite simple and we all know that "simple is better than complex"... so I've decided to use it as a base. But as "readability counts" there is a lot of space for possible customization of formatting strategy. I've created `extras` package for such extentions (contributions welcome).

### Custom formatters

#### `extras` package

All formatters are kept in a small subclass of standard python `dict` which is passed around. It maps `ast_type` to `Formatter` and introduces trivial protocol of registration operations, so you can replace given formatter quite easily. Lets use some ready formatters from `extras` package:


    >>> from code_formatter improt base, format_code
    >>> from code_formatter.extras import UnbreakableTupleFormatter
    >>> my_formatters = base.formatters.copy().register(UnbreakableTupleFormatter)
    >>> print format_code('[(x,y), (z,v)]',
    ...                   formatters_register=my_formatters, width=1, force=True)
    [(x, y),
     (z, v)]

    >>> # with standard "greedy" formatters result is different ;-)
    ... print format_code('[(x,y), (z,v)]', width=1, force=True)
    [(x,
      y),
     (z,
      v)]

Or even more interesting one:

    >>> from code_formatter.extras import LinebreakingAttributeFormatter
    >>> my_formatters.register(LinebreakingAttributeFormatter)
    >>> code = 'session.query(User).filter(User.active==True, User.country=="PL").count()'
    >>> print format_code(code, formatters_register=my_formatters, width=1, force=True)
    (session.query(User)
            .filter(User.active == True,
                    User.country == 'PL')
            .count())
    >>> # and now standard "straightforward" ast based formatting
    ... print format_code(code, width=1, force=True)
    session.query(User).filter(User.active == True,
                               User.country == 'PL').count()


My preffered combination of formatters is:

    >>> import code_formatter.base, code_formatter.extras
    >>> formatters = code_formatter.base.formatters.copy()
    >>> formatters.register(code_formatter.extras.LinebreakingAttributeFormatter)
    >>> formatters.register_formatter(code_formatter.extras.ListOfExpressionsWithSingleLineContinuationsFormatter)
    >>> formatters.register_formatter(code_formatter.extras.UnbreakableTupleFormatter)

It is worth to point out that in above example ordering of registrations is important (and probably this `API` flaw will be fixed in the future). `ListOfExpressionsWithSingleLineContinuationsFormatter` replaces configuration of `ListOfExpressionFormatter` in all other formatters from `FormattersRegister` instance, so if we want to use `UnbreakableTupleFormatter` (which uses custom `ListOfExpressionFormatter`) we have to register it at the end.


#### Own formatters

If you want to change some formatter, then you have to subclass one and override it's `_format_code` method. Of course you can completly replace it if it's necessary - just use interface which is defined by `base.AstFormatter`.

    TODO: example


# Hacking

## TODO (in order)

* Change `Formatter.format_code` API from `format_code(width, contiuation, suffix=None)` to `format_code(width, continuation=False, prefix_width=0, suffix_width=0)`.

* Add K&R formatters set.

* Add `FormattersRegister` sum operation (`default_formatters` + `fallback_formatters`). Indroduce `FormattersRegister` subclass which can be a result of summing to formatters sets.

* Add full, step by step example to "Customizing formatters -> Own formatters" section

* Add online (maybe in browser: repl.it or skuplpt) demo

* Consider making classes from `code_formatter.code` (`CodeBlock`, `CodeLine`, etc.) immutable


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
