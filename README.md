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

### Simple API example

    >>> import code_formatter
    >>> print code_formatter.format_code('fun1(f=8, s=fun2(x=9, y=10, z=20))', width=10)
    fun1(f=8,
         s=fun2(x=9,
                y=10,
                z=20))
    >>> print code_formatter.format_code('fun1(f=8, s=fun2(x=9, y=10, z=20))', width=5)
    >>> print code_formatter.format_code('fun1(f=8, s=fun2(x=9, y=10, z=20))', width=10, force=False)
    Traceback (most recent call last):
    ...
    code_formatter.NotEnoughSpace


### Example of very ugly Vim plugin
This simple (and ugly) plugin allows you to format single python statement (not single line). Just place your cursor on first line of statements and call `:python format_code(80)` (or make some convinient mapping for this action - you can check my proposition below).

    python << endpython
    import code_formatter
    import textwrap
    reload(code_formatter)

    def _format_code(lines, max_width):
        width = max(len(l) for l in lines)
        code = textwrap.dedent('\n'.join(lines))
        indent = (width - max(len(l) for l in code.split('\n')))
        formated = code_formatter._format_code(code, formatters=code_formatter._formatters,
                                               width=max_width - indent)
        indent = indent*' '
        return [unicode(l.indent(indent)).encode('utf-8') for block in formated for l in block.lines]

    def format_code(max_width):
        x = vim.current.window.cursor[0]-1
        for y in range(x+1, len(vim.current.buffer)+1):
            lines = vim.current.buffer[x:y]
            try:
                width = max(len(l) for l in lines)
                code = textwrap.dedent('\n'.join(lines))
                indent = (width - max(len(l) for l in code.split('\n')))
                vim.current.buffer[x:y] = _format_code(lines + ['%s    pass' % (indent*' ')],
                                                       max_width)[:-1]
            except SyntaxError:
                try:
                    vim.current.buffer[x:y] = _format_code(lines, max_width)
                except SyntaxError:
                    continue
                else:
                    break
            else:
                break
    endpython


My mappings for this plugin allow to change width dynamically (you can easily try different formattings):

    map <leader>fds :python format_code(60)<CR>
    map <leader>fd :python format_code(70)<CR>
    map <leader>f :python format_code(80)<CR>
    map <leader>F :python format_code(90)<CR>
    map <leader>FD :python format_code(100)<CR>
    map <leader>FDS :python format_code(110)<CR>


## Hacking

### Contributing

I'm TDD fanatic so if you are going to provide some custom formatters please provide apropriate tests for them.

### Bug reporting

If you found a bug please fill a ticket on project page on github.
