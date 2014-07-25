python << endpython
# this is example vim plugin
import code_formatter.utils, code_formatter.base, code_formatter.extras
import textwrap
reload(code_formatter)

formatters = code_formatter.base.formatters.copy()
formatters.register(code_formatter.extras.LinebreakingAttributeFormatter)

unbreakable_formatters = formatters.copy()
unbreakable_formatters.register(code_formatter.extras.UnbreakableTupleFormatter)

def _format_code(lines, max_width, break_tuples):
    width = max(len(l) for l in lines)
    code = textwrap.dedent('\n'.join(lines))
    indent = (width - max(len(l) for l in code.split('\n')))
    formated = (code_formatter.utils
                              ._format_code(code,
                                            formatters_register=(formatters if break_tuples
                                                                            else unbreakable_formatters),
                                            width=max_width - indent, force=True))
    indent = indent*' '
    return [(indent + unicode(l)).encode('utf-8') for block in formated for l in block.lines]

def format_code(max_width, break_tuples=True):
    x = vim.current.window.cursor[0]-1
    for y in range(x+1, len(vim.current.buffer)+1):
        lines = vim.current.buffer[x:y]
        try:
            width = max(len(l) for l in lines)
            code = textwrap.dedent('\n'.join(lines))
            indent = (width - max(len(l) for l in code.split('\n')))
            vim.current.buffer[x:y] = _format_code(lines + ['%s    pass' % (indent*' ')],
                                                   max_width, break_tuples)[:-1]
        except SyntaxError:
            try:
                vim.current.buffer[x:y] = _format_code(lines, max_width, break_tuples)
            except SyntaxError:
                continue
            else:
                break
        else:
            break
endpython
