python << endpython
# this is example vim plugin
import code_formatter
import textwrap
reload(code_formatter)

def _format_code(lines, max_width):
    width = max(len(l) for l in lines)
    code = textwrap.dedent('\n'.join(lines))
    indent = (width - max(len(l) for l in code.split('\n')))
    formated = code_formatter._format_code(code, formatters_register=code_formatter._formatters,
                                           width=max_width - indent, force=True)
    indent = indent*' '
    return [(indent + unicode(l)).encode('utf-8') for block in formated for l in block.lines]

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
