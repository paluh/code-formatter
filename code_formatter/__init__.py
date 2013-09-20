import ast

from . import base
from .exceptions import NotEnoughSpace


def _format_code(code, width, formatters_register, force=False):
    """Returns CodeBlock instance as result"""
    tree = ast.parse(code)
    result = []
    for e in tree.body:
        formatter = formatters_register[type(e)](expr=e,
                                                 formatters_register=formatters_register,
                                                 parent=None)
        if force:
            statement_width = width
            failing_statement_width = None
            succeeding_statement_width = None
            while True:
                try:
                    s = formatter.format_code(statement_width)
                except NotEnoughSpace:
                    failing_statement_width = statement_width
                else:
                    succeeding_statement_width = statement_width
                    if (failing_statement_width is None or
                        statement_width - 1 == failing_statement_width):
                        result.append(s)
                        break
                if (succeeding_statement_width and failing_statement_width and
                    succeeding_statement_width - failing_statement_width == 1):
                    s = formatter.format_code(succeeding_statement_width)
                    result.append(s)
                    break
                statement_width = ((failing_statement_width +
                                    (succeeding_statement_width or
                                     3 * failing_statement_width)) / 2)
        else:
            result.append(formatter.format_code(width))
    return result

def format_code(code, width=80, formatters_register=base.formatters.copy(), force=False):
    """Returns string as a result"""
    result = _format_code(code, width=width, formatters_register=formatters_register, force=force)
    return u'\n'.join(unicode(e) for e in result)
