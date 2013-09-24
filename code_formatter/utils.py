import ast
import difflib
import unittest

from .exceptions import NotEnoughSpace


def _format_code(code, width, formatters_register, force=False):
    """Returns CodeBlock instance as a result"""
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

def format_code(code, width, formatters_register, force=False):
    """Returns string as a result"""
    result = _format_code(code, width=width, formatters_register=formatters_register, force=force)
    return u'\n'.join(unicode(e) for e in result)


class FormattersTestCase(unittest.TestCase):

    formatters_register = None

    def assertFormats(self, code, expected, formatters_register=None, width=None, force=False):
        self.formatters_register = formatters_register or self.formatters_register
        assert self.formatters_register is not None
        width = width if width is not None else max(len(l) for l in expected.split('\n'))
        formated = format_code(code, width=width, force=force,
                               formatters_register=self.formatters_register)
        try:
            self.assertEqual(formated, expected)
        except AssertionError:
            print '\n'.join(difflib.unified_diff(expected.split('\n'), formated.split('\n'), fromfile='expected', tofile='formated'))
            raise


class FormattersRegister(dict):

    def copy(self):
        return type(self)(self)

    def register_formatter(self, Formatter):
        try:
            Formatter.register(self)
        except NotImplementedError:
            try:
                self[Formatter.ast_type] = Formatter
            except AttributeError:
                raise ValueError('Provided Formatter: %s should implement '
                                 '`register` method or contain `ast_type` '
                                 'attribute' % Formatter)

    def register(self, *Formatters):
        for Formatter in Formatters:
            self.register_formatter(Formatter)
        return self
