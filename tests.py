import ast
import unittest

from . import CodeBlock, CodeLine, Expression


class AtomExpressionsFormattingTestCase(unittest.TestCase):

    def _indent(self, code, level):
        return '\n'.join((CodeLine.INDENT*level + l) for l in code.split('\n'))

    def test_line_formatting(self):
        line = CodeLine([CodeLine.INDENT, CodeLine.INDENT, 'x = ', 'fun(', 'z=8', ','])
        self.assertEqual(unicode(line), self._indent('x = fun(z=8,', 2))

    def test_block_formatting(self):
        lines = [CodeLine([CodeLine.INDENT, CodeLine.INDENT, 'x = ', 'fun(', 'z=8', ',']),
                 CodeLine([CodeLine.INDENT, CodeLine.INDENT, '        ', 'y=9)'])]
        block = CodeBlock(lines)
        self.assertEqual(unicode(block), self._indent('x = fun(z=8,\n'
                                                      '        y=9)', 2))

    def test_block_merge(self):
        block = CodeBlock([CodeLine([CodeLine.INDENT, CodeLine.INDENT, 'x = ', 'fun(', 'z=8', ',']),
                           CodeLine([CodeLine.INDENT, CodeLine.INDENT, '        ', 'y='])])

        subblock = CodeBlock([CodeLine(['fun(v=8,']),
                              CodeLine(['    u=9)'])])
        block.merge(subblock)
        self.assertEqual(unicode(block), self._indent('x = fun(z=8,\n'
                                                      '        y=fun(v=8,\n'
                                                      '              u=9)', 2))

    def test_atoms_formating(self):
        for code in ['8', '9.8', "'a'", 'instance.attribute']:
            expr = Expression.from_expr(ast.parse(code).body[0].value)
            self.assertEqual(code, unicode(expr.format_code(80)))


class CallFormattingTestCase(unittest.TestCase):

    def test_args_alignment(self):
        code = 'function_with_args(argument_1,     argument_2,argument_3)'
        expr = ast.parse(code).body[0].value
        call = Expression.from_expr(expr)
        formatted = unicode(call.format_code(80))
        self.assertEqual(formatted, 'function_with_args(argument_1, argument_2, '
                                                       'argument_3)')

    def test_args_wrapping(self):
        code = 'function_with_args(argument_1,     argument_2,argument_3)'
        expr = ast.parse(code).body[0].value
        call = Expression.from_expr(expr)
        formatted = unicode(call.format_code(30))
        self.assertEqual(formatted, 'function_with_args(argument_1,\n'
                                    '                   argument_2,\n'
                                    '                   argument_3)')

    def test_wrapping_nested_functions_with_args(self):
        code = ('function_with_args(nested_function_with_args(argument_1,argument_2,argument_3),'
                                   'nested_function_with_args(argument_4,argument_5,argument_6))')
        expr = ast.parse(code).body[0].value
        call = Expression.from_expr(expr)
        formatted = unicode(call.format_code(30, force=True))
        expected = ('function_with_args(nested_function_with_args(argument_1,\n'
                    '                                             argument_2,\n'
                    '                                             argument_3),\n'
                    '                   nested_function_with_args(argument_4,\n'
                    '                                             argument_5,\n'
                    '                                             argument_6))')
        self.assertEqual(formatted, expected)

    def test_wrapping_mixed_args(self):
        code = ('function_with_args(param_1, param_2, nested(argument_1,argument_2,argument_3),'
                                   'param_3, nested(argument_4,argument_5,argument_6), param_4)')
        expr = ast.parse(code).body[0].value
        call = Expression.from_expr(expr)
        expected = ('function_with_args(param_1, param_2, nested(argument_1,\n'
                    '                                            argument_2,\n'
                    '                                            argument_3),\n'
                    '                   param_3, nested(argument_4, argument_5,\n'
                    '                                   argument_6), param_4)')
        width = max(len(l) for l in expected.split('\n'))
        formatted = unicode(call.format_code(width, force=True))
        self.assertEqual(formatted, expected)

    def test_kwargs_alignment(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        expr = ast.parse(code).body[0].value
        call = Expression.from_expr(expr)
        formatted = unicode(call.format_code(80))
        self.assertEqual(formatted, 'function_with_kwargs(argument_1=value, argument_2=value, '
                                                         'argument_3=value)')
    def test_kwargs_wrapping(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        expr = ast.parse(code).body[0].value
        call = Expression.from_expr(expr)
        formatted = unicode(call.format_code(20, force=True))
        self.assertEqual(formatted, 'function_with_kwargs(argument_1=value,\n'
                                    '                     argument_2=value,\n'
                                    '                     argument_3=value)')

    def test_dict_aligment(self):
        code = "{'key_1': 1,     'key_2'  : 2   ,key_var: 3}"
        expr = ast.parse(code).body[0].value
        d = Expression.from_expr(expr)
        formatted = unicode(d.format_code(80, force=True))
        self.assertEqual(formatted, "{'key_1': 1, 'key_2': 2, key_var: 3}")

    def test_dict_kwargs(self):
        code = "{'k1': 1,  'k2'  : 2  , v: 3}"
        expr = ast.parse(code).body[0].value
        d = Expression.from_expr(expr)
        expected = ("{'k1': 1, 'k2': 2,\n"
                    " v: 3}")
        formatted = unicode(d.format_code(max(len(l) for l in expected.split('\n')),
                            force=True))
        self.assertEqual(formatted, expected)

#"function_with_args(argument_1, argument_2, argument_3)"),
#if __name__ == "__main__":
#    snippets = [(
#"function_with_args(argument_1,argument_2,argument_3)",
#"function_with_args(argument_1, argument_2, argument_3)"),
#(
#"function_with_args(argument_1,argument_2,argument_3)",
#"function_with_args(argument_1, argument_2, argument_3)"),
#(
#"function_with_many_args(argument1,argument2,very_long_argument___3,short4,short5,short6)",
#"function_with_many_args(argument1, argument2, very_long_argument___3, short4,\n"
#"                        short5, short6)"),
#(
#"function_with_internal_calls(internal_call(argument_1,argument2,argu_3,argument_4),"
#                             "internal_call(argument_5))",
#"function_with_internal_calls(internal_call(argument_1, argument2, argu_3,\n"
#"                                           argument_4),\n"
#"                             internal_call(argument_5))"),
#(
#"function_with_internal_calls(internal_call(argument_1),func_call(argument_2,argument_3,argument_4),"
#                             "internal_call(argument_5))",
#"function_with_internal_calls(internal_call(argument_1), func_call(argument_2,\n"
#"                                                                  argument_3,\n"
#"                                                                  argument_4),\n"
#"                             internal_call(argument_5))"),
#(
#"function_with_internal_calls(internal_call(argument_1,"
#                                           "internal_internal_call(argument_2,argument_3,argument_4)),"
#                             "internal_call(argument_5,argument_6, argument_7,"
#                                           "aarrgguummeenntt__8,aarrgguummeenntt__9),"
#                                           "internal_call(aarrgguummeenntt__10, aarrgguummeenntt__11))",
#"function_with_internal_calls(internal_call(argument_1,\n"
#"                                           internal_internal_call(argument_2,\n"
#"                                                                  argument_3,\n"
#"                                                                  argument_4)),\n"
#"                             internal_call(argument_5, argument_6, argument_7,\n"
#"                                           aarrgguummeenntt__8,\n"
#"                                           aarrgguummeenntt__9),\n"
#"                             internal_call(aarrgguummeenntt__10,\n"
#"                                           aarrgguummeenntt__11))"),
#(
#"function_with_internal_calls(internal_call(argument_1,"
#                                           "ii_call(argument_2,argument_3,argument_4)),"
#                             "internal_call(argument_5,argument_6, argument_7,"
#                                           "aarrgguummeenntt__8,aarrgguummeenntt__9),"
#                                           "internal_call(aarrgguummeenntt__10, aarrgguummeenntt__11))",
#"function_with_internal_calls(internal_call(argument_1, ii_call(argument_2,\n"
#"                                                               argument_3,\n"
#"                                                               argument_4)),\n"
#"                             internal_call(argument_5, argument_6, argument_7,\n"
#"                                           aarrgguummeenntt__8,\n"
#"                                           aarrgguummeenntt__9),\n"
#"                             internal_call(aarrgguummeenntt__10,\n"
#"                                           aarrgguummeenntt__11))"),
#(
#"function_with_internal_calls(internal_call(argument_1,argument_2,argument_3,argument_4),"
#                             "internal_call(argument_5,argument_6,argument_7,aarrgguummeenntt__8,aarrgguummeenntt__9),"
#                                           "internal_call(aarrgguummeenntt__10,"
#                                                         "aarrgguummeenntt__11))",
#"function_with_internal_calls(internal_call(argument_1, argument_2, argument_3,\n"
#"                                           argument_4),\n"
#"                             internal_call(argument_5, argument_6, argument_7,\n"
#"                                           aarrgguummeenntt__8,\n"
#"                                           aarrgguummeenntt__9),\n"
#"                             internal_call(aarrgguummeenntt__10,\n"
#"                                           aarrgguummeenntt__11))"),
#(
#"function_with_kwargs( name =   variable)\n",
#"function_with_kwargs(name=variable)"),
#(
#"function_with_kwargs( name =   instance.attribute)\n",
#"function_with_kwargs(name=instance.attribute)"),
#(
#"function_with_kwargs( name\n =\n   variable, i=1, s   = 'test')\n",
#"function_with_kwargs(name=variable, i=1, s='test')"),
#(
#"function_with_kwargs(name=variable, i=1111100000123123123123112312312312312312323, s='test')\n",
#"function_with_kwargs(name=variable,\n"
#"                     i=1111100000123123123123112312312312312312323, s='test')"),
#(
#"function_with_internal_calls(a=internal_call(argument_1,argument_2,argument_3,argument_4),"
#                             "b=internal_call(argument_5,argument_6,argument_7,aarrgguummeenntt__8,aarrgguummeenntt__9),"
#                                             "c=internal_call(aarrgguummeenntt__10,"
#                                                             "aarrgguummeenntt__11))",
#"function_with_internal_calls(a=internal_call(argument_1, argument_2, argument_3,\n"
#"                                             argument_4),\n"
#"                             b=internal_call(argument_5, argument_6, argument_7,\n"
#"                                             aarrgguummeenntt__8,\n"
#"                                             aarrgguummeenntt__9),\n"
#"                             c=internal_call(aarrgguummeenntt__10,\n"
#"                                             aarrgguummeenntt__11))"),
#(
#"function_with_internal_calls(a=internal_call(argument_1,argument_2,aaaaaaaaaaaaaaaaaaaaaaaaaa=argument_3,bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=argument_4),"
#                             "b=internal_call(argument_5,argument_6,argument_7,aarrgguummeenntt__8,aarrgguummeenntt__9),"
#                                             "c=internal_call(aarrgguummeenntt__10,"
#                                                             "aarrgguummeenntt__11))",
#"function_with_internal_calls(a=internal_call(argument_1, argument_2,\n"
#"                                             aaaaaaaaaaaaaaaaaaaaaaaaaa=argument_3,\n"
#"                                             bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=argument_4),\n"
#"                             b=internal_call(argument_5, argument_6, argument_7,\n"
#"                                             aarrgguummeenntt__8,\n"
#"                                             aarrgguummeenntt__9),\n"
#"                             c=internal_call(aarrgguummeenntt__10,\n"
#"                                             aarrgguummeenntt__11))"),
#(
#"a =    b = c = function_call(test=8)",
#"a = b = c = function_call(test=8)"),
#(
#"function_call(test=d[\"test\"])",
#"function_call(test=['test'])"),
#]
#    for code, expected in snippets:
#        lines  = format_code(code)
#        result = '\n'.join([unicode(l) for l in lines])
#        #result = '\n'.join(unicode(l) for l in chain(*lines))
#        if result != expected:
#            print("\n\n\n!!!!FAILED!!!\n\nexpected:\n%s\ngenerated:\n%s\n" % (expected, result))
#
#
##import ipdb; ipdb.set_trace()
