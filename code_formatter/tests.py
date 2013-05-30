import ast
import unittest

from . import CodeBlock, CodeLine, ExpressionFormatter, format_code


class AtomExpressionFormattersFormattingTestCase(unittest.TestCase):

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
        for code in ['8', '9.8', "'a'"]:
            expr = ExpressionFormatter.from_ast(ast.parse(code).body[0].value)
            self.assertEqual(code, unicode(expr.format_code(80)))


class CallFormattingTestCase(unittest.TestCase):

    def test_simple_attribute_formatting(self):
        for code in ['instance.attribute', 'instance.attribute.inner']:
            expr = ExpressionFormatter.from_ast(ast.parse(code).body[0].value)
            self.assertEqual(code, unicode(expr.format_code(80)))

    def test_method_call_formatting(self):
        code = 'instance.method(   x,   y )'
        formatted = format_code(code)
        self.assertEqual(formatted, 'instance.method(x, y)')

    def test_args_alignment(self):
        code = 'function_with_args(argument_1,     argument_2,argument_3)'
        expr = ast.parse(code).body[0].value
        call = ExpressionFormatter.from_ast(expr)
        formatted = unicode(call.format_code(80))
        self.assertEqual(formatted, 'function_with_args(argument_1, argument_2, '
                                                       'argument_3)')

    def test_args_wrapping(self):
        code = 'function_with_args(argument_1,     argument_2,argument_3)'
        expr = ast.parse(code).body[0].value
        call = ExpressionFormatter.from_ast(expr)
        formatted = unicode(call.format_code(30))
        self.assertEqual(formatted, 'function_with_args(argument_1,\n'
                                    '                   argument_2,\n'
                                    '                   argument_3)')

    def test_wrapping_nested_functions_with_args(self):
        code = ('function_with_args(nested_function_with_args(argument_1,argument_2,argument_3),'
                                   'nested_function_with_args(argument_4,argument_5,argument_6))')
        expr = ast.parse(code).body[0].value
        call = ExpressionFormatter.from_ast(expr)
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
        call = ExpressionFormatter.from_ast(expr)
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
        call = ExpressionFormatter.from_ast(expr)
        formatted = unicode(call.format_code(80))
        self.assertEqual(formatted, 'function_with_kwargs(argument_1=value, argument_2=value, '
                                                         'argument_3=value)')
    def test_kwargs_wrapping(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        expr = ast.parse(code).body[0].value
        call = ExpressionFormatter.from_ast(expr)
        formatted = unicode(call.format_code(20, force=True))
        self.assertEqual(formatted, 'function_with_kwargs(argument_1=value,\n'
                                    '                     argument_2=value,\n'
                                    '                     argument_3=value)')

    def test_dict_aligment(self):
        code = "{'key_1': 1,     'key_2'  : 2   ,key_var: 3}"
        expr = ast.parse(code).body[0].value
        d = ExpressionFormatter.from_ast(expr)
        formatted = unicode(d.format_code(80, force=True))
        self.assertEqual(formatted, "{'key_1': 1, 'key_2': 2, key_var: 3}")

    def test_dict_kwargs(self):
        code = "{'k1': 1,  'k2'  : 2  , v: 3}"
        expr = ast.parse(code).body[0].value
        d = ExpressionFormatter.from_ast(expr)
        expected = ("{'k1': 1, 'k2': 2,\n"
                    " v: 3}")
        formatted = unicode(d.format_code(max(len(l) for l in expected.split('\n')),
                            force=True))
        self.assertEqual(formatted, expected)

    def test_assignment_alignment(self):
        code = 'x=y=z=8'
        expr = ast.parse(code).body[0]
        formatted = ExpressionFormatter.from_ast(expr).format_code(80)
        self.assertEqual(unicode(formatted), 'x = y = z = 8')

    def test_assignment_with_subexpressions(self):
        code = ('x=y=z=function_with_kwargs(argument_1=value, argument_2=value,'
                                           'argument_3=value)')
        expected =  ('x = y = z = function_with_kwargs(argument_1=value,\n'
                     '                                 argument_2=value,\n'
                     '                                 argument_3=value)')
        expr = ast.parse(code).body[0]
        e = ExpressionFormatter.from_ast(expr)
        formatted = unicode(e.format_code(max(len(l) for l in expected.split('\n')),
                            force=True))
        self.assertEqual(unicode(formatted), expected)

    def test_subscription(self):
        code = 'x=d [ "a" ] '
        self.assertEqual(format_code(code), "x = d['a']")

    def test_subscription_with_nested_function_call(self):
        code = ('dictionary[function_with_kwargs(argument_1=value, argument_2=value,'
                                           'argument_3=value)]')
        expected = ('dictionary[function_with_kwargs(argument_1=value,\n'
                    '                                argument_2=value,\n'
                    '                                argument_3=value)]')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_simple_compare(self):
        pass

    def test_generator_alignment(self):
        code = '(function(x) for x in iterable if x > 10)'
        expected = ('(function(x)\n'
                    ' for x in iterable\n'
                    ' if x > 10)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class CompareTestCase(unittest.TestCase):
    """
    comparison    ::=  or_expr ( comp_operator or_expr )*
    comp_operator ::=  "<" | ">" | "==" | ">=" | "<=" | "<>" | "!="
                       | "is" ["not"] | ["not"] "in"
    """

    def test_simple_comparison(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = '(x %s\ny)' % opt
            expected = 'x %s y' % opt
            self.assertEqual(format_code(code, 2), expected)

    def test_comparison_wrapping(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = 'fun(x, y, z) %s fun(m, n, o)' % opt
            expected = ('fun(x,\n'
                        '    y,\n'
                        '    z) %s fun(m,\n'
                        '       %s     n,\n'
                        '       %s     o)' % (opt, len(opt)*' ', len(opt)*' '))
            self.assertEqual(format_code(code, 2), expected)

    def test_multi_opt_comparison_wrapping(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = 'fun(x, y, z) %s fun(m, n, o) %s fun(p, q, r)' % (opt, opt)
            expected = ('fun(x, y, z) %(opt)s fun(m, n, o) %(opt)s fun(p,\n'
                        '             %(spc)s              %(spc)s     q,\n'
                        '             %(spc)s              %(spc)s     r)' % {'opt': opt,
                                                                              'spc': len(opt)*' '})
            width = max(len(l) for l in expected.split('\n'))
            self.assertEqual(format_code(code, width), expected)


class BooleanOperationTestCase(unittest.TestCase):
    """
    or_test  ::=  and_test | or_test "or" and_test
    and_test ::=  not_test | and_test "and" not_test
    not_test ::=  comparison | "not" not_test
    """
    def test_simple_alignemnt(self):
        code = 'x   or y'
        self.assertEqual(format_code(code), 'x or y')

    def test_wrapping(self):
        code = 'x or y or z or v'
        expected = ('(x or\n'
                    ' y or\n'
                    ' z or\n'
                    ' v)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_brackets_are_used_only_when_neccessary(self):
        code = 'x or fun(y,z,v)'
        expected = ('x or fun(y, z,\n'
                    '         v)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class TupleTestCase(unittest.TestCase):
    """
    tuple ::=  expression ( "," expression )* ","
    """

    def test_single_element_tuple_formatting(self):
        code = 'x,'
        self.assertEqual(format_code(code), '(x,)')

    def test_tuple_inside_call_preservse_brackets(self):
        code = 'fun((x,),(y,z))'
        self.assertEqual(format_code(code), 'fun((x,), (y, z))')

    def test_tuple_inside_tuple_preservse_brackets(self):
        code = '((x,),(y,z))'
        self.assertEqual(format_code(code), '(x,), (y, z)')


class ForTestCase(unittest.TestCase):
    """
    for_stmt ::=  "for" target_list "in" expression_list ":" suite
                  ["else" ":" suite]
    """

    def test_simple_for_statement(self):
        code = ('for   p   in    (1,2,3):\n   p')
        expected = 'for p in 1, 2, 3:\n%sp' % CodeLine.INDENT
        self.assertEqual(format_code(code), expected)



class SimpleStatement(unittest.TestCase):
    """
     simple_stmt ::=  expression_stmt
                 | assert_stmt
                 | assignment_stmt
                 | augmented_assignment_stmt
                 | pass_stmt
                 | del_stmt
                 | print_stmt
                 | return_stmt
                 | yield_stmt
                 | raise_stmt
                 | break_stmt
                 | continue_stmt
                 | import_stmt
                 | global_stmt
                 | exec_stmt
    """
    pass


