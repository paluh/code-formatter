import ast
import textwrap
import unittest

from . import CodeBlock, CodeLine, ExpressionFormatter, format_code, KandRAstFormatter


class AtomExpressionFormattersFormattingTestCase(unittest.TestCase):

    def _indent(self, code, level):
        return '\n'.join((CodeLine.INDENT*level + l) for l in code.split('\n'))

    def test_line_formatting(self):
        line = CodeLine([CodeLine.INDENT, CodeLine.INDENT,
                         'x = ', 'fun(', 'z=8', ','])
        self.assertEqual(unicode(line), self._indent('x = fun(z=8,', 2))

    def test_block_formatting(self):
        lines = [CodeLine([CodeLine.INDENT, CodeLine.INDENT, 'x = ',
                           'fun(', 'z=8', ',']),
                 CodeLine([CodeLine.INDENT, CodeLine.INDENT,
                           '        ', 'y=9)'])]
        block = CodeBlock(lines)
        self.assertEqual(unicode(block), self._indent('x = fun(z=8,\n'
                                                      '        y=9)', 2))

    def test_block_merge(self):
        block = CodeBlock([CodeLine([CodeLine.INDENT, CodeLine.INDENT,
                                     'x = ', 'fun(', 'z=8', ',']),
                           CodeLine([CodeLine.INDENT, CodeLine.INDENT,
                                     '        ', 'y='])])

        subblock = CodeBlock([CodeLine(['fun(v=8,']),
                              CodeLine(['    u=9)'])])
        block.merge(subblock, separator='')
        self.assertEqual(unicode(block), self._indent('x = fun(z=8,\n'
                                                      '        y=fun(v=8,\n'
                                                      '              u=9)', 2))

    def test_atoms_formating(self):
        for code in ['8', '9.8', "'a'"]:
            expr = ExpressionFormatter.from_ast(ast.parse(code).body[0].value)
            self.assertEqual(code, unicode(expr.format_code(80)))


class CallFormattingTestCase(unittest.TestCase):

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


class ListDisplaysTestCase(unittest.TestCase):
    # FIXME: test old_lambda_form branch
    """
    [5.2.4]
    list_display        ::=  "[" [expression_list | list_comprehension] "]"
    list_comprehension  ::=  expression list_for
    list_for            ::=  "for" target_list "in" old_expression_list [list_iter]
    old_expression_list ::=  old_expression [("," old_expression)+ [","]]
    old_expression      ::=  or_test | old_lambda_form
    list_iter           ::=  list_for | list_if
    list_if             ::=  "if" old_expression [list_iter]
    """
    def test_expression_list_alignment(self):
        code = '[   1 , 2,   3,]'
        expected = '[1, 2, 3]'
        self.assertEqual(format_code(code), expected)

    def test_expression_list_wrapping(self):
        code = '[   1 , 2,   3,]'
        expected = '[1, 2,\n 3]'
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_simple_list_comprehension_alignment(self):
        code = '[ x   for   x  in  iterable ]'
        expected = '[x for x in iterable]'
        self.assertEqual(format_code(code), expected)

    def test_nested_comprehensions_wrapping(self):
        code = '[function(x) for i in range(10) if (i+1)%2 for x in range(i) if x>5]'
        expected = ('[function(x)\n'
                    ' for i in range(10)\n'
                    ' if (i + 1) % 2\n'
                    ' for x in range(i)\n'
                    ' if x > 5]')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class GeneratorExpressionsTestCase(unittest.TestCase):
    """
    [5.2.5, 5.2.6]
    comp_for             ::=  "for" target_list "in" or_test [comp_iter]
    comp_iter            ::=  comp_for | comp_if
    comp_if              ::=  "if" expression_nocond [comp_iter]
    generator_expression ::=  "(" expression comp_for ")"
    """
    def test_alignment(self):
        code = '(function(x) for x in iterable if x > 10)'
        expected = ('(function(x)\n'
                    ' for x in iterable\n'
                    ' if x > 10)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_generator_inside_call_skips_brackets_when_possible(self):
        code = 'function((x for x in iterable if x>10))'
        expected = 'function(x for x in iterable if x > 10)'
        self.assertEqual(format_code(code), expected)

        code = 'function((x for x in iterable if x>10), y)'
        expected = 'function((x for x in iterable if x > 10), y)'

        self.assertEqual(format_code(code), expected)


class DictionaryDisplaysTestCase(unittest.TestCase):
    """
    [5.2.7]
    dict_display       ::=  "{" [key_datum_list | dict_comprehension] "}"
    key_datum_list     ::=  key_datum ("," key_datum)* [","]
    key_datum          ::=  expression ":" expression
    dict_comprehension ::=  expression ":" expression comp_for
    """
    def test_simple_comprehension_wrapping(self):
        code = '{x: fun(x) for x in iterable}'
        expected = ('{x: fun(x)\n'
                    ' for x\n'
                    ' in iterable}')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_comprehension_with_condition_wrapping(self):
        code = '{x: fun(x) for x in iterable if x>0}'
        expected = ('{x: fun(x)\n'
                    ' for x in iterable\n'
                    ' if x > 0}')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_nested_comprehensions_alignment(self):
        code = '{x: fun(x) for i in range(10) if (i+1)%2 for x in range(i) if x>5}'
        expected = ('{x: fun(x) for i in range(10) if (i + 1) % 2 '
                    'for x in range(i) if x > 5}')
        self.assertEqual(format_code(code, len(expected)), expected)

    def test_nested_comprehensions_wrapping(self):
        code = '{x: fun(x) for i in range(10) if (i+1)%2 for x in range(i) if x>5}'
        expected = ('{x: fun(x)\n'
                    ' for i in range(10)\n'
                    ' if (i + 1) % 2\n'
                    ' for x in range(i)\n'
                    ' if x > 5}')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_key_datum_list_alignment(self):
        code = '{x: y,    z: s,   u   : v}'
        expected = '{x: y, z: s, u: v}'
        self.assertEqual(format_code(code), expected)

    def test_key_datum_list_wrapping(self):
        code = "{'k1': 1,  'k2'  : 2  , v: 3}"
        expr = ast.parse(code).body[0].value
        d = ExpressionFormatter.from_ast(expr)
        expected = ("{'k1': 1, 'k2': 2,\n"
                    " v: 3}")
        formatted = unicode(d.format_code(max(len(l) for l in expected.split('\n')),
                            force=True))
        self.assertEqual(formatted, expected)

    def test_key_datum_list_half_wrapping(self):
        code = '{process_id: status for process_id, status in services_status if status}'
        expected = ('{process_id: status for process_id, status\n'
                    '                    in services_status if status}')
        width = max(len(l) for l in expected.split('\n'))
        #print '\n', expected
        #print '\n', format_code(code, width)
        self.assertEqual(format_code(code, width), expected)

    def test_key_datum_list_KandR_wrapping(self):
        code = textwrap.dedent("""\
            {"process_type=icecast&service_id=1": {"/": {"listeners": [],
                                                        "metadata": "Hang Massive - Once Again",
                                                        "mime": "audio/mpeg"}},
            "process_type=icecast&service_id=2": {"/": {"listeners": [],
                                                        "metadata": "Hang Massive - Once Again",
                                                        "mime": "application/ogg"}}}""")
        expected = textwrap.dedent("""\
        {
            "process_type=icecast&service_id=1": {
                "/": {
                    "listeners": [],
                    "metadata": "Hang Massive - Once Again",
                    "mime": "audio/mpeg"
                }
            },
            "process_type=icecast&service_id=2": {
                "/": {
                    "listeners": [],
                    "metadata": "Hang Massive - Once Again",
                    "mime": "application/ogg"
                }
            }
        }""")

        width = max(len(l) for l in expected.split('\n'))
        formatted = format_code(code, width=width, AstFormatter=KandRAstFormatter)
        self.assertEqual(expected, formatted)


class SetDisplaysTestCase(unittest.TestCase):
    """
    [5.2.8]
    set_display ::=  "{" (expression_list | comprehension) "}"
    """
    def test_simple_comprehension_alignment(self):
        code = '{x for x in iterable}'
        expected = ('{x for x in iterable}')
        self.assertEqual(format_code(code), expected)

    def test_comprehension_wrapping(self):
        code = '{function(x) for x in iterable if x>0}'
        expected = ('{function(x)\n'
                    ' for x in iterable\n'
                    ' if x > 0}')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_alignment(self):
        code = "{ 'a', 'b' }"
        expected = "{'a', 'b'}"
        self.assertEqual(format_code(code), expected)


class AttributeRefTestCase(unittest.TestCase):
    """
    [5.3.1]
    primary ::=  atom | attributeref | subscription | slicing | call
    attributeref ::=  primary "." identifier
    """

    def test_formatting(self):
        for code, expected in [('instance.attribute', 'instance.attribute'),
                               ('nested.instance.attribute', 'nested.instance.attribute'),
                               ('[1,2,3,4][0].imag', '[1, 2, 3, 4][0].imag')]:
            self.assertEqual(format_code(code), expected)

    def test_attribute_assignment(self):
        self.assertEqual(format_code('instance.attribute   =  x'),
                         'instance.attribute = x')


class BinaryArithmeticOperationsTestCase(unittest.TestCase):
    """
    [5.6]
    m_expr ::=  u_expr | m_expr "*" u_expr | m_expr "//" u_expr | m_expr "/" u_expr
                | m_expr "%" u_expr
    a_expr ::=  m_expr | a_expr "+" m_expr | a_expr "-" m_expr
    """

    OPERATORS = ['*', '//', '/', '%', '+', '-']

    def test_simple_alignment(self):
        for op in self.OPERATORS:
            self.assertEqual(format_code('x   %s y' % op), 'x %s y' % op)

    def test_wrapping(self):
        for op in self.OPERATORS:
            code = 'var1 %s var2 %s var3' % (op, op)
            expected = '(var1 %s\n var2 %s\n var3)' % (op, op)
            width = max(len(l) for l in expected.split('\n'))
            self.assertEqual(format_code(code, width), expected)

    def test_subexpressions_adds_brackets_when_neccessary(self):
        code = '(x+y+z)*(u+v+w)'
        expected = '(x + y + z) * (u + v + w)'
        self.assertEqual(format_code(code), expected)

        code = 'x+y+z*u+v+w'
        expected = 'x + y + z * u + v + w'
        self.assertEqual(format_code(code), expected)

    def test_string_formatting_operator_forces_brackets_on_parameters_tuple(self):
        code = "'value: %x, %y'%(1,2)"
        expected = "'value: %x, %y' % (1, 2)"
        self.assertEqual(format_code(code), expected)

    def test_string_formatting_operator_preserves_parameter_without_brackets(self):
        code = "'value: %x'%1"
        expected = "'value: %x' % 1"
        self.assertEqual(format_code(code), expected)


class ComparisonsTestCase(unittest.TestCase):
    """
    [5.9]
    comparison    ::=  or_expr ( comp_operator or_expr )*
    comp_operator ::=  "<" | ">" | "==" | ">=" | "<=" | "<>" | "!="
                       | "is" ["not"] | ["not"] "in"
    """

    def test_alignment(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = '(x %s\ny)' % opt
            expected = 'x %s y' % opt
            self.assertEqual(format_code(code, 2), expected)

    def test_wrapping(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = 'fun(x, y, z) %s fun(m, n, o)' % opt
            expected = ('fun(x,\n'
                        '    y,\n'
                        '    z) %s fun(m,\n'
                        '       %s     n,\n'
                        '       %s     o)' % (opt, len(opt)*' ', len(opt)*' '))
            self.assertEqual(format_code(code, 2), expected)

    def test_multi_opt_wrapping(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = 'fun(x, y, z) %s fun(m, n, o) %s fun(p, q, r)' % (opt, opt)
            expected = ('fun(x, y, z) %(opt)s fun(m, n, o) %(opt)s fun(p,\n'
                        '             %(spc)s              %(spc)s     q,\n'
                        '             %(spc)s              %(spc)s     r)' % {'opt': opt,
                                                                              'spc': len(opt)*' '})
            width = max(len(l) for l in expected.split('\n'))
            self.assertEqual(format_code(code, width), expected)


class BooleanOperationsTestCase(unittest.TestCase):
    """
    [5.10]
    or_test  ::=  and_test | or_test "or" and_test
    and_test ::=  not_test | and_test "and" not_test
    not_test ::=  comparison | "not" not_test
    """

    BIN_OPERATORS = ['or', 'and']

    def test_simple_alignment(self):
        for op in self.BIN_OPERATORS:
            self.assertEqual(format_code('x   %s y' % op), 'x %s y' % op)

    def test_wrapping(self):
        for op in ['or', 'and']:
            code = 'x %(op)s y %(op)s z %(op)s v' % {'op': op}
            expected = ('(x %(op)s\n'
                        ' y %(op)s\n'
                        ' z %(op)s\n'
                        ' v)' % {'op': op})
            width = max(len(l) for l in expected.split('\n'))
            self.assertEqual(format_code(code, width), expected)

    def test_brackets_are_used_only_when_neccessary(self):
        code = 'x or fun(y,z,v)'
        expected = ('x or fun(y, z,\n'
                    '         v)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_negation_alignment(self):
        code = 'not     x'
        expected = 'not x'
        self.assertEqual(format_code(code), expected)


class ConditionalExpressionsTestCase(unittest.TestCase):
    """
    [5.11]
    conditional_expression ::=  or_test ["if" or_test "else" expression]
    expression             ::=  conditional_expression | lambda_form
    """
    def test_alignment(self):
        self.assertEqual(format_code('x if    c   else y'),
                         'x if c else y')


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


class ExpressionStatementTestCase(unittest.TestCase):
    """
    [6.1]
    expression_stmt ::=  expression_list
    """
    def test_alignment(self):
        code = '3,8+9,fun(x,y)'
        expected = '3, 8+9, fun(x, y)'
        self.assertEqual(format_code(code), expected)

    def test_wrapping(self):
        code = '3*8+10,8+9,fun(x,y)'
        expected = ('(3 * 8 + 10,\n'
                    ' 8+9,\n'
                    ' fun(x, y))')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class AssignmentTestCase(unittest.TestCase):
    """
    [6.2]
    assignment_stmt ::=  (target_list "=")+ (expression_list | yield_expression)
    target_list     ::=  target ("," target)* [","]
    target          ::=  identifier
                         | "(" target_list ")"
                         | "[" target_list "]"
                         | attributeref
                         | subscription
                         | slicing
    """

    def test_identifires_target_list_aligment(self):
        code = '(s,t,u) = x'
        expected = 's, t, u = x'
        self.assertEqual(format_code(code), expected)

    def test_identifiers_target_list_wrapping(self):
        code = 'var1, var2, var3 = x'
        expected = ('(var1,\n'
                    ' var2,\n'
                    ' var3) = x')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_attributeref_target_list_alignment(self):
        code = '(i.s,i.t,i.u) = x'
        expected = 'i.s, i.t, i.u = x'
        self.assertEqual(format_code(code), expected)

    def test_subscription_target_list_alignment(self):
        code = '(i[ s ],i[t],i[u]) = x'
        expected = 'i[s], i[t], i[u] = x'
        self.assertEqual(format_code(code), expected)

    def test_slicing_target_list_alignment(self):
        code = '(i[ s :  ],i[t : u],i[:]) = x'
        expected = 'i[s:], i[t:u], i[:] = x'
        self.assertEqual(format_code(code), expected)

    def test_multi_target_list_alignment(self):
        code = 's,t=u,v=z=x'
        self.assertEqual(format_code(code), 's, t = u, v = z = x')


class AugmentAssignmentTestCase(unittest.TestCase):
    """
    [6.2.1]
    augmented_assignment_stmt ::=  augtarget augop (expression_list | yield_expression)
    augtarget                 ::=  identifier | attributeref | subscription | slicing
    augop                     ::=  "+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "**="
                                   | ">>=" | "<<=" | "&=" | "^=" | "|="
    """
    def test_aligment(self):
        raise NotImplementedError()


class SimpleStatementsTestCase(unittest.TestCase):
    """
    [6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 6.10, 6.11]
     simple_stmt ::=  assert_stmt
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

    def test_return_statement(self):
        """
        [6.7]
        return_stmt ::=  "return" [expression_list]
        """
        code = 'return x+x'
        expected = 'return x + x'
        self.assertEqual(format_code(code), expected)


