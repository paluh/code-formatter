import unittest

from . import CodeBlock, CodeLine, format_code


# FIXME: Move this testcase to structure provided by "The Python Language Reference"
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
            self.assertEqual(format_code(code), code)

    # FIXME: move these tests "somewhere"
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

    def test_tuple_brackets_are_preserved(self):
        code = '[(x, y) for (x, y) in iterable]'
        self.assertEqual(format_code(code), code)


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
        expected = ("{'k1': 1, 'k2': 2,\n"
                    " v: 3}")
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width=width), expected)

    def test_key_datum_list_half_wrapping(self):
        code = '{process_id: status for process_id, status in services_status if status}'
        expected = ('{process_id: status for process_id, status\n'
                    '                    in services_status if status}')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


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

    def test_wrapping(self):
        code = "{ 'a', 'b' }"
        expected = ("{'a',\n"
                    " 'b'}")
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class AttributeRefTestCase(unittest.TestCase):
    """
    [5.3.1]
    primary ::=  atom | attributeref | subscription | slicing | call
    attributeref ::=  primary "." identifier
    """

    d = {
        'aalsdkfjalsdfkja': {'c': 1,
                             'd': 2},
        'b': 2
    }

    def test_formatting(self):
        for code, expected in [('instance.attribute', 'instance.attribute'),
                               ('nested.instance.attribute', 'nested.instance.attribute'),
                               ('[1,2,3,4][0].imag', '[1, 2, 3, 4][0].imag')]:
            self.assertEqual(format_code(code), expected)

    def test_attribute_assignment(self):
        self.assertEqual(format_code('instance.attribute   =  x'),
                         'instance.attribute = x')


class SlicingTestCase(unittest.TestCase):
    """
    [5.3.3]
    slicing          ::=  simple_slicing | extended_slicing
    simple_slicing   ::=  primary "[" short_slice "]"
    extended_slicing ::=  primary "[" slice_list "]"
    slice_list       ::=  slice_item ("," slice_item)* [","]
    slice_item       ::=  expression | proper_slice | ellipsis
    proper_slice     ::=  short_slice | long_slice
    short_slice      ::=  [lower_bound] ":" [upper_bound]
    long_slice       ::=  short_slice ":" [stride]
    lower_bound      ::=  expression
    upper_bound      ::=  expression
    stride           ::=  expression
    ellipsis         ::=  "..."
    """

    def test_simple_expression_slice_alignemnt(self):
        self.assertEqual(format_code('x [ y ]'), 'x[y]')

    def test_tuple_expression_slice_alignemnt(self):
        self.assertEqual(format_code('x [ y, z ]'), 'x[y, z]')

    def test_short_slice_alignment(self):
        self.assertEqual(format_code('x [ y : z ]'), 'x[y:z]')

    def test_short_slice_without_upper_alignment(self):
        self.assertEqual(format_code('x [ y : ]'), 'x[y:]')

    def test_short_slice_without_lower_alignment(self):
        self.assertEqual(format_code('x [ : z ]'), 'x[:z]')

    def test_long_slice_alignment(self):
        self.assertEqual(format_code('x [ y : z : 1 ]'), 'x[y:z:1]')


class CallsTestCase(unittest.TestCase):
    """
    [5.3.4]
    call                 ::=  primary "(" [argument_list [","]
                              | expression genexpr_for] ")"
    argument_list        ::=  positional_arguments ["," keyword_arguments]
                                ["," "*" expression] ["," keyword_arguments]
                                ["," "**" expression]
                              | keyword_arguments ["," "*" expression]
                                ["," "**" expression]
                              | "*" expression ["," "*" expression] ["," "**" expression]
                              | "**" expression
    positional_arguments ::=  expression ("," expression)*
    keyword_arguments    ::=  keyword_item ("," keyword_item)*
    keyword_item         ::=  identifier "=" expression
    """

    def test_method_call_formatting(self):
        code = 'instance.method(   x,   y )'
        formatted = format_code(code)
        self.assertEqual(formatted, 'instance.method(x, y)')

    def test_tuple_args_preserves_brackets(self):
        code = 'fun((x,),(y,z))'
        self.assertEqual(format_code(code), 'fun((x,), (y, z))')

    def test_positional_arguments_alignment(self):
        code = 'function_with_args(argument_1,     argument_2,argument_3)'
        formatted = format_code(code)
        self.assertEqual(formatted, 'function_with_args(argument_1, argument_2, '
                                                       'argument_3)')

    def test_positional_arguments_wrapping(self):
        code = 'function_with_args(argument_1,     argument_2,argument_3)'
        expected = ('function_with_args(argument_1,\n'
                    '                   argument_2,\n'
                    '                   argument_3)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width=width), expected)

    def test_wrapping_positional_arguments_expressions(self):
        code = ('function_with_args(nested_function_with_args(argument_1,argument_2,argument_3),'
                                   'nested_function_with_args(argument_4,argument_5,argument_6))')
        expected = ('function_with_args(nested_function_with_args(argument_1,\n'
                    '                                             argument_2,\n'
                    '                                             argument_3),\n'
                    '                   nested_function_with_args(argument_4,\n'
                    '                                             argument_5,\n'
                    '                                             argument_6))')
        width = max(len(l) for l in expected.split('\n'))
        formatted = format_code(code, width=width)
        self.assertEqual(formatted, expected)

    def test_wrapping_positional_arguments_with_subexpressions(self):
        code = ('function_with_args(param_1, param_2, nested(argument_1,argument_2,argument_3),'
                                   'param_3, nested(argument_4,argument_5,argument_6), param_4)')
        expected = ('function_with_args(param_1, param_2, nested(argument_1,\n'
                    '                                            argument_2,\n'
                    '                                            argument_3),\n'
                    '                   param_3, nested(argument_4, argument_5,\n'
                    '                                   argument_6), param_4)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width=width), expected)

    def test_keyword_arguments_alignment(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        formatted = format_code(code)
        self.assertEqual(formatted, 'function_with_kwargs(argument_1=value, argument_2=value, '
                                                         'argument_3=value)')

    def test_keyword_arguments_wrapping(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        expected = ('function_with_kwargs(argument_1=value,\n'
                    '                     argument_2=value,\n'
                    '                     argument_3=value)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width=width), expected)

    def test_asterisk_identifier_alignmet(self):
        code = 'function( * args )'
        expected = 'function(*args)'
        self.assertEqual(format_code(code), expected)

    def test_kwargs_identifier_alignmet(self):
        code = 'function( ** kwargs )'
        expected = 'function(**kwargs)'
        self.assertEqual(format_code(code), expected)

    def test_mixed_args_type_ordering(self):
        code = 'function(x, y, *args, k=s, l=t, **kwargs)'
        self.assertEqual(format_code(code), code)


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


class BinaryBitwiseOperation(unittest.TestCase):
    """
    [5.8]
    and_expr ::=  shift_expr | and_expr "&" shift_expr
    xor_expr ::=  and_expr | xor_expr "^" and_expr
    or_expr  ::=  xor_expr | or_expr "|" xor_expr
    """

    def test_alignment(self):
        code = '8  |  4'
        expected = '8 | 4'
        self.assertEqual(format_code(code), expected)

    def test_wrapping(self):
        code = '88 | 44'
        expected = ('(88 |\n'
                    ' 44)')
        width = max(len(l) for l in expected.split('\n'))
        #print '\n', expected
        #print '\n', format_code(code, width)
        self.assertEqual(format_code(code, width), expected)

    def test_brackets_usage(self):
        code = '(((8 | 4) & (8 | 7)) ^ 3)'
        expected = '((8 | 4) & (8 | 7)) ^ 3'
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
            self.assertEqual(format_code(code), expected)

    def test_wrapping(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = 'fun(x, y, z) %s fun(m, n, o)' % opt
            operator_spacing = len(opt) * ' '
            expected = ('fun(x,\n'
                        '    y,\n'
                        '    z) %s fun(m,\n'
                        '       %s     n,\n'
                        '       %s     o)' % (opt, operator_spacing, operator_spacing))
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

    def test_brackets_usage_with_mixed_expression(self):
        code = '(x < y) | (z < v)'
        expected = ('((x < y) |\n'
                    ' (z < v))')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


    def test_brackets_usage_in_assignemnt(self):
        code = 'r = (x < y) | (z < v)'
        expected = ('r = ((x < y) |\n'
                    '     (z < v))')
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

class LambdasTestCase(unittest.TestCase):
    # FIXME: test old_lambda_form branch
    """
    [5.12] - parameter_list related tests are
             placed in FuncionDefinitionTestCase

    lambda_form     ::=  "lambda" [parameter_list]: expression
    old_lambda_form ::=  "lambda" [parameter_list]: old_expression
    """
    def test_aligment(self):
        code = 'lambda x,y,z:x+y+z'
        expected = 'lambda x, y, z: x + y + z'
        self.assertEqual(format_code(code), expected)

    def test_aligment_without_args(self):
        code = 'lambda : 8'
        expected = 'lambda: 8'
        self.assertEqual(format_code(code), expected)


class ExpressionListTestCase(unittest.TestCase):
    """
    [5.13, 6.1]
    expression_list ::=  expression ( "," expression )* [","]
    expression_stmt ::=  expression_list
    """

    def test_single_element_alignment(self):
        code = 'x,'
        self.assertEqual(format_code(code), '(x,)')

    def test_comlex_expression_alignment(self):
        code = '3,8+9,fun(x,y)'
        expected = '3, 8 + 9, fun(x, y)'
        self.assertEqual(format_code(code), expected)

    def test_nested_expression_list_preserves_brackets(self):
        code = '((x,),(y,z))'
        self.assertEqual(format_code(code), '(x,), (y, z)')

    def test_wrapping(self):
        code = '3*8+10,8+9,fun(x,y)'
        expected = ('(3 * 8 + 10,\n'
                    ' 8 + 9,\n'
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
    def test_simple_statement_aligment(self):
        for augop in ['+=', '-=', '*=', '/=', '//=', '%=', '**=', '>>=', '<<=', '&=', '^=', '|=']:
            code = 'x   %s   y' % augop
            expected = 'x %s y' % augop
            self.assertEqual(format_code(code), expected)

    def test_simple_statement_wrapping(self):
        code = 'x += [1,2,3,4]'
        expected = ('x += [1, 2,\n'
                    '      3, 4]')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width=width), expected)


class SimpleStatementsTestCase(unittest.TestCase):
    """
    [6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 6.10, 6.11, 6.13]
    simple_stmt ::=  assert_stmt
                     | pass_stmt
                     | del_stmt
                     | print_stmt
                     | return_stmt
                     | yield_stmt
                     | raise_stmt
                     | break_stmt
                     | continue_stmt
                     | global_stmt
                     | exec_stmt
        return_stmt ::=  "return" [expression_list]
        raise_stmt ::=  "raise" [expression ["," expression ["," expression]]]
    """

    def test_return_statement(self):
        code = 'return x+x'
        expected = 'return x + x'
        self.assertEqual(format_code(code), expected)

    def test_raise_simple_statement(self):
        code = "raise   Exception('oh no!')"
        expected = "raise Exception('oh no!')"
        self.assertEqual(format_code(code), expected)


class ImportStatementTestCase(unittest.TestCase):
    """
    [6.12]
    import_stmt     ::=  "import" module ["as" name] ( "," module ["as" name] )*
                         | "from" relative_module "import" identifier ["as" name]
                         ( "," identifier ["as" name] )*
                         | "from" relative_module "import" "(" identifier ["as" name]
                         ( "," identifier ["as" name] )* [","] ")"
                         | "from" module "import" "*"
    module          ::=  (identifier ".")* identifier
    relative_module ::=  "."* module | "."+
    name            ::=  identifier
    """

    def test_simple_form_alignment(self):
        code = 'import    module'
        expected = 'import module'
        self.assertEqual(format_code(code), expected)

    def test_from_form_aligment(self):
        code = 'from  module  import Class1  , Class2 , Class3'
        expected = 'from module import Class1, Class2, Class3'
        self.assertEqual(format_code(code), expected)

    def test_from_form_with_asterisk_alignment(self):
        code = 'from  module  import *'
        expected = 'from module import *'
        self.assertEqual(format_code(code), expected)

    def test_from_form_with_relative_imports_alignment(self):
        code = 'from  ..module  import *'
        expected = 'from ..module import *'
        self.assertEqual(format_code(code), expected)

    def test_from_current_package_form_aligment(self):
        code = 'from . import Class2   , Class1'
        expected = 'from . import Class1, Class2'
        self.assertEqual(format_code(code), expected)

    def test_simple_form_sorting(self):
        code = 'import module3, Module2, module1'
        expected = 'import module1, Module2, module3'
        self.assertEqual(format_code(code), expected)

    def test_from_form_sorting(self):
        code = 'from  module  import Class3, class2 , Class1'
        expected = 'from module import Class1, class2, Class3'
        self.assertEqual(format_code(code), expected)

    def test_simple_form_wrapping(self):
        code = 'import    module1, module2, module3, module4'
        expected = ('import (module1, module2,\n'
                    '        module3, module4)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_from_form_wrapping(self):
        code = 'from module import Class1, Class2, Class3, Class4'
        expected = ('from module import (Class1, Class2,\n'
                    '                    Class3, Class4)')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class IfTestCase(unittest.TestCase):
    """
    [7.1]
    if_stmt ::=  "if" expression ":" suite
                 ( "elif" expression ":" suite )*
                 ["else" ":" suite]
    """
    def test_if_alignment(self):
        code = 'if    x >   8: pass'
        expected = ('if x > 8:\n'
                    '    pass')
        self.assertEqual(format_code(code), expected)

    def test_if_wrapping(self):
        code = 'if x > 8 and y % 2 == 3:    pass'
        expected = ('if (x > 8 and\n'
                    '    y % 2 == 3):\n'
                    '    pass')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)


class ForTestCase(unittest.TestCase):
    """
    [7.3]
    for_stmt ::=  "for" target_list "in" expression_list ":" suite
                  ["else" ":" suite]
    """

    def test_simple_for_statement(self):
        code = ('for   p   in    (1,2,3):\n   p')
        expected = 'for p in 1, 2, 3:\n%sp' % CodeLine.INDENT
        self.assertEqual(format_code(code), expected)


class FunctionDefinitionTestCase(unittest.TestCase):
    """
    [7.6]
    decorated      ::=  decorators (classdef | funcdef)
    decorators     ::=  decorator+
    decorator      ::=  "@" dotted_name ["(" [argument_list [","]] ")"] NEWLINE
    funcdef        ::=  "def" funcname "(" [parameter_list] ")" ":" suite
    dotted_name    ::=  identifier ("." identifier)*
    parameter_list ::=  (defparameter ",")*
                        (  "*" identifier ["," "**" identifier]
                        | "**" identifier
                        | defparameter [","] )
    defparameter   ::=  parameter ["=" expression]
    sublist        ::=  parameter ("," parameter)* [","]
    parameter      ::=  identifier | "(" sublist ")"
    funcname       ::=  identifier
    """
    def test_identifiers_parameter_list_alignment(self):
        code = ('def fun(x,y,z):\n'
                '    pass')
        expected = ('def fun(x, y, z):\n'
                    '    pass')
        self.assertEqual(format_code(code), expected)

    def test_identifiers_parameter_list_wrapping(self):
        code = ('def fun(x,y,z):\n'
                '    pass')
        expected = ('def fun(x,\n'
                    '        y,\n'
                    '        z):\n'
                    '    pass')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_identifiers_parameter_list_wrapping_with_multiple_params_per_line(self):
        code = ('def fun(x,y,z,u,v,w,t):\n'
                '    pass')
        expected = ('def fun(x, y, z,\n'
                    '        u, v, w,\n'
                    '        t):\n'
                    '    pass')
        width = max(len(l) for l in expected.split('\n'))
        self.assertEqual(format_code(code, width), expected)

    def test_defparameters_list_alignment(self):
        code = ('def fun(x=1,y=2,z=3):\n'
                '    pass')
        expected = ('def fun(x=1, y=2, z=3):\n'
                    '    pass')
        self.assertEqual(format_code(code), expected)

    def test_mixed_parameters_list_alignment(self):
        code = ('def fun(x,y,z=3):\n'
                '    pass')
        expected = ('def fun(x, y, z=3):\n'
                    '    pass')
        self.assertEqual(format_code(code), expected)
