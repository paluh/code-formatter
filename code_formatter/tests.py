#-*- coding: utf-8 -*-
import ast
from textwrap import dedent
import sys
import unittest

from . import format_code
from .base import formatters
from .code import CodeBlock, CodeLine
from .extras import tests
from .exceptions import NotEnoughSpace
from .utils import FormatterTestCase


class LiteralsTestCase(FormatterTestCase):
    """
    [5.2.2]
    +   literal ::=  stringliteral | integer | longinteger
                     | floatnumber | imagnumber
    """
    def test_string_quotes_escaping(self):
        code = '\'"test"\''
        self.assertEqual(format_code(code), code)

    def test_string_simple_wrapping(self):
        code = "'test of text wrap'"
        expected = ("('test of '\n"
                    " 'text wrap')")
        self.assertFormats(code, expected)

    def test_string_wrapping_preserves_comments_blocks(self):
        code = ('class A(object):\n'
                '    """class A multiline\n'
                '    docstring\n'
                '    """\n'
                '    pass')
        self.assertFormats(code, code)

    def test_string_wrapping_wraps_on_newlines(self):
        code = '"""multiline string with\nvery very very long lines\nand\nsome short"""'
        self.assertFormats(code, "('multiline string with\\n'\n"
                                 " 'very very very long lines\\n'\n"
                                 " 'and\\n'\n"
                                 " 'some short')")

    def test_string_wrapping_skips_brackets_in_function_call(self):
        code = "fun('long string')"
        expected = ("fun('long '\n"
                    "    'string')")
        self.assertFormats(code, expected)

    def test_string_wrapping_skips_brackets_in_dictionary_expression(self):
        code = "{'key': 'long string'}"
        expected = ("{'key': 'long '\n"
                    "        'string'}")
        self.assertFormats(code, expected)

    def test_empty_string_formatting(self):
        code = "''"
        self.assertFormats(code, code)

    def test_float_formatting(self):
        code = '8.9'
        self.assertFormats(code, code)

    def test_long_formatting(self):
        self.assertFormats('8l', '8L')

    def test_imagnumber_formatting(self):
        code = '2   + 3j'
        expected = '2 + 3j'
        self.assertFormats(code, expected)


class ListDisplaysTestCase(FormatterTestCase):
    """
    [5.2.4]
    +   list_display        ::=  "[" [expression_list | list_comprehension] "]"
    +   list_comprehension  ::=  expression list_for
    +   list_for            ::=  "for" target_list "in" old_expression_list [list_iter]
    -   old_expression_list ::=  old_expression [("," old_expression)+ [","]]
    -   old_expression      ::=  or_test | old_lambda_form
    -   list_iter           ::=  list_for | list_if
    -   list_if             ::=  "if" old_expression [list_iter]
    """
    def test_expression_list_alignment(self):
        code = '[   1 , 2,   3,]'
        expected = '[1, 2, 3]'
        self.assertEqual(format_code(code), expected)

    def test_expression_list_wrapping(self):
        code = '[   1 , 2,   3,]'
        expected = ('[1, 2,\n'
                    ' 3]')
        self.assertFormats(code, expected)

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
        self.assertFormats(code, expected)

    def test_tuple_brackets_are_preserved(self):
        code = '[(x, y) for (x, y) in iterable]'
        self.assertFormats(code, code)

    def test_forcing_formatting(self):
        code = '[   1 , 2,   3,]'
        expected = ('[1,\n'
                    ' 2,\n'
                    ' 3]')
        self.assertFormats(code, expected, width=2, force=True)

    def test_wrapping_uses_whole_line(self):
        # REGRESSION
        code = dedent("""\
            [v1, v2,
             variable or x]""")
        self.assertFormats(code, code)


class GeneratorExpressionsTestCase(FormatterTestCase):
    """
    [5.2.5, 5.2.6]
    +   comp_for             ::=  "for" target_list "in" or_test [comp_iter]
    +   comp_iter            ::=  comp_for | comp_if
    +   comp_if              ::=  "if" expression_nocond [comp_iter]
    +   generator_expression ::=  "(" expression comp_for ")"
    """
    def test_alignment(self):
        code = '(function(x) for x in iterable if x > 10)'
        expected = ('(function(x)\n'
                    ' for x in iterable\n'
                    ' if x > 10)')
        self.assertFormats(code, expected)

    def test_generator_inside_call_skips_brackets_when_possible(self):
        code = 'function((x for x in iterable if x>10))'
        expected = 'function(x for x in iterable if x > 10)'
        self.assertEqual(format_code(code), expected)

        code = 'function((x for x in iterable if x>10), y)'
        expected = 'function((x for x in iterable if x > 10), y)'

        self.assertEqual(format_code(code), expected)

    def test_generator_inside_call_preserves_brackets_when_necessary(self):
        code = 'function((x, y) for x, y in iterable)'
        expected = 'function((x, y) for (x, y) in iterable)'
        self.assertFormats(code, expected)

    def test_string_wrapping_inside_call_skips_brackets(self):
        code = "function(x='long string')"
        expected = ("function(x='long '\n"
                    "           'string')")
        self.assertFormats(code, expected)

    def test_complex_generator_alignemnt(self):
        code = '(e for l in ll for e in l)'
        self.assertFormats(code, code)


class DictionaryDisplaysTestCase(FormatterTestCase):
    """
    [5.2.7]
    +   dict_display       ::=  "{" [key_datum_list | dict_comprehension] "}"
    +   key_datum_list     ::=  key_datum ("," key_datum)* [","]
    +   key_datum          ::=  expression ":" expression
    +   dict_comprehension ::=  expression ":" expression comp_for
    """
    def test_simple_comprehension_wrapping(self):
        code = '{x: fun(x) for x in iterable}'
        expected = ('{x: fun(x)\n'
                    ' for x\n'
                    ' in iterable}')
        self.assertFormats(code, expected)

    def test_forcing_dictionary_formatting(self):
        # REGRESSION
        code = dedent("""\
            result = {'type': type(e).__name__, 'args': e.args,
                      'traceback': traceback or
                                   logging.Formatter().formatException(sys.exc_info())}""")
        self.assertFormats(code, code, width=30, force=True)
        #self.assertFormats(code, code, width=75)

    def test_comprehension_with_condition_wrapping(self):
        code = '{x: fun(x) for x in iterable if x>0}'
        expected = ('{x: fun(x)\n'
                    ' for x in iterable\n'
                    ' if x > 0}')
        self.assertFormats(code, expected)

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
        self.assertFormats(code, expected)

    def test_comprehension_preserves_suffix(self):
        code = 'f({x: y for x, y in items})'
        self.assertFormats(code, code)

    def test_key_datum_list_alignment(self):
        code = '{x: y,    z: s,   u   : v}'
        expected = '{x: y, z: s, u: v}'
        self.assertFormats(code, expected)

    def test_key_datum_list_wrapping(self):
        code = "{'k1': 1,  'k2'  : 2  , v: 3}"
        expected = ("{'k1': 1, 'k2': 2,\n"
                    " v: 3}")
        self.assertFormats(code, expected)

    def test_key_datum_list_half_wrapping(self):
        code = '{process_id: status for process_id, status in services_status if status}'
        expected = ('{process_id: status for process_id, status\n'
                    '                    in services_status if status}')
        self.assertFormats(code, expected)


class SetDisplaysTestCase(FormatterTestCase):
    """
    [5.2.8]
    +   set_display ::=  "{" (expression_list | comprehension) "}"
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
        self.assertFormats(code, expected)

    def test_alignment(self):
        code = "{ 'a', 'b' }"
        expected = "{'a', 'b'}"
        self.assertFormats(code, expected)

    def test_wrapping(self):
        code = "{ 'a', 'b' }"
        expected = ("{'a',\n"
                    " 'b'}")
        self.assertFormats(code, expected)


class AttributeRefTestCase(FormatterTestCase):
    """
    [5.3.1]
    +   primary ::=  atom | attributeref | subscription | slicing | call
    +   attributeref ::=  primary "." identifier
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

    def test_attribute_formatting_preserves_brackets_of_subexpression(self):
        # REGRESSION
        code = dedent("""\
            x.timegm((f(x=0) -
                      v).m(x=y))""")
        self.assertFormats(code, code)


class SubscriptionsTestCase(FormatterTestCase):
    """
    [5.3.2]
    +   subscription ::=  primary "[" expression_list "]"
    """

    def test_simple_expression_alignment(self):
        code = 'd [ "a" ] '
        self.assertFormats(code, "d['a']")

    def test_expression_list_alignment(self):
        code = 'd [ 1,  2,  3]'
        self.assertFormats(code, 'd[1, 2, 3]')

    def test_complex_expression_alignment(self):
        code = ('dictionary[function_with_kwargs(argument_1=value, argument_2=value,'
                                           'argument_3=value)]')
        expected = ('dictionary[function_with_kwargs(argument_1=value,\n'
                    '                                argument_2=value,\n'
                    '                                argument_3=value)]')
        self.assertFormats(code, expected)

    def test_subexpressions_handles_suffix_correctly(self):
        code = 'fun(v[k])'
        self.assertFormats(code, code)


class SlicingTestCase(FormatterTestCase):
    """
    [5.3.3]
    -   slicing          ::=  simple_slicing | extended_slicing
    +   simple_slicing   ::=  primary "[" short_slice "]"
    -   extended_slicing ::=  primary "[" slice_list "]"
    -   slice_list       ::=  slice_item ("," slice_item)* [","]
    -   slice_item       ::=  expression | proper_slice | ellipsis
    +   proper_slice     ::=  short_slice | long_slice
    +   short_slice      ::=  [lower_bound] ":" [upper_bound]
    +   long_slice       ::=  short_slice ":" [stride]
    +   lower_bound      ::=  expression
    +   upper_bound      ::=  expression
    +   stride           ::=  expression
    -   ellipsis         ::=  "..."
    """

    def test_simple_expression_slice_alignemnt(self):
        self.assertEqual(format_code('x [ y ]'), 'x[y]')

    def test_tuple_expression_slice_alignemnt(self):
        self.assertEqual(format_code('x [ y, z ]'), 'x[y, z]')

    def test_short_slice_alignment(self):
        self.assertEqual(format_code('x [ y : z ]'), 'x[y:z]')

    def test_short_slice_without_lower_and_upper_alignment(self):
        self.assertEqual(format_code('x [ : ]'), 'x[:]')

    def test_short_slice_without_upper_alignment(self):
        self.assertEqual(format_code('x [ y : ]'), 'x[y:]')

    def test_short_slice_without_lower_alignment(self):
        self.assertEqual(format_code('x [ : z ]'), 'x[:z]')

    def test_long_slice_alignment(self):
        self.assertEqual(format_code('x [ y : z : 1 ]'), 'x[y:z:1]')


class CallsTestCase(FormatterTestCase):
    """
    [5.3.4]
    +   primary              ::=  atom | attributeref | subscription
                                  | slicing | call
    +   call                 ::=  primary "(" [argument_list [","]
                                  | expression genexpr_for] ")"
    +   argument_list        ::=  positional_arguments ["," keyword_arguments]
                                    ["," "*" expression] ["," keyword_arguments]
                                    ["," "**" expression]
                                  | keyword_arguments ["," "*" expression]
                                    ["," "**" expression]
                                  | "*" expression ["," "*" expression] ["," "**" expression]
                                  | "**" expression
    +   positional_arguments ::=  expression ("," expression)*
    +   keyword_arguments    ::=  keyword_item ("," keyword_item)*
    +   keyword_item         ::=  identifier "=" expression
    """

    def test_method_call_alignment(self):
        code = 'instance.method(   x,   y )'
        self.assertFormats(code, 'instance.method(x, y)')

    def test_method_call_wrapping(self):
        code = 'instance.method(x, y)'
        expected = ('instance.method(x,\n'
                    '                y)')
        self.assertFormats(code, expected)

    def test_method_call_chain_wrapping(self):
        code = 'instance.method(x, y).method(u, v)'
        expected = ('instance.method(x,\n'
                    '                y).method(u,\n'
                    '                          v)')
        self.assertFormats(code, expected)

    def test_call_with_non_arguments_alignment(self):
        code = 'fun(     \n )'
        self.assertFormats(code, 'fun()')

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
        self.assertFormats(code, expected)

    def test_positional_arguments_wrapping_to_non_exact_width(self):
        # REGRESSION
        code = 'fun(x, y, z)'
        expected = ('fun(x,\n'
                    '    y,\n'
                    '    z)')
        self.assertFormats(code, expected, width=7)
        self.assertFormats(code, expected, width=8)

        expected = ('fun(x, y,\n'
                    '    z)')
        self.assertFormats(code, expected, width=9)
        self.assertFormats(code, expected, width=10)

    def test_wrapping_positional_arguments_expressions(self):
        code = ('function_with_args(nested_function_with_args(argument_1,argument_2,argument_3),'
                                   'nested_function_with_args(argument_4,argument_5,argument_6))')
        expected = ('function_with_args(nested_function_with_args(argument_1,\n'
                    '                                             argument_2,\n'
                    '                                             argument_3),\n'
                    '                   nested_function_with_args(argument_4,\n'
                    '                                             argument_5,\n'
                    '                                             argument_6))')
        self.assertFormats(code, expected)

    def test_wrapping_positional_arguments_with_subexpressions(self):
        code = ('function_with_args(param_1, param_2, nested(argument_1,argument_2,argument_3),'
                                   'param_3, nested(argument_4,argument_5,argument_6), param_4)')
        expected = ('function_with_args(param_1, param_2, nested(argument_1,\n'
                    '                                            argument_2,\n'
                    '                                            argument_3),\n'
                    '                   param_3, nested(argument_4, argument_5,\n'
                    '                                   argument_6), param_4)')
        self.assertFormats(code, expected)

    def test_keyword_arguments_alignment(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        self.assertFormats(code, ('function_with_kwargs(argument_1=value, '
                                                       'argument_2=value, '
                                                       'argument_3=value)'))

    def test_keyword_arguments_wrapping(self):
        code = 'function_with_kwargs(argument_1=value,     argument_2=value,argument_3=value)'
        expected = ('function_with_kwargs(argument_1=value,\n'
                    '                     argument_2=value,\n'
                    '                     argument_3=value)')
        self.assertFormats(code, expected)

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

    def test_wrapping_positional_arguments_combination(self):
        # REGRESSION
        code = 'f(p, n(x, xxxxx, y, z), p)'
        expected = ('f(p, n(x, xxxxx,\n'
                    '       y, z), p)')
        self.assertFormats(code, expected)

    def test_forcing_formatting(self):
        # REGRESSION
        code = 'fun(x, y, z)'
        expected = dedent("""\
            fun(x,
                y,
                z)""")
        self.assertFormats(code, expected, width=3, force=True)

    def test_call_with_nested_boolean_expression_wrapping(self):
        # REGRESSION
        code = 'f(g(x,y) == None)'
        expected = ('f(g(x,\n'
                    '    y) == None)')
        self.assertFormats(code, expected)

    def test_args_wrapping_uses_whole_line(self):
        # REGRESSION
        code = dedent("""\
            f(v1, v2,
              variable or x)""")
        self.assertFormats(code, code)

    def test_kwargs_wrapping_uses_whole_line(self):
        # REGRESSION
        code = dedent("""\
            f(k1=v1, k2=v2,
              k3=variable or x)""")
        self.assertFormats(code, code)

    def test_nested_function_call_wrapping(self):
        code = dedent("""\
            f(x, g(x,
                   x=2))""")
        self.assertFormats(code, code)


class BinaryArithmeticOperationsTestCase(FormatterTestCase):
    """
    [5.6]
    +   m_expr ::=  u_expr | m_expr "*" u_expr | m_expr "//" u_expr | m_expr "/" u_expr
                    | m_expr "%" u_expr
    +   a_expr ::=  m_expr | a_expr "+" m_expr | a_expr "-" m_expr
    """

    OPERATORS = ['*', '//', '/', '%', '+', '-']

    def test_simple_alignment(self):
        for op in self.OPERATORS:
            self.assertEqual(format_code('x   %s y' % op), 'x %s y' % op)

    def test_wrapping(self):
        for op in self.OPERATORS:
            code = 'var1 %s var2 %s var3' % (op, op)
            expected = ('(var1 %s\n'
                        ' var2 %s\n'
                        ' var3)' % (op, op))
            self.assertFormats(code, expected)

    def test_subexpressions_adds_brackets_when_necessary(self):
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

    def test_arithmetic_operation_alignment_in_tuple_definition(self):
        code = '(2 + 3,)'
        self.assertFormats(code, code)

    def test_arithmetic_operation_wrapping_in_tuple_definition(self):
        code = '(2 + 3,)'
        expected = ('(2 +\n'
                    ' 3,)')
        self.assertFormats(code, expected)

    def test_arithmetic_operation_wrapping_in_boolean_expression(self):
        code = 'x and y - z'
        expected = ('x and (y -\n'
                    '       z)')
        self.assertFormats(code, expected)

    def test_forcing_formatting(self):
        code = '1 + 2 + 3 + 4'
        expected = ('(1 +\n'
                    ' 2 +\n'
                    ' 3 +\n'
                    ' 4)')
        self.assertFormats(code, expected, width=2, force=True)


class BinaryBitwiseOperation(FormatterTestCase):
    """
    [5.8]
    +   and_expr ::=  shift_expr | and_expr "&" shift_expr
    +   xor_expr ::=  and_expr | xor_expr "^" and_expr
    +   or_expr  ::=  xor_expr | or_expr "|" xor_expr
    """

    def test_alignment(self):
        for operator in ['|', '&', '^']:
            code = '8  %s  4' % operator
            expected = '8 %s 4' % operator
            self.assertFormats(code, expected)

    def test_wrapping(self):
        for operator in ['|', '&', '^']:
            code = '88 %s 44' % operator
            expected = ('(88 %s\n'
                        ' 44)') % operator
            self.assertFormats(code, expected)


class ComparisonsTestCase(FormatterTestCase):
    """
    [5.9]
    +   comparison    ::=  or_expr ( comp_operator or_expr )*
    +   comp_operator ::=  "<" | ">" | "==" | ">=" | "<=" | "<>" | "!="
                           | "is" ["not"] | ["not"] "in"
    """

    def test_alignment(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'is not', 'in', 'not in']:
            code = '(x %s\ny)' % opt
            expected = 'x %s y' % opt
            self.assertEqual(format_code(code), expected)

    def test_wrapping(self):
        for opt in ['==' , '<' , '>' , '>=' , '<=' , '!=', 'is',
                    'is not', 'in', 'not in']:
            code = 'fun(x, y, z) %s fun(m, n, o)' % opt
            operator_spacing = len(opt) * ' '
            expected = ('fun(x, y,\n'
                        '    z) %s fun(m,\n'
                        '       %s     n,\n'
                        '       %s     o)' % (opt, operator_spacing,
                                              operator_spacing))
            self.assertFormats(code, expected)

    def test_wrapping_raises_exception_when_necessary(self):
        code = 'y >= x'
        self.assertRaises(NotEnoughSpace, lambda: format_code(code, width=4))

    def test_multi_opt_wrapping(self):
        for opt in ['<' , '>' , '==' , '>=' , '<=' , '!=',
                    'is', 'in', 'not in', 'is not']:
            code = 'fun(x, y, z) %s fun(m, n, o) %s fun(p, q, r)' % (opt, opt)
            expected = ('fun(x, y, z) %(opt)s fun(m, n, o) %(opt)s fun(p,\n'
                        '             %(spc)s              %(spc)s     q,\n'
                        '             %(spc)s              %(spc)s     r)' % {'opt': opt,
                                                                              'spc': len(opt)*' '})
            self.assertFormats(code, expected)

    def test_brackets_usage_with_mixed_expression(self):
        code = '(x < y) | (z < v)'
        expected = ('((x < y) |\n'
                    ' (z < v))')
        self.assertFormats(code, expected)


    def test_brackets_usage_in_assignemnt(self):
        code = 'r = (x < y) | (z < v)'
        expected = ('r = ((x < y) |\n'
                    '     (z < v))')
        self.assertFormats(code, expected)

    def test_suffix_is_preserved(self):
        code = 'f(x == 2).a'
        self.assertFormats(code, code)

    def test_forced_formatting_of_simple_expression(self):
        # REGRESSION
        code = dedent("parent is None")
        self.assertFormats(code, code, width=6, force=True)


class BooleanOperationsTestCase(FormatterTestCase):
    """
    [5.10]
    +   or_test  ::=  and_test | or_test "or" and_test
    +   and_test ::=  not_test | and_test "and" not_test
    +   not_test ::=  comparison | "not" not_test
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
            self.assertFormats(code, expected)

    def test_wrapping_brackets_are_used_only_when_necessary(self):
        code = 'x or fun(y,z,v)'
        expected = ('x or fun(y, z,\n'
                    '         v)')
        self.assertFormats(code, expected)

    def test_brackets_are_preserved_in_case_of_attr_ref_expression(self):
        # REGRESSION
        code = dedent("""\
        fun((value or Value()).width)""")
        self.assertFormats(code, code)

    def test_brackets_are_skiped_in_case_of_same_priority_operators(self):
        # REGRESSION
        code = '(x or y) or z'
        expected = dedent("""\
        (x or
         y or
         z)""")
        self.assertFormats(code, expected)


    def test_brackets_are_preserved_in_case_of_mixed_operators_and_broken_line(self):
        # REGRESSION
        code = dedent("""\
            if x <= 0 or y is not None and y >= x:
                pass""")
        expected = dedent("""\
            if (x <= 0 or
                y is not None and y >= x):
                pass""")
        self.assertFormats(code, expected)

    def test_negation_alignment(self):
        code = 'not     x'
        expected = 'not x'
        self.assertFormats(code, expected)

    def test_negation_handles_suffix_correctly(self):
        code = 'fun(not x)'
        self.assertFormats(code, code)


class ConditionalExpressionsTestCase(FormatterTestCase):
    """
    [5.11]
    +   conditional_expression ::=  or_test ["if" or_test "else" expression]
    +   expression             ::=  conditional_expression | lambda_form
    """
    def test_alignment(self):
        self.assertEqual(format_code('x if    c   else y'),
                         'x if c else y')
    def test_wrapping(self):
        code = 'x if c else y'
        expected = ('(x if c\n'
                    '   else y)')
        self.assertFormats(code, expected)

    def test_wrapping_nested_expressions(self):
        # REGRESSION
        code = 'hour = Hour.from_datetime(hour) if isinstance(hour, datetime.datetime) else hour or datetime.datetime.now()'
        expected = ('hour = (Hour.from_datetime(hour) if isinstance(hour, datetime.datetime)\n'
                    '                                 else hour or datetime.datetime.now())')
        self.assertFormats(code, expected)

    def test_inside_bigger_operation_brackets_are_forced(self):
        code = """width - left_block.last_line.width - (2 if with_brackets else 1)"""
        self.assertFormats(code, code)

    def test_expression_with_lambda_form(self):
        code = 'x if t else lambda: r'
        self.assertFormats(code, code)


class LambdasTestCase(FormatterTestCase):
    # FIXME: test old_lambda_form branch
    """
    [5.12] (parameter_list related tests are placed in FuncionDefinitionTestCase)
    +   lambda_form     ::=  "lambda" [parameter_list]: expression
    +   old_lambda_form ::=  "lambda" [parameter_list]: old_expression
    """
    def test_aligment(self):
        code = 'lambda x,y,z:x+y+z'
        expected = 'lambda x, y, z: x + y + z'
        self.assertEqual(format_code(code), expected)

    def test_aligment_without_args(self):
        code = 'lambda : 8'
        expected = 'lambda: 8'
        self.assertEqual(format_code(code), expected)

    def test_formatting_preserves_suffix(self):
        code = 'f(x, lambda: y)'
        self.assertFormats(code, code)


class ExpressionListTestCase(FormatterTestCase):
    """
    [5.13, 6.1]
    +   expression_list ::=  expression ( "," expression )* [","]
    +   expression_stmt ::=  expression_list
    """

    def test_single_element_alignment(self):
        code = 'x,'
        self.assertEqual(format_code(code), '(x,)')

    def test_complex_expression_alignment(self):
        code = '3,8+9,fun(x,y)'
        expected = '3, 8 + 9, fun(x, y)'
        self.assertEqual(format_code(code), expected)

    def test_nested_expression_list_preserves_brackets(self):
        code = '((x,),(y,z))'
        self.assertEqual(format_code(code), '(x,), (y, z)')

    def test_wrapping(self):
        code = '3*8+10,8+9,fun(x,y)'
        expected = ('(3 * 8 +\n'
                    ' 10, 8 + 9,\n'
                    ' fun(x, y))')
        self.assertFormats(code, expected)


class OperatorPrecedenceTestCase(FormatterTestCase):
    """
    [5.15]
    +   lambda                          Lambda expression
    +   if – else                       Conditional expression
    +   or                              Boolean OR
    +   and                             Boolean AND
    +   not x                           Boolean NOT
    -   in, not in, is, is not,
    -   <, <=, >, >=, <>, !=, ==        Comparisons, including membership tests and identity tests
    -   |                               Bitwise OR
    -   ^                               Bitwise XOR
    -   &                               Bitwise AND
    -   <<, >>                          Shifts
    -   +, -                            Addition and subtraction
    -   *, /, //, %                     Multiplication, division, remainder [8]
    -   +x, -x, ~x                      Positive, negative, bitwise NOT
    -   **                              Exponentiation
    -   x[index], x[index:index],
    -   x(arguments...), x.attribute    Subscription, slicing, call, attribute reference
    -   (expressions...),
    -   [expressions...],
    -   {key: value...},
    -   `expressions...`                Binding or tuple display, list display,
    -                                   dictionary display, string conversion
    """
    def test_lambda_vs_if_else(self):
        code = "lambda x: (x if c else y)"
        expected = "lambda x: x if c else y"
        self.assertFormats(code, expected)

        code = "(lambda x: x) if c else y"
        self.assertFormats(code, code)

    def test_if_else_vs_boolean(self):
        code = '(1 + 1) if True else 0'
        expected = '1 + 1 if True else 0'
        self.assertFormats(code, expected)

        code = '1 + (1 if True else 0)'
        self.assertFormats(code, code)

    def test_boolean_operators(self):
        code = 'a or ((not b) and c)'
        expected = 'a or not b and c'
        self.assertFormats(code, expected)

        code = '(a or not (b or d)) and c'
        self.assertFormats(code, code)

    def test_bitwise_operators(self):
        code = '(((8 | 4) ^ (8 | 7)) & 3)'
        expected = '((8 | 4) ^ (8 | 7)) & 3'
        self.assertEqual(format_code(code), expected)

    def test_brackets_are_preserved_for_different_operators_with_same_precendence(self):
        code = '8 / (2 * 4)'
        self.assertFormats(code, code)

    def test_brackets_are_skiped_in_case_of_same_operators(self):
        code = '1 + 1 + 1 + 1 + 1'
        self.assertFormats(code, code)


class AssignmentTestCase(FormatterTestCase):
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
        self.assertFormats(code, expected)

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


class AugmentAssignmentTestCase(FormatterTestCase):
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
        self.assertFormats(code, expected)


class AssertTestCase(FormatterTestCase):
    """
    [6.3]
    assert_stmt ::=  "assert" expression ["," expression]
    """

    def test_simple_form_alignemnt(self):
        code = 'assert       x > 0'
        expected = 'assert x > 0'
        self.assertFormats(code, expected)

    def test_message_form_alignment(self):
        code = "assert  x > 0   ,  'x should be positive, integer value'"
        expected = "assert x > 0, 'x should be positive, integer value'"
        self.assertFormats(code, expected)


class SimpleStatementsTestCase(FormatterTestCase):
    """
    [6.4, 6.5, 6.7, 6.8, 6.9, 6.10, 6.11, 6.13]
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

    def test_return_alignment(self):
        code = 'return x+x'
        expected = 'return x + x'
        self.assertEqual(format_code(code), expected)

    def test_return_without_value(self):
        code = 'return'
        self.assertFormats(code, code)

    def test_continue(self):
        code = 'continue'
        self.assertFormats(code, code)

    def test_raise_simple_statement(self):
        code = "raise   Exception('oh no!')"
        expected = "raise Exception('oh no!')"
        self.assertFormats(code, expected)


class PrintStatementTestCase(FormatterTestCase):
    """
    [6.6]
        print_stmt ::=  "print" ([expression ("," expression)* [","]]
                        | ">>" expression [("," expression)+ [","]])
    """
    # FIXME: a lot more to test
    def test_simple_statement(self):
        code = "print 'test'"
        self.assertFormats(code, code)


class ImportStatementTestCase(FormatterTestCase):
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
        code = 'import    module1, module2, module3, module4, module5, module6'
        expected = ('import (module1, module2,\n'
                    '        module3, module4,\n'
                    '        module5, module6)')
        self.assertFormats(code, expected)

    def test_from_form_wrapping(self):
        code = 'from module import Class1, Class2, Class3, Class4'
        expected = ('from module import (Class1, Class2,\n'
                    '                    Class3, Class4)')
        self.assertFormats(code, expected)


class IfTestCase(FormatterTestCase):
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
        self.assertFormats(code, expected)

class WhileStatementTestCase(FormatterTestCase):
    """
    [7.2]
    while_stmt ::=  "while" expression ":" suite
                    ["else" ":" suite]
    """
    def test_simple_form_alignment(self):
        code = dedent("""\
            while True:

                pass
        """)
        expected = dedent("""\
            while True:
                pass""")
        self.assertFormats(code, expected)

    def test_while_else_form_alignment(self):
        code = dedent("""\
            while True:

                pass
            else:

                pass
        """)
        expected = dedent("""\
            while True:
                pass
            else:
                pass""")
        self.assertFormats(code, expected)


class ForStatementTestCase(FormatterTestCase):
    """
    [7.3]
    for_stmt ::=  "for" target_list "in" expression_list ":" suite
                  ["else" ":" suite]
    """

    def test_simple_for_statement(self):
        code = ('for   p   in    (1,2,3):\n   p')
        expected = 'for p in 1, 2, 3:\n%sp' % CodeLine.INDENT
        self.assertEqual(format_code(code), expected)


class TryExceptStatementTestCase(FormatterTestCase):
    """
    [7.4]
    try_stmt  ::=  try1_stmt | try2_stmt
    try1_stmt ::=  "try" ":" suite
                   ("except" [expression [("as" | ",") target]] ":" suite)+
                   ["else" ":" suite]
                   ["finally" ":" suite]
    try2_stmt ::=  "try" ":" suite
                   "finally" ":" suite
    """
    def test_except_alignement(self):
        code = dedent("""\
        try: pass
        except: pass""")
        expected = dedent("""\
        try:
            pass
        except:
            pass""")
        self.assertFormats(code, expected)

    def test_except_else_alignment(self):
        code = dedent("""\
        try: pass
        except: pass
        else: pass""")
        expected = dedent("""\
        try:
            pass
        except:
            pass
        else:
            pass""")
        self.assertFormats(code, expected)

    def test_except_else_finally_alignment(self):
        code = dedent("""\
        try: pass
        except: pass
        else: pass
        finally: pass""")
        expected = dedent("""\
        try:
            pass
        except:
            pass
        else:
            pass
        finally:
            pass""")
        self.assertFormats(code, expected)

    def test_except_alignment(self):
        code = dedent("""\
        try:
            pass
        except  Exception,   e:
            pass""")
        expected = dedent("""\
        try:
            pass
        except Exception as e:
            pass""")
        self.assertFormats(code, expected)


class FunctionDefinitionTestCase(FormatterTestCase):
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
        self.assertFormats(code, expected)

    def test_identifiers_parameter_list_wrapping_with_multiple_params_per_line(self):
        code = ('def fun(x,y,z,u,v,w,t):\n'
                '    pass')
        expected = ('def fun(x, y, z,\n'
                    '        u, v, w,\n'
                    '        t):\n'
                    '    pass')
        self.assertFormats(code, expected)

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
        self.assertFormats(code, expected)

    def test_vararg_alignment(self):
        code = dedent("""\
            def fun( *  args ):
                pass""")
        expected = dedent("""\
            def fun(*args):
                pass""")
        self.assertFormats(code, expected)


class ClassDefinitionTestCase(FormatterTestCase):
    """
    [7.7]
    classdef    ::=  "class" classname [inheritance] ":" suite
    inheritance ::=  "(" [expression_list] ")"
    classname   ::=  identifier
    """
    def test_definition_alignment(self):
        code = 'class     A:\n    pass'
        self.assertEqual(format_code(code), 'class A:\n    pass')

    def test_definition_with_inheritance_alignment(self):
        code = 'class    A(  Base1, Base2,    Base3, Base4    ):\n    pass'
        expected = ('class A(Base1,\n'
                    '        Base2,\n'
                    '        Base3,\n'
                    '        Base4):\n'
                    '    pass')
        self.assertFormats(code, expected)


class FuzzyTestCase(FormatterTestCase):
    """
    [665.999]
    Some regression/random code samples formatting tests
    """

    def test_README_example(self):
        code = 'foo(f=8, s=bar(x=9, y=10, z=20))'
        self.assertRaises(NotEnoughSpace,
                          lambda: format_code(code, width=10))
        expected = dedent("""\
            foo(f=8,
                s=bar(x=9,
                      y=10,
                      z=20))""")
        self.assertFormats(code, expected, width=10, force=True)

    def test_suffixes_aggregation(self):
        # REGRESSION
        code = dedent("""\
            ProductAdmin(formsets=[FormsetContainer(inlineformset_factory(fields=['image', 'color',
                                                                                  'on_home_page',
                                                                                  'on_product_page']))])""")
        self.assertFormats(code, code)

    def test_nested_statement_formatting(self):
        code = dedent("""\
        admin_site = AdminSite([
                ProductAdmin(model=TShirt, extra_context=admin_context, formsets=[
                    FormsetContainer(_('Variants'), 'variants',
                                     inlineformset_factory(TShirt, TShirtVariant, extra=0, can_delete=False,
                                                           max_num=len(TShirtVariant.COLOR_CHOICES) * len(TShirtVariant.SIZE_CHOICES) * len(TShirtVariant.FASHION_CHOICES),
                                                           fields=['available'])),
                    FormsetContainer(_('Photos'), 'photos',
                                     inlineformset_factory(TShirt, TShirtImage, extra=1,
                                                           form=modelform_factory(TShirtImage,
                                                                                  forms.BaseImageForm),
                                                           fields=['image', 'color', 'on_home_page', 'on_product_page'])),
                ]),
                ProductAdmin(model=BabyBodySuit, extra_context=admin_context, formsets=[
                    FormsetContainer(_('Variants'), 'variants',
                                     inlineformset_factory(BabyBodySuit, BabyBodySuitVariant, extra=0, can_delete=False,
                                                           max_num=len(BabyBodySuitVariant.SIZE_CHOICES),
                                                           fields=['available'])),
                    FormsetContainer(_('Photos'), 'photos',
                                     inlineformset_factory(BabyBodySuit, BabyBodySuitImage, extra=1,
                                                           form=modelform_factory(BabyBodySuitImage,
                                                                                  forms.BaseImageForm),
                                                           fields=['image', 'on_home_page', 'on_product_page'])),
                ]),
        ])""")
        expected = dedent("""\
        admin_site = AdminSite([ProductAdmin(model=TShirt, extra_context=admin_context,
                                             formsets=[FormsetContainer(_('Variants'), 'variants',
                                                                        inlineformset_factory(TShirt, TShirtVariant, extra=0,
                                                                                              can_delete=False,
                                                                                              max_num=len(TShirtVariant.COLOR_CHOICES) *
                                                                                                      len(TShirtVariant.SIZE_CHOICES) *
                                                                                                      len(TShirtVariant.FASHION_CHOICES),
                                                                                              fields=['available'])),
                                                       FormsetContainer(_('Photos'), 'photos',
                                                                        inlineformset_factory(TShirt, TShirtImage, extra=1,
                                                                                              form=modelform_factory(TShirtImage,
                                                                                                                     forms.BaseImageForm),
                                                                                              fields=['image', 'color', 'on_home_page',
                                                                                                      'on_product_page']))]),
                                ProductAdmin(model=BabyBodySuit, extra_context=admin_context,
                                             formsets=[FormsetContainer(_('Variants'), 'variants',
                                                                        inlineformset_factory(BabyBodySuit, BabyBodySuitVariant, extra=0,
                                                                                              can_delete=False,
                                                                                              max_num=len(BabyBodySuitVariant.SIZE_CHOICES),
                                                                                              fields=['available'])),
                                                       FormsetContainer(_('Photos'), 'photos',
                                                                        inlineformset_factory(BabyBodySuit, BabyBodySuitImage, extra=1,
                                                                                              form=modelform_factory(BabyBodySuitImage,
                                                                                                                     forms.BaseImageForm),
                                                                                              fields=['image', 'on_home_page',
                                                                                                      'on_product_page']))])])""")
        self.assertFormats(code, expected, width=80, force=True)

    def test_long_list_formatting(self):
        code = dedent("""\
            [
                ('Alternative', 'Alternative'),
                ('Blues', 'Blues'),
                ('Classical', 'Classical'),
                ('Country', 'Country'),
                ('Decades', 'Decades'),
                ('Easy Listening', 'Easy Listening'),
                ('Electronic', 'Electronic'),
                ('Folk', 'Folk'),
                ('Inspirational', 'Inspirational'),
                ('International', 'International'),
                ('Jazz', 'Jazz'),
                ('Latin', 'Latin'),
                ('Metal', 'Metal'),
                ('Misc', 'Misc'),
                ('New Age', 'New Age'),
                ('Pop', 'Pop'),
                ('Public Radio', 'Public Radio'),
                ('R&B and Urban', 'R&B and Urban'),
                ('Rap', 'Rap'),
                ('Reggae', 'Reggae'),
                ('Rock', 'Rock'),
                ('Seasonal and Holiday', 'Seasonal and Holiday'),
                ('Soundtracks', 'Soundtracks'),
                ('Talk', 'Talk'),
                ('Themes', 'Themes'),
            ]""")
        expected = dedent("""\
            [('Alternative',
              'Alternative'), ('Blues',
                               'Blues'),
             ('Classical',
              'Classical'), ('Country',
                             'Country'),
             ('Decades', 'Decades'),
             ('Easy Listening',
              'Easy Listening'),
             ('Electronic',
              'Electronic'), ('Folk',
                              'Folk'),
             ('Inspirational',
              'Inspirational'),
             ('International',
              'International'), ('Jazz',
                                 'Jazz'),
             ('Latin',
              'Latin'), ('Metal',
                         'Metal'),
             ('Misc',
              'Misc'), ('New Age',
                        'New Age'),
             ('Pop',
              'Pop'), ('Public Radio',
                       'Public Radio'),
             ('R&B and Urban',
              'R&B and Urban'), ('Rap',
                                 'Rap'),
             ('Reggae',
              'Reggae'), ('Rock', 'Rock'),
             ('Seasonal and Holiday',
              'Seasonal and Holiday'),
             ('Soundtracks',
              'Soundtracks'), ('Talk',
                               'Talk'),
             ('Themes', 'Themes')]""")
        self.assertFormats(code, expected, width=30)


class FormattersUnitTests(FormatterTestCase):

    def test_single_argument_call_is_not_formatable(self):
        code = "len(TShirtVariant.COLOR_CHOICES)"
        call_statement = ast.parse(code).body[0].value
        call_formatter = formatters[type(call_statement)](call_statement,
                                                           formatters_register=formatters)
        self.assertFalse(call_formatter.formatable)

    def test_binary_arithmetic_operation_is_passing_suffix_to_subexpression(self):
        code = 'f(x - y)'
        self.assertFormats(code, code)

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


_test_loader = unittest.TestLoader()
test_suite = _test_loader.loadTestsFromModule(tests)
test_suite.addTests(_test_loader.loadTestsFromModule(sys.modules[__name__]))

# FIXME: strange line break in arithmetic operation and
#        unecessary brackets in conditional expression
#   block.merge(lower_formatter.format_code(
#                                   width - block.width -
#                                   1, suffix=(suffix if not self.expr.upper and
#                                                        not self.expr.step
#                                                     else None)))
