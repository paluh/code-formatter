from textwrap import dedent

from .. import base
from ..exceptions import NotEnoughSpace
from ..utils import FormattersTestCase

from . import (CallFormatterWithLinebreakingFallback, LinebreakingAttributeFormatter,
               ListOfExpressionsWithSingleLineContinuationsFormatter,
               UnbreakableTupleFormatter)


class ListOfExpressionsWithSingleLineContinuationsFormatterTestCase(FormattersTestCase):

    formatters_register = base.formatters.copy()
    formatters_register.register(ListOfExpressionsWithSingleLineContinuationsFormatter)

    def test_line_breaking_can_occure_only_on(self):
        code = dedent("""\
            (var1 +
             var2, var3,
             var4)""")
        expected = dedent("""\
            (var1 + var2,
             var3, var4)""")
        self.assertFormats(code, expected)

    def test_nested_list_wrapping(self):
        # REGRESSION
        code = dedent("""\
            [['/m', 'm'], ['/s',
                           's']]""")
        self.assertFormats(code, code)


class CustomCallFormatterMixedWithListOfExpressionsWithSingleLineContinuationsFormatterTestCase(FormattersTestCase):

    formatters_register = base.formatters.copy()
    formatters_register.register(ListOfExpressionsWithSingleLineContinuationsFormatter,
                                 LinebreakingAttributeFormatter)

    def test_line_continuation_formatter_mixed_with_line_breaking_attribute_formatter(self):
        code = dedent("""\
            instance.attribute_instance.method(key1=value1, key2=value2, key3=value3,
                                      list_param=['element 1', 'element 2',
                                                  'element 3'], key4=v4)""")
        # we expected mixed effect: attribute line breaking + line
        expected = dedent("""\
            (instance.attribute_instance
                     .method(key1=value1, key2=value2, key3=value3,
                             list_param=['element 1', 'element 2',
                                         'element 3'],
                             key4=v4))""")
        self.assertFormats(code, expected)

    def test(self):
        code = dedent("""\
                r.m(_register=register,
                    parent=parent,
                    func_formatter=fun)""")
        self.assertFormats(code, code, width=37)


class UnbreakableTupleFormatterTestCase(FormattersTestCase):

    formatters_register = base.formatters.copy()
    formatters_register.register(UnbreakableTupleFormatter)

    def test_alignment(self):
        code = '(   1,   2,  3)'
        expected = '1, 2, 3'
        self.assertFormats(code, expected, width=3, force=True)

    def test_wrapping(self):
        code = dedent("""\
            [('Alternative', 'Alternative'),
             ('Blues', 'Blues'),
             ('Classical', 'Classical'),
             ('Country', 'Country'),
             ('Decades', 'Decades')]""")
        self.assertFormats(code, code, width=3, force=True)


class CallFormatterWithLinebreakingFallback(FormattersTestCase):

    formatters_register = base.formatters.copy()
    formatters_register.register(CallFormatterWithLinebreakingFallback)

    def test_wrapping(self):
        code = dedent("""\
                function(
                    1, 2)""")
        self.assertFormats(code, code)

    def test_formats_line_continuation_if_there_is_enough_space(self):
        code = 'function(1, 2)'
        self.assertFormats(code, code)

    def test_empty_argument_list_doesnt_break(self):
        code = 'function()'
        not_expected = dedent("""\
            function(
                )""")
        self.assertRaises(
            NotEnoughSpace, lambda: self.assertFormats(
                                        code, not_expected))

    def test_indent_is_counted_from_last_attribute_ref_subexpression(self):
        code = 'instance.attr.attr_method(1, 2)'
        expected = dedent("""\
             instance.attr.attr_method(
                              1, 2)""")
        self.assertFormats(code, expected)

    def test_breaking_occur_only_when_there_are_at_least_to_columns_profit(self):
        code = dedent("""\
             test(
                 1)""")
        self.assertRaises(NotEnoughSpace, lambda: self.assertFormats(code, code))


class LinebreakingAttributeFormatterTestCase(FormattersTestCase):
    """
    In general primary expression can produce attributeref expression
        primary             ::=  atom | attributeref ...

    According to this here are all cases where attributeref can occure as subexpression:

    [5.3.1]
    +   attributeref        ::=  primary "." identifier

    [5.3.2]
    +   subscription        ::=  primary "[" expression_list "]"

    [5.3.3]
    +   slicing             ::=  simple_slicing | extended_slicing
    +   simple_slicing      ::=  primary "[" short_slice "]"
    +   extended_slicing    ::=  primary "[" slice_list "]"

    [5.3.4]
    +   call                ::=  primary "(" [argument_list [","]
    """
    formatters_register = base.formatters.copy()
    formatters_register.register(LinebreakingAttributeFormatter)

    def test_identifiers_wrapping(self):
        code = 'fun().identifier1.identifier2'
        expected = dedent("""\
            (fun().identifier1
                  .identifier2)""")
        self.assertFormats(code, expected)

    def test_call_wrapping(self):
        code = 'fun().method1().method2().method3()'
        expected = dedent("""\
            (fun().method1()
                  .method2()
                  .method3())""")
        self.assertFormats(code, expected)

    def test_wrapping_skips_parentheses_inside_function_call(self):
        code = 'fun(instance.method1().method2())'
        expected = dedent("""\
            fun(instance.method1()
                        .method2())""")
        self.assertFormats(code, expected)

    def test_wrapping_skips_parentheses_inside_list(self):
        code = '[instance.method1().method2()]'
        expected = dedent("""\
            [instance.method1()
                     .method2()]""")
        self.assertFormats(code, expected)

    def test_wrapping_skips_parentheses_inside_nested_binary_operation(self):
        code = 'fun(8 + instance.method1().method2())'
        expected = dedent("""\
            fun(8 + instance.method1()
                            .method2())""")
        self.assertFormats(code, expected)

    def test_wrapping_uses_parentheses_inside_binary_operation_when_necessary(self):
        code = '8 + instance.method1().method2()'
        expected = dedent("""\
            8 + (instance.method1()
                         .method2())""")
        self.assertFormats(code, expected)

    def test_suffix_passing_for_single_element_chain(self):
        code = 'method1(instance2.method2)'
        self.assertFormats(code, code)

    def test_wrapping_is_done_only_when_necessary(self):
        code = 'fun().method1().method2().method3()'
        expected = dedent("""\
            (fun().method1().method2()
                  .method3())""")
        self.assertFormats(code, expected)

    def test_wrapping_uses_parentheses_only_when_necessary(self):
        code = 'instance.method1().method2()'
        self.assertFormats(code, code)

    def test_subscription_wrapping(self):
        code = 'identifier1[value1].identifier2[value2].identifier3[value3]'
        expected = dedent("""\
            (identifier1[value1].identifier2[value2]
                                .identifier3[value3])""")
        self.assertFormats(code, expected)

    def test_slicing_wrapping(self):
        code = 'identifier1[lower1:upper1:step1].identifier2[lower2:upper2:step2].identifier3[lower3:]'
        expected = dedent("""\
            (identifier1[lower1:upper1:step1].identifier2[lower2:upper2:step2]
                                             .identifier3[lower3:])""")
        self.assertFormats(code, expected)
