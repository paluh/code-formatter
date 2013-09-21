from textwrap import dedent

from .. import base
from ..exceptions import NotEnoughSpace
from ..utils import FormatterTestCase

from . import (CallFormatterWithLinebreakingFallback, LinebreakingAttributeFormatter,
               UnbreakableTupleFormatter)


class CustomFormatterTestCase(FormatterTestCase):

    custom_formatters = []

    def setUp(self):
        self.formatters_register = dict(base.formatters, **{F.ast_type: F for F in self.custom_formatters})

    def assertFormats(self, code, expected, width=None, force=False):
        assert self.formatters_register is not None, ('You have to setup custom '
                                                      '`formatters_register` attribute')
        super(CustomFormatterTestCase, self).assertFormats(code, expected, width=width, force=force,
                                                           formatters_register=self.formatters_register)

class UnbreakableTupleFormatterTestCase(CustomFormatterTestCase):

    custom_formatters = [UnbreakableTupleFormatter]

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


class CallFormatterWithLinebreakingFallback(CustomFormatterTestCase):

    custom_formatters = [CallFormatterWithLinebreakingFallback]

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


class LinebreakingAttributeFormatterTestCase(CustomFormatterTestCase):
    """
    In general primary expression can produce attributeref expression
        primary             ::=  atom | attributeref ...

    According to this here are all cases where attributeref can occure as subexpression:

    [5.3.1]
    +   attributeref        ::=  primary "." identifier

    [5.3.2]
    +   subscription        ::=  primary "[" expression_list "]"

    [5.3.3]
    -   slicing             ::=  simple_slicing | extended_slicing
    -   simple_slicing      ::=  primary "[" short_slice "]"
    -   extended_slicing    ::=  primary "[" slice_list "]"

    [5.3.4]
    +   call                ::=  primary "(" [argument_list [","]
    """

    custom_formatters = [LinebreakingAttributeFormatter,
                         LinebreakingAttributeFormatter.CallFormatter,
                         LinebreakingAttributeFormatter.SubscriptionFormatter]

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

    def test_suffix_passing_for_single_element_chain(self):
        code = 'method1(instance2.method2)'
        self.assertFormats(code, code)

    def test_wrapping_is_done_only_when_necessary(self):
        code = 'fun().method1().method2().method3()'
        expected = dedent("""\
            (fun().method1().method2()
                  .method3())""")
        self.assertFormats(code, expected)

    def test_wrapping_uses_brackets_only_when_necessary(self):
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
