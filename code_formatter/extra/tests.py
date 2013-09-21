from textwrap import dedent

from .. import base
from ..exceptions import NotEnoughSpace
from ..utils import FormatterTestCase

from . import CallFormatterWithLineBreakingFallback, UnbreakableTupleFormatter


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


class CallFormatterWithLineBreakingFallback(CustomFormatterTestCase):

    custom_formatters = [CallFormatterWithLineBreakingFallback]

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

    def test_indent_is_counted_from_last_attribute_access_subexpression(self):
        code = 'instance.attr.attr_method(1, 2)'
        expected = dedent("""\
             instance.attr.attr_method(
                              1, 2)""")
        self.assertFormats(code, expected)
