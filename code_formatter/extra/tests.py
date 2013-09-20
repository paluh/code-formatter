from textwrap import dedent

from ..utils import FormatterTestCase
from .. import base

from . import UnbreakableTupleFormatter


class CustomFormatterTestCase(FormatterTestCase):

    formatters_register = None

    def assertFormats(self, code, expected, width=None, force=False):
        assert self.formatters_register is not None, 'You have to setup custom `formatters_register` attribute'
        super(CustomFormatterTestCase, self).assertFormats(code, expected, width=width, force=force,
                                                           formatters_register=self.formatters_register)

class UnbreakableTupleFormatterTestCase(CustomFormatterTestCase):

    formatters_register = dict(base.formatters,
                               **{UnbreakableTupleFormatter.ast_type: UnbreakableTupleFormatter})


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
