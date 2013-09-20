import difflib
import unittest

from . import format_code
from .base import formatters

class FormatterTestCase(unittest.TestCase):

    def assertFormats(self, code, expected, formatters_register=formatters, width=None, force=False):
        width = width if width is not None else max(len(l) for l in expected.split('\n'))
        try:
            formated = format_code(code, width=width, force=force,
                                   formatters_register=formatters_register)
            self.assertEqual(formated, expected)
        except AssertionError:
            print '\n'.join(difflib.unified_diff(expected.split('\n'), formated.split('\n'), fromfile='expected', tofile='formated'))
            raise

