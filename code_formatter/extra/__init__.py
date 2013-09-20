from .. import base


class UnbreakableListOfExpressionFormatter(base.ListOfExpressionsFormatter):

    def _format_code(self, width, suffix, line_width=None):
        line_width = line_width or width
        return self._format_line_continuation(width, suffix, line_width)


class UnbreakableTupleFormatter(base.TupleFormatter):

    ListOfExpressionsFormatter = UnbreakableListOfExpressionFormatter
