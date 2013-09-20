from .. import base


class UnbreakableListOfExpressionFormatter(base.ListOfExpressionsFormatter):

    ast_type = base.TupleFormatter.ast_type

    def _format_code(self, width, suffix, line_width=None):
        line_width = line_width or width
        return self._format_line_continuation(width, suffix, line_width)


class UnbreakableTupleFormatter(base.TupleFormatter):

    ast_type = base.TupleFormatter.ast_type

    def __init__(self, *args, **kwargs):
        super(UnbreakableTupleFormatter, self).__init__(*args, **kwargs)
        items = [v for v in self.expr.elts]
        self._items_formatter = UnbreakableListOfExpressionFormatter.from_expressions(items, self)
