import ast

from .. import base
from ..code import CodeBlock, CodeLine
from ..exceptions import NotEnoughSpace


class UnbreakableListOfExpressionFormatter(base.ListOfExpressionsFormatter):

    def _format_code(self, width, suffix, line_width=None):
        line_width = line_width or width
        return self._format_line_continuation(width, suffix, line_width)


class LinebreakingListOfExpressionFormatter(base.ListOfExpressionsFormatter):

    def _format_code(self, width, suffix, line_width=None):
        return self._format_line_break(width, suffix, line_width or width)


class UnbreakableTupleFormatter(base.TupleFormatter):
    """Keep tuples in one line - for example:
        [('Alternative', 'Alternative'),
         ('Blues', 'Blues'),
         ('Classical', 'Classical')]
    """
    ListOfExpressionsFormatter = UnbreakableListOfExpressionFormatter


class CallFormatterWithLineBreakingFallback(base.CallFormatter):

    formatable = True

    def _format_code(self, width, suffix):
        try:
            return super(CallFormatterWithLineBreakingFallback, self)._format_code(width, suffix)
        except NotEnoughSpace:
             if not self._arguments_formatters:
                raise
        suffix = self._extend_suffix(suffix, ')')
        for i in range(width+1):
            curr_width = width - i
            block = self._func_formatter.format_code(curr_width)
            block.append_tokens('(')
            try:
                subblock = self._arguments_formatter.format_code(width -
                                                                 len(CodeLine.INDENT),
                                                                 suffix=suffix)
            except NotEnoughSpace:
                continue
            else:
                # FIXME: this is really ugly way to detect last method access subexpression
                indent = max(unicode(block.last_line).rfind('.'), 0) + len(CodeLine.INDENT)
                block.extend(subblock, indent=indent)
                break
        return block


class LinebreakingAttributeFormatter(base.AttributeFormatter):

    class IdentifierFormatter(base.CodeFormatter):

        def __init__(self, identifier, formatters_register):
            self.identifier = identifier
            super(LinebreakingAttributeFormatter.IdentifierFormatter,
                  self).__init__(formatters_register)

        def _format_code(self, width, suffix=None):
            block = CodeBlock.from_tokens(self.identifier)
            if suffix is not None:
                block.merge(suffix)
            return block

    def __init__(self, *args, **kwargs):
        super(base.AttributeFormatter, self).__init__(*args, **kwargs)
        self._attrs_formatters = [LinebreakingAttributeFormatter.IdentifierFormatter(self.expr.attr, self.formatters_register)]
        expr = self.expr
        if isinstance(expr.value, ast.Attribute):
            while isinstance(expr.value, ast.Attribute):
                expr = expr.value
                self._attrs_formatters.insert(0,
                                            LinebreakingAttributeFormatter.IdentifierFormatter(
                                                                                     expr.attr, self.formatters_register))
        self.value_formatter = self.get_formatter(expr.value)

    def _format_code(self, width, suffix):
        block = CodeBlock()
        if len(self._attrs_formatters) > 1:
            block.append_tokens('(')
            suffix = self._extend_suffix(suffix, ')')
        block.merge(self.value_formatter.format_code(width - block.width))
        attr_ref_indent = block.width
        separator = CodeBlock.from_tokens('.')
        block.merge(separator.copy())
        block.merge(self._attrs_formatters[0].format_code(width - attr_ref_indent))
        for attr_formatter in self._attrs_formatters[1:]:
            block.extend(separator, indent=attr_ref_indent)
            s = suffix if self._attrs_formatters[-1] == attr_formatter else None
            block.merge(attr_formatter.format_code(width - attr_ref_indent, suffix=s))
        return block
