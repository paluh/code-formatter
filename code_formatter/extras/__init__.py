import ast

from .. import base
from ..code import CodeBlock, CodeLine
from ..exceptions import NotEnoughSpace

__all__ = ['UnbreakableListOfExpressionFormatter', 'LinebreakingListOfExpressionFormatter',
           'UnbreakableTupleFormatter', 'LinebreakingAttributeFormatter']


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


# FIXME: we should refactor this so "fallback" behaviour will be provided
#        by generic Formatter aggregator
class CallFormatterWithLinebreakingFallback(base.CallFormatter):

    formatable = True

    def _format_code(self, width, suffix):
        try:
            return super(CallFormatterWithLinebreakingFallback, self)._format_code(width, suffix)
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
    """This is really expermiental formatter (it hacks ast structure in many places).
    It handles line breaking on attributes references - for example this piece:

         instance.method().attribute

    can be formatted into:

         (instance.method()
                  .attribute)

    If you want to use it you have to replace also CallFormatter and SubscriptionFormatter - for example:

        >>> from ast import Attribute, Call, Subscript
        >>> from code_formatter import base, format_code
        >>> from code_formatter.extra import LinebreakingAttributeFormatter
        >>> formatters = dict(base.formatters,
        ...                   **{Call: LinebreakingAttributeFormatter.CallFormatter,
        ...                      Attribute: LinebreakingAttributeFormatter,
        ...                      Subscript: LinebreakingAttributeFormatter.SubscriptionFormatter})
        >>> print format_code('instance.identifier.identifier()',
        ...                   formatters_register=formatters, width=3, force=True)
        (instance.identifier
                 .identifier())
    """

    class CallFormatter(CallFormatterWithLinebreakingFallback):

        def __new__(cls, expr, formatters_register, parent):
            if isinstance(expr.func, ast.Attribute):
                return LinebreakingAttributeFormatter(expr, formatters_register, parent)
            return super(LinebreakingAttributeFormatter.CallFormatter, cls).__new__(cls,
                                                                                    formatters_register,
                                                                                    parent)

    class SubscriptionFormatter(base.SubscriptionFormatter):

        def __new__(cls, expr, formatters_register, parent):
            if isinstance(expr.value, ast.Attribute):
                return LinebreakingAttributeFormatter(expr, formatters_register, parent)
            return super(LinebreakingAttributeFormatter.SubscriptionFormatter, cls).__new__(cls,
                                                                                            formatters_register,
                                                                                            parent)


    class _IdentifierFormatter(base.CodeFormatter):

        def __init__(self, identifier, formatters_register, parent):
            self.identifier = identifier
            self.parent = parent
            super(LinebreakingAttributeFormatter._IdentifierFormatter,
                  self).__init__(formatters_register)

        def _format_code(self, width, suffix=None):
            block = CodeBlock.from_tokens(self.identifier)
            if suffix is not None:
                block.merge(suffix)
            return block


    class _CallFormatter(base.CallFormatter):

        def __init__(self, func_formatter, *args, **kwargs):
            super(LinebreakingAttributeFormatter._CallFormatter, self).__init__(*args, **kwargs)
            self._func_formatter = func_formatter


    class _SubscriptionFormatter(base.SubscriptionFormatter):

        def __init__(self, value_formatter, *args, **kwargs):
            super(LinebreakingAttributeFormatter._SubscriptionFormatter, self).__init__(*args, **kwargs)
            self._value_formatter = value_formatter

    def __init__(self, *args, **kwargs):
        super(base.AttributeFormatter, self).__init__(*args, **kwargs)
        self._attrs_formatters = []
        expr = self.expr
        while (isinstance(expr, ast.Attribute) or
               isinstance(expr, ast.Call) and isinstance(expr.func,
                                                         ast.Attribute) or
               isinstance(expr, ast.Subscript) and isinstance(expr.value,
                                                              ast.Attribute)):
            if isinstance(expr, ast.Attribute):
                self._attrs_formatters.insert(0,
                                              LinebreakingAttributeFormatter._IdentifierFormatter(expr.attr,
                                                                                                  self.formatters_register,
                                                                                                  parent=self))
                expr = expr.value
            elif isinstance(expr, ast.Call):
                func_formatter = LinebreakingAttributeFormatter._IdentifierFormatter(
                                                                    (expr.func
                                                                         .attr),
                                                                    self.formatters_register,
                                                                    parent=self)
                self._attrs_formatters.insert(0,
                                              LinebreakingAttributeFormatter._CallFormatter(func_formatter, expr,
                                                                                            self.formatters_register,
                                                                                            parent=self))
                expr = expr.func.value
            elif isinstance(expr, ast.Subscript):
                value_formatter = LinebreakingAttributeFormatter._IdentifierFormatter(
                                                                    (expr.value.attr),
                                                                    self.formatters_register,
                                                                    parent=self)
                (self._attrs_formatters
                     .insert(0,
                             LinebreakingAttributeFormatter._SubscriptionFormatter(
                                                                value_formatter, expr,
                                                                self.formatters_register,
                                                                parent=self)))
                expr = expr.value.value
        self.value_formatter = self.get_formatter(expr)

    def _format_code(self, width, suffix):
        def _format(inside_scope, prefix=None):
            block = CodeBlock.from_tokens(prefix) if prefix else CodeBlock()
            block.merge(self.value_formatter.format_code(width - block.width))
            attr_ref_indent = block.width
            separator = CodeBlock.from_tokens('.')
            block.merge(separator.copy())
            block.merge(self._attrs_formatters[0].format_code(
                                                      width - attr_ref_indent,
                                                      suffix=(suffix if len(self._attrs_formatters) == 1
                                                                     else None)))
            for attr_formatter in self._attrs_formatters[1:]:
                s = suffix if self._attrs_formatters[-1] == attr_formatter else None
                try:
                    attr_block = attr_formatter.format_code(width - block.last_line.width - separator.width, suffix=s)

                except NotEnoughSpace:
                    if not inside_scope:
                        raise
                    block.extend(separator, indent=attr_ref_indent)
                    block.merge(attr_formatter.format_code(width - attr_ref_indent, suffix=s))
                else:
                    block.merge(separator)
                    block.merge(attr_block)
            return block
        try:
            return _format(self._inside_scope())
        except NotEnoughSpace:
            if self._inside_scope():
                raise
        suffix = self._extend_suffix(suffix, ')')
        return _format(True, '(')