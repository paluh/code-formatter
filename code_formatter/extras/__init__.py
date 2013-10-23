"""All formatters from this pacakge should be easily mixed whith default ones using this pattern:

    >>> from code_formatter.base import formatters
    >>> from code_formatter import extras
    >>> custom_formatters = formatters.copy()
    >>> custom_formatters.register(extras.UnbreakableTupleFormatter,
                                   extras.ListOfExpressionsWithSingleLineContinuationsFormatter)
"""
import ast

from .. import base
from ..code import CodeBlock, CodeLine
from ..exceptions import NotEnoughSpace

__all__ = ['UnbreakableListOfExpressionFormatter', 'LinebreakingListOfExpressionFormatter',
           'UnbreakableTupleFormatter', 'LinebreakingAttributeFormatter']


class ListOfExpressionsWithSingleLineContinuationsFormatter(base.ListOfExpressionsFormatter):

    multiline_continuation = False


class UnbreakableListOfExpressionFormatter(base.ListOfExpressionsFormatter):

    def _format_code(self, width, continuation, suffix, line_width=None):
        line_width = line_width or width
        return self._format_line_continuation(width, continuation, suffix, line_width)


class LinebreakingListOfExpressionFormatter(base.ListOfExpressionsFormatter):

    def _format_code(self, width, continuation, suffix, line_width=None):
        return self._format_line_break(width, continuation, suffix, line_width or width)


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

    def _format_code(self, width, continuation, suffix):
        try:
            return super(CallFormatterWithLinebreakingFallback, self)._format_code(width, continuation, suffix)
        except NotEnoughSpace:
             if not self._arguments_formatters:
                raise
        suffix = self._append_to_suffix(suffix, ')')
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
                if indent + 1 >= block.last_line.width:
                    continue
                block.extend(subblock, indent=indent)
                break
        return block


class LinebreakingAttributeFormatter(base.AttributeFormatter):
    """This is really expermiental (as it API requires cleanup and it hacks
    `ast` structure in many places) formatter.
       It handles line breaking on attributes references, and alignes indentation to
     first attribute reference in expression. For example this piece:

         instance.method().attribute

    can be formatted into:

         (instance.method()
                  .attribute)

    During registration this formatter replaces `AttributeFormatter` (which is quite obvious) but also
    `CallFormatter` and `SubscriptionFormatter` by derived formatters from current formatters - so simple
    `formatters.register(LinebreakingAttributeFormatter)` follows below logic:

        >>> from ast import Attribute, Call, Subscript
        >>> from code_formatter import base, format_code
        >>> from code_formatter.extra import LinebreakingAttributeFormatter
        >>> formatters = dict(base.formatters,
        ...                   **{Call: LinebreakingAttributeFormatter.call_formatter_factory(base.formatters[ast.Call]),
        ...                      Attribute: LinebreakingAttributeFormatter,
        ...                      Subscript: LinebreakingAttributeFormatter.subscription_formatter_factory(base.formatters[ast.Subscript])})
        >>> print format_code('instance.identifier.identifier()',
        ...                   formatters_register=formatters, width=3, force=True)
        (instance.identifier
                 .identifier())

    """

    class _IdentifierFormatter(base.CodeFormatter):

        def __init__(self, identifier, formatters_register, parent):
            self.identifier = identifier
            self.parent = parent
            super(LinebreakingAttributeFormatter._IdentifierFormatter,
                  self).__init__(formatters_register)

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock.from_tokens(self.identifier)
            if suffix is not None:
                block.merge(suffix)
            return block


    @classmethod
    def call_formatter_factory(cls, CallFormatter):
        class RedirectingCallFormatter(CallFormatter):
            def __new__(cls, expr, formatters_register, parent=None, func_formatter=None):
                # if func_formatter is not provided check wether we are not part of method call
                if func_formatter is None and isinstance(expr.func, ast.Attribute):
                    return LinebreakingAttributeFormatter(expr, formatters_register, parent)
                return super(RedirectingCallFormatter, cls).__new__(cls, expr=expr,
                                                                    formatters_register=formatters_register,
                                                                    parent=parent, func_formatter=func_formatter)
            def __init__(self, expr, formatters_register, parent=None, func_formatter=None):
                super(RedirectingCallFormatter, self).__init__(expr, formatters_register, parent)
                if func_formatter:
                    self._func_formatter = func_formatter
        return RedirectingCallFormatter

    @classmethod
    def subscription_formatter_factory(cls, SubscriptionFormatter):
        class RedirectingSubsriptionFormatter(SubscriptionFormatter):
            def __new__(cls, expr, formatters_register, parent=None, value_formatter=None):
                # if value_formatter is not provided check wether we are not part of attribute ref
                if value_formatter is None and isinstance(expr.value, ast.Attribute):
                    return LinebreakingAttributeFormatter(expr, formatters_register, parent)
                return super(RedirectingSubsriptionFormatter, cls).__new__(cls,
                                                             expr=expr,
                                                             formatters_register=formatters_register,
                                                             parent=parent,  value_formatter=value_formatter)
            def __init__(self, expr, formatters_register, parent=None, value_formatter=None):
                super(RedirectingSubsriptionFormatter, self).__init__(expr, formatters_register, parent)
                if value_formatter:
                    self._value_formatter = value_formatter
        return RedirectingSubsriptionFormatter

    @classmethod
    def register(cls, formatters_register):
        formatters_register[ast.Attribute] = cls
        formatters_register[ast.Subscript] = cls.subscription_formatter_factory(formatters_register[ast.Subscript])
        formatters_register[ast.Call] = cls.call_formatter_factory(formatters_register[ast.Call])
        return formatters_register

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
                # FIXME: how to fix parent?? should we change type of parent to ast type?
                func_formatter = LinebreakingAttributeFormatter._IdentifierFormatter(
                                                                    (expr.func
                                                                         .attr),
                                                                    self.formatters_register,
                                                                    parent=self)
                CallFormatter = self.get_formatter_class(expr)
                call_formater = CallFormatter(func_formatter=func_formatter, expr=expr,
                                              formatters_register=self.formatters_register, parent=self)
                self._attrs_formatters.insert(0, call_formater)
                expr = expr.func.value
            elif isinstance(expr, ast.Subscript):
                # FIXME: how to fix parent?? should we change type of parent to ast type?
                value_formatter = LinebreakingAttributeFormatter._IdentifierFormatter(
                                                                    (expr.value.attr),
                                                                    self.formatters_register,
                                                                    parent=self)
                SubscriptionFormatter = self.get_formatter_class(expr)
                subscription_formatter = SubscriptionFormatter(value_formatter=value_formatter, expr=expr,
                                                                formatters_register=self.formatters_register,
                                                                parent=self)
                self._attrs_formatters.insert(0, subscription_formatter)
                expr = expr.value.value
        self.value_formatter = self.get_formatter(expr)

    def _format_code(self, width, continuation, suffix):
        def _format(continuation, prefix=None):
            block = CodeBlock.from_tokens(prefix) if prefix else CodeBlock()
            for i in range(0, width - block.width + 1):
                block.merge(self.value_formatter.format_code(width - block.width - i))
                separator = CodeBlock.from_tokens('.')
                attr_ref_indent = block.width
                block.merge(separator.copy())
                try:
                    block.merge(self._attrs_formatters[0]
                                    .format_code(width - block.last_line.width, False,
                                                 suffix=(suffix if len(self._attrs_formatters) == 1
                                                                else None)))
                    for attr_formatter in self._attrs_formatters[1:]:
                        s = suffix if self._attrs_formatters[-1] == attr_formatter else None
                        try:
                            attr_block = attr_formatter.format_code(width - block.last_line.width -
                                                                    separator.width,
                                                                    False, suffix=s)

                        except NotEnoughSpace:
                            if not continuation:
                                raise
                            block.extend(separator, indent=attr_ref_indent)
                            block.merge(attr_formatter.format_code(width - attr_ref_indent, continuation, suffix=s))
                        else:
                            block.merge(separator)
                            block.merge(attr_block)
                except NotEnoughSpace:
                    block = CodeBlock.from_tokens(prefix) if prefix else CodeBlock()
                    continue

                return block
        try:
            return _format(continuation)
        except NotEnoughSpace:
            if continuation:
                raise
        suffix = self._append_to_suffix(suffix, ')')
        return _format(True, '(')
