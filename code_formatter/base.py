#-*- coding: utf-8 -*-
import ast
from itertools import chain, izip_longest
import re
import textwrap
import sys

from .code import CodeBlock, CodeLine
from .exceptions import NotEnoughSpace
from .utils import FormattersRegister


# It's better to avoid metaclasses in this case - simple register is sufficient and
# it can be customized quite easily
formatters = FormattersRegister()

# formatters.register can accept multiple classes at once and returns None
# we are introducing trivial decorator
def register(cls):
    formatters.register(cls)
    return cls


class CodeFormatter(object):

    def __init__(self, formatters_register):
        self.formatters_register = formatters_register
        # maps formatting context to max failing width
        self._known_max_width_of_failure = {}

    def _append_to_suffix(self, suffix, *tokens):
        if suffix:
             block = CodeBlock.from_tokens(*tokens)
             block.merge(suffix)
             return block
        return CodeBlock.from_tokens(*tokens)

    def _format_code(self, width, continuation, suffix):
        raise NotImplementedError()

    def format_code(self, width, continuation=False, suffix=None):
        context = (continuation, suffix)
        if width <= 0 or (context in self._known_max_width_of_failure and
                          self._known_max_width_of_failure[context] >= width):
            raise NotEnoughSpace()

        try:
            code = self._format_code(width, continuation, suffix)
        except NotEnoughSpace:
            if (context not in self._known_max_width_of_failure or
                  self._known_max_width_of_failure[context] < width):
                self._known_max_width_of_failure[context] = width
            raise

        if code.width > width:
            if (context not in self._known_max_width_of_failure or
                  self._known_max_width_of_failure[context] < width):
                self._known_max_width_of_failure[context] = width
            raise NotEnoughSpace()
        return code

    @classmethod
    def register(cls, formatters_register):
        raise NotImplementedError()


class AstFormatter(CodeFormatter):

    ast_type = None

    def __init__(self, expr, formatters_register, parent=None):
        assert self.ast_type is not None
        self.expr = expr
        self.parent = parent
        super(AstFormatter, self).__init__(formatters_register)

    def get_formatter_class(self, expr, formatters_register=None):
        formatters_register = (self.formatters_register if formatters_register is None
                                                        else formatters_register)
        return formatters_register[type(expr)]

    def get_formatter(self, expr, parent=None, formatters_register=None):
        formatters_register = (self.formatters_register if formatters_register is None
                                                        else formatters_register)
        Formatter = self.get_formatter_class(expr,
                                             formatters_register=formatters_register)
        return Formatter(expr=expr, formatters_register=formatters_register,
                         parent=self if parent is None else parent)


class StatementFormatter(AstFormatter):

    pass


class ExpressionFormatter(AstFormatter):

    pass


class AtomFormatter(ExpressionFormatter):

    ast_type = None

    def _format(self, width):
        raise NotImplementedError()

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock([CodeLine([self._format(width)])])
        if suffix:
            block.merge(suffix)
        return block


@register
class NameFormatter(AtomFormatter):

    ast_type = ast.Name

    def _format(self, width):
        return unicode(self.expr.id)


class OperatorFormatter(AtomFormatter):

    operator = None
    priority = 0

    def _format(self, width):
        return self.operator

ast_operator2priority = {}

for priority, ast_type, operator in [(11, ast.Pow, '**'),
                                     (10, ast.UAdd, '+'),
                                     (10, ast.Invert, '~'),
                                     (10, ast.USub, '-'),
                                     (9, ast.Mult, '*'),
                                     (9, ast.FloorDiv, '//'),
                                     (9, ast.Div, '/'),
                                     (9, ast.Mod, '%'),
                                     (8, ast.Add, '+'),
                                     (8, ast.Sub, '-'),
                                     (7, ast.RShift, '>>'),
                                     (7, ast.LShift, '<<'),
                                     (6, ast.BitAnd, '&'),
                                     (5, ast.BitXor, '^'),
                                     (4, ast.BitOr, '|'),
                                     (3, ast.Gt, '>'),
                                     (3, ast.GtE, '>='),
                                     (3, ast.Lt, '<'),
                                     (3, ast.LtE, '<='),
                                     (3, ast.Eq, '=='),
                                     (3, ast.NotEq, '!='),
                                     (3, ast.Is, 'is'),
                                     (3, ast.IsNot, 'is not'),
                                     (3, ast.In, 'in'),
                                     (3, ast.NotIn, 'not in'),
                                     (2, ast.Not, 'not'),
                                     (1, ast.And, 'and'),
                                     (0, ast.Or, 'or')]:
    ast_operator2priority[ast_type] = priority
    register(type('%sFormatter' % ast_type.__name__, (OperatorFormatter,),
                        {'ast_type': ast_type,
                         'operator': operator,
                         'priority': priority}))


class OperationFormatter(ExpressionFormatter):

    @property
    def separator(self):
        raise NotImplementedError()

    @property
    def priority(self):
        return ast_operator2priority[type(self.expr.op)]


@register
class UnaryOperationFormatter(OperationFormatter):

    ast_type = ast.UnaryOp
    operator = None

    def __init__(self, *args, **kwargs):
        super(UnaryOperationFormatter, self).__init__(*args, **kwargs)
        self._operator_formatter = self.get_formatter(self.expr.op)
        self._value_formatter = self.get_formatter(self.expr.operand)

    @property
    def separator(self):
        if not isinstance(self.expr.op, ast.Not):
            return CodeBlock.from_tokens('')
        return CodeBlock.from_tokens(' ')

    def _format_code(self, width, continuation, suffix):
        block = self._operator_formatter.format_code(width)
        block.merge(self.separator)
        value_block = self._value_formatter.format_code(width - block.width, continuation,
                                                        suffix=suffix)
        block.merge(value_block)
        return block


class BinaryOperationFormatter(OperationFormatter):

    def are_parentheses_required(self):
        # FIXME: check against parent.expr and
        #        handle parent.expr access in some sane way in Formatter API...
        if isinstance(self.parent, AttributeFormatter):
            return True
        if self.parent is None or not isinstance(self.parent,
                                                 OperationFormatter):
            return False
        _get_op = lambda f: f.expr.ops[0] if isinstance(f, CompareFormatter) else f.expr.op
        parent_operator = _get_op(self.parent)
        operator = _get_op(self)
        return (self.parent.priority >= self.priority and
                type(operator) is not type(parent_operator))


@register
class BinaryArithmeticOperationFormatter(BinaryOperationFormatter):

    ast_type = ast.BinOp

    def __init__(self, *args, **kwargs):
        super(BinaryArithmeticOperationFormatter, self).__init__(*args, **kwargs)
        self.opt_formatter = self.get_formatter(self.expr.op)
        self.left_formatter = self.get_formatter(self.expr.left)
        self.right_formatter = self.get_formatter(self.expr.right)

    def are_parentheses_required(self):
        if (self.parent and isinstance(self.parent.expr, self.ast_type) and
            self.parent.priority == self.priority):
            return self.parent.expr.right == self.expr
        return super(BinaryArithmeticOperationFormatter, self).are_parentheses_required()


    @property
    def separator(self):
        return CodeBlock.from_tokens(' ')

    def _format_code(self, width, continuation, suffix):
        def _format(continuation, prefix=None):
            try:
                block = CodeBlock.from_maybe_token(prefix)
                block.merge(self.left_formatter.format_code(width - block.width,
                                                            continuation))
                block.merge(self.opt_formatter
                                .format_code(width - block.last_line.width -
                                             self.separator.width,
                                             continuation),
                            separator=self.separator)
                block.merge(self.right_formatter
                                .format_code(width - block.last_line.width -
                                             self.separator.width,
                                             continuation, suffix),
                            separator=self.separator)
            except NotEnoughSpace:
                if not continuation:
                    raise
                block = CodeBlock.from_maybe_token(prefix)
                indent = block.width
                block.merge(self.left_formatter.format_code(width - block.width,
                                                            continuation))
                block.merge(self.opt_formatter
                                .format_code(width - block.last_line.width -
                                             self.separator.width,
                                             continuation),
                            separator=self.separator)
                block.extend(self.right_formatter
                                 .format_code(width - indent,
                                              continuation,
                                              suffix=suffix),
                             indent=indent)
            return block
        if not self.are_parentheses_required():
            try:
                return _format(continuation)
            except NotEnoughSpace:
                if continuation:
                    raise
        suffix = self._append_to_suffix(suffix, ')')
        return _format(True, '(')


@register
class CompareFormatter(OperationFormatter):

    ast_type = ast.Compare

    @property
    def priority(self):
        return ast_operator2priority[type(self.expr.ops[0])]

    @property
    def separator(self):
        return CodeBlock.from_tokens(' ')

    def are_parentheses_required(self):
        with_parentheses = False
        if self.parent:
            # FIXME: check against parent.expr and
            #        handle parent.expr access in some sane way in Formatter API...
            with_parentheses = ((isinstance(self.parent, OperationFormatter) and
                              self.parent.priority >= self.priority) or
                             isinstance(self.parent, AttributeFormatter))
        return with_parentheses

    def _format_operator_chain(self, width, operators, comparators):
        if not operators:
            return CodeBlock()
        if width <= 0:
            raise NotEnoughSpace()
        block = CodeBlock()
        operator = operators[0]
        comparator = comparators[0]
        operator_formatter = self.get_formatter(operator)
        comparator_formatter = self.get_formatter(comparator)
        for i in range(width+1):
            operator_block = operator_formatter.format_code(width - i)
            comparator_block = comparator_formatter.format_code(width -
                                                                operator_block.width -
                                                                self.separator.width)
            try:
                chain_block = self._format_operator_chain(width - operator_block.width -
                                                          comparator_block.last_line.width -
                                                          2 * self.separator.width,
                                                          operators[1:], comparators[1:])
            except NotEnoughSpace:
                continue
            else:
                break
        block.merge(operator_block)
        block.merge(comparator_block, self.separator)
        block.merge(chain_block, separator=self.separator)
        if block.width > width:
            raise NotEnoughSpace()
        return block

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock()
        left_formatter = self.get_formatter(self.expr.left)
        with_parentheses = self.are_parentheses_required()
        if with_parentheses:
            block.append_tokens('(')
        for i in range(width-block.width+1):
            left_block = left_formatter.format_code(width-block.width-i)
            try:
                chain_block = self._format_operator_chain(width - left_block.last_line.width -
                                                          ((1 if with_parentheses else 0) +
                                                           self.separator.width),
                                                          self.expr.ops, self.expr.comparators)
            except NotEnoughSpace:
                continue
            else:
                block.merge(left_block)
                block.merge(chain_block, separator=self.separator)
                break
        if with_parentheses:
            block.append_tokens(')')
        if suffix:
            block.merge(suffix)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class BooleanOperationFormatter(BinaryOperationFormatter):

    ast_type = ast.BoolOp

    def __init__(self, *args, **kwargs):
        super(BooleanOperationFormatter, self).__init__(*args, **kwargs)
        self._operator_formatter = self.get_formatter(self.expr.op)
        self._values_formatters = [self.get_formatter(v) for v in self.expr.values]

    @property
    def separator(self):
        return CodeBlock.from_tokens(' ')

    def _format_code(self, width, continuation, suffix):
        def _format(continuation, prefix=None):
            block = CodeBlock.from_tokens(prefix) if prefix else CodeBlock()
            indent = block.width * ' '
            block.merge(self._values_formatters[0]
                            .format_code(width - block.width,
                                         continuation=continuation))
            for value_formatter in self._values_formatters[1:]:
                s = suffix if value_formatter is self._values_formatters[-1] else None
                try:
                    operator_block = self._operator_formatter.format_code(width - self.separator.width)
                    value_block = value_formatter.format_code(width - block.width -
                                                              operator_block.width - 2*self.separator.width,
                                                              continuation=continuation,
                                                              suffix=s)
                    block.merge(operator_block, separator=self.separator)
                    block.merge(value_block, separator=self.separator)
                except NotEnoughSpace:
                    if not continuation:
                        raise
                    operator_block = self._operator_formatter.format_code(width - 1)
                    value_block = value_formatter.format_code(width - len(indent),
                                                              continuation=continuation,
                                                              suffix=s)
                    block.merge(operator_block, separator=self.separator)
                    block.extend(value_block, indent)
            return block
        if not self.are_parentheses_required():
            try:
                return _format(continuation)
            except NotEnoughSpace:
                if continuation:
                    raise
        suffix = self._append_to_suffix(suffix, ')')
        return _format(True, '(')


@register
class NumFormatter(AtomFormatter):

    ast_type = ast.Num

    def _format(self, width):
        return repr(self.expr.n)


@register
class StringFormatter(ExpressionFormatter):

    ast_type = ast.Str

    def __init__(self, *args, **kwargs):
        super(StringFormatter, self).__init__(*args, **kwargs)

    def _trim_docstring(self, docstring):
        """Taken from: http://www.python.org/dev/peps/pep-0257/#handling-docstring-indentation"""
        if not docstring:
            return ''
        # Convert tabs to spaces (following the normal Python rules)
        # and split into a list of lines:
        lines = docstring.expandtabs().splitlines()
        # Determine minimum indentation (first line doesn't count):
        indent = sys.maxint
        for line in lines[1:]:
            stripped = line.lstrip()
            if stripped:
                indent = min(indent, len(line) - len(stripped))
        # Remove indentation (first line is special):
        trimmed = [lines[0].strip()]
        if indent < sys.maxint:
            for line in lines[1:]:
                trimmed.append(line[indent:].rstrip())
        # Strip off trailing and leading blank lines:
        while trimmed and not trimmed[-1]:
            trimmed.pop()
        while trimmed and not trimmed[0]:
            trimmed.pop(0)
        # Return a single string:
        return '\n'.join(trimmed)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock()
        # check wether this expression is a docstring:
        # * if it is a docstring it is child of an expression statement
        # * parent of this expression statement should be class or function definition statement
        # * this expression statement should be first statement in parent's body
        real_parent = self.parent.parent if (isinstance(self.parent,
                                                        ExprFormatter) and
                                             self.parent.parent) else self.parent
        # FIXME: check against parent.expr and
        #        handle parent.expr access in some sane way in Formatter API...
        if isinstance(real_parent, (FunctionDefinitionFormatter,
                                     ClassDefinitionFormater)) and self.parent.expr == real_parent.expr.body[0]:
            lines = self._trim_docstring(self.expr.s).split('\n')
            if len(lines) > 1:
                lines = ['"""' + lines[0]] + lines[1:] + ['"""']
            else:
                lines[0] = '"""' + lines[0] + '"""'
            block.append_lines(*(CodeLine([l]) for l in lines))
        else:
            # textwrap raises an exception on negative width and we... don't :-P
            def format_lines(string, width):
                if len(filter(None, string.split('\n'))) > 1:
                    splited = [line for line in re.split('(\n+)', string)]
                    i = iter(splited)
                    lines = [next(i)] if re.match('\n+', splited[0]) else []
                    lines += [l+e for l,e in izip_longest(i, i, fillvalue='')]
                else:
                    lines = [string]
                result = []
                for line in lines:
                    if line == '':
                        result.append('')
                    else:
                        result.extend(textwrap.wrap(line, width=width, expand_tabs=False,
                                                    replace_whitespace=False, fix_sentence_endings=False,
                                                    break_long_words=False, drop_whitespace=False))
                return result
            lines = format_lines(self.expr.s, width if width > 0 else 1)
            if len(lines) > 1:
                lines = format_lines(self.expr.s, width-2 if width-2 > 0 else 2)
            if len(lines) > 1:
                if continuation:
                    block.append_tokens(repr(lines[0]))
                    block.append_lines(*(CodeLine([repr(l)]) for l in lines[1:]))
                else:
                    block.append_tokens('(')
                    block.append_tokens(repr(lines[0]))
                    block.append_lines(*(CodeLine([' ', repr(l)]) for l in lines[1:]))
                    block.append_tokens(')')
            elif lines:
                block.append_tokens(repr(lines[0]))
        if suffix:
            block.merge(suffix)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class AttributeFormatter(ExpressionFormatter):

    ast_type = ast.Attribute

    def __init__(self, *args, **kwargs):
        super(AttributeFormatter, self).__init__(*args, **kwargs)
        self.value_formatter = self.get_formatter(self.expr.value)

    def _format_code(self, width, continuation, suffix):
        block = self.value_formatter.format_code(width-len(self.expr.attr)-1)
        block.append_tokens('.', self.expr.attr)
        if suffix:
            block.merge(suffix)
        return block


class ListOfExpressionsFormatter(CodeFormatter):

    class SuffixFormatter(CodeFormatter):
        """Empty expression list returns this trivial object"""
        def _format_code(self, width, continuation, suffix):
            return suffix or CodeBlock()


    class FormatterAdapter(CodeFormatter):
        """Adapt single element expression list to interal ListOfExpressionsFormatter API"""
        def __init__(self, expression_formatter, *args, **kwargs):
            super(ListOfExpressionsFormatter.FormatterAdapter, self).__init__(*args, **kwargs)
            self._expression_formatter = expression_formatter

        def _format_code(self, width, continuation, suffix, line_width=None):
            expression_block = self._expression_formatter.format_code(width, continuation,
                                                                      suffix=suffix)
            if line_width is not None:
                block = CodeBlock()
                expression_block = block.merge(expression_block,
                                               indent=line_width - width)
            return expression_block

        def format_code(self, width, continuation=False, suffix=None, line_width=None):
            return self._format_code(width, continuation, suffix, line_width=line_width)

    # You can find second version of this class in extras:
    # `code_formatter.extras.ListOfExpressionsWithSingleLineContinuationsFormatter`
    #
    # * True - try to squash elements and continue line even if subelements
    #          are formatted in muliple lines already. This is really simple and
    #          greedy and expected behaviour, so it is default. For example:
    #
    #       (('test '
    #         'expression'), (z, y))
    #
    #
    # * False - force joins only on single line of expressions. Line breaks can occur
    #           only at the end of sublist or inside last element. This not obvious strategy
    #           produces often more readable code, but it's behaviour is less expected.
    #           Example formatting:
    #
    #       (('test expression'), (z,
    #                              y))
    multiline_continuation = True


    def __new__(cls, expressions_formatters, formatters_register, parent=None):
        if len(expressions_formatters) > 1:
            return super(ListOfExpressionsFormatter,
                         cls).__new__(cls, expressions_formatters,
                                      formatters_register, parent)
        elif len(expressions_formatters) == 1:
            return ListOfExpressionsFormatter.FormatterAdapter(expressions_formatters[0],
                                                               formatters_register)
        else:
            return ListOfExpressionsFormatter.SuffixFormatter(formatters_register)


    def __init__(self, expressions_formatters, formatters_register, parent=None):
        super(ListOfExpressionsFormatter, self).__init__(formatters_register)
        self._expression_formatter = expressions_formatters[0]
        # allow easily subclass this monster
        self._expressions_formatter = type(self)(expressions_formatters[1:],
                                                 formatters_register, parent)
        # (continuation, suffix, line_width) -> _known_max_width_of_failure
        self._known_max_width_of_failure = {}
        self._cache = {}

    @classmethod
    def from_expressions(cls, expressions, parent):
        formatters = parent.formatters_register
        expressions_formatters = [formatters[type(e)](e, formatters_register=formatters,
                                                      parent=parent)
                                  for e in expressions]
        return cls(expressions_formatters, formatters)

    def _format_line_continuation(self, width, continuation, suffix, line_width):
        # binary search for maximal correct (first) expression width:
        # * lower_boundry - max known width when first expression fails
        # * upper_boundry - min known/possible width to format rest of expressions
        lower_boundry = 0
        upper_boundry = curr_width = width
        separator = CodeBlock.from_tokens(', ')
        succeeding_width = None
        while True:
            try:
                expression_block = (self._expression_formatter
                                        .format_code(curr_width, continuation))
            except NotEnoughSpace:
                lower_boundry = curr_width
            else:
                if not self.multiline_continuation and expression_block.height > 1:
                    lower_boundry = curr_width
                else:
                    try:
                        expressions_block = (self._expressions_formatter
                                                 .format_code(width - expression_block.last_line.width -
                                                              separator.width,
                                                              continuation, suffix=suffix,
                                                              line_width=line_width))
                    except NotEnoughSpace:
                        upper_boundry = curr_width
                    else:
                        lower_boundry = succeeding_width = curr_width

            if succeeding_width and upper_boundry - succeeding_width <= 1:
                if succeeding_width != curr_width:
                    expression_block = self._expression_formatter.format_code(succeeding_width)
                    expressions_block = (self._expressions_formatter
                                             .format_code(width - expression_block.last_line.width -
                                                          separator.width,
                                                          continuation, suffix=suffix,
                                                          line_width=line_width))
                block = CodeBlock()
                block.merge(expression_block, indent=line_width - width)
                block.merge(separator)
                block.merge(expressions_block, indent=0)
                return block
            elif upper_boundry - lower_boundry <= 1:
                break
            curr_width = (lower_boundry + upper_boundry) / 2
        raise NotEnoughSpace()

    def _format_line_break(self, width, continuation, suffix, line_width=None):
        # try to break line
        separator = CodeBlock.from_tokens(',')
        expression_block = self._expression_formatter.format_code(width - separator.width,
                                                                  continuation)
        expressions_block = self._expressions_formatter.format_code(line_width,
                                                                    continuation,
                                                                    line_width=line_width,
                                                                    suffix=suffix)
        block = CodeBlock()
        block.merge(expression_block, indent=line_width-width)
        block.merge(separator)
        block.extend(expressions_block)
        return block

    def _format_code(self, width, continuation, suffix, line_width=None):
        line_width = line_width or width
        try:
            return self._format_line_continuation(width, continuation, suffix, line_width)
        except NotEnoughSpace:
            return self._format_line_break(width, continuation, suffix, line_width)

    def format_code(self, width, continuation=False, suffix=None, line_width=None):
        context = (continuation, suffix, line_width)
        if width <= 0 or (self._known_max_width_of_failure.get(context) is not None and
                          self._known_max_width_of_failure[context] >= width):
            raise NotEnoughSpace()

        if context in self._cache:
            return self._cache[context].copy()
        try:
            code = self._format_code(width, continuation, suffix, line_width)
        except NotEnoughSpace:
            if (self._known_max_width_of_failure.get(context) is None or
                  self._known_max_width_of_failure[context] < width):
                self._known_max_width_of_failure[context] = width
            raise
        self._cache[context] = code.copy()
        return code

    @classmethod
    def register(cls, formatters_register):
        # replace itself in all Formatters which uses internally ListOfExpressionsFormatter
        for F in [F for F in formatters_register.values()
                    if hasattr(F, 'ListOfExpressionsFormatter')]:
            Formatter = type(F.__name__, (F,),
                             {'ListOfExpressionsFormatter': cls})
            formatters_register.register(Formatter)


@register
class CallFormatter(ExpressionFormatter):

    ast_type = ast.Call
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    @register
    class KeywordArg(ExpressionFormatter):

        ast_type = ast.keyword

        def __init__(self, *args, **kwargs):
            super(CallFormatter.KeywordArg, self).__init__(*args, **kwargs)
            self.expression_formatter = self.get_formatter(self.expr.value)

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock([CodeLine(['%s=' % self.expr.arg])])
            expression_block = self.expression_formatter.format_code(width - block.width,
                                                                     True, suffix=suffix)
            block.merge(expression_block)
            return block


    class _StarArgsFormatter(CodeFormatter):

        def __init__(self, subexpression, formatters_register, parent, prefix):
            super(CallFormatter._StarArgsFormatter, self).__init__(formatters_register)
            self.subexpression = subexpression
            self.parent = parent
            SubexpressionFormater = self.parent.get_formatter_class(self.subexpression)
            self.subexpression_formatter = SubexpressionFormater(self.subexpression,
                                                                 formatters_register=self.formatters_register,
                                                                 parent=parent)
            self.prefix = prefix

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock.from_tokens(self.prefix)
            block.merge(self.subexpression_formatter.format_code(width - block.width,
                                                                 continuation,
                                                                 suffix=suffix))
            return block

    def __init__(self, *args, **kwargs):
        super(CallFormatter, self).__init__(*args, **kwargs)
        self._func_formatter = self.get_formatter(self.expr.func)
        self._arguments_formatters = self._get_arguments_formatters()
        self._arguments_formatter = self.ListOfExpressionsFormatter(self._arguments_formatters,
                                                                    self.formatters_register)

    def _get_arguments_formatters(self):
        formatters = [self.get_formatter(e) for e in self.expr.args]
        if self.expr.starargs:
            formatters.append(CallFormatter._StarArgsFormatter(self.expr.starargs,
                                                               formatters_register=self.formatters_register,
                                                               parent=self, prefix='*'))
        formatters += [self.get_formatter(e) for e in self.expr.keywords]
        if self.expr.kwargs:
            formatters.append(CallFormatter._StarArgsFormatter(self.expr.kwargs,
                                                               formatters_register=self.formatters_register,
                                                               parent=self, prefix='**'))
        return formatters

    def _format_code(self, width, continuation, suffix):
        suffix = self._append_to_suffix(suffix, ')')
        for i in range(width+1):
            curr_width = width - i
            block = self._func_formatter.format_code(curr_width)
            block.append_tokens('(')
            try:
                subblock = self._arguments_formatter.format_code(width -
                                                                 block.last_line.width,
                                                                 True,
                                                                 suffix=suffix)
            except NotEnoughSpace:
                continue
            else:
                block.merge(subblock)
                break
        return block


@register
class DictionaryFormatter(ExpressionFormatter):

    ast_type = ast.Dict
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    class _ItemFormatter(CodeFormatter):

        def __init__(self, key, value, formatters_register, parent):
            super(DictionaryFormatter._ItemFormatter, self).__init__(formatters_register)
            self.parent = parent
            self.key = self.formatters_register[type(key)](key, parent=self.parent,
                                                           formatters_register=self.formatters_register)
            self.value = self.formatters_register[type(value)](value, parent=self.parent,
                                                               formatters_register=self.formatters_register)

        def _format_code(self, width, continuation, suffix):
            # FIXME: search for solution on failure
            separator = ':'
            block = self.key.format_code(width - len(separator) - 2 -
                                         (suffix or CodeBlock()).width)
            block.lines[-1].append(separator)
            block.merge(self.value.format_code(width - block.width,
                                               continuation=True,
                                               suffix=suffix),
                        separator=' ')
            return block

    def __init__(self, *args, **kwargs):
        super(DictionaryFormatter, self).__init__(*args, **kwargs)
        expressions = [DictionaryFormatter._ItemFormatter(k, v,
                                                          formatters_register=self.formatters_register,
                                                          parent=self)
                       for (k, v) in zip(self.expr.keys, self.expr.values)]
        self._items_formatter = self.ListOfExpressionsFormatter(expressions,
                                                                self.formatters_register)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock([CodeLine(['{'])])
        suffix = self._append_to_suffix(suffix, '}')
        subblock = self._items_formatter.format_code(width - block.width,
                                                     continuation=True,
                                                     suffix=suffix)
        block.merge(subblock)
        return block


@register
class ListFormatter(ExpressionFormatter):

    ast_type = ast.List
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    def __init__(self, *args, **kwargs):
        super(ListFormatter, self).__init__(*args, **kwargs)
        self._items_formatter = (self.ListOfExpressionsFormatter
                                     .from_expressions(self.expr.elts, self))

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('[')
        suffix = self._append_to_suffix(suffix, ']')
        subblock = self._items_formatter.format_code(width - block.width,
                                                     continuation=True,
                                                     suffix=suffix)
        block.merge(subblock)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ListComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.ListComp

    def __init__(self, *args, **kwargs):
        super(ListComprehensionFormatter, self).__init__(*args, **kwargs)
        self.elt_formatter = self.get_formatter(self.expr.elt)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('[')
        indent = block.width * ' '
        elt_block = self.elt_formatter.format_code(width - block.width)
        block.merge(elt_block)
        suffix = self._append_to_suffix(suffix, ']')
        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters_register=self.formatters_register,
                                                 suffix=suffix)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent), parent=self,
                                                 formatters_register=self.formatters_register,
                                                 suffix=suffix)
            block.extend(generators_block, indent)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class SetFormatter(ExpressionFormatter):

    ast_type = ast.Set
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    def __init__(self, *args, **kwargs):
        super(SetFormatter, self).__init__(*args, **kwargs)
        self._items_formatter = (self.ListOfExpressionsFormatter
                                     .from_expressions(self.expr.elts, self))


    def _format_code(self, width, continuation, suffix):
        # FIXME: handle suffix correctly
        block = CodeBlock([CodeLine(['{'])])
        subblock = self._items_formatter.format_code(width=width-block.width)
        block.merge(subblock)
        block.lines[-1].append('}')
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class SetComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.SetComp

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('{')
        indent = block.width * ' '
        elt_formatter = self.get_formatter(self.expr.elt)
        elt_block = elt_formatter.format_code(width - block.width)
        block.merge(elt_block)
        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters_register=self.formatters_register)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent),
                                                 parent=self,
                                                 formatters_register=self.formatters_register)
            block.extend(generators_block, indent)
        block.append_tokens('}')
        return block


@register
class IfExpressionFormatter(ExpressionFormatter):

    ast_type = ast.IfExp

    def __init__(self, *args, **kwargs):
        super(IfExpressionFormatter, self).__init__(*args, **kwargs)
        self._body_formatter = self.get_formatter(self.expr.body)
        self._test_formatter = self.get_formatter(self.expr.test)
        self._orelse_formatter = self.get_formatter(self.expr.orelse)


    def _format_code(self, width, continuation, suffix):
        block = CodeBlock()
        # conditional expression has lowest priority
        def use_parentheses():
            block.append_tokens('(')
            return self._append_to_suffix(suffix, ')')
        with_parentheses = False
        # FIXME: check against parent.expr and
        #        handle parent.expr access in some sane way in Formatter API...
        if isinstance(self.parent, OperationFormatter):
            with_parentheses = True
            suffix = use_parentheses()
        body_block = self._body_formatter.format_code(width)
        if_keyword_block = CodeBlock.from_tokens(' ', 'if', ' ')
        test_block = self._test_formatter.format_code(width - block.width -
                                                      if_keyword_block.width)
        else_keyword_block = CodeBlock.from_tokens(' ', 'else', ' ')
        try:
            orelse_block = self._orelse_formatter.format_code(width -
                                                              body_block.width -
                                                              if_keyword_block.width -
                                                              else_keyword_block.width -
                                                              test_block.width,
                                                              suffix=suffix)
        except NotEnoughSpace:
            if not with_parentheses:
                suffix = use_parentheses()
                body_block = self._body_formatter.format_code(width -
                                                              block.width)
                test_block = self._test_formatter.format_code(width - block.width -
                                                              if_keyword_block.width)
            block.merge(body_block)
            block.merge(if_keyword_block)
            block.merge(test_block)
            block.extend(else_keyword_block, indent=' '*(body_block.width+ 1))
            orelse_block = self._orelse_formatter.format_code(width=width -
                                                                    block.last_line.width,
                                                              suffix=suffix)
        else:
            block.merge(body_block)
            block.merge(if_keyword_block)
            block.merge(test_block)
            block.merge(else_keyword_block)
        block.merge(orelse_block)
        return block


@register
class SubscriptionFormatter(ExpressionFormatter):

    ast_type = ast.Subscript

    def __init__(self, *args, **kwargs):
        super(SubscriptionFormatter, self).__init__(*args, **kwargs)
        self._value_formatter = self.get_formatter(self.expr.value)
        self._slice_formatter = self.get_formatter(self.expr.slice)

    def _format_code(self, width, continuation, suffix):
        block = self._value_formatter.format_code(width)
        block.merge(self._slice_formatter.format_code(width -
                                                      len(block.lines[-1]),
                                                      suffix=suffix))
        return block


@register
class SliceFormatter(ExpressionFormatter):

    ast_type = ast.Slice

    def __init__(self, *args, **kwargs):
        super(SliceFormatter, self).__init__(*args, **kwargs)
        self._lower_formatter = self.get_formatter(self.expr.lower) if self.expr.lower else None
        self._upper_formatter = self.get_formatter(self.expr.upper) if self.expr.upper else None
        self._step_formatter = self.get_formatter(self.expr.step) if self.expr.step else None

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('[')
        suffix = self._append_to_suffix(suffix, ']')
        if self._lower_formatter:
            if not self._upper_formatter and not self._step_formatter:
                suffix = self._append_to_suffix(suffix, ':')
                block.merge(self._lower_formatter.format_code(width - block.width -
                                                              1, suffix=suffix))
                return block
            block.merge(self._lower_formatter.format_code(width - block.width - 1))
        block.append_tokens(':')

        if self._upper_formatter:
            block.merge(self._upper_formatter
                            .format_code(width - block.width, continuation,
                                         suffix=(suffix if not self.expr.step
                                                        else None)))
        if self._step_formatter:
            block.append_tokens(':')
            block.merge(self._step_formatter.format_code(width - block.width, continuation,
                                                         suffix=suffix))
        if not self._upper_formatter and not self._step_formatter:
            block.merge(suffix)
        return block


@register
class IndexFormatter(ExpressionFormatter):

    ast_type = ast.Index

    def __init__(self, *args, **kwargs):
        super(IndexFormatter, self).__init__(*args, **kwargs)
        self._value_formatter = self.get_formatter(self.expr.value)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('[')
        suffix = self._append_to_suffix(suffix, ']')
        block.merge(self._value_formatter.format_code(width - block.width, continuation,
                                                      suffix=suffix))
        return block


@register
class GeneratorFormatter(ExpressionFormatter):

    ast_type = ast.GeneratorExp

    def _format_code(self, width, continuation, suffix):
        value_formatter = self.get_formatter(self.expr.elt)
        with_parentheses = (not self.parent or not isinstance(self.parent,
                                                           CallFormatter) or
                         len(self.parent.expr.args) != 1)
        if with_parentheses:
            block = CodeBlock([CodeLine(['('])])
            indent = block.width * ' '
            block.merge(value_formatter.format_code(width))
        else:
            indent = ''
            block = value_formatter.format_code(width)
        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters_register=self.formatters_register,
                                                 suffix=suffix)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent),
                                                 parent=self,
                                                 formatters_register=self.formatters_register,
                                                 suffix=suffix)
            block.extend(generators_block, indent)

        if with_parentheses:
            block.append_tokens(')')
        return block


def format_generators(generators, width, parent, formatters_register, suffix=None):
    block = CodeBlock()
    def _get_formatter(expr):
        return formatters_register[type(expr)](expr, parent=parent,
                                               formatters_register=formatters_register)
    for generator_number, generator in enumerate(generators):
        target_formatter = _get_formatter(generator.target)
        iter_formatter = _get_formatter(generator.iter)
        ifs_formatters = [_get_formatter(if_) for if_ in generator.ifs]

        f2t = chain([(target_formatter, 'for'), (iter_formatter, 'in')],
                    izip_longest(ifs_formatters, (), fillvalue='if'))
        for part, (formatter, keyword) in enumerate(f2t, generator_number):
            try:
                spaces_count = 2 if part > 0 else 1
                formatter_block = formatter.format_code(width -
                                                        len(block.last_line or []) -
                                                        len(keyword) -
                                                        spaces_count)
                if part > 0:
                    block.append_tokens(' ', keyword)
                else:
                    block.append_tokens(keyword)
                block.merge(formatter_block, separator=' ')
            except NotEnoughSpace:
                formatter_block = formatter.format_code(width - len(keyword) - 1)
                block.lines.append(CodeLine([keyword]))
                block.merge(formatter_block, separator=' ')
    # FIXME: we should pass this suffix to last formatter
    if suffix:
        block.merge(suffix)
    return block


@register
class DictComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.DictComp

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('{')
        indent = block.width * ' '
        key_formatter = self.get_formatter(self.expr.key)
        value_formatter = self.get_formatter(self.expr.value)
        separator = ': '
        key_block = key_formatter.format_code(width - block.width - len(separator))
        value_block = value_formatter.format_code(width - block.width -
                                                  len(separator))
        block.merge(key_block)
        block.append_tokens(separator)
        block.merge(value_block)
        suffix = self._append_to_suffix(suffix, '}')
        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters_register=self.formatters_register,
                                                 suffix=suffix)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent),
                                                 parent=self,
                                                 formatters_register=self.formatters_register,
                                                 suffix=suffix)
            block.extend(generators_block, indent)
        return block


@register
class TupleFormatter(ExpressionFormatter):

    ast_type = ast.Tuple
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    def __init__(self, *args, **kwargs):
        super(TupleFormatter, self).__init__(*args, **kwargs)
        items = [v for v in self.expr.elts]
        self._items_formatter = self.ListOfExpressionsFormatter.from_expressions(items,
                                                                                 self)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock()
        def _get_closing_suffix(base_suffix=None):
            block = CodeBlock()
            if len(self.expr.elts) == 1:
                block.append_tokens(',')
            block.append_tokens(')')
            if base_suffix:
                block.merge(base_suffix)
            return block

        if (isinstance(self.parent.expr, (ast.Tuple, ast.Call, ast.List,
                                          ast.BinOp, ast.ListComp, ast.FunctionDef,
                                          ast.Dict, ast.GeneratorExp)) or
            len(self.expr.elts) < 2):
            block.append_tokens('(')
            suffix = _get_closing_suffix(suffix)
            expression_block = self._items_formatter.format_code(width -
                                                                 block.width,
                                                                 continuation=True,
                                                                 suffix=suffix)
        else:
            expression_block = self._items_formatter.format_code(width -
                                                                 block.width,
                                                                 continuation=True,
                                                                 suffix=suffix)
            if expression_block.height > 1:
                block.append_tokens('(')
                suffix = _get_closing_suffix(suffix)
                expression_block = self._items_formatter.format_code(width -
                                                                     block.width,
                                                                     continuation=True,
                                                                     suffix=suffix)
        block.merge(expression_block)
        return block


@register
class ParameterListFormatter(AstFormatter):

    ast_type = ast.arguments
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    class DefaultsFormatter(CodeFormatter):

        def __init__(self, parameter, expression, parent, formatters_register):
            super(ParameterListFormatter.DefaultsFormatter, self).__init__(formatters_register)
            self.parameter = parameter
            self.expression = expression
            self.parent = parent

        def _format_code(self, width, continuation, suffix):
            parameter_formatter = self.parent.get_formatter(self.parameter)
            expression_formatter = self.parent.get_formatter(self.expression)
            block = parameter_formatter.format_code(width-1)
            block.append_tokens('=')
            block.merge(expression_formatter.format_code(width - block.width, continuation,
                                                         suffix=suffix))
            if block.width > width:
                raise NotEnoughSpace()
            return block

    class KwargFormatter(CodeFormatter):

        def __init__(self, kwarg, parent, formatters_register):
            super(ParameterListFormatter.KwargFormatter, self).__init__(formatters_register)
            self.kwarg = kwarg
            self.parent = parent

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock.from_tokens('**%s' % self.kwarg)
            block.merge(suffix)
            if block.width > width:
                raise NotEnoughSpace()
            return block

    class VarargFormatter(CodeFormatter):

        def __init__(self, vararg, parent, formatters_register):
            super(ParameterListFormatter.VarargFormatter, self).__init__(formatters_register)
            self.vararg = vararg
            self.parent = parent

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock.from_tokens('*%s' % self.vararg)
            block.merge(suffix)
            if block.width > width:
                raise NotEnoughSpace()
            return block

    def __init__(self, *args, **kwargs):
        super(ParameterListFormatter, self).__init__(*args, **kwargs)
        parameters_formatters = [self.parent.get_formatter(arg)
                                 for arg
                                 in self.expr.args[:len(self.expr.args) -
                                                    len(self.expr.defaults)]]
        if self.expr.vararg:
            parameters_formatters.append(ParameterListFormatter.VarargFormatter(self.expr.vararg,
                                                                                self,
                                                                                self.formatters_register))
        parameters_formatters += [ParameterListFormatter.DefaultsFormatter(arg, default,
                                                                           self,
                                                                           self.formatters_register)
                                  for (arg, default)
                                  in zip(self.expr.args[len(self.expr.args) -
                                                        len(self.expr.defaults):],
                                         self.expr.defaults)]
        if self.expr.kwarg:
            parameters_formatters.append(ParameterListFormatter.KwargFormatter(self.expr.kwarg,
                                                                               self,
                                                                               self.formatters_register))
        self._paramters_formatter = self.ListOfExpressionsFormatter(parameters_formatters,
                                                                    self.formatters_register)

    def _format_code(self, width, continuation, suffix):
        return self._paramters_formatter.format_code(width, continuation, suffix)


@register
class LambdaFormatter(ExpressionFormatter):

    ast_type = ast.Lambda

    def _format_code(self, width, continuation, suffix):
        # FIXME: check against parent.expr and
        #        handle parent.expr access in some sane way in Formatter API...
        with_parentheses = (isinstance(self.parent, OperatorFormatter) or
                            isinstance(self.parent, IfExpressionFormatter) and
                            self.parent.expr.orelse is not self.expr)
        if with_parentheses:
            block = CodeBlock.from_tokens('(lambda')
            suffix = self._append_to_suffix(suffix, ')')
        else:
            block = CodeBlock.from_tokens('lambda')
        parameter_list_formatter = self.get_formatter(self.expr.args)
        parameter_list_block = parameter_list_formatter.format_code(width-block.width)
        if parameter_list_block.width > 0:
            block.append_tokens(' ')
            block.merge(parameter_list_formatter.format_code(width-block.width))
        block.append_tokens(':', ' ')
        subexpression_formatter = self.get_formatter(self.expr.body)
        block.merge(subexpression_formatter.format_code(width - block.width,
                                                        continuation,
                                                        suffix=suffix))
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ExprFormatter(StatementFormatter):

    ast_type = ast.Expr

    def __init__(self, *args, **kwargs):
        super(ExprFormatter, self).__init__(*args, **kwargs)
        self.formatter = self.get_formatter(self.expr.value)

    def _format_code(self, *args, **kwargs):
        return self.formatter.format_code(*args, **kwargs)


class SimpleStatementFormatterBase(AstFormatter):

    keyword = None

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens(self.keyword)
        if block.width > width:
            raise NotEnoughSpace
        return block


for keyword, ast_type in [('pass', ast.Pass), ('continue', ast.Continue), ('break', ast.Break)]:
    register(type(keyword.capitalize() + 'Formatter', (SimpleStatementFormatterBase,),
                        {'keyword': keyword, 'ast_type': ast_type}))


@register
class AssertStatementFormatter(StatementFormatter):

    ast_type = ast.Assert

    def _format_code(self, width, continuation, suffix):
        test_formatter = self.get_formatter(self.expr.test)
        if self.expr.msg:
            for i in range(width+1):
                block = CodeBlock.from_tokens('assert', ' ')
                block.merge(test_formatter.format_code(width - block.width - i))
                separator = CodeBlock.from_tokens(', ')
                msg_formatter = self.get_formatter(self.expr.msg)
                try:
                    block.merge(separator)
                    block.merge(msg_formatter.format_code(width -
                                                          block.last_line.width,
                                                          continuation,
                                                          suffix=suffix))
                except NotEnoughSpace:
                    continue
                else:
                    break
        else:
            block = CodeBlock.from_tokens('assert', ' ')
            block.merge(test_formatter.format_code(width -
                                                   block.width, suffix=suffix))
        return block


@register
class ReturnFormatter(StatementFormatter):

    ast_type = ast.Return

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('return')
        if self.expr.value:
            expression_formatter = self.get_formatter(self.expr.value)
            expression_block = expression_formatter.format_code(width -
                                                                block.width - 1)
            block.merge(expression_block, separator=' ')
        if block.width > width:
            raise NotEnoughSpace
        return block

@register
class PrintFormatter(StatementFormatter):

    ast_type = ast.Print
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('print ')
        values_formatters = [self.get_formatter(e) for e in self.expr.values]
        values_formatter = self.ListOfExpressionsFormatter(values_formatters,
                                                           self.formatters_register)
        values_block = values_formatter.format_code(width=width)
        if values_block.height > 1:
            block.append_tokens('(')
            values_block = values_formatter.format_code(width, continuation,
                                                        suffix=CodeBlock.from_tokens(')'))
        block.merge(values_block)
        if block.width > width:
            raise NotEnoughSpace
        return block


@register
class RaiseFormatter(StatementFormatter):

    ast_type = ast.Raise

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('raise')
        type_formatter = self.get_formatter(self.expr.type)
        type_block = type_formatter.format_code(width - block.width - 1)
        block.merge(type_block, separator=' ')
        if self.expr.inst or self.expr.tback:
            if self.expr.inst:
                inst_block = self.get_formatter(self.expr.inst).format_code(width -
                                                                            block.width -
                                                                            2)
                block.merge(inst_block, separator=', ')
            else:
                block.append_tokens(',', ' ', 'None')
            if self.expr.tback:
                tback_block = self.get_formatter(self.expr.tback).format_code(width -
                                                                              block.width -
                                                                              2)
                block.merge(tback_block, separator=', ')

        if block.width > width:
            raise NotEnoughSpace
        return block


class ImportFormatterBase(StatementFormatter):

    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    @register
    class AliasFormatter(StatementFormatter):

        ast_type = ast.alias

        @property
        def name(self):
            return self.expr.name

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock.from_tokens(self.expr.name)
            if self.expr.asname:
                block.append_tokens('as', ' ', self.expr.asname)
            if suffix:
                block.merge(suffix)
            if block.width > width:
                raise NotEnoughSpace()
            return block

    def format_aliases(self, width):
        block = CodeBlock()
        aliases = sorted([alias for alias in self.expr.names], key=lambda a: a.name.lower())

        aliases_formatter = self.ListOfExpressionsFormatter.from_expressions(aliases, self)
        aliases_block = aliases_formatter.format_code(width)
        if aliases_block.height > 1:
            block.append_tokens('(')
            aliases_block = aliases_formatter.format_code(width - block.width, continuation=True,
                                                          suffix=CodeBlock.from_tokens(')'))
            block.merge(aliases_block)
        else:
            block.merge(aliases_block)
        return block


@register
class ImportFormatter(ImportFormatterBase):

    ast_type = ast.Import

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('import', ' ')
        block.merge(self.format_aliases(width-block.width))
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ImportFromFormatter(ImportFormatterBase):

    ast_type = ast.ImportFrom

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('from', ' ', '.' * self.expr.level)
        if self.expr.module:
            block.append_tokens(self.expr.module)
        block.append_tokens(' ', 'import', ' ')

        block.merge(self.format_aliases(width-block.width))
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class WhileFormatter(StatementFormatter):

    ast_type = ast.While

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('while ')
        test_formatter = self.get_formatter(self.expr.test)
        block.merge(test_formatter.format_code(width-block.width-1))
        block.append_tokens(':')
        block.extend(format_list_of_statements(self, self.expr.body,
                                               width=width - len(CodeLine.INDENT)),
                     indent=CodeLine.INDENT)
        if self.expr.orelse:
            block.extend(CodeBlock.from_tokens('else:'))
            block.extend(format_list_of_statements(self, self.expr.orelse,
                                                   width=width - len(CodeLine.INDENT)),
                         indent=CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block

@register
class ForFormatter(StatementFormatter):

    ast_type = ast.For

    def _format_code(self, width, continuation, suffix):
        in_ = 'in'
        block = CodeBlock([CodeLine(['for'])])
        target_formatter = self.get_formatter(self.expr.target)
        block.merge(target_formatter.format_code(width - block.width - len(in_) - 1, continuation),
                                                 separator=' ')
        block.append_tokens(' ', in_)
        iter_formatter = self.get_formatter(self.expr.iter)
        block.merge(iter_formatter.format_code(width - block.width - 1, continuation),
                                               separator=' ')
        block.append_tokens(':')
        for a in self.expr.body:
            formatter = self.get_formatter(a)
            block.extend(formatter.format_code(width-len(CodeLine.INDENT), continuation),
                                               CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block


def format_list_of_statements(parent, list_of_statements, width):
    block = CodeBlock()
    for statement in list_of_statements:
        statement_formatter = parent.get_formatter(statement)
        block.extend(statement_formatter.format_code(width=width))
    return block


@register
class TryExceptFormatter(StatementFormatter):

    ast_type = ast.TryExcept

    @register
    class ExceptHandlerFormatter(StatementFormatter):

        ast_type = ast.ExceptHandler

        def _format_code(self, width, continuation, suffix):
            block = CodeBlock.from_tokens('except')
            if self.expr.type:
                type_formatter = self.get_formatter(self.expr.type)
                block.merge(type_formatter.format_code(width - block.width - 1, continuation),
                            separator=' ')
                if self.expr.name:
                    block.append_tokens(' as ', self.expr.name.id)
            block.append_tokens(':')
            block.extend(format_list_of_statements(self, self.expr.body,
                                                   width - len(CodeLine.INDENT)),
                         indent=CodeLine.INDENT)
            return block

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('try:')
        block.extend(format_list_of_statements(self, self.expr.body,
                                               width - len(CodeLine.INDENT)),
                     indent=CodeLine.INDENT)
        for handler in self.expr.handlers:
            handler_formatter = self.get_formatter(handler)
            block.extend(handler_formatter.format_code(width=width))
        if self.expr.orelse:
            block.extend(CodeBlock.from_tokens('else:'))
            block.extend(format_list_of_statements(self, self.expr.orelse,
                                                   width - len(CodeLine.INDENT)),
                         indent=CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class TryFinallyFormatter(StatementFormatter):

    ast_type = ast.TryFinally

    def _format_code(self, width, continuation, suffix):
        block = format_list_of_statements(self, self.expr.body, width)
        block.append_lines(CodeLine('finally:'))
        block.extend(format_list_of_statements(self, self.expr.finalbody,
                                               width - len(CodeLine.INDENT)),
                     indent=CodeLine.INDENT)
        return block


@register
class AssignmentFormatter(StatementFormatter):

    ast_type = ast.Assign

    def __init__(self, *args, **kwargs):
        super(AssignmentFormatter, self).__init__(*args, **kwargs)
        self.targets_formatters = [self.get_formatter(target) for target in self.expr.targets]
        self.value_formatter = self.get_formatter(self.expr.value)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock()
        for target_formatter in self.targets_formatters:
            block.merge(target_formatter.format_code(width - block.width - 3))
            block.append_tokens(' = ')
        block.merge(self.value_formatter.format_code(width - block.width))
        return block


@register
class AugAssigmentFormatter(StatementFormatter):

    ast_type = ast.AugAssign

    def __init__(self, *args, **kwargs):
        super(AugAssigmentFormatter, self).__init__(*args, **kwargs)
        self.target_formatter = self.get_formatter(self.expr.target)
        self.operator_formatter = self.get_formatter(self.expr.op)
        self.value_formatter = self.get_formatter(self.expr.value)

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock()

        block.merge(self.target_formatter.format_code(width))
        block.merge(self.operator_formatter.format_code(width - block.width, continuation),
                    separator=' ')
        block.append_tokens('=')
        block.merge(self.value_formatter.format_code(width - block.width, continuation),
                    separator=' ')
        return block


@register
class IfFormatter(StatementFormatter):

    ast_type = ast.If

    def _format_code(self, width, continuation, suffix):
        block = CodeBlock.from_tokens('if', ' ')
        test_formatter = self.get_formatter(self.expr.test)
        block.merge(test_formatter.format_code(width-block.width-1))
        block.append_tokens(':')
        for subexpression in self.expr.body:
            subexpression_formatter = self.get_formatter(subexpression)
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT)),
                         CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block


class DecoratedDefinitionMixin(StatementFormatter):

    def __init__(self, *args, **kwargs):
        super(DecoratedDefinitionMixin, self).__init__(*args, **kwargs)
        self._decorators_formatters = [self.get_formatter(d) for d in self.expr.decorator_list]

    def _format_decorators(self, width, continuation):
        block = CodeBlock()
        for decorator_formatter in self._decorators_formatters:
            block.merge(decorator_formatter.format_code(width-1, continuation), separator='@')
            block.append_lines(CodeLine())
        return block


@register
class FunctionDefinitionFormatter(DecoratedDefinitionMixin, StatementFormatter):

    ast_type = ast.FunctionDef

    def __init__(self, *args, **kwargs):
        super(FunctionDefinitionFormatter, self).__init__(*args, **kwargs)
        self._parameter_list_formatter = self.get_formatter(self.expr.args)
        self._subexpressions_formatters = [self.get_formatter(s) for s in self.expr.body]

    def _format_code(self, width, continuation, suffix):
        block = self._format_decorators(width, continuation)
        block.append_tokens('def', ' ', self.expr.name, '(')
        block.merge(self._parameter_list_formatter
                        .format_code(width - block.last_line.width,
                                     continuation=True, suffix=CodeBlock.from_tokens('):')))
        for subexpression_formatter in self._subexpressions_formatters:
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT)),
                         CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ClassDefinitionFormater(DecoratedDefinitionMixin, StatementFormatter):

    ast_type = ast.ClassDef
    ListOfExpressionsFormatter = ListOfExpressionsFormatter

    def _format_code(self, width, continuation, suffix):
        block = self._format_decorators(width, continuation)
        block.append_tokens('class', ' ', self.expr.name)
        if self.expr.bases:
            block.append_tokens('(')
            bases_formatter = self.ListOfExpressionsFormatter.from_expressions(self.expr.bases, self)
            bases_block = bases_formatter.format_code(width - block.width)
            block.merge(bases_block)
            block.append_tokens(')')
        block.append_tokens(':')
        for subexpression in self.expr.body:
            subexpression_formatter = self.get_formatter(subexpression)
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT)),
                         CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block
