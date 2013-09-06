#-*- coding: utf-8 -*-
import ast
from itertools import chain, izip_longest
import re
import textwrap
import sys


class NotEnoughSpace(Exception):

    pass


class UnknownNodeType(Exception):

    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        attrs = ', '.join('%s=%s' % (a, getattr(self.expr, a))
                          for a in dir(self.expr)
                          if not a.startswith('_'))
        return (('Unkown expression type: %s;\n\n'
                 'dir(expr) = %s\n\n'
                 'attrs: %s') % (type(self.expr),
                                 dir(self.expr), attrs))

class CodeLine(object):

    INDENT = '    '

    def __init__(self, tokens=None):
        self.tokens = tokens or []

    def append(self, token):
        return self.tokens.append(token)

    def extend(self, tokens):
        return self.tokens.extend(tokens)

    @property
    def width(self):
        return sum((len(t) for t in self.tokens))

    def __len__(self):
        return sum((len(t) for t in self.tokens))

    def __unicode__(self):
        return u''.join(self.tokens)


class CodeBlock(object):

    def __init__(self, lines=None):
        self.lines = lines or []

    @classmethod
    def from_tokens(cls, *tokens):
        lines = [CodeLine(list(tokens))]
        return cls(lines)

    def copy(self):
        return CodeBlock(*[list(l) for l in self.lines])

    def extend(self, block, indent=None):
        if indent is not None:
            indent = indent * ' ' if isinstance(indent, int) else indent
            self.lines.extend((CodeLine([indent] + l.tokens)
                               for l in block.lines))
        else:
            self.lines.extend((CodeLine(l.tokens)
                               for l in block.lines))
        return self

    def merge(self, block, separator=None, indent=None):
        if not block.lines:
            return
        if separator:
            self.append_tokens(separator)
        lines = block.lines
        if not self.lines:
            self.append_lines(CodeLine())
        indent = len(self.lines[-1]) * ' ' if indent is None else indent * ' ' if isinstance(indent, int) else indent
        self.last_line.extend(block.lines[0].tokens)
        for original in lines[1:]:
            line = CodeLine([indent])
            line.extend(original.tokens)
            self.lines.append(line)
        return self

    def append_lines(self, *lines):
        for line in lines:
            self.lines.append(line)

    def append_tokens(self, *tokens):
        if self.last_line:
            self.last_line.extend(tokens)
        else:
            self.append_lines(CodeLine(list(tokens)))
        return self

    @property
    def last_line(self):
        return self.lines[-1] if self.lines else None

    @property
    def width(self):
        return max(len(l) for l in self.lines) if self.lines else 0

    @property
    def height(self):
        return len(self.lines)

    def __unicode__(self):
        return '\n'.join(unicode(l) for l in self.lines)


# It's better to avoid metaclasses in this case - simple register is sufficient and
# you can customize it really easily - check API usage examples in README.md

_formatters = {}

def register(cls):
    _formatters[cls.ast_type] = cls
    return cls


class CodeFormatter(object):

    # if given formatter instance allows multiple formattings
    formatable = True

    def __init__(self, formatters):
        self.formatters = formatters

    def get_formatter(self, expr, parent=None):
        return self.formatters[type(expr)](expr=expr, formatters=self.formatters,
                                           parent=self if parent is None else parent)

    def _format_code(self, width, suffix=None):
        raise NotImplementedError()

    def format_code(self, width, suffix=None):
        code = self._format_code(width, suffix)
        if code.width > width:
            raise NotEnoughSpace()
        return code


class AstFormatter(CodeFormatter):

    ast_type = None

    def __init__(self, expr, formatters, parent=None):
        assert self.ast_type is not None
        self.expr = expr
        self.parent = parent
        super(AstFormatter, self).__init__(formatters)

    def _inside_scope(self):
        return (self.parent and isinstance(self.parent.expr,
                                           (ast.Tuple, ast.Call, ast.List,
                                            ast.BinOp, ast.ListComp, ast.Dict)) or
                (getattr(self.parent, 'parent') is not None and
                 self.parent._inside_scope()))


@register
class ExprFormatter(AstFormatter):

    ast_type = ast.Expr

    def __init__(self, *args, **kwargs):
        super(ExprFormatter, self).__init__(*args, **kwargs)
        self.formatter = self.get_formatter(self.expr.value)

    def _format_code(self, *args, **kwargs):
        return self.formatter.format_code(*args, **kwargs)


class ExpressionFormatter(AstFormatter):

    pass


class AtomFormatter(ExpressionFormatter):

    ast_type = None

    def _format(self, width):
        raise NotImplementedError()

    def _format_code(self, width, suffix=None):
        block = CodeBlock([CodeLine([self._format(width)])])
        if block.width > width:
            raise NotEnoughSpace()
        return block

    def format_code(self, width, suffix=None):
        code = self._format_code(width)
        if suffix:
            code.merge(suffix)
        if code.width > width:
            raise NotEnoughSpace()
        return code


@register
class NameFormatter(AtomFormatter):

    ast_type = ast.Name

    formatable = False

    def _format(self, width):
        return unicode(self.expr.id)


class OperatorFormatter(AtomFormatter):

    operator = None
    priority = 0

    formatable = False

    def _format(self, width):
        return self.operator

ast_operator2priority = {}

for priority, ast_type, operator in [(10, ast.Pow, '**'),
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
    register(type(ast_type.__name__, (OperatorFormatter,),
                  {'ast_type': ast_type,
                   'operator': operator,
                   'priority': priority}))


class OperationFormatter(ExpressionFormatter):

    @property
    def priority(self):
        return ast_operator2priority[type(self.expr.op)]

    def force_brackets(self):
        with_brackets = False
        if self.parent:
            with_brackets = ((isinstance(self.parent, OperationFormatter) and
                              self.parent.priority > self.priority) or
                             isinstance(self.parent, AttributeFormatter))
        return with_brackets


@register
class UnaryOperationFormatter(OperationFormatter):

    ast_type = ast.UnaryOp
    operator = None

    def _format_code(self, width, suffix=None):
        op_formatter = self.get_formatter(self.expr.op)
        operator = '%s' % op_formatter.operator
        separator = ' '
        value_formatter = self.get_formatter(self.expr.operand)
        value_block = value_formatter.format_code(width - len(operator) -
                                                  len(separator))
        block = CodeBlock.from_tokens(operator)
        block.merge(value_block, separator=' ')
        return block


@register
class BinaryArithmeticOperationFormatter(OperationFormatter):

    ast_type = ast.BinOp

    def _format_code(self, width, suffix=None):
        opt_formatter = self.get_formatter(self.expr.op)
        left_formatter = self.get_formatter(self.expr.left)
        right_formatter = self.get_formatter(self.expr.right)
        def _format(with_brackets):
            block = CodeBlock()
            if with_brackets:
                block.append_tokens('(')
            indent = block.width*' '
            try:
                left_block = left_formatter.format_code(width-block.width)
                operator_block = opt_formatter.format_code(width - block.width -
                                                           left_block.width - 1)
                right_block = right_formatter.format_code(width - block.width -
                                                          operator_block.width - 2 -
                                                          left_block.width)
                block.merge(left_block)
                block.merge(operator_block, separator= ' ')
                block.merge(right_block, separator=' ')
            except NotEnoughSpace:
                operator = opt_formatter.operator
                left_block = left_formatter.format_code(width - len(indent))
                right_block = right_formatter.format_code(width - len(indent))
                block.merge(left_block)
                block.append_tokens(' ', operator)
                block.extend(right_block, indent)
            if with_brackets:
                block.append_tokens(')')
            return block, right_block
        with_brackets = self.force_brackets()
        block, right_subblock = _format(with_brackets)
        if ((not self.parent or
             not isinstance(self.parent,
                            (OperationFormatter, CallFormatter))) and
            block.height > 1 and
            right_subblock.height != block.height):
            block, _ = _format(not self._inside_scope())
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class CompareFormatter(OperationFormatter):

    ast_type = ast.Compare

    @property
    def priority(self):
        return ast_operator2priority[type(self.expr.ops[0])]

    def _format_operator_chain(self, width, operators, comparators):
        if not operators:
            return CodeBlock()
        block = CodeBlock()
        operator = operators[0]
        comparator = comparators[0]
        operator_formatter = self.get_formatter(operator)
        comparator_formatter = self.get_formatter(comparator)
        width = width if width > 0 else 1
        for i in range(width):
            operator_block = operator_formatter.format_code(width - i)
            comparator_block = comparator_formatter.format_code(width -
                                                                operator_block.width - 1)
            try:
                chain_block = self._format_operator_chain(width - operator_block.width -
                                                          comparator_block.last_line.width - 2,
                                                          operators[1:], comparators[1:])
            except NotEnoughSpace:
                continue
            else:
                break
        block.merge(operator_block)
        block.merge(comparator_block, separator=' ')
        block.merge(chain_block, separator=' ')
        if block.width > width:
            raise NotEnoughSpace()
        return block

    def _format_code(self, width, suffix=None):
        block = CodeBlock()
        left_formatter = self.get_formatter(self.expr.left)
        with_brackets = self.force_brackets()
        if with_brackets:
            block.append_tokens('(')
        for i in range(width-block.width):
            left_block = left_formatter.format_code(width-block.width-i)
            try:
                chain_block = self._format_operator_chain(width - left_block.last_line.width - (2 if with_brackets else 1),
                                                          self.expr.ops, self.expr.comparators)
            except NotEnoughSpace:
                continue
            else:
                block.merge(left_block)
                block.merge(chain_block, separator=' ')
                break
        if with_brackets:
            block.append_tokens(')')
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class BooleanOperationFormatter(OperationFormatter):

    ast_type = ast.BoolOp

    def _format_code(self, width, suffix=None):
        def _format(with_brackets):
            block = CodeBlock()
            if with_brackets:
                block.append_tokens('(')
            opt_formatter = self.get_formatter(self.expr.op)
            value_formatter = self.get_formatter(self.expr.values[0])
            indent = block.width*' '
            block.merge(value_formatter.format_code(width - block.width))
            for e in self.expr.values[1:]:
                value_formatter = self.get_formatter(e)
                try:
                    operator_block = opt_formatter.format_code(width-2)
                    value_block = value_formatter.format_code(width -
                                                              block.width -
                                                              operator_block.width -
                                                              2)
                    block.merge(operator_block, separator=' ')
                    block.merge(value_block, separator=' ')
                except NotEnoughSpace:
                    value_block = value_formatter.format_code(width -
                                                              len(indent) - 1)
                    block.append_tokens(' ', opt_formatter.operator)
                    block.extend(value_block, indent)
            if with_brackets:
                block.append_tokens(')')
            return block, value_block
        with_brackets = self.force_brackets()
        block, last_subblock = _format(with_brackets)
        if (not with_brackets and not self._inside_scope() and
            block.height > 1 and last_subblock.height != block.height and
             not isinstance(self.parent, BooleanOperationFormatter)):
            block, _ = _format(True)
        # FIXME: suffix should be passed to last expression value formatter
        if suffix:
            block.merge(suffix)
        return block


@register
class NumFormatter(AtomFormatter):

    ast_type = ast.Num

    formatable = False

    def _format(self, width):
        return repr(self.expr.n)


@register
class StringFormatter(ExpressionFormatter):

    ast_type = ast.Str

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

    def _format_code(self, width, suffix=None):
        block = CodeBlock()
        # check wether this expression is a docstring:
        # * if it is a docstring it is child of an expression statement
        # * parent of this expression statement should be class or function definition statement
        # * this expression statement should be first statement in parent's body
        real_parent = self.parent.parent if (isinstance(self.parent,
                                                        ExprFormatter) and
                                             self.parent.parent) else self.parent
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
                if self._inside_scope():
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

    @property
    def formatable(self):
        return self.value_formatter.formatable

    def _format_code(self, width, suffix=None):
        block = self.value_formatter.format_code(width-len(self.expr.attr)-1)
        block.append_tokens('.', self.expr.attr)
        if suffix:
            block.merge(suffix)
        return block


def format_list_of_expressions(expressions, width, line_width=None, suffix=None):
    block = CodeBlock()
    if not expressions:
        if suffix:
            return suffix
        else:
            return block
    line_width = line_width if line_width is not None else width
    expression = expressions[0]
    expressions = expressions[1:]
    merged_block_indent = lambda: line_width - width + (block.last_line or CodeLine()).width
    if expressions:
        if not expression.formatable:
            try:
                expression_block = expression.format_code(width - 2)
                expressions_block = format_list_of_expressions(expressions,
                                                               width - expression_block.last_line.width - 2,
                                                               line_width, suffix)
            except NotEnoughSpace:
                pass
            else:
                block.merge(expression_block, indent=merged_block_indent())
                block.append_tokens(', ')
                block.merge(expressions_block, indent=0)
                return block
        else:
            failing_expression_width = None
            failing_expressions_width = None
            curr_width = width
            while True:
                try:
                    expression_block = expression.format_code(curr_width)
                except NotEnoughSpace:
                    failing_expression_width = curr_width
                    if failing_expressions_width is None or (failing_expressions_width-failing_expression_width) <= 1:
                        break
                    curr_width += 1
                else:
                    try:
                        expressions_block = format_list_of_expressions(expressions,
                                                                       width - expression_block.last_line.width - 2,
                                                                       line_width, suffix)
                    except NotEnoughSpace:
                        failing_expressions_width = curr_width
                        if failing_expression_width is not None and (failing_expressions_width-failing_expression_width) <= 1:
                                break
                        elif failing_expression_width is None:
                            curr_width = failing_expressions_width/2
                        else:
                            curr_width = failing_expression_width + (failing_expressions_width - failing_expressions_width)/2
                    else:
                        if (failing_expressions_width is None or
                              (curr_width - failing_expressions_width <= 1)):
                            block.merge(expression_block, indent=merged_block_indent())
                            block.append_tokens(', ')
                            block.merge(expressions_block, indent=0)
                            return block
                        curr_width = (failing_expression_width + failing_expressions_width)/2

        expression_block = expression.format_code(width - 1)
        expressions_block = format_list_of_expressions(expressions, line_width,
                                                       line_width, suffix)
        block.merge(expression_block, indent=merged_block_indent())
        block.append_tokens(',')
        block.extend(expressions_block)
        return block
    expression_block = expression.format_code(width, suffix=suffix)
    block.merge(expression_block,
                indent=merged_block_indent())
    #if suffix:
    #    block.merge(suffix)
    #    if block.width > width:
    #        raise NotEnoughSpace()
    return block


@register
class CallFormatter(ExpressionFormatter):

    ast_type = ast.Call

    @register
    class KeywordArg(ExpressionFormatter):

        ast_type = ast.keyword

        @property
        def formatable(self):
            return self.get_formatter(self.expr.value).formatable

        def _format_code(self, width, suffix=None):
            block = CodeBlock([CodeLine(['%s=' % self.expr.arg])])
            expression_formatter = self.get_formatter(self.expr.value)
            expression_block = expression_formatter.format_code(width - block.width,
                                                                suffix=suffix)
            block.merge(expression_block)
            return block

    class StarArgsFormatter(CodeFormatter):

        def __init__(self, subexpression, prefix, formatters):
            self.subexpression = subexpression
            self.prefix = prefix
            self.formatters = formatters

        @property
        def formatable(self):
            return self.formatters[type(self.subexpression)](self.subexpression,
                                                             formatters=self.formatters).formatable

        def _format_code(self, width, suffix=None):
            block = CodeBlock.from_tokens(self.prefix)
            subexpression_formatter = self.formatters[type(self.subexpression)](self.subexpression,
                                                                                formatters=self.formatters)
            block.merge(subexpression_formatter.format_code(width - block.width,
                                                            suffix=suffix))
            return block

    def __init__(self, *args, **kwargs):
        super(CallFormatter, self).__init__(*args, **kwargs)
        self._func_formatter = self.get_formatter(self.expr.func)
        self._arguments_formatters = self._get_arguments_formatters()

    @property
    def formatable(self):
        return (self._func_formatter.formatable or
                self._arguments_formatters and (len(self._arguments_formatters) > 1 or
                                                self._arguments_formatters[0].formatable))

    def _get_arguments_formatters(self):
        expressions = [self.get_formatter(e) for e in self.expr.args]
        if self.expr.starargs:
            expressions.append(CallFormatter.StarArgsFormatter(self.expr.starargs, '*',
                                                               formatters=self.formatters))
        expressions += [self.get_formatter(e) for e in self.expr.keywords]
        if self.expr.kwargs:
            expressions.append(CallFormatter.StarArgsFormatter(self.expr.kwargs, '**',
                                                               formatters=self.formatters))
        return expressions

    def _format_code(self, width, suffix=None):
        block = self._func_formatter.format_code(width)
        block.lines[-1].append('(')
        suffix = CodeBlock.from_tokens(')').merge(suffix) if suffix else CodeBlock.from_tokens(')')
        subblock = format_list_of_expressions(self._arguments_formatters, width - block.width,
                                              suffix=suffix)
        block.merge(subblock)
        return block


@register
class DictonaryFormatter(ExpressionFormatter):

    ast_type = ast.Dict

    class Item(ExpressionFormatter):

        def __init__(self, key, value, parent, formatters):
            self.formatters = formatters
            self.key = self.formatters[type(key)](key, parent=parent, formatters=formatters)
            self.value = self.formatters[type(value)](value, parent=parent,
                                                      formatters=formatters)

        def _format_code(self, width, suffix=None):
            # FIXME: search for solution on failure
            separator = ':'
            block = self.key.format_code(width - len(separator) - 2 -
                                         (suffix or CodeBlock()).width)
            block.lines[-1].append(separator)
            block.merge(self.value.format_code(width - block.width, suffix=suffix),
                        separator=' ')
            return block

    def _format_code(self, width, suffix=None):
        block = CodeBlock([CodeLine(['{'])])
        expressions = [DictonaryFormatter.Item(k, v, self, self.formatters)
                       for k, v in zip(self.expr.keys,
                                       self.expr.values)]
        suffix = CodeBlock.from_tokens('}').merge(suffix) if suffix else CodeBlock.from_tokens('}')
        subblock = format_list_of_expressions(expressions=expressions,
                                              width=width-block.width,
                                              suffix=suffix)
        block.merge(subblock)
        return block


@register
class ListFormatter(ExpressionFormatter):

    ast_type = ast.List

    def _format_code(self, width, suffix=None):
        block = CodeBlock([CodeLine(['['])])
        expressions = [self.get_formatter(v)
                       for v in self.expr.elts]
        suffix = CodeBlock.from_tokens(']').merge(suffix) if suffix else CodeBlock.from_tokens(']')
        subblock = format_list_of_expressions(expressions=expressions,
                                              width=width - block.width,
                                              suffix=suffix)
        block.merge(subblock)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ListComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.ListComp

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('[')
        indent = block.width * ' '
        elt_formatter = self.get_formatter(self.expr.elt)
        elt_block = elt_formatter.format_code(width - block.width)
        block.merge(elt_block)
        suffix = CodeBlock.from_tokens(']').merge(suffix) if suffix else CodeBlock.from_tokens(']')
        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters=self.formatters,
                                                 suffix=suffix)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent), parent=self,
                                                 formatters=self.formatters,
                                                 suffix=suffix)
            block.extend(generators_block, indent)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class SetFormatter(ExpressionFormatter):

    ast_type = ast.Set

    def _format_code(self, width, suffix=None):
        block = CodeBlock([CodeLine(['{'])])
        expressions = [self.get_formatter(v)
                       for v in self.expr.elts]
        subblock = format_list_of_expressions(expressions=expressions,
                                              width=width-block.width)
        block.merge(subblock)
        block.lines[-1].append('}')
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class SetComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.SetComp

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('{')
        indent = block.width * ' '
        elt_formatter = self.get_formatter(self.expr.elt)
        elt_block = elt_formatter.format_code(width - block.width)
        block.merge(elt_block)
        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters=self.formatters)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent),
                                                 parent=self,
                                                 formatters=self.formatters)
            block.extend(generators_block, indent)
        block.append_tokens('}')
        return block


@register
class IfExpressionFormatter(ExpressionFormatter):

    ast_type = ast.IfExp

    def _format_code(self, width, suffix=None):
        block = CodeBlock()
        # conditional expression has lowest priority
        with_brackets = isinstance(self.parent, OperationFormatter)
        if with_brackets:
            block.append_tokens('(')
        body_formatter = self.get_formatter(self.expr.body)
        block.merge(body_formatter.format_code(width))
        test_formatter = self.get_formatter(self.expr.test)

        block.append_tokens(' ', 'if', ' ')
        test_block = test_formatter.format_code(width-block.width)
        block.merge(test_block)
        orelse_formatter = self.get_formatter(self.expr.orelse)
        block.append_tokens(' ', 'else', ' ')
        orelse_block = orelse_formatter.format_code(width - block.width - (1 if with_brackets else 0))
        block.merge(orelse_block)
        if with_brackets:
            block.append_tokens(')')
        return block


@register
class SubscriptionFormatter(ExpressionFormatter):

    ast_type = ast.Subscript

    def _format_code(self, width, suffix=None):
        value_formatter = self.get_formatter(self.expr.value)
        block = value_formatter.format_code(width)
        slice_formatter = self.get_formatter(self.expr.slice)
        block.merge(slice_formatter.format_code(width - len(block.lines[-1])))
        return block


@register
class SliceFormatter(ExpressionFormatter):

    ast_type = ast.Slice

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('[')
        if self.expr.lower:
            lower_formatter = self.get_formatter(self.expr.lower)
            block.merge(lower_formatter.format_code(width-block.width-1))
        block.append_tokens(':')

        if self.expr.upper:
            upper_formatter = self.get_formatter(self.expr.upper)
            block.merge(upper_formatter.format_code(width-block.width-1))

        if self.expr.step:
            block.append_tokens(':')
            step_formatter = self.get_formatter(self.expr.step)
            block.merge(step_formatter.format_code(width-block.width-1))

        block.append_tokens(']')
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class IndexFormatter(ExpressionFormatter):

    ast_type = ast.Index

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('[')
        value_formatter = self.get_formatter(self.expr.value)
        block.merge(value_formatter.format_code(width - 2))
        block.append_tokens(']')
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class GeneratorFormatter(ExpressionFormatter):

    ast_type = ast.GeneratorExp

    def __init__(self, expr, formatters, parent=None):
        self.expr = expr
        self.parent = parent
        self.formatters = formatters

    def _format_code(self, width, suffix=None):
        value_formatter = self.get_formatter(self.expr.elt)
        with_brackets = (not self.parent or not isinstance(self.parent,
                                                           CallFormatter) or
                         len(self.parent.expr.args) != 1)
        if with_brackets:
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
                                                 formatters=self.formatters,
                                                 suffix=suffix)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent),
                                                 parent=self,
                                                 formatters=self.formatters,
                                                 suffix=suffix)
            block.extend(generators_block, indent)

        if with_brackets:
            block.append_tokens(')')
        # FIXME: raise exception
        return block


def format_generators(generators, width, parent, formatters, suffix=None):
    block = CodeBlock()
    def _get_formatter(expr):
        return formatters[type(expr)](expr, parent=parent, formatters=formatters)
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

    def _format_code(self, width, suffix=None):
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

        try:
            generators_block = format_generators(self.expr.generators,
                                                 width - block.width - 1,
                                                 parent=self,
                                                 formatters=self.formatters)
            block.merge(generators_block, separator=' ')
        except NotEnoughSpace:
            generators_block = format_generators(self.expr.generators,
                                                 width - len(indent),
                                                 parent=self,
                                                 formatters=self.formatters)
            block.extend(generators_block, indent)
        block.append_tokens('}')
        return block


@register
class TupleFormatter(ExpressionFormatter):

    ast_type = ast.Tuple

    def _format_code(self, width, suffix=None):
        with_brackets = (isinstance(self.parent.expr, (ast.Tuple, ast.Call,
                                                       ast.List, ast.BinOp,
                                                       ast.ListComp, ast.GeneratorExp)) or
                         len(self.expr.elts) < 2)
        block = CodeBlock()
        expressions = [self.get_formatter(v)
                       for v in self.expr.elts]
        if with_brackets:
            block.append_tokens('(')
        expression_block = format_list_of_expressions(expressions,
                                                      width-block.width,
                                                      suffix=suffix)
        if expression_block.height > 1 and not with_brackets:
            block.append_tokens('(')
            with_brackets = True
            expression_block = format_list_of_expressions(expressions,
                                                          width - block.width,
                                                          suffix=suffix)
        block.merge(expression_block)
        # FIXME: to be 'super' consistent we should check last line
        #        and enforce reformatting... or change API somehow
        if len(self.expr.elts) == 1:
            block.append_tokens(',')
        if with_brackets:
            block.append_tokens(')')
        return block


@register
class ParameterListFormatter(AstFormatter):

    ast_type = ast.arguments

    class KwargFormatter(CodeFormatter):

        def __init__(self, parameter, expression, parent, formatters):
            self.parameter = parameter
            self.expression = expression
            self.parent = parent
            super(ParameterListFormatter.KwargFormatter, self).__init__(formatters)

        def get_formatter(self, expression):
            return super(ParameterListFormatter.KwargFormatter,
                         self).get_formatter(expression,
                                             self.parent)

        def _format_code(self, width, suffix=None):
            parameter_formatter = self.get_formatter(self.parameter)
            expression_formatter = self.get_formatter(self.expression)
            block = parameter_formatter.format_code(width-1)
            block.append_tokens('=')
            block.merge(expression_formatter.format_code(width - block.width,
                                                         suffix=suffix))
            if block.width > width:
                raise NotEnoughSpace()
            return block


    class VarargFormatter(CodeFormatter):

        def __init__(self, vararg, parent, formatters):
            self.vararg = vararg
            self.parent = parent
            super(ParameterListFormatter.VarargFormatter, self).__init__(formatters)

        def get_formatter(self, expression):
            return super(ParameterListFormatter.VarargFormatter,
                         self).get_formatter(expression,
                                             self.parent)

        def _format_code(self, width, suffix=None):
            block = CodeBlock.from_tokens('*%s' % self.vararg)
            if block.width > width:
                raise NotEnoughSpace()
            return block


    def _format_code(self, width, suffix=None):
        formatters = [self.get_formatter(arg)
                      for arg in self.expr.args[:len(self.expr.args) -
                                                 len(self.expr.defaults)]]
        if self.expr.vararg:
            formatters.append(ParameterListFormatter.VarargFormatter(self.expr.vararg, self,
                                                                     self.formatters))
        formatters += [ParameterListFormatter.KwargFormatter(arg, default, self, self.formatters)
                       for (arg, default)
                       in zip(self.expr.args[(len(self.expr.args)-len(self.expr.defaults)):],
                              self.expr.defaults)]
        return format_list_of_expressions(formatters, width)


@register
class LambdaFormatter(ExpressionFormatter):

    ast_type = ast.Lambda

    def _format_code(self, width, suffix=None):
        with_brackets = isinstance(self.parent, (OperatorFormatter, IfExpressionFormatter))
        if with_brackets:
            block = CodeBlock.from_tokens('(lambda')
        else:
            block = CodeBlock.from_tokens('lambda')
        parameter_list_formatter = self.get_formatter(self.expr.args)
        parameter_list_block = parameter_list_formatter.format_code(width-block.width)
        if parameter_list_block.width > 0:
            block.append_tokens(' ')
            block.merge(parameter_list_formatter.format_code(width-block.width))
        block.append_tokens(':', ' ')
        subexpression_formatter = self.get_formatter(self.expr.body)
        block.merge(subexpression_formatter.format_code(width - block.width))
        if with_brackets:
            block.append_tokens(')')
        if block.width > width:
            raise NotEnoughSpace()
        return block


class StatementFormatter(AstFormatter):

    pass


class SimpleStatementFormatterBase(AstFormatter):

    keyword = None

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens(self.keyword)
        if block.width > width:
            raise NotEnoughSpace
        return block


for keyword, ast_type in [('pass', ast.Pass), ('continue', ast.Continue), ('break', ast.Break)]:
    register(type(keyword.capitalize() + 'Formatter', (SimpleStatementFormatterBase,),
             {'keyword': keyword, 'ast_type': ast_type}))


@register
class ReturnFormatter(StatementFormatter):

    ast_type = ast.Return

    def _format_code(self, width, suffix=None):
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

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('print ')
        values_formatters = [self.get_formatter(e) for e in self.expr.values]
        values_block = format_list_of_expressions(values_formatters, width=width)
        if values_block.height > 1:
            block.append_tokens('(')
            values_block = format_list_of_expressions(values_formatters, width=width,
                                                      suffix=CodeBlock.from_tokens(')'))
        block.merge(values_block)
        if block.width > width:
            raise NotEnoughSpace
        return block


@register
class RaiseFormatter(StatementFormatter):

    ast_type = ast.Raise

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('raise')
        type_formatter = self.get_formatter(self.expr.type)
        type_block = type_formatter.format_code(width - block.width - 1)
        block.merge(type_block, separator=' ')
        if self.expr.inst or self.expr.tback:
            if self.expr.inst:
                inst_block = self.get_formatter(self.expr.inst).format_code(width - block.width - 2)
                block.merge(inst_block, separator=', ')
            else:
                block.append_tokens(',', ' ', 'None')
            if self.expr.tback:
                tback_block = self.get_formatter(self.expr.tback).format_code(width - block.width - 2)
                block.merge(tback_block, separator=', ')

        if block.width > width:
            raise NotEnoughSpace
        return block


class ImportFormatterBase(StatementFormatter):

    @register
    class AliasFormatter(StatementFormatter):

        ast_type = ast.alias

        @property
        def name(self):
            return self.expr.name

        def _format_code(self, width, suffix=None):
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
        aliases = sorted([self.get_formatter(alias)
                          for alias in self.expr.names], key=lambda a: a.name.lower())
        aliases_block = format_list_of_expressions(aliases, width)
        if aliases_block.height > 1:
            block.append_tokens('(')
            aliases_block = format_list_of_expressions(aliases, width - block.width,
                                                       suffix=CodeBlock.from_tokens(')'))
            block.merge(aliases_block)
        else:
            block.merge(aliases_block)
        return block


@register
class ImportFormatter(ImportFormatterBase):

    ast_type = ast.Import

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('import', ' ')
        block.merge(self.format_aliases(width-block.width))
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ImportFromFormatter(ImportFormatterBase):

    ast_type = ast.ImportFrom

    def _format_code(self, width, suffix=None):
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

    def _format_code(self, width, suffix=None):
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

    def _format_code(self, width, suffix=None):
        in_ = 'in'
        block = CodeBlock([CodeLine(['for'])])
        target_formatter = self.get_formatter(self.expr.target)
        block.merge(target_formatter.format_code(width - block.width - len(in_) - 1),
                                                 separator=' ')
        block.append_tokens(' ', in_)
        iter_formatter = self.get_formatter(self.expr.iter)
        block.merge(iter_formatter.format_code(width - block.width - 1),
                                               separator=' ')
        block.append_tokens(':')
        for a in self.expr.body:
            formatter = self.get_formatter(a)
            block.extend(formatter.format_code(width-len(CodeLine.INDENT)),
                                               CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block


def format_list_of_statements(parent, list_of_statements, width):
    block = CodeBlock()
    for statement in list_of_statements:
        #print statement, parent.get_formatter(statement)
        statement_formatter = parent.get_formatter(statement)
        block.extend(statement_formatter.format_code(width=width))
    return block

@register
class TryExceptFormatter(StatementFormatter):

    ast_type = ast.TryExcept


    @register
    class ExceptHandlerFormatter(StatementFormatter):

        ast_type = ast.ExceptHandler

        def _format_code(self, width, suffix=None):
            block = CodeBlock.from_tokens('except')
            if self.expr.type:
                type_formatter = self.get_formatter(self.expr.type)
                block.merge(type_formatter.format_code(width=width - block.width - 1),
                            separator=' ')
                if self.expr.name:
                    block.append_tokens(' as ', self.expr.name.id)
            block.append_tokens(':')
            block.extend(format_list_of_statements(self, self.expr.body,
                                                   width - len(CodeLine.INDENT)),
                         indent=CodeLine.INDENT)
            return block


    def _format_code(self, width, suffix=None):
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

    def _format_code(self, width, suffix=None):
        block = format_list_of_statements(self, self.expr.body, width)
        block.append_lines(CodeLine('finally:'))
        block.extend(format_list_of_statements(self, self.expr.finalbody,
                                               width - len(CodeLine.INDENT)),
                     indent=CodeLine.INDENT)
        return block



@register
class AssignmentFormatter(StatementFormatter):

    ast_type = ast.Assign

    def _format_code(self, width, suffix=None):
        block = CodeBlock()
        for target in self.expr.targets:
            target_formatter = self.get_formatter(target)

            block.merge(target_formatter.format_code(width - block.width - 3))
            block.append_tokens(' = ')
        value_formatter = self.get_formatter(self.expr.value)
        block.merge(value_formatter.format_code(width - block.width))
        return block

@register
class AugAssigmentFormatter(StatementFormatter):

    ast_type = ast.AugAssign

    def _format_code(self, width, suffix=None):
        block = CodeBlock()
        target_formatter = self.get_formatter(self.expr.target)
        operator_formatter = self.get_formatter(self.expr.op)
        value_formatter = self.get_formatter(self.expr.value)

        block.merge(target_formatter.format_code(width))
        block.merge(operator_formatter.format_code(width - block.width),
                    separator=' ')
        block.append_tokens('=')
        block.merge(value_formatter.format_code(width - block.width),
                    separator=' ')
        return block


@register
class IfFormatter(StatementFormatter):

    ast_type = ast.If

    def _format_code(self, width, suffix=None):
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


@register
class FunctionDefinitionFormatter(StatementFormatter):

    ast_type = ast.FunctionDef

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('def', ' ', self.expr.name, '(')
        parameter_list_formatter = self.get_formatter(self.expr.args)
        block.merge(parameter_list_formatter.format_code(width-block.width))
        block.append_tokens('):')
        for subexpression in self.expr.body:
            subexpression_formatter = self.get_formatter(subexpression)
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT)),
                         CodeLine.INDENT)
        if block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ClassDefinitionFormater(StatementFormatter):

    ast_type = ast.ClassDef

    def _format_code(self, width, suffix=None):
        block = CodeBlock.from_tokens('class', ' ', self.expr.name)
        if self.expr.bases:
            block.append_tokens('(')
            bases = [self.formatters[type(b)](b, self.formatters, parent=self) for b in self.expr.bases]
            bases_block = format_list_of_expressions(bases, width - block.width)
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


def _format_code(code, width, formatters, force=False):
    """Returns CodeBlock instance as result"""
    tree = ast.parse(code)
    result = []
    for e in tree.body:
        formatter = formatters[type(e)](expr=e, formatters=formatters, parent=None)
        if force:
            statement_width = width
            failing_statement_width = None
            succeeding_statement_width = None
            while True:
                try:
                    s = formatter.format_code(statement_width)
                except NotEnoughSpace:
                    failing_statement_width = statement_width
                else:
                    succeeding_statement_width = statement_width
                    if (failing_statement_width is None or
                        statement_width - 1 == failing_statement_width):
                        result.append(s)
                        break
                if succeeding_statement_width and failing_statement_width and succeeding_statement_width - failing_statement_width == 1:
                    s = formatter.format_code(succeeding_statement_width)
                    result.append(s)
                    break
                statement_width = ((failing_statement_width +
                                    (succeeding_statement_width or
                                     3 * failing_statement_width)) / 2)
        else:
            result.append(formatter.format_code(width))
    return result

def format_code(code, width=80, formatters=_formatters.copy(), force=False):
    """Returns string as a result"""
    result = _format_code(code, width=width, formatters=formatters, force=force)
    return u'\n'.join(unicode(e) for e in result)
