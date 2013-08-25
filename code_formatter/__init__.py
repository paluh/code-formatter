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
        return (('Unkown expression type: %s;\n'
                 '\n'
                 'dir(expr) = %s\n'
                 '\n'
                 'attrs: %s') % (type(self.expr),
                                 dir(self.expr), attrs))

class CodeLine(object):

    INDENT = '    '

    def __init__(self, tokens=None):
        self.tokens = tokens or []

    def indent(self, indent):
        self.tokens.insert(0, indent)
        return self

    def append(self, token):
        return self.tokens.append(token)

    def extend(self, tokens):
        return self.tokens.extend(tokens)

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

    def extend(self, block, indent=None):
        if indent:
            self.lines.extend((CodeLine([indent] + l.tokens)
                               for l in block.lines))
        else:
            self.lines.extend((CodeLine(l.tokens)
                               for l in block.lines))
        return self

    def merge(self, block, separator=None):
        if not block.lines:
            return
        if separator:
            self.append_tokens(separator)
        lines = block.lines
        if not self.lines:
            self.append_lines(CodeLine())
        indent = len(self.lines[-1])*' '
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


class AstFormatter(object):

    def __init__(self, expr, formatters, parent=None):
        self.expr = expr
        self.parent = parent
        self.formatters = formatters

    def get_formatter(self, expr):
        return self.formatters[type(expr)](expr=expr, formatters=self.formatters,
                                           parent=self)

    def format_code(self, width, force=False):
        raise NotImplementedError()


@register
class ExprFormatter(AstFormatter):

    ast_type = ast.Expr

    def __init__(self, *args, **kwargs):
        super(ExprFormatter, self).__init__(*args, **kwargs)
        self.formatter = self.get_formatter(self.expr.value)

    def format_code(self, *args, **kwargs):
        return self.formatter.format_code(*args, **kwargs)


class ExpressionFormatter(AstFormatter):

    pass


class AtomFormatter(ExpressionFormatter):

    ast_type = None

    def _format_code(self):
        raise NotImplementedError()

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine([self._format_code()])])
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class NameFormatter(AtomFormatter):

    ast_type = ast.Name

    def _format_code(self):
        return unicode(self.expr.id)


class OperatorFormatter(AtomFormatter):

    operator = None
    priority = 0

    def _format_code(self):
        return self.operator

ast_operator2priority = {}

for priority, ast_type, operator in [(8, ast.Pow, '**'),
                                     (7, ast.Mult, '*'),
                                     (7, ast.FloorDiv, '//'),
                                     (7, ast.Div, '/'),
                                     (7, ast.Mod, '%'),
                                     (6, ast.Add, '+'),
                                     (6, ast.Sub, '-'),
                                     (5, ast.RShift, '>>'),
                                     (5, ast.LShift, '<<'),
                                     (4, ast.BitXor, '^'),
                                     (3, ast.BitAnd, '&'),
                                     (2, ast.BitOr, '|'),
                                     (1, ast.Gt, '>'),
                                     (1, ast.GtE, '>='),
                                     (1, ast.Lt, '<'),
                                     (1, ast.LtE, '<='),
                                     (1, ast.Eq, '=='),
                                     (1, ast.NotEq, '!='),
                                     (1, ast.Is, 'is'),
                                     (1, ast.IsNot, 'is not'),
                                     (1, ast.In, 'in'),
                                     (1, ast.NotIn, 'not in'),
                                     (0, ast.Or, 'or'),
                                     (0, ast.And, 'and'),
                                     (0, ast.Not, 'not')]:
    ast_operator2priority[ast_type] = priority
    register(type(ast_type.__name__, (OperatorFormatter,),
                  {'ast_type': ast_type,
                   'operator': operator,
                   'priority': priority}))


class OperationFormatter(ExpressionFormatter):

    @property
    def priority(self):
        return ast_operator2priority[type(self.expr.op)]

    def should_force_brackets(self):
        with_brackets = False
        if self.parent:
            with_brackets = (isinstance(self.parent, OperationFormatter) and
                             self.parent.priority > self.priority)
        return with_brackets


@register
class UnaryOperationFormatter(OperationFormatter):

    ast_type = ast.UnaryOp
    operator = None

    def format_code(self, width, force=False):
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

    def format_code(self, width, force=False):
        opt_formatter = self.get_formatter(self.expr.op)
        left_formatter = self.get_formatter(self.expr.left)
        right_formatter = self.get_formatter(self.expr.right)
        def _format_code(with_brackets):
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
                operator = '%s' % opt_formatter.operator
                left_block = left_formatter.format_code(width - len(indent) -
                                                        len(operator) - 1,
                                                        force=force)
                right_block = right_formatter.format_code(width-len(indent), force=force)
                block.merge(left_block)
                block.append_tokens(' ', operator)
                block.extend(right_block, indent)
            if with_brackets:
                block.append_tokens(')')
            return block, right_block
        with_brackets = self.should_force_brackets()
        block, right_subblock = _format_code(with_brackets)
        if ((not self.parent or
             not isinstance(self.parent,
                            (OperationFormatter, Call))) and
            block.height > 1 and
            right_subblock.height != block.height):
            block, _ = _format_code(True)
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class CompareFormatter(OperationFormatter):

    ast_type = ast.Compare

    @property
    def priority(self):
        return ast_operator2priority[type(self.expr.ops[0])]

    def format_code(self, width, force=False):
        block = CodeBlock()
        with_brackets = self.should_force_brackets()
        if with_brackets:
            block.append_tokens('(')
        left_formatter = self.get_formatter(self.expr.left)
        block.merge(left_formatter.format_code(width-block.width, force=force))
        # FIXME: here we should generate all possible permutations of comparators
        #        formatting
        for operator, comparator in zip(self.expr.ops, self.expr.comparators):
            operator_formatter = self.get_formatter(operator)
            block.merge(operator_formatter.format_code(width - block.width - 1,
                                                       force=force),
                        separator=' ')
            comparator_formatter = self.get_formatter(comparator)
            block.merge(comparator_formatter.format_code(width - block.width,
                                                         force=force),
                        separator=' ')
        if with_brackets:
            block.append_tokens(')')
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class BooleanOperationFormatter(OperationFormatter):

    ast_type = ast.BoolOp

    def format_code(self, width, force=False):
        def _format_code(with_brackets):
            block = CodeBlock()
            if with_brackets:
                block.append_tokens('(')
            opt_formatter = self.get_formatter(self.expr.op)
            value_formatter = self.get_formatter(self.expr.values[0])
            indent = block.width*' '
            block.merge(value_formatter.format_code(width - block.width,
                                                    force=force))
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
                                                              len(indent) - 1,
                                                              force=force)
                    block.append_tokens(' ', opt_formatter.operator)
                    block.extend(value_block, indent)
            if with_brackets:
                block.append_tokens(')')
            return block, value_block
        with_brackets = self.should_force_brackets()
        block, last_subblock = _format_code(with_brackets)
        if not with_brackets and block.height > 1 and last_subblock.height != block.height:
            block, _ = _format_code(True)
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class NumFormatter(AtomFormatter):

    ast_type = ast.Num

    def _format_code(self):
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

    def format_code(self, width, force=False):
        block = CodeBlock()
        # FIXME: rewrite parent detection in more generic maner
        real_parent = self.parent.parent if isinstance(self.parent, ExprFormatter) and self.parent.parent else self.parent
        if isinstance(real_parent, (FunctionDefinitionFormatter, ClassDefinitionFormater)):
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
                    lines = [line for line in re.split('([^\n]*\n+)', string)]
                else:
                    lines = [string]
                result = []
                for line in lines:
                    result.extend(textwrap.wrap(line, width=width, expand_tabs=False,
                                                replace_whitespace=False, fix_sentence_endings=False,
                                                break_long_words=False, drop_whitespace=False))
                return result
            lines = format_lines(self.expr.s, width if width > 0 else 1)
            if len(lines) > 1:
                lines = format_lines(self.expr.s, width-2 if width-2 > 0 else 2)
            if len(lines) > 1:
                block.append_tokens('(')
                block.append_tokens(repr(lines[0]))
                block.append_lines(*(CodeLine([' ', repr(l)]) for l in lines[1:]))
                block.append_tokens(')')
            else:
                block.append_tokens(repr(lines[0]))
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class AttributeFormatter(ExpressionFormatter):

    ast_type = ast.Attribute

    def format_code(self, width, force=False):
        value_formatter = self.get_formatter(self.expr.value)
        block = value_formatter.format_code(width-len(self.expr.attr)-1, force=force)
        return block.append_tokens('.', self.expr.attr)


def format_list_of_expressions(expressions, width, force=False):
    block = CodeBlock()
    for param, expr in enumerate(expressions):
        try:
            free_space = width - len(block.last_line or [])
            if param > 0:
                separator = ', '
                free_space = free_space - len(separator)
            subblock = expr.format_code(free_space, force=param==0)
            if param > 0:
                block.last_line.append(separator)
            block.merge(subblock)
        except NotEnoughSpace:
            subblock = expr.format_code(width, force=force)
            if param > 0:
                block.last_line.append(',')
            block.extend(subblock)
        if not force and block.width > width:
            raise NotEnoughSpace()
    return block


@register
class Call(ExpressionFormatter):

    ast_type = ast.Call

    @register
    class KeywordArg(ExpressionFormatter):

        ast_type = ast.keyword

        def format_code(self, width, force=False):
            block = CodeBlock([CodeLine(['%s=' % self.expr.arg])])
            expression_formatter = self.get_formatter(self.expr.value)
            expression_block = expression_formatter.format_code(width - block.width,
                                                                force=force)
            block.merge(expression_block)
            return block

    class StarArgsFormatter(object):

        def __init__(self, subexpression, prefix, formatters):
            self.subexpression = subexpression
            self.prefix = prefix
            self.formatters = formatters

        def format_code(self, width, force=False):
            block = CodeBlock.from_tokens(self.prefix)
            subexpression_formatter = self.formatters[type(self.subexpression)](self.subexpression,
                                                                                formatters=self.formatters)
            block.merge(subexpression_formatter.format_code(width - block.width,
                                                            force=force))
            return block


    def format_code(self, width, force=False):
        block = self.get_formatter(self.expr.func).format_code(width, force=force)
        block.lines[-1].append('(')
        # FIXME: introduce permutation of formatting
        expressions = [self.get_formatter(e) for e in self.expr.args]
        if self.expr.starargs:
            expressions.append(Call.StarArgsFormatter(self.expr.starargs, '*',
                                                      formatters=self.formatters))
        expressions += [self.get_formatter(e) for e in self.expr.keywords]
        if self.expr.kwargs:
            expressions.append(Call.StarArgsFormatter(self.expr.kwargs, '**',
                                                      formatters=self.formatters))
        subblock = format_list_of_expressions(expressions, width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append(')')
        return block


@register
class DictFormatter(ExpressionFormatter):

    ast_type = ast.Dict

    class Item(ExpressionFormatter):

        def __init__(self, key, value, parent, formatters):
            self.formatters = formatters
            self.key = self.formatters[type(key)](key, parent=parent, formatters=formatters)
            self.value = self.formatters[type(value)](value, parent=parent,
                                                      formatters=formatters)

        def format_code(self, width, force=False):
            # FIXME: search for solution on failure
            separator = ':'
            block = self.key.format_code(width-len(separator)-1,
                                         force=force)
            block.lines[-1].append(separator)
            block.merge(self.value.format_code(width-block.width,
                                               force=force), separator=' ')
            return block

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine(['{'])])
        expressions = [DictFormatter.Item(k, v, self.expr, self.formatters)
                       for k, v in zip(self.expr.keys,
                                       self.expr.values)]
        subblock = format_list_of_expressions(expressions=expressions,
                                              width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append('}')
        return block


@register
class ListFormatter(ExpressionFormatter):

    ast_type = ast.List

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine(['['])])
        expressions = [self.get_formatter(v)
                       for v in self.expr.elts]
        subblock = format_list_of_expressions(expressions=expressions,
                                              width=width-block.width,
                                              force=force)
        block.merge(subblock)
        block.lines[-1].append(']')
        return block


@register
class ListComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.ListComp

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('[')
        indent = block.width * ' '
        elt_formatter = self.get_formatter(self.expr.elt)
        elt_block = elt_formatter.format_code(width - block.width,
                                              force=force)
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
                                                 formatters=self.formatters,
                                                 force=force)
            block.extend(generators_block, indent)
        block.append_tokens(']')
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class SetFormatter(ExpressionFormatter):

    ast_type = ast.Set

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine(['{'])])
        expressions = [self.get_formatter(v)
                       for v in self.expr.elts]
        subblock = format_list_of_expressions(expressions=expressions,
                                              width=width-block.width,
                                              force=force)
        block.merge(subblock)
        block.lines[-1].append('}')
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class SetComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.SetComp

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('{')
        indent = block.width * ' '
        elt_formatter = self.get_formatter(self.expr.elt)
        elt_block = elt_formatter.format_code(width - block.width,
                                              force=force)
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
                                                 formatters=self.formatters,
                                                 force=force)
            block.extend(generators_block, indent)
        block.append_tokens('}')
        return block


@register
class IfExpressionFormatter(ExpressionFormatter):

    ast_type = ast.IfExp

    def format_code(self, width, force=False):
        body_formatter = self.get_formatter(self.expr.body)
        block = body_formatter.format_code(width, force=force)
        test_formatter = self.get_formatter(self.expr.test)
        block.append_tokens(' ', 'if', ' ')
        test_block = test_formatter.format_code(width-block.width, force=force)
        block.merge(test_block)
        orelse_formatter = self.get_formatter(self.expr.orelse)
        block.append_tokens(' ', 'else', ' ')
        orelse_block = orelse_formatter.format_code(width - block.width,
                                                    force=force)
        block.merge(orelse_block)
        return block


@register
class SubscriptionFormatter(ExpressionFormatter):

    ast_type = ast.Subscript

    def format_code(self, width, force=False):
        value_formatter = self.get_formatter(self.expr.value)
        block = value_formatter.format_code(width, force=force)
        slice_formatter = self.get_formatter(self.expr.slice)
        block.merge(slice_formatter.format_code(width - len(block.lines[-1]),
                                                force=force))
        return block


@register
class SliceFormatter(ExpressionFormatter):

    ast_type = ast.Slice

    def format_code(self, width, force=False):
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
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class IndexFormatter(ExpressionFormatter):

    ast_type = ast.Index

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('[')
        value_formatter = self.get_formatter(self.expr.value)
        block.merge(value_formatter.format_code(width - 2, force=force))
        block.append_tokens(']')
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class GeneratorFormatter(ExpressionFormatter):

    ast_type = ast.GeneratorExp

    def __init__(self, expr, formatters, parent=None):
        self.expr = expr
        self.parent = parent
        self.formatters = formatters

    def format_code(self, width, force=False):
        value_formatter = self.get_formatter(self.expr.elt)
        with_brackets = (not self.parent or not isinstance(self.parent, Call) or
                         len(self.parent.expr.args) != 1)
        if with_brackets:
            block = CodeBlock([CodeLine(['('])])
            indent = block.width * ' '
            block.merge(value_formatter.format_code(width, force=force))
        else:
            indent = ''
            block = value_formatter.format_code(width, force=force)
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
                                                 formatters=self.formatters,
                                                 force=force)
            block.extend(generators_block, indent)

        if with_brackets:
            block.append_tokens(')')
        # FIXME: raise exception
        return block


def format_generators(generators, width, parent, formatters, force=False):
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
                formatter_block = formatter.format_code(width - len(keyword) - 1,
                                                        force=force)
                block.lines.append(CodeLine([keyword]))
                block.merge(formatter_block, separator=' ')
    return block


@register
class DictComprehensionFormatter(ExpressionFormatter):

    ast_type = ast.DictComp

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('{')
        indent = block.width * ' '
        key_formatter = self.get_formatter(self.expr.key)
        value_formatter = self.get_formatter(self.expr.value)
        separator = ': '
        key_block = key_formatter.format_code(width - block.width - len(separator),
                                              force=force)
        value_block = value_formatter.format_code(width - block.width -
                                                  len(separator), force=force)
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
                                                 formatters=self.formatters,
                                                 force=force)
            block.extend(generators_block, indent)
        block.append_tokens('}')
        return block


@register
class TupleFormatter(ExpressionFormatter):

    ast_type = ast.Tuple

    def format_code(self, width, force=False):
        with_brackets = (isinstance(self.parent.expr, (ast.Tuple, ast.Call,
                                                       ast.List, ast.BinOp,
                                                       ast.ListComp)) or
                         len(self.expr.elts) < 2)
        block = CodeBlock()
        expressions = [self.get_formatter(v)
                       for v in self.expr.elts]
        if with_brackets:
            block.append_tokens('(')
        expression_block = format_list_of_expressions(expressions,
                                                      width-block.width,
                                                      force=force)
        if expression_block.height > 1 and not with_brackets:
            block.append_tokens('(')
            with_brackets = True
            expression_block = format_list_of_expressions(expressions,
                                                          width-block.width,
                                                          force=force)
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

    class KwargFormatter(object):

        def __init__(self, parameter, expression, parent):
            self.parameter = parameter
            self.expression = expression
            self.parent = parent

        def format_code(self, width, force=False):
            parameter_formatter = self.parent.get_formatter(self.parameter)
            expression_formatter = self.parent.get_formatter(self.expression)
            block = parameter_formatter.format_code(width-1)
            block.append_tokens('=')
            block.merge(expression_formatter.format_code(width - block.width,
                                                         force=force))
            return block

    def format_code(self, width, force=False):
        block = CodeBlock()
        args_formatters = [self.get_formatter(arg)
                           for arg in self.expr.args[:(len(self.expr.args) -
                                                       len(self.expr.defaults))]]
        args_formatters += [ParameterListFormatter.KwargFormatter(arg, default, self)
                            for (arg, default)
                            in zip(self.expr.args[len(args_formatters):],
                                   self.expr.defaults)]

        for param, arg_formatter in enumerate(args_formatters):
            # FIXME: move to next line
            try:
                separator = ', '
                free_space = width - len(block.last_line or []) - len(separator)
                arg_block = arg_formatter.format_code(free_space, force=False)
                if param > 0:
                    block.append_tokens(separator)
                block.merge(arg_block)
            except NotEnoughSpace:
                separator = ','
                arg_block = arg_formatter.format_code(width, force=force)
                if param > 0:
                    block.append_tokens(',')
                block.extend(arg_block)
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class LambdaFormatter(ExpressionFormatter):

    ast_type = ast.Lambda

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('lambda')
        parameter_list_formatter = self.get_formatter(self.expr.args)
        parameter_list_block = parameter_list_formatter.format_code(width-block.width)
        if parameter_list_block.width > 0:
            block.append_tokens(' ')
            block.merge(parameter_list_formatter.format_code(width-block.width))
        block.append_tokens(':', ' ')
        subexpression_formatter = self.get_formatter(self.expr.body)
        block.merge(subexpression_formatter.format_code(width - block.width,
                                                        force=force))
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


class StatementFormatter(AstFormatter):

    pass


@register
class PassFormatter(StatementFormatter):

    ast_type = ast.Pass

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('pass')
        if not force and block.width > width:
            raise NotEnoughSpace
        return block


@register
class ReturnFormatter(StatementFormatter):

    ast_type = ast.Return

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('return')
        expression_formatter = self.get_formatter(self.expr.value)
        expression_block = expression_formatter.format_code(width -
                                                            block.width - 1,
                                                            force=force)
        block.merge(expression_block, separator=' ')
        if not force and block.width > width:
            raise NotEnoughSpace
        return block


@register
class RaiseFormatter(StatementFormatter):

    ast_type = ast.Raise

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('raise')
        type_formatter = self.get_formatter(self.expr.type)
        type_block = type_formatter.format_code(width - block.width - 1,
                                                force=force)
        block.merge(type_block, separator=' ')
        if self.expr.inst or self.expr.tback:
            if self.expr.inst:
                inst_block = self.get_formatter(self.expr.inst).format_code(width - block.width - 2,
                                                                            force=force)
                block.merge(inst_block, separator=', ')
            else:
                block.append_tokens(',', ' ', 'None')
            if self.expr.tback:
                tback_block = self.get_formatter(self.expr.tback).format_code(width - block.width - 2,
                                                                              force=force)
                block.merge(tback_block, separator=', ')

        if not force and block.width > width:
            raise NotEnoughSpace
        return block


class ImportFormatterBase(StatementFormatter):

    @register
    class AliasFormatter(StatementFormatter):

        ast_type = ast.alias

        @property
        def name(self):
            return self.expr.name

        def format_code(self, width, force=False):
            block = CodeBlock.from_tokens(self.expr.name)
            if self.expr.asname:
                block.append_tokens('as', ' ', self.expr.asname)
            if not force and block.width > width:
                raise NotEnoughSpace()
            return block

    def format_aliases(self, width, force):
        block = CodeBlock()
        aliases = sorted([self.get_formatter(alias)
                          for alias in self.expr.names], key=lambda a: a.name.lower())
        aliases_block = format_list_of_expressions(aliases, width,
                                                   force)
        if aliases_block.height > 1:
            block.append_tokens('(')
            aliases_block = format_list_of_expressions(aliases, width-block.width,
                                                       force=force)
            block.merge(aliases_block)
            block.append_tokens(')')
        else:
            block.merge(aliases_block)
        return block


@register
class ImportFormatter(ImportFormatterBase):

    ast_type = ast.Import

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('import', ' ')
        block.merge(self.format_aliases(width-block.width, force=force))
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ImportFromFormatter(ImportFormatterBase):

    ast_type = ast.ImportFrom

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('from', ' ', '.' * self.expr.level)
        if self.expr.module:
            block.append_tokens(self.expr.module)
        block.append_tokens(' ', 'import', ' ')

        block.merge(self.format_aliases(width-block.width, force=force))
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ForFormatter(StatementFormatter):

    ast_type = ast.For

    def format_code(self, width, force=False):
        in_ = 'in'
        block = CodeBlock([CodeLine(['for'])])
        target_formatter = self.get_formatter(self.expr.target)
        block.merge(target_formatter.format_code(width - block.width - len(in_) - 1,
                                                 force=force), separator=' ')
        block.append_tokens(' ', in_)
        iter_formatter = self.get_formatter(self.expr.iter)
        block.merge(iter_formatter.format_code(width - block.width - 1,
                                               force=force), separator=' ')
        block.append_tokens(':')
        for a in self.expr.body:
            formatter = self.get_formatter(a)
            block.extend(formatter.format_code(width-len(CodeLine.INDENT),
                                               force=force), CodeLine.INDENT)
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class AssignmentFormatter(StatementFormatter):

    ast_type = ast.Assign

    def format_code(self, width, force=False):
        block = CodeBlock()
        for target in self.expr.targets:
            target_formatter = self.get_formatter(target)

            block.merge(target_formatter.format_code(width - block.width - 3,
                                                     force=force))
            block.append_tokens(' = ')
        value_formatter = self.get_formatter(self.expr.value)
        block.merge(value_formatter.format_code(width - block.width,
                                                force=force))
        return block

@register
class AugAssigmentFormatter(StatementFormatter):

    ast_type = ast.AugAssign

    def format_code(self, width, force=False):
        block = CodeBlock()
        target_formatter = self.get_formatter(self.expr.target)
        operator_formatter = self.get_formatter(self.expr.op)
        value_formatter = self.get_formatter(self.expr.value)

        block.merge(target_formatter.format_code(width, force=force))
        block.merge(operator_formatter.format_code(width - block.width,
                                                   force=force), separator=' ')
        block.append_tokens('=')
        block.merge(value_formatter.format_code(width - block.width, force=force),
                    separator=' ')
        return block


@register
class IfFormatter(StatementFormatter):

    ast_type = ast.If

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('if', ' ')
        test_formatter = self.get_formatter(self.expr.test)
        block.merge(test_formatter.format_code(width-block.width-1))
        block.append_tokens(':')
        for subexpression in self.expr.body:
            subexpression_formatter = self.get_formatter(subexpression)
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT),
                                                             force=force), CodeLine.INDENT)
        #if not force and block.width > width:
        #    raise NotEnoughSpace()
        return block


@register
class FunctionDefinitionFormatter(StatementFormatter):

    ast_type = ast.FunctionDef

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('def', ' ', self.expr.name, '(')
        parameter_list_formatter = self.get_formatter(self.expr.args)
        # FIXME: This will be precise formatting when we move to new
        #        format_code API: format_code(block_width, first_line_width=None,
        #                                     suffix=None, force=False):
        #        currently we are ignoring closing bracket width
        block.merge(parameter_list_formatter.format_code(width-block.width))
        block.append_tokens('):')
        for subexpression in self.expr.body:
            subexpression_formatter = self.get_formatter(subexpression)
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT),
                                                             force=force), CodeLine.INDENT)
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


@register
class ClassDefinitionFormater(StatementFormatter):

    ast_type = ast.ClassDef

    def format_code(self, width, force=False):
        block = CodeBlock.from_tokens('class', ' ', self.expr.name)
        if self.expr.bases:
            block.append_tokens('(')
            bases = [self.formatters[type(b)](b, self.formatters, parent=self) for b in self.expr.bases]
            bases_block = format_list_of_expressions(bases, width - block.width - 2,
                                                     force)
            block.merge(bases_block)
            block.append_tokens(')')
        block.append_tokens(':')
        for subexpression in self.expr.body:
            subexpression_formatter = self.get_formatter(subexpression)
            block.extend(subexpression_formatter.format_code(width - len(CodeLine.INDENT),
                                                             force=force), CodeLine.INDENT)
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


def _format_code(code, width, formatters, force=True):
    """Returns CodeBlock instance as result"""
    tree = ast.parse(code)
    result = []
    for e in tree.body:
        formatter = formatters[type(e)](expr=e, formatters=formatters, parent=None)
        result.append(formatter.format_code(width, force=force))
    return result

def format_code(code, width=80, formatters=_formatters.copy(), force=True):
    """Returns string as a result"""
    result = _format_code(code, width=width, formatters=formatters, force=force)
    return u'\n'.join(unicode(e) for e in result)
