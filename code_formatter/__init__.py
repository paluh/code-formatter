import ast
from itertools import chain


class NotEnoughSpace(Exception):

    pass


class UnkownExpressionType(Exception):

    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        attrs = ', '.join('%s=%s' % (a, getattr(self.expr, a)) for a in dir(self.expr) if not a.startswith('_'))
        return ('Unkown expression type: %s; dir(expr) = %s'
                'attrs: %s' % (type(self.expr), dir(self.expr), attrs))

class CodeLine(object):

    INDENT = '    '

    def __init__(self, tokens=None):
        self.tokens = tokens or []

    def indent(self, indentation):
        self.tokens.insert(0, indentation)

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

    def extend(self, block, indentation=''):
        if indentation:
            self.lines.extend(CodeLine([indentation] + l.tokens) for l in block.lines)
        else:
            self.lines.extend(CodeLine(l.tokens) for l in block.lines)

    def merge(self, block):
        lines = block.lines
        indent = len(self.lines[-1])*' '
        self.lines[-1].extend(block.lines[0].tokens)
        for original in lines[1:]:
            line = CodeLine([indent])
            line.extend(original.tokens)
            self.lines.append(line)

    @property
    def width(self):
        return max(len(l) for l in self.lines)

    def __unicode__(self):
        return '\n'.join(unicode(l) for l in self.lines)


class ExpressionFormatterMetaclass(type):

    def __new__(cls, name, bases, attrs):
        klass = type.__new__(cls, name, bases, attrs)
        if attrs.get('ast_type') is not None:
            ExpressionFormatter._a2e[attrs['ast_type']] = klass
        return klass


class ExpressionFormatter(object):

    __metaclass__ = ExpressionFormatterMetaclass
    _a2e = {}

    def __init__(self, expr, parent=None):
        self.expr = expr
        self.parent = parent

    def format_code(self, width, force=False):
        raise NotImplementedError()

    @classmethod
    def from_expr(cls, expr, parent=None):
        try:
            return cls._a2e[type(expr)](expr, parent=parent)
        except KeyError:
            raise UnkownExpressionType(expr)


class Atom(ExpressionFormatter):

    ast_type = None

    def _format_code(self):
        raise NotImplementedError()

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine([self._format_code()])])
        if not force and block.width > width:
            raise NotEnoughSpace()
        return block


class Name(Atom):

    ast_type = ast.Name

    def _format_code(self):
        return unicode(self.expr.id)


class Operator(Atom):

    def _format_code(self):
        return u' %s ' % self.operator

class Gt(Operator):

    ast_type = ast.Gt
    operator = '>'


class GtE(Operator):

    ast_type = ast.GtE
    operator = '>='


class Lt(Operator):

    ast_type = ast.Lt
    operator = '<'


class LtE(Operator):

    ast_type = ast.LtE
    operator = '<='


class Eq(Operator):

    ast_type = ast.Eq
    operator = '=='


class NotEq(Operator):

    ast_type = ast.NotEq
    operator = '!='


class Is(Operator):

    ast_type = ast.Is
    operator = 'is'


class IsNot(Operator):

    ast_type = ast.IsNot
    operator = 'is not'


class In(Operator):

    ast_type = ast.In
    operator = 'in'


class NotIn(Operator):

    ast_type = ast.NotIn
    operator = 'not in'


class Num(Atom):

    ast_type = ast.Num

    def _format_code(self):
        return unicode(self.expr.n)


class Str(Atom):

    ast_type = ast.Str

    def _format_code(self):
        return unicode("'%s'" % self.expr.s)


class Attribute(ExpressionFormatter):

    ast_type = ast.Attribute

    def format_code(self, width, force=False):
        block = ExpressionFormatter.from_expr(self.expr.value,
                                              self.expr).format_code(width-len(self.expr.attr)-1,
                                                                     force=force)
        block.lines[-1].append('.')
        block.lines[-1].append(self.expr.attr)
        return block


class ExpressionFormatterList(ExpressionFormatter):

    def __init__(self, expressions):
        self.expressions = expressions

    def format_code(self, width, force=False):
        curr_line = CodeLine([])
        block = CodeBlock([curr_line])

        for param, expr in enumerate(self.expressions):
            try:
                free_space = width - len(block.lines[-1])
                if param > 0:
                    separator = ', '
                    free_space = free_space - len(separator)
                subblock = expr.format_code(free_space, force=param==0)
                if param > 0:
                    curr_line.append(separator)
                block.merge(subblock)
            except NotEnoughSpace:
                subblock = expr.format_code(width, force=True)
                curr_line.append(',')
                block.extend(subblock)
            curr_line = block.lines[-1]
            if not force and block.width > width:
                raise NotEnoughSpace()
        return block


class Call(ExpressionFormatter):

    ast_type = ast.Call

    class KeywordArg(ExpressionFormatter):

        ast_type = ast.keyword

        def format_code(self, width, force=False):
            block = CodeBlock([CodeLine(['%s=' % self.expr.arg])])
            block.merge(ExpressionFormatter.from_expr(self.expr.value, self.expr)
                                  .format_code(width-block.width, force=force))
            return block


    def format_code(self, width, force=False):
        block = ExpressionFormatter.from_expr(self.expr.func).format_code(width, force=force)
        block.lines[-1].append('(')
        expressions_list = ExpressionFormatterList([ExpressionFormatter.from_expr(e, self.expr)
                                           for e in chain(self.expr.args, self.expr.keywords)])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append(')')
        return block


class Dict(ExpressionFormatter):

    ast_type = ast.Dict

    class Item(ExpressionFormatter):

        def __init__(self, key, value, parent):
            self.key = ExpressionFormatter.from_expr(key, parent)
            self.value = ExpressionFormatter.from_expr(value, parent)

        def format_code(self, width, force=False):
            # FIXME: search for solution on failure
            separator = ': '
            block = self.key.format_code(width-len(separator),
                                         force=force)
            block.lines[-1].append(separator)
            block.merge(self.value.format_code(width-block.width,
                                               force=force))
            return block

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine(['{'])])
        expressions_list = ExpressionFormatterList([Dict.Item(k,v, self.expr)
                                                    for k,v in zip(self.expr.keys,
                                                                   self.expr.values)])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append('}')
        return block


class List(ExpressionFormatter):

    ast_type = ast.List

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine(['['])])
        expressions_list = ExpressionFormatterList([ExpressionFormatter.from_expr(v, self.expr)
                                                    for v in self.expr.elts])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append(']')
        return block


class Assignment(ExpressionFormatter):

    ast_type = ast.Assign

    def format_code(self, width, force=False):
        curr_line = CodeLine()
        block = CodeBlock([curr_line])
        for t in self.expr.targets:
            curr_line.append(t.id)
            curr_line.append(' = ')
        value_formatter = ExpressionFormatter.from_expr(self.expr.value, self.expr)
        block.merge(value_formatter.format_code(width-block.width, force=force))
        return block


class Subscript(ExpressionFormatter):

    ast_type = ast.Subscript

    def format_code(self, width, force=False):
        value_formatter = ExpressionFormatter.from_expr(self.expr.value, self.expr)
        block = value_formatter.format_code(width, force=force)
        block.lines[-1].append('[')
        index_formatter = ExpressionFormatter.from_expr(self.expr.slice.value, self.expr)
        block.merge(index_formatter.format_code(width-len(block.lines[-1])-1,
                                                force=force))
        block.lines[-1].append(']')
        return block


class Compare(ExpressionFormatter):

    ast_type = ast.Compare

    def format_code(self, width, force=False):
        block = ExpressionFormatter.from_expr(self.expr.left).format_code(width, force=force)
        for operator, comparator in zip(self.expr.ops, self.expr.comparators):
            block.merge(ExpressionFormatter.from_expr(operator).format_code(width-block.width,
                                                                            force=force))
            block.merge(ExpressionFormatter.from_expr(comparator).format_code(width-block.width,
                                                                              force=force))
        return block


class Generator(ExpressionFormatter):

    ast_type = ast.GeneratorExp

    def __init__(self, expr, parent=None, standalone=True):
        self.expr = expr
        self.parent = parent
        self.standalone = standalone

    def format_code(self, width, force=False):
        value_formatter = ExpressionFormatter.from_expr(self.expr.elt, self.expr)
        if self.standalone:
            block = CodeBlock([CodeLine(['('])])
            indent = block.width * ' '
            block.merge(value_formatter.format_code(width, force=force))
        else:
            indent = ''
            block = value_formatter.format_code(width, force=force)
        curr_line = block.lines[-1]
        for generator in self.expr.generators:
            # try to keep "for x in iterable" in one line
            target_formatter = ExpressionFormatter.from_expr(generator.target)
            iter_formatter = ExpressionFormatter.from_expr(generator.iter)
            try:
                for_operator = ' for '
                in_operator = ' in '
                target_block = target_formatter.format_code(width-len(curr_line)-len(for_operator))
                iter_block = iter_formatter.format_code(width-len(curr_line)-len(for_operator)-len(in_operator))
                curr_line.append(for_operator)
                block.merge(target_block)
                curr_line.append(in_operator)
                block.merge(iter_block)
            except NotEnoughSpace:
                try:
                    for_operator = 'for '
                    in_operator = ' in '
                    target_block = target_formatter.format_code(width-len(curr_line)-len(for_operator))
                    iter_block = iter_formatter.format_code(width-len(for_operator)-len(in_operator))
                    block.lines.append(CodeLine([indent, for_operator]))
                    block.merge(target_block)
                    curr_line = block.lines[-1]
                    curr_line.append(in_operator)
                    block.merge(iter_block)
                except NotEnoughSpace:
                    for_operator = 'for '
                    target_block = target_formatter.format_code(width-len(curr_line)-len(for_operator),
                                                                force=force)
                    block.lines.append(CodeLine([indent, for_operator]))
                    block.merge(target_block)
                    in_operator = 'in '
                    iter_block = iter_formatter.format_code(width-len(curr_line)-len(for_operator)-len(in_operator),
                                                            force=force)
                    block.lines.append(CodeLine([indent, in_operator]))
                    block.merge(iter_block)

            curr_line = block.lines[-1]

            for if_ in generator.ifs:
                try:
                    separator = ' if '
                    iter_formatter = ExpressionFormatter.from_expr(if_)
                    iter_block =iter_formatter.format_code(width-block.width-len(separator))
                    curr_line.append(separator)
                    block.merge(iter_block)
                except NotEnoughSpace:
                    curr_line = CodeLine([indent, 'if '])
                    block.lines.append(curr_line)
                    block.merge(iter_formatter.format_code(width, force=True))
            curr_line = block.lines[-1]
        if self.standalone:
            curr_line.append(')')
        # FIXME: raise exception
        return block


def format_code(code, width=80):
    tree = ast.parse(code)
    result = []
    for e in tree.body:
        try:
            formatter = ExpressionFormatter.from_expr(e)
        except UnkownExpressionType:
            formatter = ExpressionFormatter.from_expr(e.value)
        result.append(formatter.format_code(width, force=True))
    # mambo jumbo :-P
    unicode(result[0])
    return u'\n'.join(unicode(e) for e in result)
