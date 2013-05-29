import ast
from itertools import chain


class NotEnoughSpace(Exception):

    pass


class UnkownExpressionType(Exception):

    def __init__(self, expr):
        self.expr = expr

    def __unicode__(self):
        return 'Unkown expression type: %s; dir(expr) = %s' % (type(self.expr),
                                                               dir(self.expr))

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

    def __init__(self, expr):
        self.expr = expr

    def format_code(self, width, force=False):
        raise NotImplementedError()

    @classmethod
    def from_expr(cls, expr):
        try:
            return cls._a2e[type(expr)](expr)
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


class Num(Atom):

    ast_type = ast.Num

    def _format_code(self):
        return unicode(self.expr.n)


class Str(Atom):

    ast_type = ast.Str

    def _format_code(self):
        return unicode("'%s'" % self.expr.s)


class Attribute(Atom):

    ast_type = ast.Attribute

    def _format_code(self):
        return u'%s.%s' % (self.expr.value.id, self.expr.attr)


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
            block.merge(ExpressionFormatter.from_expr(self.expr.value)
                                  .format_code(width-block.width, force=force))
            return block


    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine([self.expr.func.id, '('])])
        expressions_list = ExpressionFormatterList([ExpressionFormatter.from_expr(e)
                                           for e in chain(self.expr.args, self.expr.keywords)])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append(')')
        return block


class Dict(ExpressionFormatter):

    ast_type = ast.Dict

    class Item(ExpressionFormatter):

        def __init__(self, key, value):
            self.key = ExpressionFormatter.from_expr(key)
            self.value = ExpressionFormatter.from_expr(value)

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
        expressions_list = ExpressionFormatterList([Dict.Item(k,v)
                                           for k,v in zip(self.expr.keys, self.expr.values)])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append('}')
        return block


class List(ExpressionFormatter):

    ast_type = ast.List

    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine(['['])])
        expressions_list = ExpressionFormatterList([ExpressionFormatter.from_expr(v)
                                           for v in self.expr.value.elts])
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
        value_formatter = ExpressionFormatter.from_expr(self.expr.value)
        block.merge(value_formatter.format_code(width-block.width, force=force))
        return block

def format_code(code, width):
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

#        if isinstance(e, ast.Assign):
#            result = format_code_assigment(e, indent)
#        elif isinstance(e.value, ast.Call):
#            result = format_code_function_call(e.value)
#        else:
#            raise ValueError(type(e))
#    return result

#def format_code_assigment(expr, indent):
#
#
