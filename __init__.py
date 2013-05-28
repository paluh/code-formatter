import ast
from itertools import chain


class NotEnoughSpace(Exception):

    pass


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


class ExpressionMetaclass(type):

    def __new__(cls, name, bases, attrs):
        klass = type.__new__(cls, name, bases, attrs)
        if attrs.get('ast_type') is not None:
            Expression._a2e[attrs['ast_type']] = klass
        return klass


class Expression(object):

    __metaclass__ = ExpressionMetaclass
    _a2e = {}

    def __init__(self, expr):
        self.expr = expr

    def format_code(self, width, force=False):
        raise NotImplementedError()

    @classmethod
    def from_expr(cls, expr):
        return cls._a2e[type(expr)](expr)


class Atom(Expression):

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


class ExpressionList(Expression):

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


class Call(Expression):

    ast_type = ast.Call

    class KeywordArg(Expression):

        ast_type = ast.keyword

        def format_code(self, width, force=False):
            block = CodeBlock([CodeLine(['%s=' % self.expr.arg])])
            block.merge(Expression.from_expr(self.expr.value)
                                  .format_code(width-block.width, force=force))
            return block


    def format_code(self, width, force=False):
        block = CodeBlock([CodeLine([self.expr.func.id, '('])])
        expressions_list = ExpressionList([Expression.from_expr(e)
                                           for e in chain(self.expr.args, self.expr.keywords)])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append(')')
        return block


class Dict(Expression):

    ast_type = ast.Dict

    class Item(Expression):

        def __init__(self, key, value):
            self.key = Expression.from_expr(key)
            self.value = Expression.from_expr(value)

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
        expressions_list = ExpressionList([Dict.Item(k,v)
                                           for k,v in zip(self.expr.keys, self.expr.values)])
        subblock = expressions_list.format_code(width=width-block.width, force=force)
        block.merge(subblock)
        block.lines[-1].append('}')
        return block


#MAX_LENGTH = 80
#
#def format_code_function_call(expr, indent=''):
#    fun = expr.func.id
#    curr_line = Line(indent)
#    curr_line.extend([fun, '('])
#    lines = [curr_line]
#
#    param_indent = indent + (' ' * (len(fun) + 1))
#
#    args = [('', arg) for arg in expr.args]
#    args.extend((kwarg.arg+'=', kwarg.value) for kwarg in expr.keywords)
#
#    for param, (prefix, arg) in enumerate(args):
#        if isinstance(arg, (ast.Name, ast.Num, ast.Str, ast.Attribute, ast.Subscript)):
#            s = str({ast.Num: attrgetter('n'), ast.Name: attrgetter('id'),
#                     ast.Str: lambda i: "'%s'" % i.s,
#                     ast.Attribute: lambda i: '%s.%s' % (i.value.id, i.attr),
#                     ast.Subscript: lambda i: '%s[%s]' % (i.value.id,i.slice)}[type(arg)](arg))
#            if isinstance(arg, ast.Subscript):
#                import ipdb; ipdb.set_trace()
#            if len(curr_line) + (2 if param != 0 else 0) + len(prefix) + len(s) > MAX_LENGTH:
#                if param != 0:
#                    curr_line.append(',')
#                curr_line = Line(param_indent)
#                lines.append(curr_line)
#            elif param != 0 and len(curr_line.tokens) > 0:
#                curr_line.append(', ')
#            if prefix:
#                curr_line.append(prefix)
#            curr_line.append(s)
#        elif isinstance(arg, ast.Call):
#            # les't check .....function(param,
#            if (len(curr_line) + (2 if param != 0 else 0) + len(prefix) +
#                len(arg.func.id)  + 1 + len(arg.args[0].id) + 1)  > MAX_LENGTH:
#                if param != 0:
#                    curr_line.append(',')
#                curr_line = Line(param_indent)
#                lines.append(curr_line)
#                curr_line.append(prefix)
#                sublines = format_code_function_call(arg, param_indent+((len(prefix)*' ')))
#            else:
#                if param != 0 and len(curr_line.tokens) > 0:
#                    curr_line.append(', ')
#                if prefix:
#                    curr_line.append(prefix)
#                sublines = format_code_function_call(arg, len(curr_line)*' ')
#            curr_line.extend(sublines[0].tokens)
#            for subline in sublines[1:]:
#                lines.append(subline)
#                curr_line = subline
#        else:
#            attrs = '\n'.join(['getattr(i, %s) = %s'%(a, getattr(arg, a)) for a in dir(arg) if not a.startswith('_')])
#            raise ValueError('Unknown token type: %s,\ninstance attrs: %s,\npublic:\n%s' % (arg, dir(arg), attrs))
#    curr_line.append(')')
#    return lines
#
#def format_code_assigment(expr, indent):
#    targets = [t.id for t in expr.targets]
#    value_indent = sum([3+len(t) for t in targets], len(indent)) * ' '
#    if isinstance(expr.value, ast.Call):
#        lines = format_code_function_call(expr.value, value_indent)
#    else:
#        # FIXME: we should try format_code primitives here
#        raise ValueError()
#    return [Line(indent, [' = '.join(targets), ' = ' ] + lines[0].tokens)] + lines[1:]
#
#
#def format_code_code(code):
#    c = textwrap.dedent(code)
#    indent = (len(code.split('\n')[0]) - len(c.split('\n')[0]))*' '
#    tree = ast.parse(c)
#    for e in tree.body:
#        if isinstance(e, ast.Assign):
#            result = format_code_assigment(e, indent)
#        elif isinstance(e.value, ast.Call):
#            result = format_code_function_call(e.value)
#        else:
#            raise ValueError(type(e))
#    return result
#
#
