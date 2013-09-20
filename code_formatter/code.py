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
        return CodeBlock([CodeLine(list(l.tokens)) for l in self.lines])

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
            return self
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
