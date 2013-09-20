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
