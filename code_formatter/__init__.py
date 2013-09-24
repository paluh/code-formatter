from . import base
from . import extras
from . import utils
from .exceptions import NotEnoughSpace

__all__ = ['base', 'extras', 'format_code', 'NotEnoughSpace']


# add defaults formatters + width to base format_code function
def format_code(code, width=80, formatters_register=base.formatters, force=False):
    return utils.format_code(code, width=width,
                             formatters_register=formatters_register, force=force)
