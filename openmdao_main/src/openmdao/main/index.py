# some constants used in the get/set downstream protocol
INDEX = 0
ATTR = 1
CALL = 2
SLICE = 3
EXTSLICE = 4

def _handle_simple_index(obj, idx):
    return obj[idx[1]]

def _handle_attr(obj, idx):
    return getattr(obj, idx[1])

def _handle_call(obj, idx):
    if len(idx) == 1:
        return obj.__call__()
    else:
        args = idx[1]
        if len(idx) == 3:
            kwargs = dict(idx[2])
        else:
            kwargs = {}
        return obj.__call__(*args, **kwargs)

def _handle_slice(obj, idx):
    return obj.__getitem__(slice(*idx[1]))

def _handle_extslice(obj, idx):
    args = []
    for a in idx[1:]:
        if isinstance(a, tuple):
            args.append(slice(a[0],a[1],a[2]))
        else:
            args.append(a)
    return obj.__getitem__(args)

# dict of functions to handle various index tuples
_index_functs = {
    INDEX: _handle_simple_index,
    ATTR: _handle_attr,
    CALL: _handle_call,
    SLICE: _handle_slice,
    EXTSLICE: _handle_extslice,
}

def process_index_entry(obj, idx):
    """

    Return a new object based on a starting object and some operation
    indicated by idx that can be either an index into a container, an
    attribute access, or a function call.  idx can be a non-tuple hashable
    object, which will be interpreted as an index to a container, or it can
    be a tuple of the form (operation_id, stuff) where operation_id is
    as follows (the named constants are defined in expreval.py)::

          INDEX = 0
          ATTR = 1
          CALL = 2
          SLICE = 3
          EXTSLICE = 4

    On the off chance that you want to use a tuple as a key into a dict, you'll have to
    nest your key tuple inside of an INDEX tuple to avoid ambiguity, e.g., (INDEX, my_tuple)

    The forms of the various tuples are::

          INDEX:   (0, idx)  where idx is some hashable value
          ATTR:    (1, name) where name is the attribute name
          CALL:    (2, args, kwargs) where args is a list of values, and kwargs is a list
                   of tuples of the form (keyword,value). kwargs can be left out if
                   empty.  args can be left out if empty as long as kwargs are also
                   empty, for example, (2,) and  (2,[],[('foo',1)]) are valid but
                   (2,[('foo',1)]) is not.
          SLICE:   (3, lower, upper, step) All members must be present and should have a
                   value of None if not set.
          EXTSLICE:  (4, plus a combination of (lower,upper,step) tuples and indexes)
                     For example, (4, (None,None,None), 2)
    """
    if isinstance(idx, tuple):
        return _index_functs[idx[0]](obj, idx)
    else:
        return obj[idx]

def index_retains_metadata(index):
    fails = (CALL, ATTR)
    for idx in index:
        if idx[0] in fails:
            return False
    return True

def get_indexed_value(obj, name, index=None):
    if name:
        obj = getattr(obj, name)
    if index:
        for idx in index:
            if isinstance(idx, tuple):
                obj = _index_functs[idx[0]](obj, idx)
            else:
                obj = obj[idx]
    return obj

def index_to_text(index):
    """Returns a string representation of the given index. Doesn't work for functions"""
    if index:
        parts = []
        for idx in index:
            if not isinstance(idx, tuple):
                parts.append('[%s]' % idx)
            elif idx[0] == INDEX:
                parts.append('[%s]' % idx[1])
            elif idx[0] == ATTR:
                parts.append('.%s' % idx[1])
            elif idx[0] == CALL:
                if len(idx) == 1:
                    parts.append('()')
                else:
                    raise NotImplementedError('conversion of function calls within an index to text is not supported yet')
            elif idx[0] == SLICE:
                s = '[%s:%s:%s]' % tuple(idx[1])
                parts.append(s.replace('None', '').replace('::',':'))
        return ''.join(parts)
    else:
        return ''


def deep_hasattr(obj, pathname):
    """Returns True if the attrbute indicated by the given pathname
    exists; False otherwise.
    """
    try:
        parts = pathname.split('.')
        for name in parts[:-1]:
            obj = getattr(obj, name)
    except Exception:
        return False
    return hasattr(obj, parts[-1])


def deep_getattr(obj, pathname):
    """Returns the attrbute indicated by the given pathname or raises
    an exception if it doesn't exist.
    """
    for name in pathname.split('.'):
        obj = getattr(obj, name)
    return obj
