"""
Instancemethod utilities.

Normally :mod:`pickle` won't handle instance methods.
This code is intended to work around that limitation.
"""

import inspect

__all__ = ('IMHolder', 'fix_instancemethods', 'restore_instancemethods')


class IMHolder(object):
    """ Holds an instance method object in a pickleable form. """

    def __init__(self, obj):
        # The inspect module notes 'class object that *asked* for this method'.
        # This obscure note and Pickle's reluctance to work on instancemethods
        # prompts the verification here...
        if not hasattr(obj.im_class, obj.__name__):
            raise RuntimeError('IMHolder: %r not a member of class (%s)'
                               % (obj, obj.im_class))
        self.name = obj.__name__
        self.im_self = obj.im_self
        if obj.im_self is not None:
            self.im_class = None  # Avoid possible __main__ issues.
        else:
            # TODO: handle __main__ for im_class.__module__.
            if obj.im_class.__module__ == '__main__':
                raise RuntimeError('IMHolder: %r with module __main__ (%s)'
                                   % (obj, obj.im_self))
            self.im_class = obj.im_class

    @property
    def method(self):
        """ Return instancemethod corresponding to saved state. """
        if self.im_self is not None:
            return getattr(self.im_self, self.name)
        else:
            return getattr(self.im_class, self.name)


def fix_instancemethods(root):
    """
    Traverse the object tree starting at `root`, replacing references to
    instance methods with :class:`IMHolder` objects. Note that
    :meth:`__getstate__` may have :mod:`pickle` process less than the complete
    runtime state, either for efficiency reasons or because something may not
    be pickleable.  This code will check for a :meth:`__getstate__` attribute
    on non-builtin objects, and if it exists it will call that to get an
    object's state.  If that state is a dictionary which is a subset of
    :attr:`__dict__`, then only those entries of :attr:`__dict__` are
    processed.  This implies that :meth:`__getstate__` implementations should
    return a copy of :attr:`__dict__` with entries removed rather than
    replacing the value with a placeholder such as None.

    .. note::

        Tuples are immutable, so any tuple containing a reference to an
        instance method cannot be fixed and will raise a :exc:`RuntimeError`.
    """

    def _fix_im_recurse(obj, visited, ancestors):
        """ Replace recursively. """
        visited.add(id(obj))
        if isinstance(obj, dict):
            for key, obj2 in obj.items():
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    obj[key] = IMHolder(obj2)
                else:
                    ancestors.append((obj, key))
                    _fix_im_recurse(obj2, visited, ancestors)
                    ancestors.pop()

        elif isinstance(obj, list) or isinstance(obj, set):
            for i, obj2 in enumerate(obj):
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    obj[i] = IMHolder(obj2)
                else:
                    ancestors.append((obj, i))
                    _fix_im_recurse(obj2, visited, ancestors)
                    ancestors.pop()

        elif isinstance(obj, tuple):
            for i, obj2 in enumerate(obj):
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    ancestor_str = ',\n'.join([str(item) for item in ancestors])
                    raise RuntimeError('fix_instancemethods: tuple %r contains'
                                       ' reference to instance method %r.\n'
                                       'Ancestors:\n%s'
                                       % (obj, obj2, ancestor_str))
                else:
                    ancestors.append((obj, i))
                    _fix_im_recurse(obj2, visited, ancestors)
                    ancestors.pop()

        elif hasattr(obj, '__dict__'):
            if inspect.ismodule(obj) or \
               inspect.isclass(obj) or \
               inspect.ismethod(obj) or \
               inspect.isfunction(obj):
                # Skip builtins.
                return
           
            if hasattr(obj, '__getstate__'):
                try:
                    if hasattr(obj.__getstate__, 'im_self'):
                        if obj.__getstate__.im_self is None:
                            state = obj.__getstate__(obj)
                        else:
                            state = obj.__getstate__()
                    else:
                        state = obj.__getstate__(obj)
                except Exception:
                    print obj.__getstate__
                    raise

                if isinstance(state, dict):
                    obj_keys = obj.__dict__.keys()
                    state_keys = state.keys()
                    for key in state_keys:
                        if key in obj_keys:
                            if state[key] != obj.__dict__[key]:
                                break
                        else:
                            break
                    else:
                        # Process if subset.
                        if len(state_keys) < len(obj_keys):
                            saved = {}  # Save items not in state.
                            for key in obj_keys:
                                if key not in state_keys:
                                    saved[key] = obj.__dict__[key]
                                    del obj.__dict__[key]
                            try:
                                ancestors.append((obj, '__dict__'))
                                _fix_im_recurse(obj.__dict__, visited, ancestors)
                                ancestors.pop()
                                return
                            finally:
                                for key in saved.keys():
                                    obj.__dict__[key] = saved[key]

            ancestors.append((obj, '__dict__'))
            _fix_im_recurse(obj.__dict__, visited, ancestors)
            ancestors.pop()

    _fix_im_recurse(root, set(), [])


def restore_instancemethods(root):
    """
    Traverse the object tree starting at `root`, replacing references to
    :class:`IMHolder` with the saved instance method.
    """

    def _restore_im_recurse(obj, visited):
        """ Restore recursively. """
        visited.add(id(obj))
        if isinstance(obj, dict):
            for key, obj2 in obj.items():
                if obj2 is None or id(obj2) in visited:
                    continue
                if isinstance(obj2, IMHolder):
                    obj[key] = obj2.method
                else:
                    _restore_im_recurse(obj2, visited)

        elif isinstance(obj, list) or isinstance(obj, set):
            for i, obj2 in enumerate(obj):
                if obj2 is None or id(obj2) in visited:
                    continue
                if isinstance(obj2, IMHolder):
                    obj[i] = obj2.method
                else:
                    _restore_im_recurse(obj2, visited)

        elif isinstance(obj, tuple):
            for obj2 in obj:
                if obj2 is None or id(obj2) in visited:
                    continue
                if isinstance(obj2, IMHolder):
                    # This really shouldn't happen...
                    raise RuntimeError('restore_instancemethods: tuple %r'
                                       ' contains reference to IMHolder %r'
                                       % (obj, obj2))
                else:
                    _restore_im_recurse(obj2, visited)

        elif hasattr(obj, '__dict__'):
            _restore_im_recurse(obj.__dict__, visited)

    _restore_im_recurse(root, set())

