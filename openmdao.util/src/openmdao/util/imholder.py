"""
Instancemethod utilities.

Normally pickle won't handle instance methods.
This code is intended to work around that limitation.
"""

import inspect

__all__ = ('IMHolder', 'fix_instancemethods', 'restore_instancemethods')


class IMHolder(object):
    """ Holds an instancemethod object in a pickleable form. """

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
    """ Replace references to instancemethods with IMHolders. """

    def _fix_im_recurse(obj, visited):
        """ Replace recursively. """
        visited.add(id(obj))
        if isinstance(obj, dict):
            for key, obj2 in obj.items():
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    obj[key] = IMHolder(obj2)
                else:
                    _fix_im_recurse(obj2, visited)
        elif isinstance(obj, list) or isinstance(obj, set):
            for i, obj2 in enumerate(obj):
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    obj[i] = IMHolder(obj2)
                else:
                    _fix_im_recurse(obj2, visited)
        elif isinstance(obj, tuple):
            for obj2 in obj:
                if obj2 is None or id(obj2) in visited:
                    continue
                if inspect.ismethod(obj2):
                    raise RuntimeError('_fix_im_recurse: tuple %r contains'
                                       ' reference to instance method %r'
                                       % (obj, obj2))
                else:
                    _fix_im_recurse(obj2, visited)
        elif hasattr(obj, '__dict__'):
            _fix_im_recurse(obj.__dict__, visited)

    _fix_im_recurse(root, set())


def restore_instancemethods(root):
    """ Restore references to instancemethods. """

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
                    raise RuntimeError('_restore_im_recurse: tuple %r contains'
                                       ' reference to IMHolder %r'
                                       % (obj, obj2))
                else:
                    _restore_im_recurse(obj2, visited)
        elif hasattr(obj, '__dict__'):
            _restore_im_recurse(obj.__dict__, visited)

    _restore_im_recurse(root, set())

