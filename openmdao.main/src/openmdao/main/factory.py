import inspect

#public symbols
__all__ = ["Factory"]


class Factory(object):
    """Base class for objects that know how to create other objects
    based on a type argument and several optional arguments (version,
    server id, and resource description).
    """
    def __init__(self):
        pass

    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """Return an object of type *typname* (or a proxy to it if it resides
        in another process) using the specified package version, server
        location, and resource description. Returns None if this factory is
        unable to create the specified type.
        """
        raise NotImplementedError('create')

    def get_available_types(self, groups=None):
        """Return a set of tuples of the form (typename, metadata_dict), one
        for each available plugin type in the given entry point groups.
        If groups is *None,* return the set for all openmdao entry point groups.
        """
        raise NotImplementedError('get_available_types')

    def get_signature(self, typname, version=None):
        """Return constructor argument signature for *typname,* using the
        specified package version. The return value is a dictionary:

        args: list
            List of 1 or 2-element lists. The first element is the argument
            name; the second element is the default value.

        varargs: string
            The name of the '*' argument.

        kwargs: string
            The name of the '**' argument.
        """
        raise NotImplementedError('get_signature')

    @staticmethod
    def form_signature(cls):
        """Return constructor signature for class `cls`."""
        argspec = inspect.getargspec(cls.__init__)
        arglist = argspec.args[1:]  # Drop self.
        non_default = len(arglist)
        if argspec.defaults is not None:
            non_default -= len(argspec.defaults)
        args = [[arg] for arg in arglist[:non_default]]
        if argspec.defaults is not None:
            defstrs = [repr(default) for default in argspec.defaults]
            args.extend([arg, default]
                        for arg, default in zip(arglist[non_default:], defstrs))
        return dict(args=args,
                    varargs=argspec.varargs or '',
                    kwargs=argspec.keywords or '')
