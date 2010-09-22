"""
Support for Role-Based Access Control.

Remote access to methods/functions is determined by a role-based access control
attribute attached to the method/function. This attribute is a sequence of role
patterns. The `rbac` decorator is available for assigning role patterns to
methods.

Remote access to attributes is checked based on role, accessing method, object,
and attribute.

These access checks are mediated by a :class:`RoleMapper`.  There is a default
mapper assigned to each :class:`OpenMDAO_Server`.  The server will check for an
object-specific mapper before using the default.

The current role is determined from a :class:`Credentials` object which is
attached to the current thread.  Credentials are currently just a user
identifier string.  Mapping from credentials to roles can become fairly
involved in a real system, typically with site-specific configuration.
The default role mapping here just returns the roles 'owner' or 'user'.
"""

import fnmatch
import inspect
import os
import socket
import threading


class RoleError(Exception):
    """
    Raised when the current role is not allowed to invoke the requested
    method/function.  Also raised when encountering some internal problems.
    """
    pass


class Credentials(object):
    """ Currently just ``user@host``. """

    def __init__(self):
        self.user = '%s@%s' % (os.getenv('LOGNAME'), socket.gethostname())

    def __eq__(self, other):
        return self.user == other.user

    def __str__(self):
        return self.user


def set_credentials(credentials):
    """ Set the current thread's credentials. """
    threading.current_thread().credentials = credentials


def get_credentials():
    """ Get the current thread's credentials. """
    try:
        return threading.current_thread().credentials
    except AttributeError:
        return None


class rbac(object):
    """ Decorator for specifying RBAC roles for a method/function. """

    def __init__(self, roles, proxy_role='', proxy_types=None):
        self.roles = (roles,) if isinstance(roles, basestring) else tuple(roles)
        self.proxy_role = proxy_role
        self.proxy_types = proxy_types or []

    def __call__(self, func):
        func._rbac = (self.roles, self.proxy_role, self.proxy_types, {})
        return func


def rbac_methods(obj):
    """ Returns a list of names of the methods of `obj` to be exposed. """
    methods = []
    for name in dir(obj):
        if name[0] != '_':
            attr = getattr(obj, name)
            if inspect.ismethod(attr):
                if hasattr(attr.__func__, '_rbac'):
                    methods.append(name)
            elif inspect.isfunction(attr):
                if hasattr(attr, '_rbac'):
                    methods.append(name)
    return methods


def need_proxy(function, result):
    """ Returns True if `result` from `function` requires a proxy. """
    try:
        rbac = function._rbac
    except AttributeError:
        return False

    types = tuple(rbac[2])
    if not types:
        return False

    cls = result.__class__
    cache = rbac[3]
    try:
        return cache[cls]
    except KeyError:
        # Check if this result class or any base classes are in types.
        cache[cls] = isinstance(result, types)
        return cache[cls]


class RoleMapper(object):
    """
    Responsible for mapping :class:`Credentials` to roles and optionally
    getting different credentials for executing a method.
    """

    def __init__(self):
        self.owner = get_credentials()
        if self.owner is None:
            raise RoleError('No current credentials')
        self.credentials_map = {}
        self.set_proxy_credentials('owner', self.owner)
        self.attr_proxy_map = {}
        self.proxy_types = []

    def get_role(self, credentials):
        """
        Trivial :class:`Credentials`-to-role mapping.
        Returns null string for no credentials, 'owner' if credentials matches
        the credentials in effect when we were created, and 'user' otherwise.
        """
        if credentials is None:
            return ''  # Should be allowed for *very* few methods!
        assert isinstance(credentials, Credentials)
        if credentials == self.owner:
            return 'owner'
        return 'user'

    def get_proxy_credentials(self, func, credentials):
        """
        If special credentials are needed while executing `func`, return
        them, else return `credentials`.
        """
        try:
            proxy_role = func._rbac[1]
        except AttributeError:
            raise RoleError('No RBAC for function %s' % func)
        
        if proxy_role:
            try:
                return self.credentials_map[proxy_role]
            except KeyError:
                raise RoleError('No credentials for proxy role %s' % proxy_role)
        else:
            return credentials

    def set_proxy_credentials(self, proxy_role, credentials):
        """ Set credentials to be used for `proxy_role` to `credentials`. """
        self.credentials_map[proxy_role] = credentials

    def check_access(self, role, methodname, obj, attr):
        """
        Verify that `role` is allowed to invoke `methodname` of `obj` for
        `attr`.  Used for access control on :func:`getattr`, :func:`setattr`,
        and :func:`delattr` operations. This default version requires a valid
        role, and only 'owner' may set or delete attributes.
        """
        if role is None:
            raise RoleError('No access by role None')
        if methodname == '__getattribute__' or methodname == '__getattr__':
            return
        if role != 'owner':
            raise RoleError("No access to '%s' by role '%s'"
                            % (methodname, role))

    def need_proxy(self, obj, attr, res):
        """
        Returns True if `attr` of `obj` whose value is `res` requires a proxy.
        """
        types = tuple(self.proxy_types)
        if isinstance(res, types):
            return True

        key = '%s.%s' % (id(obj), attr)
        try:
            return self.attr_proxy_map[key]
        except KeyError:
            return False

    def attr_proxy_required(self, obj, attr, required=True):
        """ Record that a proxy is/is not required for `obj.attr`. """
        key = '%s.%s' % (id(obj), attr)
        self.attr_proxy_map[key] = required

    def class_proxy_required(self, cls):
        """ Record that a proxy is required for `cls` or any subclasses. """
        self.proxy_types.append(cls)


def check_role(role, func):
    """
    Verifies that `role` is matched by at least one :mod:`fnmatch`-style
    pattern in `func`s RBAC. Raises :class:`RoleError` if no match is found.
    """
    try:
        patterns = func._rbac[0]
    except AttributeError:
        raise RoleError('No RBAC for function!')

    for pattern in patterns:
        if fnmatch.fnmatchcase(role, pattern):
            return
    raise RoleError("No access for role '%s'" % role)

