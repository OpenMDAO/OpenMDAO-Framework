"""
This module defines modified versions of some classes and functions defined in
:mod:`multiprocessing.managers` to support OpenMDAO distributed simulations.

Channel communication can be secured by setting `authkey` to 'PublicKey'.
When `authkey` is 'PublicKey', an additional session establishment protocol
is used between the Proxy and Server:

1. Proxy sends session request containing the proxy's public key, encrypted with
   the server's public key (obtained when told what server to connect to).

2. Server responds with random session key, encrypted with proxy's public key.

3. Subsequent communication is encrypted with the session key (which presumably
   is quicker than public/private key encryption).

If `authkey` is not 'PublicKey', then the above session protocol is not used,
and channel data is in the clear.

Public methods of an object are determined by a role-based access control
attribute associated with the method. The server will verify that the current
role is allowed access. The current role is determined by an
:class:`AccessController` based on a :class:`Credentials` object received from
the proxy.

Assuming the credentials check passes, the server will set it's credentials
to those specified by the :class:`AccessController` during the execution of the
method.
"""

# Unfortunately, there's a lot of multiprocessing package code duplication here.
# And a lot of this should be done at a lower layer. But then we'd be
# duplicating connection.py code as well as (more) managers.py code. Another
# alternative is to just use our own conection.py and managers.py (and whatever
# else might bleed in) as our private multiprocessing package.
# No obvious 'best' alternative.

import errno
import hashlib
import inspect
import logging
import os
import socket
import sys
import threading
import time
import traceback

from multiprocessing import Process, current_process, connection, util
from multiprocessing.forking import Popen
from multiprocessing.managers import BaseManager, BaseProxy, RebuildProxy, \
                                     Server, State, Token, convert_to_error, \
                                     dispatch, listener_client

if sys.platform == 'win32':  #pragma no cover
    from _multiprocessing import win32

from enthought.traits.trait_handlers import TraitDictObject

from openmdao.main.interfaces import obj_has_interface
from openmdao.main.mp_util import decrypt, encrypt, is_legal_connection, \
                                  keytype, make_typeid, public_methods, \
                                  SPECIALS
from openmdao.main.rbac import AccessController, RoleError, check_role, \
                               rbac_methods, need_proxy, \
                               Credentials, get_credentials, set_credentials

from openmdao.util.publickey import decode_public_key, encode_public_key, \
                                    get_key_pair, HAVE_PYWIN32, \
                                    pk_encrypt, pk_decrypt

# Classes which require proxying (used by default AccessController)
# Used to break import loop between this and openmdao.main.container.
CLASSES_TO_PROXY = []

# Cache of proxies created by _make_proxy_type().
_PROXY_CACHE = {}


def is_instance(obj, typ):
    """
    :func:`isinstance` replacement for when `obj` might be a proxy.

    obj: object
        Object to be tested.

    typ: class
        Class to be tested against.

    Returns True if `obj` is an instance of `typ`, or the object `obj` refers
    to is an instance of `typ`.
    """
    if isinstance(obj, OpenMDAO_Proxy):
        typename = '%s.%s' % (typ.__module__, typ.__name__)
        return obj.__is_instance__(typename)
    else:
        return isinstance(obj, typ)


def has_interface(obj, *ifaces):
    """
    :func:`obj_has_interface` replacement for when `obj` might be a proxy.

    obj: object
        Object to be tested.

    ifaces: list[Interface]
        Interfaces to be tested against.

    Returns True if `obj` or the object `obj` refers to supports at least one
    :class:`Interface` in `ifaces`.
    """
    if isinstance(obj, OpenMDAO_Proxy):
        for typ in ifaces:
            typename = '%s.%s' % (typ.__module__, typ.__name__)
            if obj.__has_interface__(typename):
                return True
        return False
    else:
        return obj_has_interface(obj, *ifaces)


class OpenMDAO_Server(Server):
    """
    A :class:`Server` that supports dynamic proxy generation and credential
    checking.

    registry: dict
        Manager's proxy registry.

    address: tuple or string
        A :mod:`multiprocessing` address specifying an Internet address or
        a pipe.

    authkey: string
        Authorization key. Inherited from the current :class:`Process`
        object if not specified.

    serializer: string
        Which serialization method to use.

    name: string
        Name for server, used in log files, etc.

    allowed_hosts: list(string)
        Host address patterns to check against.
        Ignored if `allowed_users` is specified.

    allowed_users: dict
        Dictionary of users and corresponding public keys allowed access.
        If None, any user may access. If empty, no user may access.
        The host portions of user strings are used for address patterns.
    """

    def __init__(self, registry, address, authkey, serializer, name=None,
                 allowed_hosts=None, allowed_users=None):
        super(OpenMDAO_Server, self).__init__(registry, address, authkey,
                                              serializer)
        self.name = name or 'OMS_%d' % os.getpid()
        self.host = socket.gethostname()
        self._allowed_users = allowed_users
        if self._allowed_users is not None:
            hosts = set()
            for user_host in self._allowed_users.keys():
                user, host = user_host.split('@')
                hosts.add(socket.gethostbyname(host))
            self._allowed_hosts = list(hosts)
        else:
            self._allowed_hosts = allowed_hosts or []

        self._logger = logging.getLogger(name)
        self._logger.info('OpenMDAO_Server process %d started, %r',
                          os.getpid(), keytype(authkey))
        if self._allowed_users is None:
            self._logger.warning('    allowed_users: ANY')
        else:
            self._logger.info('    allowed_users: %s',
                              sorted(self._allowed_users.keys()))
        if self._allowed_hosts:
            self._logger.info('    allowed_hosts: %s',
                              sorted(self._allowed_hosts))
        else:
            self._logger.warning('    allowed_hosts: ANY')

        self._authkey = authkey
        if authkey == 'PublicKey':
            # While we may be 'owned' by some remote user, use default
            # credentials for getting our key pair. This avoids generation
            # overhead and also issues with propagating our public key
            # back through a proxy.
            self._key_pair = get_key_pair(Credentials.user_host, self._logger)
        else:
            self._key_pair = None
        self._id_to_controller = {}
        self._access_controller = AccessController()
        for cls in CLASSES_TO_PROXY:
            self._access_controller.class_proxy_required(cls)
        self._address_type = connection.address_type(self.address)

    @property
    def public_key(self):
        """ Public key for session establishment. """
        if self._authkey == 'PublicKey':
            return self._key_pair.publickey()
        # Just being defensive.
        else:  #pragma no cover
            raise RuntimeError('No public key available')

    # This happens on the remote server side and we'll check when connecting.
    @property
    def public_key_text(self):  #pragma no cover
        """ Text representation of public key. """
        if self._authkey == 'PublicKey':
            return encode_public_key(self.public_key)
        else:
            return ''

    def serve_forever(self):
        """
        Run the server forever.
        This version supports host connection filtering.
        Connection filtering allows for PublicKey servers which aren't
        accessible by just any host.
        """
        current_process()._manager_server = self
        try:
            try:
                while 1:
                    try:
                        conn = self.listener.accept()
                    # Hard to cause this to happen.
                    except (OSError, IOError):  #pragma no cover
                        continue

                    address = self.listener.last_accepted
                    if address:
                        if not is_legal_connection(address, self._allowed_hosts,
                                                   self._logger):
                            conn.close()
                            continue

                    t = threading.Thread(target=self.handle_request,
                                         args=(conn,))
                    t.daemon = True
                    try:
                        t.start()
                    # Don't want to cause this to happen.
                    except Exception as exc:  #pragma no cover
                        self._logger.error("Can't start server thread: %r", exc)
                        conn.close()
                        continue
            # Don't want to cause this to happen.
            except (KeyboardInterrupt, SystemExit):  #pragma no cover
                pass
        finally:
            self.stop = 999
            try:
                self.listener.close()
            except Exception as exc:
                self._logger.error('Exception closing listener: %r', exc)

    def handle_request(self, conn):
        """
        Handle a new connection.

        conn: socket or pipe
            Connection to process.

        This version filters host connections and avoids getting upset if it
        can't deliver a challenge. This is to deal with immediately closed
        connections caused by :meth:`manager_is_alive` which are used to avoid
        getting hung trying to connect to a manager which is no longer there.
        """
        funcname = result = request = None

        try:
            connection.deliver_challenge(conn, self.authkey)
        except (EOFError, IOError) as exc:
            conn.close()
            return
        # Hard to cause this to happen. It rarely happens, and then at shutdown.
        except Exception as exc:  #pragma no cover
            msg = ('#TRACEBACK', 'Exception delivering challenge: %r' % exc)
            try:
                conn.send(msg)
            except Exception as exc:
                pass
            util.info('Failure to send message: %r', msg)
            util.info(' ... request was %r', request)
            util.info(' ... exception was %r', exc)
            conn.close()
            return

        try:
            connection.answer_challenge(conn, self.authkey)
            request = conn.recv()
            ignore, funcname, args, kwds = request
            assert funcname in self.public, '%r unrecognized' % funcname
            func = getattr(self, funcname)
        # Hard to cause this to happen. It rarely happens, and then at shutdown.
        except Exception as exc:  #pragma no cover
            msg = ('#TRACEBACK', 'Exception answering challenge: %r' % exc)
        else:
            try:
                result = func(conn, *args, **kwds)
            # Hard to cause this to happen. It rarely happens, and then at shutdown.
            except Exception as exc:  #pragma no cover
                try:  # Sometimes at shutdown 'traceback' is None!?
                    msg = ('#TRACEBACK', traceback.format_exc())
                except Exception:
                    msg = ('#TRACEBACK',
                           'Exception from %r: %r' % (funcname, exc))
            else:
                msg = ('#RETURN', result)

        try:
            conn.send(msg)
        # Hard to cause this to happen. It rarely happens, and then at shutdown.
        except Exception as exc:  #pragma no cover
            try:
                conn.send(('#TRACEBACK', 'Exception sending reply: %r' % exc))
            except Exception:
                pass
            util.info('Failure to send message: %r', msg)
            util.info(' ... request was %r', request)
            util.info(' ... exception was %r', exc)

        conn.close()

    def serve_client(self, conn):
        """
        Handle requests from the proxies in a particular process/thread.

        conn: socket or pipe
            Connection to process.

        This version supports dynamic proxy generation and credential checking.
        """
        self._logger.debug('starting server thread to service %r, %s',
                           threading.current_thread().name,
                           keytype(self._authkey))
        recv = conn.recv
        send = conn.send
        id_to_obj = self.id_to_obj
        id_to_controller = self._id_to_controller

        if self._authkey == 'PublicKey':
            client_key, session_key = self._init_session(conn)
        else:
            client_key = ''
            session_key = ''

        while not self.stop:

            try:
                ident = methodname = args = kwds = credentials = None
                obj = exposed = gettypeid = None
                data = recv()
                try:
                    request = decrypt(data, session_key)
                except Exception as exc:
                    trace = traceback.format_exc()
                    msg = "Can't decrypt/unpack request. This could be the" \
                          " result of referring to a dead server."
                    self._logger.error(msg)
                    self._logger.error(trace)
                    raise RuntimeError(msg)

                ident, methodname, args, kwds, credentials = request
#                self._logger.debug('request %s %s %s',
#                                   ident, methodname, credentials)
#                self._logger.debug('id_to_obj:\n%s', self.debug_info(conn))

                # Decode and verify valid credentials.
                try:
                    credentials = Credentials.verify(credentials,
                                                     self._allowed_users)
                except Exception as exc:
                    self._logger.error('%r' % exc)
                    raise

                try:
                    obj, exposed, gettypeid = id_to_obj[ident]
                # Hard to cause this to happen.
                except KeyError:  #pragma no cover
                    msg = 'No object for ident %s' % ident
                    self._logger.error(msg)
                    raise KeyError('%s %r: %s' % (self.host, self.name, msg))

                if methodname not in exposed:
                    # Try to raise with a useful error message.
                    if methodname == '__getattr__':
                        try:
                            val = getattr(obj, args[0])
                        except AttributeError:
                            raise AttributeError(
                                  'attribute %r of %r object does not exist'
                                  % (args[0], type(obj)))
                        if inspect.ismethod(val):
                            methodname = args[0]
                        else:
                            raise AttributeError(
                                  'attribute %r of %r is not accessible'
                                  % (args[0], type(obj)))
                    raise AttributeError(
                                  'method %r of %r object is not in exposed=%r'
                                  % (methodname, type(obj), exposed))

                # Set correct credentials for function lookup.
                set_credentials(credentials)
                function = getattr(obj, methodname)

                # Proxy pass-through only happens remotely.
                if isinstance(obj, BaseProxy):  #pragma no cover
                    role = None
                    access_controller = None
                else:
                    # Check for allowed access.
                    role, credentials, access_controller = \
                        self._check_access(ident, methodname, function, args,
                                           credentials)
                if methodname != 'echo':
                    # 'echo' is used for performance tests, keepalives, etc.
                    self._logger.debug('Invoke %s %s %s',
                                       methodname, role, credentials)
#                    self._logger.debug('       %s %s', args, kwds)

                # Invoke function.
                try:
                    try:
                        res = function(*args, **kwds)
#                        self._logger.debug('       res %r', res)
                    except AttributeError as exc:
                        if isinstance(obj, BaseProxy) and \
                           methodname == '__getattribute__':
                            # Avoid an extra round-trip.
                            res = obj.__getattr__(*args, **kwds)
                        else:
                            raise
                except Exception as exc:
                    self._logger.error('%s %s %s: %r',
                                       methodname, role, credentials, exc)
                    msg = ('#ERROR', exc)
                else:
                    msg = self._form_reply(res, ident, methodname, function,
                                           args, access_controller, conn)

            except AttributeError:
                # Just being defensive, this should never happen.
                if methodname is None:  #pragma no cover
                    msg = ('#TRACEBACK', traceback.format_exc())
                else:
                    orig_traceback = traceback.format_exc()
                    try:
                        fallback_func = self.fallback_mapping[methodname]
                        self._logger.debug('Fallback %s', methodname)
                        result = fallback_func(self, conn, ident, obj,
                                               *args, **kwds)
                        msg = ('#RETURN', result)
                    except Exception:
                        msg = ('#TRACEBACK', orig_traceback)

            except EOFError:
                util.debug('got EOF -- exiting thread serving %r',
                           threading.current_thread().name)
                sys.exit(0)

            # Just being defensive, this should never happen.
            except Exception:  #pragma no cover
                trace = traceback.format_exc()
                self._logger.error('serve_client exception, method %s',
                                   methodname)
                self._logger.error(trace)
                msg = ('#TRACEBACK', trace)

            try:
                try:
                    send(encrypt(msg, session_key))
                except Exception:
                    send(encrypt(('#UNSERIALIZABLE', repr(msg)), session_key))
            # Just being defensive, this should never happen.
            except Exception as exc: #pragma no cover
                self._logger.error('exception in thread serving %r',
                                   threading.current_thread().name)
                self._logger.error(' ... message was %r', msg)
                self._logger.error(' ... exception was %r', exc)
                conn.close()
                sys.exit(1)

    def _init_session(self, conn):
        """ Receive client public key, send session key. """
        # Hard to cause exceptions to happen where we'll see them.
        try:
            client_data = conn.recv()
        except Exception as exc:  #pragma no cover
            self._logger.error("Can't receive client data: %r", exc)
            raise

        client_version = client_data[0]
        if client_version != 1:  #pragma no cover
            msg = 'Expected client protocol version 1, got %r' % client_version
            self._logger.error(msg)
            raise RuntimeError(msg)

        n, e, encrypted = client_data[1:]
        if e != self._key_pair.e or n != self._key_pair.n:  #pragma no cover
            msg = 'Server key mismatch'
            self._logger.error(msg)
            raise RuntimeError(msg)

        try:
            text = pk_decrypt(encrypted, self._key_pair)
            client_key = decode_public_key(text)
        except Exception as exc:  #pragma no cover
            self._logger.error("Can't recreate client key: %r", exc)
            raise

        server_version = 1
        try:
            session_key = hashlib.sha1(str(id(conn))).hexdigest()
            data = client_key.encrypt(session_key, '')
            conn.send((server_version, data))
        except Exception as exc:  #pragma no cover
            self._logger.error("Can't send session key: %r", exc)
            raise

        return (client_key, session_key)

    def _check_access(self, ident, methodname, function, args, credentials):
        """ Check for valid access, return (role, credentials, controller). """
        obj, exposed, gettypeid = self.id_to_obj[ident]

        # Get access controller for obj.
        access_controller = self._id_to_controller.get(ident)
        if access_controller is None:
            try:
                get_access_controller = getattr(obj, 'get_access_controller')
            except AttributeError:
                access_controller = self._access_controller
            # Only happens on remote protected object.
            else:  #pragma no cover
                if get_access_controller is None:
                    access_controller = self._access_controller
                else:
                    access_controller = get_access_controller()
            self._id_to_controller[ident] = access_controller

        # Get role based on credentials.
        role = access_controller.get_role(credentials)

        if methodname in SPECIALS:
            # Check for valid access based on role.
            access_controller.check_access(role, methodname, obj, args[0])
        else:
            # Check for valid role.
            try:
                check_role(role, function)
            except RoleError as exc:
                raise RoleError('%s(): %s' % (methodname, exc))

            # Set credentials for execution of function. Typically
            # these are just the credentials of the caller, but
            # sometimes a function needs to execute with different
            # credentials.
            credentials = \
                access_controller.get_proxy_credentials(function, credentials)
            set_credentials(credentials)

        return (role, credentials, access_controller)

    def _form_reply(self, res, ident, methodname, function, args,
                    access_controller, conn):
        """ Return reply message for `res`. """
        obj, exposed, gettypeid = self.id_to_obj[ident]
        msg = None
        typeid = None

        if inspect.ismethod(res) and \
           (methodname == '__getattribute__' or methodname == '__getattr__'):
            # More informative for common case of missing RBAC.
            raise AttributeError('method %r of %r object is not in exposed=%r'
                                 % (args[0], type(obj), exposed))

        # Proxy pass-through only happens remotely.
        if isinstance(res, OpenMDAO_Proxy):  #pragma no cover
            res_address = res._token.address
            res_type = connection.address_type(res_address)
            if (res_type != self._address_type) or \
               (res_type == 'AF_INET' and res_address[0] != self.address[0]):
                # Create proxy for proxy.
                # (res is probably unreachable by our client)
                typeid = res._token.typeid
                proxyid = make_typeid(res)
                self._logger.debug('Creating proxy for proxy %s', proxyid)
                if proxyid not in self.registry:
                    self.registry[proxyid] = (None, None, None, _auto_proxy)
            else:
                # Propagate the proxy info.
                res._close.cancel()  # Don't decref when reaped.
                msg = ('#PROXY', (res._exposed_, res._token, res._pubkey))

        elif access_controller is not None:
            if methodname in SPECIALS:
                if access_controller.need_proxy(obj, args[0], res):
                    # Create proxy if in declared proxy types.
                    typeid = make_typeid(res)
                    proxyid = typeid
                    if typeid not in self.registry:
                        self.registry[typeid] = (None, None, None, None)
            elif need_proxy(function, res, access_controller):
                # Create proxy if in declared proxy types.
                typeid = make_typeid(res)
                proxyid = typeid
                if typeid not in self.registry:
                    self.registry[typeid] = (None, None, None, None)

        # Proxy pass-through only happens remotely.
        else:  #pragma no cover
            # Create proxy if registered.
            typeid = gettypeid and gettypeid.get(methodname, None)
            proxyid = typeid

        if msg is None:
            if typeid:
                rident, rexposed = self.create(conn, proxyid, res)
                token = Token(typeid, self.address, rident)
                self._logger.debug('Returning proxy for %s at %s',
                                   typeid, self.address)
                if self._key_pair is None:
                    pubkey = None
                else:
                    pubkey = self._key_pair.publickey()
                msg = ('#PROXY', (rexposed, token, pubkey))
            else:
                msg = ('#RETURN', res)

        return msg

    def _fallback_isinstance(self, conn, ident, obj, typename):
        """ Check if `obj` is an instance of `typename`. """
        # It's very difficult to get an arbitrary `typename` here.
        dot = typename.rfind('.')
        module_name = typename[:dot]
        class_name = typename[dot+1:]
        try:
            module = __import__(module_name)
        except ImportError:  #pragma no cover
            return False
        module = sys.modules[module_name]
        try:
            cls = getattr(module, class_name)
        except AttributeError:  #pragma no cover
            return False
        return isinstance(obj, cls)

    Server.fallback_mapping['__is_instance__'] = _fallback_isinstance

    def _fallback_hasinterface(self, conn, ident, obj, typename):
        """ Check if `obj` supports `typename`. """
        # It's very difficult to get an arbitrary `typename` here.
        dot = typename.rfind('.')
        module_name = typename[:dot]
        class_name = typename[dot+1:]
        try:
            module = __import__(module_name)
        except ImportError:  #pragma no cover
            return False
        module = sys.modules[module_name]
        try:
            cls = getattr(module, class_name)
        except AttributeError:  #pragma no cover
            return False
        return obj_has_interface(obj, cls)

    Server.fallback_mapping['__has_interface__'] = _fallback_hasinterface

    # This is for low-level debugging of servers.
    def debug_info(self, conn):  #pragma no cover
        """
        Return string representing state of id_to_obj mapping.

        conn: socket or pipe
            Unused.

        This version handles proxies in a special manner.
        """
        self.mutex.acquire()
        try:
            result = []
            keys = self.id_to_obj.keys()
            keys.sort()
            for ident in keys:
                if ident != 0:
                    obj = self.id_to_obj[ident][0]
                    if isinstance(obj, BaseProxy):
                        obj_str = '%s proxy for %s %s' \
                                  % (keytype(obj._authkey), obj._id,
                                     obj._token.typeid)
                    else:
                        obj_str = str(obj)[:75]
                    result.append('  %s:       refcount=%s\n    %s' %
                                  (ident, self.id_to_refcount[ident], obj_str))
            return '\n'.join(result)
        finally:
            self.mutex.release()

    def create(self, conn, typeid, *args, **kwds):
        """
        Create a new shared object and return its id.

        conn: socket or pipe
            Connection to process.

        typeid: string
            Identifier string for type of object to be created.

        This version uses :func:`public_methods`.
        """
        self.mutex.acquire()
        try:
            try:
                callable, exposed, method_to_typeid, proxytype = \
                    self.registry[typeid]
            # Just being defensive.
            except KeyError:  #pragma no cover
                logging.error('mp_support.create: %r registry', typeid)
                for key, value in self.registry.items():
                    logging.error('    %s: %s', key, value)
                raise

            if callable is None:
                assert len(args) == 1 and not kwds
                obj = args[0]
            else:
                obj = callable(*args, **kwds)

            if exposed is None:
                exposed = public_methods(obj)
            if method_to_typeid is not None:
                assert type(method_to_typeid) is dict
                exposed = list(exposed) + list(method_to_typeid)

            ident = '%x' % id(obj)  # convert to string because xmlrpclib
                                    # only has 32 bit signed integers
            util.debug('%r callable returned object with id %r', typeid, ident)

            self.id_to_obj[ident] = (obj, set(exposed), method_to_typeid)
            if ident not in self.id_to_refcount:
                self.id_to_refcount[ident] = 0
            # increment the reference count immediately, to avoid
            # this object being garbage collected before a Proxy
            # object for it can be created.  The caller of create()
            # is responsible for doing a decref once the Proxy object
            # has been created.
            self.incref(conn, ident)
            return ident, tuple(exposed)
        finally:
            self.mutex.release()

    # Will only be seen on remote.
    def shutdown(self, conn):  #pragma no cover
        """ Shutdown this process. """
        self._logger.debug('received shutdown request, running exit functions')
        # Deprecated, but marginally better than atexit._run_exitfuncs()
        if hasattr(sys, 'exitfunc'):
            sys.exitfunc()
        super(OpenMDAO_Server, self).shutdown(conn)


class OpenMDAO_Manager(BaseManager):
    """
    Uses :class:`OpenMDAO_Server`, retains the public key, and puts some slack
    in shutdown timing.

    address: tuple or string
        A :mod:`multiprocessing` address specifying an Internet address or
        a pipe.

    authkey: string
        Authorization key. Inherited from the current :class:`Process`
        object if not specified.

    serializer: string
        Which serialization method to use.

    pubkey: public key
        Public portion of server's public/private key pair.
        Needed for client/proxy side.

    name: string
        Name for server, used in log files, etc.

    allowed_hosts: list(string)
        Host address patterns to check against.

    allowed_users: dict
        Dictionary of users and corresponding public keys allowed access.
        If None, any user may access. If empty, no user may access.
    """

    _Server = OpenMDAO_Server

    def __init__(self, address=None, authkey=None, serializer='pickle',
                 pubkey=None, name=None, allowed_hosts=None, allowed_users=None):
        super(OpenMDAO_Manager, self).__init__(address, authkey, serializer)
        self._pubkey = pubkey
        self._name = name
        self._allowed_hosts = allowed_hosts
        self._allowed_users = allowed_users

    def get_server(self):
        """
        Return a server object with :meth:`serve_forever` and address attribute.
        """
        assert self._state.value == State.INITIAL
        return OpenMDAO_Server(self._registry, self._address, self._authkey,
                               self._serializer, self._name,
                               self._allowed_hosts, self._allowed_users)

    def start(self, cwd=None):
        """
        Spawn a server process for this manager object.

        cwd: string
            Directory to start in.

        This version retrieves the server's public key.
        """
        assert self._state.value == State.INITIAL

        # Pipe over which we will retrieve address of server.
        reader, writer = connection.Pipe(duplex=False)

        if sys.platform == 'win32':  #pragma no cover
            # Make registry pickleable.
            registry = {}
            for typeid, info in self._registry.items():
                callable, exposed, method_to_typeid, proxytype = info
                if proxytype and proxytype != _auto_proxy:
                    registry[typeid] = \
                        (callable, exposed, method_to_typeid, 'rebuild')
        else:
            registry = self._registry

        # Spawn process which runs a server.
        credentials = get_credentials()
        self._process = Process(
            target=type(self)._run_server,
            args=(registry, self._address, self._authkey,
                  self._serializer, self._name, self._allowed_hosts,
                  self._allowed_users, writer, credentials, cwd),
            )
        ident = ':'.join(str(i) for i in self._process._identity)
        self._process.name = type(self).__name__  + '-' + ident
        self._process.start()
        pid = self._process.pid

        # Get address of server.
        if self._authkey == 'PublicKey':
            if sys.platform == 'win32' and not HAVE_PYWIN32:  #pragma no cover
                timeout = 120
            else:
                timeout = 5
        else:
            timeout = 5

        writer.close()
        start = time.time()
        for retry in range(timeout):
            if reader.poll(1):
                break
            if not self._process.is_alive():
                raise RuntimeError('Server process %d exited: %s'
                                   % (pid, self._process.exitcode))
        # Hard to cause a timeout.
        else:  #pragma no cover
            et = time.time() - start
            self._process.terminate()
            raise RuntimeError('Server process %d startup timed-out in %.2f' \
                               % (pid, et))
        reply = reader.recv()
        if isinstance(reply, Exception):
            raise RuntimeError('Server process %d startup failed: %s'
                               % (pid, reply))
        self._address = reply
        if self._authkey == 'PublicKey':
            self._pubkey = reader.recv()
        reader.close()
        et = time.time() - start
        logging.debug('Server process %d startup in %.2f', pid, et)

        # Register a finalizer.
        self._state.value = State.STARTED
        self.shutdown = util.Finalize(
            self, type(self)._finalize_manager,
            args=(self._process, self._address, self._authkey,
                  self._state, self._Client),
            exitpriority=0
            )

    # This happens on the remote server side and we'll check when using it.
    @classmethod
    def _run_server(cls, registry, address, authkey, serializer, name,
                    allowed_hosts, allowed_users,
                    writer, credentials, cwd=None): #pragma no cover
        """
        Create a server, report its address and public key, and run it.
        """
        try:
            if sys.platform == 'win32':
                set_credentials(credentials)
                # Recreate registry proxytypes.
                for typeid, info in registry.items():
                    callable, exposed, method_to_typeid, proxytype = info
                    if proxytype == 'rebuild':
                        registry[typeid] = (callable, exposed, method_to_typeid,
                                            _make_proxy_type(typeid, exposed))

            # If specified, move to new directory.
            if cwd is not None:
                os.chdir(cwd)

                # Reset stdout & stderr.
                for handler in logging._handlerList:
                    try:
                        handler.flush()
                    except AttributeError:
                        h = handler()
                        if h:
                            h.flush()
                sys.stdout.flush()
                sys.stderr.flush()
                sys.stdout = open('stdout', 'w')
                sys.stderr = open('stderr', 'w')

                # Reset logging.
                logging.root.handlers = []
                logging.basicConfig(level=logging.NOTSET,
                    datefmt='%b %d %H:%M:%S',
                    format='%(asctime)s %(levelname)s %(name)s: %(message)s',
                    filename='openmdao_log.txt', filemode='w')

            # Create server.
            server = cls._Server(registry, address, authkey, serializer, name,
                                 allowed_hosts, allowed_users)
        except Exception as exc:
            writer.send(exc)
            return
        else:
            # Inform parent process of the server's address.
            writer.send(server.address)
            if authkey == 'PublicKey':
                writer.send(server.public_key)
        finally:
            writer.close()

        # Run the manager.
        util.info('manager serving at %r', server.address)
        server.serve_forever()

    @staticmethod
    def _finalize_manager(process, address, authkey, state, _Client):
        """
        Shutdown the manager process; will be registered as a finalizer.
        This version uses relaxed timing.
        """
        if process.is_alive():
            logging.debug('sending shutdown message to manager')
            try:
                conn = _get_connection(_Client, address, authkey)
                try:
                    dispatch(conn, None, 'shutdown')
                finally:
                    conn.close()
            # Just being defensive here.
            except Exception:  #pragma no cover
                pass

            process.join(timeout=2)
            # No good way to cause process to not shut down.
            if process.is_alive():  #pragma no cover
                logging.warning('manager still alive after shutdown request')
                if hasattr(process, 'terminate'):
                    logging.warning('trying to `terminate()` manager process')
                    process.terminate()
                    process.join(timeout=1)
                    if process.is_alive():
                        logging.warning('manager still alive after terminate')

        state.value = State.SHUTDOWN
        try:
            del BaseProxy._address_to_local[address]
        # Just being defensive here.
        except KeyError:  #pragma no cover
            pass


class ObjectManager(object):
    """
    Provides a multiprocessing interface for an existing object.

    obj: object
        Object to provide remote access to.

    address: tuple or string
        A :mod:`multiprocessing` address specifying an Internet address or
        a pipe.

    serializer: string
        Which serialization method to use.

    authkey: string
        Authorization key. Inherited from the current :class:`Process`
        object if not specified.

    name: string
        Name for server, used in log files, etc.

    allowed_hosts: list(string)
        Host address patterns to check against.

    allowed_users: dict
        Dictionary of users and corresponding public keys allowed access.
        If None, any user may access. If empty, no user may access.
    """

    def __init__(self, obj, address=None, serializer='pickle', authkey=None,
                 name=None, allowed_hosts=None, allowed_users=None):
        self._typeid = make_typeid(obj)
        self._ident = '%x' % id(obj)
        logging.debug('ObjectManager address %s, %r, name %r, ident %r',
                      address, keytype(authkey), name, self._ident)
        self._manager = OpenMDAO_Manager(address=address, serializer=serializer,
                                         authkey=authkey, name=name,
                                         allowed_hosts=allowed_hosts,
                                         allowed_users=allowed_users)
        self._server = self._manager.get_server()
        self._exposed = public_methods(obj)

        with self._server.mutex:
            self._server.id_to_obj[self._ident] = (obj, self._exposed, None)
            self._server.id_to_refcount[self._ident] = 1

        self._server_thread = threading.Thread(target=self._run_server)
        self._server_thread.daemon = True
        self._server_thread.start()

        self._proxy = None

    @property
    def proxy(self):
        """ Proxy for our object server. """
        if self._proxy is None:
            token = Token(self._typeid, self._server.address, self._ident)
            authkey = self._server._authkey
            pubkey = self._server.public_key if authkey == 'PublicKey' else None
            self._proxy = _auto_proxy(token, self._manager._serializer,
                                      authkey=authkey, exposed=self._exposed,
                                      pubkey=pubkey)
            self._server.incref(None, self._ident)
        return self._proxy

    def _run_server(self):
        """ Run a manager server (in a separate thread!). """
        self._server.serve_forever()


class OpenMDAO_Proxy(BaseProxy):
    """
    Proxy for a remote object.

    args: tuple
        Passed to :class:`BaseProxy`.

    kwds: dict
        If it contains `pubkey`, then that value is used for the remote public
        key, and the entry is removed before being passed to :class:`BaseProxy`.

    This version sends credentials and provides dynamic result proxy generation.

    .. note::

        :meth:`BaseProxy.__str__` (used by :meth:`str` and the ``%s`` format)
        will return :meth:`__repr__` of the remote object.  To avoid the
        network round-trip delay, use :meth:`repr` or the ``%r`` format.

    """

    def __init__(self, *args, **kwds):
        try:
            pubkey = kwds['pubkey']
        except KeyError:
            pubkey = None
        else:
            del kwds['pubkey']

        super(OpenMDAO_Proxy, self).__init__(*args, **kwds)

        if self._manager is None:
            self._pubkey = pubkey
        else:
            self._pubkey = self._manager._pubkey

    def _callmethod(self, methodname, args=None, kwds=None):
        """
        Try to call a method of the referrent and return a copy of the result.
        This version optionally encrypts the channel and sends the current
        thread's credentials with method arguments.
        """
        args = args or ()
        kwds = kwds or {}

        try:
            conn = self._tls.connection
        except AttributeError:
            curr_thread = threading.current_thread()
            util.debug('thread %r does not own a connection', curr_thread.name)
            try:
                self._connect()
            except Exception as exc:
                msg = "Can't connect to server at %r for %r: %r" \
                      % (self._token.address, methodname, exc)
                logging.error(msg)
                raise RuntimeError(msg)
            conn = self._tls.connection
            if self._authkey == 'PublicKey':
                self._init_session(conn)
            else:
                self._tls.session_key = ''

        session_key = self._tls.session_key

# FIXME: Bizarre problem evidenced by test_extcode.py (Python 2.6.1)
# For some reason pickling the env_vars dictionary causes:
#    PicklingError: Can't pickle <class 'openmdao.main.mp_support.ObjServer'>:
#                     attribute lookup openmdao.main.mp_support.ObjServer failed
# The reported type is not in the (current) Dict items.
# Apparently this is some Traits 'feature'.
        new_args = []
        for arg in args:
            if isinstance(arg, TraitDictObject):
                new_args.append(dict(arg))
            else:
                new_args.append(arg)

        try:
            conn.send(encrypt((self._id, methodname, new_args, kwds,
                               get_credentials().encode()), session_key))
        except IOError as exc:
            msg = "Can't send to server at %r for %r: %r" \
                  % (self._token.address, methodname, exc)
            logging.error(msg)
            raise RuntimeError(msg)

        kind, result = decrypt(conn.recv(), session_key)

        if kind == '#RETURN':
            return result

        elif kind == '#PROXY':
            exposed, token, pubkey = result

            # Proxy passthru only happens remotely.
            if self._manager is None:  #pragma no cover
                self._manager = OpenMDAO_Manager(token.address, self._authkey,
                                                 pubkey=pubkey)
            try:
                proxytype = self._manager._registry[token.typeid][-1]
            except KeyError:
                self._manager.register(token.typeid, None, _auto_proxy)
                proxytype = self._manager._registry[token.typeid][-1]

            if token.address != self._manager.address:
                # Proxy to different server than request was sent to.
                manager = OpenMDAO_Manager(token.address, self._authkey,
                                           pubkey=pubkey)
            else:
                manager = self._manager

            proxy = proxytype(token, self._serializer, manager=manager,
                              authkey=self._authkey, exposed=exposed,
                              pubkey=pubkey)

            conn = _get_connection(self._Client, token.address, self._authkey)
            dispatch(conn, None, 'decref', (token.id,))
            return proxy

        raise convert_to_error(kind, result)

    def _init_session(self, conn):
        """ Send client public key, receive session key. """
        key_pair = get_key_pair(Credentials.user_host)
        public_key = key_pair.publickey()
        text = encode_public_key(public_key)

        server_key = self._pubkey
        # Just being defensive, this should never happen.
        if not server_key:  #pragma no cover
            msg = 'No server PublicKey for %s(%s, %s)' \
                  % (methodname, args, kwds)
            logging.error(msg)
            raise RuntimeError(msg)

        encrypted = pk_encrypt(text, server_key)
        client_version = 1
        conn.send((client_version, server_key.n, server_key.e, encrypted))

        server_data = conn.recv()
        server_version = server_data[0]
        # Just being defensive, this should never happen.
        if server_version != 1:  #pragma no cover
            msg = 'Expecting server protocol version 1, got %r' % server_version
            logging.error(msg)
            if server_version == '#TRACEBACK':
                try:
                    logging.error(''.join(server_data[1]))
                except Exception:
                    pass
            raise RuntimeError(msg)
        
        self._tls.session_key = key_pair.decrypt(server_data[1])

    def _incref(self):
        """
        Tell server to increment its reference count.
        This version avoids a hang in _Client if the server no longer exists.
        """
        # Hard to cause this to happen.
        if not OpenMDAO_Proxy.manager_is_alive(self._token.address):  #pragma no cover
            raise RuntimeError('Cannot connect to manager at %r' 
                               % (self._token.address,))

        conn = _get_connection(self._Client, self._token.address, self._authkey)
        dispatch(conn, None, 'incref', (self._id,))
        # Enable this with care. While testing CaseIteratorDriver it can cause a
        # deadlock in logging (called via BaseProxy._after_fork()).
        #util.debug('INCREF %r', self._token.id)

        self._idset.add(self._id)

        state = self._manager and self._manager._state

        self._close = util.Finalize(
            self, OpenMDAO_Proxy._decref,
            args=(self._token, self._authkey, state,
                  self._tls, self._idset, self._Client),
            exitpriority=10
            )

    @staticmethod
    def _decref(token, authkey, state, tls, idset, _Client):
        """
        Tell server to decrement its reference count.
        This version avoids a hang in _Client if the server no longer exists.
        """
        idset.discard(token.id)

        # check whether manager is still alive
        if state is None or state.value == State.STARTED:
            # Avoid a hang in _Client() if the server isn't there anymore.
            if OpenMDAO_Proxy.manager_is_alive(token.address):
                # tell manager this process no longer cares about referent
                try:
                    util.debug('DECREF %r', token.id)
                    conn = _get_connection(_Client, token.address, authkey)
                    dispatch(conn, None, 'decref', (token.id,))
                # Hard to cause this to happen.
                except Exception as exc:  #pragma no cover
                    util.debug('... decref failed %s', exc)
        else:
            util.debug('DECREF %r -- manager already shutdown', token.id)

        # check whether we can close this thread's connection because
        # the process owns no more references to objects for this manager
        if not idset and hasattr(tls, 'connection'):
            util.debug('thread %r has no more %r proxies so closing conn',
                       threading.current_thread().name, token.typeid)
            tls.connection.close()
            del tls.connection

    @staticmethod
    def manager_is_alive(address):
        """
        Check whether manager is still alive.

        address: tuple or string
            A :mod:`multiprocessing` address specifying an Internet address or
            a pipe.
        """
        addr_type = connection.address_type(address)
        if addr_type == 'AF_PIPE':  #pragma no cover
            try:
                win32.WaitNamedPipe(address, 10)  # 0.01 sec
            except WindowsError:
                alive = False
            else:
                alive = True
        else:
            if addr_type == 'AF_INET':
                sock = socket.socket(socket.AF_INET)
            elif addr_type == 'AF_UNIX':
                sock = socket.socket(socket.AF_UNIX)

            try:
                sock.connect(address)
            except socket.error as exc:
                if exc.args[0] == errno.ECONNREFUSED or \
                   exc.args[0] == errno.ENOENT:
                    alive = False
                # Just being defensive.
                else:  #pragma no cover
                    raise
            else:
                sock.close()
                alive = True
        return alive

    def __reduce__(self):
        """ For unpickling.  This version uses :func:`_auto_proxy`. """
        kwds = {}
        kwds['pubkey'] = self._pubkey
        # Happens on other side of fork().
        if Popen.thread_is_spawning():  #pragma no cover
            kwds['authkey'] = self._authkey
        elif self._pubkey:
            # Can't pickle an AuthenticationString.
            kwds['authkey'] = 'PublicKey'

        kwds['exposed'] = self._exposed_
        return (RebuildProxy,
                (_auto_proxy, self._token, self._serializer, kwds))


def register(cls, manager, module=None):
    """
    Register class `cls` proxy info with `manager`. The class will be
    registered under it's full path, with '.' replaced by '_'.
    Not typically called by user code.

    cls: class
        Class to be registered.

    manager: :class:`OpenMDAO_Manager`
        Manager to register with.

    module: string
        Module name for registration. Necessary if `cls` might be defined
        by module '__main__'.
    """
    if module is None:
        module = cls.__module__
    typeid = '%s.%s' % (module, cls.__name__)
    typeid = typeid.replace('.', '_')
    exposed = public_methods(cls)
    proxytype = _make_proxy_type(typeid, exposed)
    manager.register(typeid, callable=cls, exposed=exposed, proxytype=proxytype)


def _make_proxy_type(name, exposed):
    """
    Return a proxy type whose methods are given by `exposed`.
    This version supports special attribute access methods.
    """
    exposed = tuple(exposed)
    try:
        return _PROXY_CACHE[(name, exposed)]
    except KeyError:
        pass

    dic = {'_instance_names': {}, '_interface_names': {}}

    for meth in exposed:
        if meth == '__is_instance__':
            # Call remote if we don't have the answer in cache.
            exec """
def __is_instance__(self, *args, **kwds):
    try:
        return self._instance_names[args[0]]
    except KeyError:
        yes_no = self._callmethod('__is_instance__', args, kwds)
        self._instance_names[args[0]] = yes_no
        return yes_no
""" in dic

        elif meth == '__has_interface__':
            # Call remote if we don't have the answer in cache.
            exec """
def __has_interface__(self, *args, **kwds):
    try:
        return self._interface_names[args[0]]
    except KeyError:
        yes_no = self._callmethod('__has_interface__', args, kwds)
        self._interface_names[args[0]] = yes_no
        return yes_no
""" in dic

        elif meth == '__getattr__':
            pass  # Avoid recursion loop, but must be in 'exposed'.

        elif meth == '__getattribute__':
            # Call remote if not private or defined locally (proxied methods).
            exec """
def __getattr__(self, *args, **kwds):
    if args[0][0] == '_':
        return object.__getattribute__(self, *args, **kwds)
    try:
        return object.__getattribute__(self, *args, **kwds)
    except AttributeError:
        try:
            return self._callmethod('__getattribute__', args, kwds)
        except AttributeError:
            return self._callmethod('__getattr__', args, kwds)
""" in dic

        elif meth == '__setattr__' or meth == '__delattr__':
            # Call remote if not private.
            exec """
def %s(self, *args, **kwds):
    if args[0][0] == '_':
        return object.%s(self, *args, **kwds)
    return self._callmethod(%r, args, kwds)
""" % (meth, meth, meth) in dic

        elif meth == '__exit__':
            # Can't pickle traceback argument.
            exec """
def __exit__(self, exc_type, exc_value, traceback):
    return self._callmethod('__exit__', (exc_type, exc_value, None))
""" in dic

        else:
            # Normal method always calls remote.
            exec """
def %s(self, *args, **kwds):
    return self._callmethod(%r, args, kwds)
""" % (meth, meth) in dic

    ProxyType = type(name, (OpenMDAO_Proxy,), dic)
    ProxyType._exposed_ = exposed
    _PROXY_CACHE[(name, exposed)] = ProxyType
    return ProxyType


def _auto_proxy(token, serializer, manager=None, authkey=None,
                exposed=None, incref=True, pubkey=None):
    """
    Return an auto-proxy for `token`.
    This version uses :func:`_make_proxy_type`.
    """
    ProxyType = _make_proxy_type('OpenMDAO_AutoProxy[%s]' % token.typeid,
                                 exposed)
    proxy = ProxyType(token, serializer, manager=manager, authkey=authkey,
                      incref=incref, pubkey=pubkey)
    proxy._isauto = True
    return proxy


def _get_connection(_client, address, authkey):
    """
    Get client connection to `address` using `authkey`.
    Avoids dying on 'Interrupted system call'. (Should be in lower layer)
    """
    for retry in range(3):
        try:
            conn = _client(address, authkey=authkey)
        except IOError as exc:
            if exc.errno != 4 or retry >= 2:
                raise
        except Exception:
            raise
        else:
            return conn

    raise RuntimeError('Too many connection attempts')

