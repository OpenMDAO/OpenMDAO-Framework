"""
This module defines modified versions of some classes and functions defined in
:mod:`multiprocessing.managers` to support OpenMDAO distributed simulations.
Unfortunately, there's a lot of code duplication here.

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

import base64
import ConfigParser
import cPickle
import errno
import getpass
import hashlib
import inspect
import logging
import os.path
import socket
import sys
import threading
from traceback import format_exc

from Crypto.Cipher import AES
from Crypto.PublicKey import RSA
from Crypto.Util.randpool import RandomPool

from multiprocessing import Process, current_process, connection, util
from multiprocessing.forking import Popen
from multiprocessing.managers import BaseManager, BaseProxy, RebuildProxy, \
                                     Server, State, Token, convert_to_error, \
                                     dispatch, listener_client
if sys.platform == 'win32':
    from _multiprocessing import win32

from openmdao.main.interfaces import obj_has_interface
from openmdao.main.rbac import AccessController, RoleError, check_role, \
                               rbac_methods, need_proxy, \
                               get_credentials, set_credentials

# Classes which require proxying (used by default AccessController)
# Used to break import loop between this and openmdao.main.container.
CLASSES_TO_PROXY = []

# Cache of proxies created by make_proxy_type().
_PROXY_CACHE = {}

# Cache of client key pairs indexed by user (from credentials).
_KEY_CACHE = {}
_KEY_CACHE_LOCK = threading.Lock()

# Names of attribute access methods requiring special handling.
_SPECIALS = ('__getattribute__', '__getattr__', '__setattr__', '__delattr__')


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


class _SHA1(object):
    """
    Just to get around a deprecation message when using the default
    :class:`RandomPool` hash.
    """

    digest_size = None

    def __init__(self):
        self.hash = hashlib.sha1()
        if _SHA1.digest_size is None:
            _SHA1.digest_size = self.hash.digest_size

    @staticmethod
    def new(data=None):
        """ Return new hash object, optionally initialized with `data`. """
        obj = _SHA1()
        if data:
            obj.update(data)
        return obj

    def digest(self):
        """ Return hash result. """
        return self.hash.digest()

    def update(self, data):
        """ Update hash with `data`. """
        self.hash.update(data)


def _generate_key_pair(credentials, logger=None):
    """
    Returns RSA key containing both public and private keys for the user
    identified in `credentials`.  This can be an expensive operation, so
    we avoid generating a new key pair whenever possible.
    """
    with _KEY_CACHE_LOCK:
        # Look in previously generated keys.
        try:
            key_pair = _KEY_CACHE[credentials.user]
        except KeyError:
            # If key for current user (typical), check filesystem.
            user, host = credentials.user.split('@')
            if user == getpass.getuser() and host == socket.gethostname():
                current_user = True
                if sys.platform == 'win32':  #pragma no cover
                    home = os.environ['HOMEDRIVE'] + os.environ['HOMEPATH']
                else:
                    home = os.environ['HOME']
                key_dir = os.path.join(home, '.openmdao')
                key_file = os.path.join(key_dir, 'keys')
                try:
                    with open(key_file, 'rb') as inp:
                        key_pair = cPickle.load(inp)
                except Exception:
                    generate = True
                else:
                    generate = False
            else:
                current_user = False
                generate = True

            if generate:
                if logger is None:
                    logger = logging.getLogger()
                logger.debug('generating public key...')
                pool = RandomPool(2048, hash=_SHA1)
                pool.stir()
                key_pair = RSA.generate(2048, pool.get_bytes)
                logger.debug('    done')

                if current_user:
                    # Save in protected file.
# FIXME: this protection does *not* work on Windows!
                    if not os.path.exists(key_dir):
                        os.mkdir(key_dir)
                        os.chmod(key_dir, 0700)  # Read/Write/Execute
                    with open(key_file, 'wb') as out:
                        cPickle.dump(key_pair, out)
                    os.chmod(key_file, 0600)     # Read/Write

            _KEY_CACHE[credentials.user] = key_pair

    return key_pair


def _encode_public_key(key):
    """ Return text representation of public key `key`. """
    # Just being defensive, this should never happen.
    if key.has_private():  #pragma no cover
        key = key.publickey()
    return base64.b64encode(cPickle.dumps(key, cPickle.HIGHEST_PROTOCOL))

def decode_public_key(text):
    """ Return public key from text representation. """
    return cPickle.loads(base64.b64decode(text))


# This happens on the remote server side and we'll check when connecting.
def write_server_config(server, filename):  #pragma no cover
    """
    Write server connection information.

    server: OpenMDAO_Server
        Server to be recorded.

    filename: string
        Path to file to be written.

    Connection information including IP address, port, and public key is
    written using :class:`ConfigParser`.
    """
    parser = ConfigParser.ConfigParser()
    section = 'ServerInfo'
    parser.add_section(section)
    parser.set(section, 'address', str(server.address[0]))
    parser.set(section, 'port', str(server.address[1]))
    parser.set(section, 'key', server.public_key_text)
    with open(filename, 'w') as cfg:
        parser.write(cfg)

def read_server_config(filename):
    """
    Read a server's connection information.

    filename: string
        Path to file to be read.

    Returns ``(address, port, key)``.
    """
    if not os.path.exists(filename):
        raise IOError("No such file '%s'" % filename)
    parser = ConfigParser.ConfigParser()
    parser.read(filename)
    section = 'ServerInfo'
    address = parser.get(section, 'address')
    port = parser.getint(section, 'port')
    key = parser.get(section, 'key')
    if key:
        key = decode_public_key(key)
    return (address, port, key)


def _encrypt(obj, session_key):
    """
    If `session_key` is specified, returns ``(length, data)`` of encrypted,
    pickled, `obj`. Otherwise `obj` is returned.
    """
    if session_key:
        # Just being defensive, this should never happen.
        if len(session_key) < 16:  #pragma no cover
            session_key += '!'*16
        session_key = session_key[:16]
        encryptor = AES.new(session_key, AES.MODE_CBC, '?'*AES.block_size)
        text = cPickle.dumps(obj, cPickle.HIGHEST_PROTOCOL)
        length = len(text)
        pad = length % AES.block_size
        if pad:
            pad = AES.block_size - pad
            text += '-'*pad
        data = encryptor.encrypt(text)
        return (length, data)
    else:
        return obj

def _decrypt(msg, session_key):
    """
    If `session_key` is specified, returns object from encrypted pickled data
    contained in `msg`. Otherwise `msg` is returned.
    """
    if session_key:
        # Just being defensive, this should never happen.
        if len(msg) != 2:  #pragma no cover
            raise RuntimeError('_decrypt: msg not encrypted?')
        # Just being defensive, this should never happen.
        if len(session_key) < 16:  #pragma no cover
            session_key += '!'*16
        session_key = session_key[:16]
        decryptor = AES.new(session_key, AES.MODE_CBC, '?'*AES.block_size)
        length, data = msg
        text = decryptor.decrypt(data)
        return cPickle.loads(text[:length])
    else:
        return msg


def _public_methods(obj):
    """
    Returns a list of names of the methods of `obj` to be exposed.
    Supports attribute access in addition to RBAC decorated methods.

    obj: object
        Object to be scanned.
    """
    # Proxy pass-through only happens remotely.
    if isinstance(obj, BaseProxy):  #pragma no cover
        methods = []
        for name in dir(obj):
            if name[0] != '_':
                attr = getattr(obj, name)
                if inspect.ismethod(attr) or inspect.isfunction(attr):
                    methods.append(name)
    else:
        methods = rbac_methods(obj)

    # Add special methods for attribute access.
    methods.extend([name for name in _SPECIALS if hasattr(obj, name)])

    # Add special __is_instance__ and __has_interface__ methods.
    methods.append('__is_instance__')
    methods.append('__has_interface__')
    return methods


def _make_typeid(obj):
    """
    Returns a type ID string from `obj`'s module and class names
    by replacing '.' with '_'.
    """
    if isinstance(obj, type):
        typeid = '%s.%s' % (obj.__module__, obj.__name__)
    else:
        typeid = '%s.%s' % (obj.__class__.__module__, obj.__class__.__name__)
    return typeid.replace('.', '_')

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
    exposed = _public_methods(cls)
    proxytype = _make_proxy_type(typeid, exposed)
    manager.register(typeid, callable=cls, exposed=exposed, proxytype=proxytype)


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
    """

    def __init__(self, registry, address, authkey, serializer, name):
        super(OpenMDAO_Server, self).__init__(registry, address, authkey,
                                              serializer)
        if name is None:
            name = str(os.getpid())
        self._logger = logging.getLogger(name)
        self._logger.debug('OpenMDAO_Server process %d started', os.getpid())
        self._authkey = authkey
        if authkey == 'PublicKey':
            self._key_pair = _generate_key_pair(get_credentials(), self._logger)
        else:
            self._key_pair = None
        self._id_to_controller = {}
        self._access_controller = AccessController()
        for cls in CLASSES_TO_PROXY:
            self._access_controller.class_proxy_required(cls)

    @property
    def public_key(self):
        """ Public key for session establishment. """
        if self._authkey == 'PublicKey':
            return self._key_pair.publickey()
        else:
            raise RuntimeError('No public key available')

    # This happens on the remote server side and we'll check when connecting.
    @property
    def public_key_text(self):  #pragma no cover
        """ Text representation of public key. """
        if self._authkey == 'PublicKey':
            return _encode_public_key(self.public_key)
        else:
            return ''

    def serve_client(self, conn):
        """
        Handle requests from the proxies in a particular process/thread.
        This version supports dynamic proxy generation and credential checking.
        """
        util.debug('starting server thread to service %r',
                   threading.current_thread().name)

        recv = conn.recv
        send = conn.send
        id_to_obj = self.id_to_obj
        id_to_controller = self._id_to_controller

        if self._authkey == 'PublicKey':
            # Receive client public key, send session key.
            chunks = recv()
            text = ''
            for chunk in chunks:
                text += self._key_pair.decrypt(chunk)
            client_key = decode_public_key(text)
            session_key = hashlib.sha1(str(id(conn))).hexdigest()
            data = client_key.encrypt(session_key, '')
            send(data)
        else:
            session_key = ''

        while not self.stop:

            try:
                methodname = obj = None
                request = _decrypt(recv(), session_key)
                ident, methodname, args, kwds, credentials = request
                obj, exposed, gettypeid = id_to_obj[ident]

                if methodname not in exposed:
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

                # Assume we'll just propagate credentials.
                set_credentials(credentials)

                function = getattr(obj, methodname)

                # Proxy pass-through only happens remotely.
                if isinstance(obj, BaseProxy):  #pragma no cover
                    access_controller = None
                    role = None
                else:
                    # Get access controller for obj.
                    access_controller = id_to_controller.get(ident)
                    if access_controller is None:
                        try:
                            get_access_controller = \
                                getattr(obj, 'get_access_controller')
                        except AttributeError:
                            access_controller = self._access_controller
                        # Only happens on remote protected object.
                        else:  #pragma no cover
                            access_controller = get_access_controller()
                        id_to_controller[ident] = access_controller

                    # Get role based on credentials.
                    role = access_controller.get_role(credentials)

                    if methodname in _SPECIALS:
                        # Check for valid access based on role.
                        access_controller.check_access(role, methodname, obj,
                                                       args[0])
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
                        set_credentials(
                            access_controller.get_proxy_credentials(function,
                                                                   credentials))

                self._logger.debug('Invoke %s %s %s', methodname, role,
                                   get_credentials())
                try:
                    res = function(*args, **kwds)
                except Exception as exc:
                    self._logger.error('%s %s %s: %s', methodname, role,
                                       get_credentials(), exc)
                    msg = ('#ERROR', exc)
                else:
                    typeid = None

                    if inspect.ismethod(res) and \
                       (methodname == '__getattribute__' or \
                        methodname == '__getattr__'):
                        # More informative for common case of missing RBAC.
                        raise AttributeError(
                            'method %r of %r object is not in exposed=%r' %
                            (args[0], type(obj), exposed))

                    # Proxy pass-through only happens remotely.
                    if isinstance(res, BaseProxy):  #pragma no cover
                        # Create proxy for proxy.
                        # (res may be unreachable by our client)
                        # TODO: avoid extra proxy if address is compatible
                        #       with our own.
                        typeid = res._token.typeid
                        proxyid = _make_typeid(res)
                        self._logger.debug('Creating proxy for proxy %s',
                                           proxyid)
                        if proxyid not in self.registry:
                            self.registry[proxyid] = \
                                (None, None, None, _auto_proxy)
                    elif access_controller is not None:
                        if methodname in _SPECIALS:
                            if access_controller.need_proxy(obj, args[0], res):
                                # Create proxy if in declared proxy types.
                                typeid = _make_typeid(res)
                                proxyid = typeid
                                if typeid not in self.registry:
                                    self.registry[typeid] = \
                                        (None, None, None, None)
                        elif need_proxy(function, res):
                            # Create proxy if in declared proxy types.
                            typeid = _make_typeid(res)
                            proxyid = typeid
                            if typeid not in self.registry:
                                self.registry[typeid] = (None, None, None, None)
                    # Proxy pass-through only happens remotely.
                    else:  #pragma no cover
                        # Create proxy if registered.
                        typeid = gettypeid and gettypeid.get(methodname, None)
                        proxyid = typeid

                    if typeid:
                        rident, rexposed = self.create(conn, proxyid, res)
                        token = Token(typeid, self.address, rident)
                        self._logger.debug('Returning proxy for %s at %s',
                                           typeid, self.address)
                        msg = ('#PROXY', (rexposed, token))
                    else:
                        msg = ('#RETURN', res)

            except AttributeError:
                # Just being defensive, this should never happen.
                if methodname is None:  #pragma no cover
                    msg = ('#TRACEBACK', format_exc())
                else:
                    orig_traceback = format_exc()
                    try:
                        fallback_func = self.fallback_mapping[methodname]
                        result = fallback_func(
                            self, conn, ident, obj, *args, **kwds
                            )
                        msg = ('#RETURN', result)
                    except Exception:
                        msg = ('#TRACEBACK', orig_traceback)

            except EOFError:
                self._logger.debug('got EOF -- exiting thread serving %r',
                                   threading.current_thread().name)
                sys.exit(0)

            # Just being defensive, this should never happen.
            except Exception:  #pragma no cover
                msg = ('#TRACEBACK', format_exc())

            try:
                try:
                    send(_encrypt(msg, session_key))
                except Exception:
                    send(_encrypt(('#UNSERIALIZABLE', repr(msg)), session_key))
            except Exception as exc:
                self._logger.info('exception in thread serving %r',
                                  threading.current_thread().name)
                self._logger.info(' ... message was %r', msg)
                self._logger.info(' ... exception was %r', exc)
                conn.close()
                sys.exit(1)

    def _fallback_isinstance(self, conn, ident, obj, typename):
        """ Check if `obj` is an instance of `typename`. """
        dot = typename.rfind('.')
        module_name = typename[:dot]
        class_name = typename[dot+1:]
        try:
            module = __import__(module_name)
        except ImportError:
            return False
        module = sys.modules[module_name]
        try:
            cls = getattr(module, class_name)
        except AttributeError:
            return False
        return isinstance(obj, cls)

    Server.fallback_mapping['__is_instance__'] = _fallback_isinstance

    def _fallback_hasinterface(self, conn, ident, obj, typename):
        """ Check if `obj` supports `typename`. """
        dot = typename.rfind('.')
        module_name = typename[:dot]
        class_name = typename[dot+1:]
        try:
            module = __import__(module_name)
        except ImportError:
            return False
        module = sys.modules[module_name]
        try:
            cls = getattr(module, class_name)
        except AttributeError:
            return False
        return obj_has_interface(obj, cls)

    Server.fallback_mapping['__has_interface__'] = _fallback_hasinterface

    def create(self, c, typeid, *args, **kwds):
        """
        Create a new shared object and return its id.
        This version uses :func:`_public_methods`.
        """
        self.mutex.acquire()
        try:
            try:
                callable, exposed, method_to_typeid, proxytype = \
                    self.registry[typeid]
            except KeyError:
                logging.critical('mp_support.create: %r registry', typeid)
                for key, value in self.registry.items():
                    logging.critical('    %s: %s', key, value)
                raise

            if callable is None:
                assert len(args) == 1 and not kwds
                obj = args[0]
            else:
                obj = callable(*args, **kwds)

            if exposed is None:
                exposed = _public_methods(obj)
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
            self.incref(c, ident)
            return ident, tuple(exposed)
        finally:
            self.mutex.release()


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
    """

    _Server = OpenMDAO_Server

    def __init__(self, address=None, authkey=None, serializer='pickle',
                 pubkey=None, name=None):
        super(OpenMDAO_Manager, self).__init__(address, authkey, serializer)
        self._pubkey = pubkey
        self._name = name

    def get_server(self):
        """
        Return a server object with :meth:`serve_forever` and address attribute.
        """
        assert self._state.value == State.INITIAL
        return OpenMDAO_Server(self._registry, self._address,
                               self._authkey, self._serializer, self._name)

    def start(self):
        """
        Spawn a server process for this manager object.
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
        self._process = Process(
            target=type(self)._run_server,
            args=(registry, self._address, self._authkey,
                  self._serializer, self._name, writer, get_credentials()),
            )
        ident = ':'.join(str(i) for i in self._process._identity)
        self._process.name = type(self).__name__  + '-' + ident
        self._process.start()

        # Get address of server.
        writer.close()
        reply = reader.recv()
        if isinstance(reply, Exception):
            raise RuntimeError('Server process startup failed: %s' % exc)
        self._address = reply
        if self._authkey == 'PublicKey':
            self._pubkey = reader.recv()
        reader.close()

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
    def _run_server(cls, registry, address, authkey, serializer, name, writer,
                    credentials): #pragma no cover
        """
        Create a server, report its address and public key, and run it.
        """
        if sys.platform == 'win32':  #pragma no cover
            set_credentials(credentials)
            # Recreate registry proxytypes.
            for typeid, info in registry.items():
                callable, exposed, method_to_typeid, proxytype = info
                if proxytype == 'rebuild':
                    registry[typeid] = (callable, exposed, method_to_typeid,
                                        _make_proxy_type(typeid, exposed))
        try:
            # Create server.
            server = cls._Server(registry, address, authkey, serializer, name)
        except Exception as exc:
            writer.send(exc)
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
            util.info('sending shutdown message to manager')
            try:
                conn = _Client(address, authkey=authkey)
                try:
                    dispatch(conn, None, 'shutdown')
                finally:
                    conn.close()
            except Exception:
                pass

            process.join(timeout=2)
            # No good way to cause process to not shut down.
            if process.is_alive():  #pragma no cover
                util.info('manager still alive')
                if hasattr(process, 'terminate'):
                    util.info('trying to `terminate()` manager process')
                    process.terminate()
                    process.join(timeout=1)
                    if process.is_alive():
                        util.info('manager still alive after terminate')

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
    """

    def __init__(self, obj, address=None, serializer='pickle', authkey=None,
                 name=None):
        self._typeid = _make_typeid(obj)
        self._ident = '%x' % id(obj)
        self._manager = OpenMDAO_Manager(address=address, serializer=serializer,
                                         authkey=authkey, name=name)
        self._server = self._manager.get_server()
        self._exposed = _public_methods(obj)
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
                                      exposed=self._exposed, authkey=authkey,
                                      pubkey=pubkey)
        return self._proxy

    def _run_server(self):
        """ Run a manager server (in a separate thread!). """
        self._server.serve_forever()


class OpenMDAO_Proxy(BaseProxy):
    """ Sends credentials and provides dynamic result proxy generation. """

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

        credentials = get_credentials()
        if self._authkey == 'PublicKey':
            if credentials is None or not credentials.user:
                msg = 'No credentials for PublicKey authentication of %s' \
                      % methodname
                logging.error(msg)
                raise RuntimeError(msg)
        try:
            conn = self._tls.connection
        except AttributeError:
            curr_thread = threading.current_thread()
            util.debug('thread %r does not own a connection', curr_thread.name)
            self._connect()
            conn = self._tls.connection

            if self._authkey == 'PublicKey':
                # Send client public key, receive session key.
                key_pair = _generate_key_pair(credentials)
                public_key = key_pair.publickey()
                text = _encode_public_key(public_key)

                server_key = self._pubkey
                # Just being defensive, this should never happen.
                if not server_key:  #pragma no cover
                    msg = 'No server PublicKey for %s(%s, %s)' \
                          % (methodname, args, kwds)
                    logging.error(msg)
                    raise RuntimeError(msg)
                chunk_size = server_key.size() / 8
                chunks = []
                while text:
                    chunks.append(server_key.encrypt(text[:chunk_size], ''))
                    text = text[chunk_size:]
                conn.send(chunks)

                data = conn.recv()
                self._tls.session_key = key_pair.decrypt(data)
            else:
                self._tls.session_key = ''

        session_key = self._tls.session_key

# FIXME: Bizarre problem evidenced by test_extcode.py (Python 2.6.1)
# For some reason pickling the env_vars dictionary causes:
#    PicklingError: Can't pickle <class 'openmdao.main.mp_support.ObjServer'>:
#                     attribute lookup openmdao.main.mp_support.ObjServer failed
# Possibly some Trait feature?
        new_args = []
        for arg in args:
            if isinstance(arg, dict):
                new_args.append(dict(arg))
            else:
                new_args.append(arg)
# Cause error
#        new_args = args

        try:
            conn.send(_encrypt((self._id, methodname, new_args, kwds,
                                credentials), session_key))
        except cPickle.PicklingError as exc:
            print 'mp_suppport._callmethod: %s, %s' % (methodname, exc)
            print '    args:', len(new_args)
            for arg in new_args:
                print '        %r' % arg,
                try:
                    cPickle.dumps(arg)
                except cPickle.PicklingError as exc:
                    print '-- error', exc
                else:
                    print '-- ok'
            print '    kwds:', len(kwds)
            for key, val in kwds.items():
                print '        %s: %r' % (key, val),
                try:
                    cPickle.dumps(val)
                except cPickle.PicklingError as exc:
                    print '-- error', exc
                else:
                    print '-- ok'
            raise

        kind, result = _decrypt(conn.recv(), session_key)

        if kind == '#RETURN':
            return result
        elif kind == '#PROXY':
            exposed, token = result
            try:
                proxytype = self._manager._registry[token.typeid][-1]
            except KeyError:
                self._manager.register(token.typeid, None, _auto_proxy)
                proxytype = self._manager._registry[token.typeid][-1]
            proxy = proxytype(
                token, self._serializer, manager=self._manager,
                authkey=self._authkey, exposed=exposed
                )
            conn = self._Client(token.address, authkey=self._authkey)
            dispatch(conn, None, 'decref', (token.id,))
            return proxy
        raise convert_to_error(kind, result)

    def _incref(self):
        """
        This version avoids a hang in _Client if the server no longer exists.
        """
        if not OpenMDAO_Proxy.manager_is_alive(self._token.address):
            raise RuntimeError('Cannot connect to manager')

        conn = self._Client(self._token.address, authkey=self._authkey)
        dispatch(conn, None, 'incref', (self._id,))
        util.debug('INCREF %r', self._token.id)

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
                    conn = _Client(token.address, authkey=authkey)
                    dispatch(conn, None, 'decref', (token.id,))
                except Exception as exc:
                    util.debug('... decref failed %s', exc)
        else:
            util.debug('DECREF %r -- manager already shutdown', token.id)

        # check whether we can close this thread's connection because
        # the process owns no more references to objects for this manager
        if not idset and hasattr(tls, 'connection'):
            util.debug('thread %r has no more proxies so closing conn',
                       threading.current_thread().name)
            tls.connection.close()
            del tls.connection

    @staticmethod
    def manager_is_alive(address):
        """ Check whether manager is still alive. """
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

        if getattr(self, '_isauto', False):
            kwds['exposed'] = self._exposed_
            return (RebuildProxy,
                    (_auto_proxy, self._token, self._serializer, kwds))
        else:
            return (RebuildProxy,
                    (type(self), self._token, self._serializer, kwds))


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
    _Client = listener_client[serializer][1]

    if authkey is None and manager is not None:
        authkey = manager._authkey
    if authkey is None:
        authkey = current_process().authkey

    if exposed is None:
        conn = _Client(token.address, authkey=authkey)
        try:
            exposed = dispatch(conn, None, 'get_methods', (token,))
        finally:
            conn.close()

    ProxyType = _make_proxy_type('OpenMDAO_AutoProxy[%s]' % token.typeid,
                                exposed)
    proxy = ProxyType(token, serializer, manager=manager, authkey=authkey,
                      incref=incref, pubkey=pubkey)
    proxy._isauto = True
    return proxy

