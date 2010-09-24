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
role is allowed access. The current role is determined by a :class:`RoleMapper`
based on a :class:`Credentials` object received from the proxy.

Assuming the credentials check passes, the server will set it's credentials
to those specified by the :class:`RoleMapper` during the execution of the
method.
"""

import base64
import ConfigParser
import cPickle
import hashlib
import inspect
import os.path
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

from openmdao.main.api import Container
from openmdao.main.rbac import RoleError, RoleMapper, check_role, \
                               rbac_methods, need_proxy, \
                               get_credentials, set_credentials

# Cache of proxies created by make_proxy_type().
_PROXY_CACHE = {}

# Names of methods requiring special handling.
_SPECIALS = ('__getattribute__', '__getattr__', '__setattr__', '__delattr__')


class SHA1(object):
    """
    Just to get around a deprecation message when using the default
    :class:`RandomPool` hash.
    """

    digest_size = None

    def __init__(self):
        self.hash = hashlib.sha1()
        if SHA1.digest_size is None:
            SHA1.digest_size = self.hash.digest_size

    @staticmethod
    def new(bytes=None):
        obj = SHA1()
        if bytes:
            obj.update(bytes)
        return obj

    def digest(self):
        return self.hash.digest()

    def update(self, bytes):
        self.hash.update(bytes)


def _generate_key_pair():
    """ Returns RSA key containing both public and private keys. """
    pool = RandomPool(2048, hash=SHA1)
    pool.stir()
    return RSA.generate(2048, pool.get_bytes)

def _encode_public_key(key):
    """ Return text representation of public key `key`. """
    if key.has_private():
        key = key.publickey()
    return base64.b64encode(cPickle.dumps(key, cPickle.HIGHEST_PROTOCOL))

def _decode_public_key(text):
    """ Return public key from text representation. """
    return cPickle.loads(base64.b64decode(text))


def write_server_config(server, filename):
    """ Write `server`s connection information to `filename`. """
    parser = ConfigParser.ConfigParser()
    section = 'ServerInfo'
    parser.add_section(section)
    parser.set(section, 'address', str(server.address[0]))
    parser.set(section, 'port', str(server.address[1]))
    parser.set(section, 'key', server.public_key_text())
    with open(filename, 'w') as cfg:
        parser.write(cfg)

def read_server_config(filename):
    """ Read a server's connection information from `filename`. """
    if not os.path.exists(filename):
        raise IOError("No such file '%s'" % filename)
    parser = ConfigParser.ConfigParser()
    parser.read(filename)
    section = 'ServerInfo'
    address = parser.get(section, 'address')
    port = parser.getint(section, 'port')
    key = parser.get(section, 'key')
    if key:
        key = _decode_public_key(key)
    return (address, port, key)


def _encrypt(obj, session_key):
    """
    If `session_key` is specified, returns ``(length, bytes)`` of encrypted,
    pickled, `obj`. Otherwise `obj` is returned.
    """
    if session_key:
        if len(session_key) < 16:
            session_key += '!'*16
        session_key = session_key[:16]
        encryptor = AES.new(session_key, AES.MODE_CBC, '?'*AES.block_size)
        text = cPickle.dumps(obj, cPickle.HIGHEST_PROTOCOL)
        length = len(text)
        pad = length % AES.block_size
        if pad:
            pad = AES.block_size - pad
            text += '-'*pad
        bytes = encryptor.encrypt(text)
        return (length, bytes)
    else:
        return obj

def _decrypt(msg, session_key):
    """
    If `session_key` is specified, returns object from encrypted pickled data
    contained in `msg`. Otherwise `msg` is returned.
    """
    if session_key:
        if len(msg) != 2:
            raise RuntimeError('_decrypt: msg not encrypted?')
        if len(session_key) < 16:
            session_key += '!'*16
        session_key = session_key[:16]
        decryptor = AES.new(session_key, AES.MODE_CBC, '?'*AES.block_size)
        length, bytes = msg
        text = decryptor.decrypt(bytes)
        return cPickle.loads(text[:length])
    else:
        return msg


def public_methods(obj):
    """
    Returns a list of names of the methods of `obj` to be exposed.
    Supports attribute access in addition to decorated methods.
    """
    if isinstance(obj, BaseProxy):
        methods = []
        for name in dir(obj):
            if name[0] != '_':
                attr = getattr(obj, name)
                if inspect.ismethod(attr) or inspect.isfunction(attr):
                    methods.append(name)
    else:
        methods = rbac_methods(obj)

    # Add special methods for attribute access.
#    methods.extend([name for name in _SPECIALS if hasattr(obj, name)])
    return methods


def register(cls, manager):
    """
    Register class `cls` proxy info with `manager`.
    Not typically called by user code.
    """
    typeid = cls.__name__
    exposed = public_methods(cls)
    proxytype = make_proxy_type(typeid, exposed)
    manager.register(typeid, callable=cls, exposed=exposed, proxytype=proxytype)


def dump_registry(title, registry, logger):
    logger.debug('%s:', title)
    for key, (callable, exposed, meth2type, proxytype) in registry.items():
        logger.debug('    %s:', key)
        logger.debug('        callable: %s', callable)
        logger.debug('        exposed: %s', exposed)
        logger.debug('        meth2type: %s', meth2type)
        logger.debug('        proxytype: %s', proxytype)


class OpenMDAO_Server(Server):
    """ Supports dynamic proxy generation and credential checking. """

    def __init__(self, registry, address, authkey, serializer):
        super(OpenMDAO_Server, self).__init__(registry, address, authkey,
                                              serializer)
        self._authkey = authkey  # Before turning into an AuthenticationString.
        if authkey == 'PublicKey':
            self._key_pair = _generate_key_pair()
        else:
            self._key_pair = None
        self._id_to_mapper = {}
        self._role_mapper = RoleMapper()
        self._role_mapper.class_proxy_required(Container)

    @property
    def public_key(self):
        """ Public key for session establishment. """
        if self._authkey == 'PublicKey':
            return self._key_pair.publickey()
        else:
            raise RuntimeError('No public key available')

    def public_key_text(self):
        """ Return text representation of public key. """
        if self._authkey == 'PublicKey':
            return _encode_public_key(self.public_key)
        else:
            return ''

    def serve_client(self, conn):
        """
        Handle requests from the proxies in a particular process/thread
        Supports dynamic proxy generation and credential checking.
        """
        util.debug('starting server thread to service %r',
                   threading.current_thread().name)

        recv = conn.recv
        send = conn.send
        id_to_obj = self.id_to_obj
        id_to_mapper = self._id_to_mapper

        if self._authkey == 'PublicKey':
            # Receive client public key, send session key.
            chunks = recv()
            text = ''
            for chunk in chunks:
                text += self._key_pair.decrypt(chunk)
            client_key = _decode_public_key(text)
            session_key = hashlib.sha1(str(id(conn))).hexdigest()
            bytes = client_key.encrypt(session_key, '')
            send(bytes)
        else:
            session_key = ''

        while not self.stop:

            try:
                methodname = obj = None
                request = _decrypt(recv(), session_key)
                ident, methodname, args, kwds, credentials = request
                obj, exposed, gettypeid = id_to_obj[ident]

                if methodname not in exposed:
                    raise AttributeError(
                        'method %r of %r object is not in exposed=%r' %
                        (methodname, type(obj), exposed)
                        )

                function = getattr(obj, methodname)

                if isinstance(obj, BaseProxy):
                    role_mapper = None
                    # Just propagate credentials.
                    set_credentials(credentials)
                else:
                    # Get role mapper for obj.
                    role_mapper = id_to_mapper.get(ident)
                    if role_mapper is None:
                        try:
                            get_role_mapper = getattr(obj, 'get_role_mapper')
                        except AttributeError:
                            role_mapper = self._role_mapper
                        else:
                            role_mapper = get_role_mapper()
                        id_to_mapper[ident] = role_mapper

                    # Get role based on credentials.
                    role = role_mapper.get_role(credentials)

                    if methodname in _SPECIALS:
                        # Check for valid access based on role.
                        role_mapper.check_access(role, methodname, obj, args[0])

                        # Just propagate credentials.
                        set_credentials(credentials)
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
                        set_credentials(role_mapper.get_proxy_credentials(
                                                                   function,
                                                                   credentials))

                    print 'invoke', methodname, args, kwds, \
                                    role, get_credentials()
                    sys.stdout.flush()
                try:
                    res = function(*args, **kwds)
                except Exception as exc:
                    msg = ('#ERROR', exc)
                else:
                    typeid = None

                    if isinstance(res, BaseProxy):
                        # Create proxy for proxy.
                        # (res may be unreachable by our client)
                        # TODO: avoid extra proxy if address is compatible
                        #       with our own.
                        typeid = res._token.typeid
                        proxyid = res.__class__.__name__
                        util.debug('Creating proxy for proxy %s', proxyid)
                        if proxyid not in self.registry:
                            self.registry[proxyid] = \
                                (None, None, None, auto_proxy)
                    elif role_mapper is not None:
                        if methodname in _SPECIALS:
                            if role_mapper.need_proxy(obj, args[0], res):
                                # Create proxy if in declared proxy types.
                                typeid = res.__class__.__name__
                                proxyid = typeid
                                if typeid not in self.registry:
                                    self.registry[typeid] = (None, None, None, None)
                        elif need_proxy(function, res):
                            # Create proxy if in declared proxy types.
                            typeid = res.__class__.__name__
                            proxyid = typeid
                            if typeid not in self.registry:
                                self.registry[typeid] = (None, None, None, None)
                    else:
                        # Create proxy if registered.
                        typeid = gettypeid and gettypeid.get(methodname, None)
                        proxyid = typeid

                    if typeid:
                        rident, rexposed = self.create(conn, proxyid, res)
                        token = Token(typeid, self.address, rident)
                        util.debug('Returning proxy for %s at %s',
                                   typeid, self.address)
                        msg = ('#PROXY', (rexposed, token))
                    else:
                        util.debug("Returning '%s'", res)
                        msg = ('#RETURN', res)

            except AttributeError:
                if methodname is None:
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
                util.debug('got EOF -- exiting thread serving %r',
                           threading.current_thread().name)
                sys.exit(0)

            except Exception:
                msg = ('#TRACEBACK', format_exc())

            try:
                try:
                    send(_encrypt(msg, session_key))
                except Exception:
                    send(_encrypt(('#UNSERIALIZABLE', repr(msg)), session_key))
            except Exception as exc:
                util.info('exception in thread serving %r',
                          threading.current_thread().name)
                util.info(' ... message was %r', msg)
                util.info(' ... exception was %r', exc)
                conn.close()
                sys.exit(1)

    def create(self, c, typeid, *args, **kwds):
        """
        Create a new shared object and return its id.
        Uses :func:`public_methods`.
        """
        self.mutex.acquire()
        try:
            callable, exposed, method_to_typeid, proxytype = \
                      self.registry[typeid]

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
            self.incref(c, ident)
            return ident, tuple(exposed)
        finally:
            self.mutex.release()


class OpenMDAO_Manager(BaseManager):
    """
    Uses :class:`OpenMDAO_Server`, retains the public key, and puts some slack
    in shutdown timing.
    """

    _Server = OpenMDAO_Server

    def __init__(self, address=None, authkey=None, serializer='pickle',
                 pubkey=None):
        super(OpenMDAO_Manager, self).__init__(address, authkey, serializer)
        self._pubkey = pubkey

    def get_server(self):
        """
        Return server object with serve_forever() method and address attribute
        """
        assert self._state.value == State.INITIAL
        return OpenMDAO_Server(self._registry, self._address,
                               self._authkey, self._serializer)

    def start(self):
        """
        Spawn a server process for this manager object
        Retrieves server's public key.
        """
        assert self._state.value == State.INITIAL

        # pipe over which we will retrieve address of server
        reader, writer = connection.Pipe(duplex=False)

        # spawn process which runs a server
        self._process = Process(
            target=type(self)._run_server,
            args=(self._registry, self._address, self._authkey,
                  self._serializer, writer),
            )
        ident = ':'.join(str(i) for i in self._process._identity)
        self._process.name = type(self).__name__  + '-' + ident
        self._process.start()

        # get address of server
        writer.close()
        self._address = reader.recv()
        if self._authkey == 'PublicKey':
            self._pubkey = reader.recv()
        reader.close()

        # register a finalizer
        self._state.value = State.STARTED
        self.shutdown = util.Finalize(
            self, type(self)._finalize_manager,
            args=(self._process, self._address, self._authkey,
                  self._state, self._Client),
            exitpriority=0
            )

    @classmethod
    def _run_server(cls, registry, address, authkey, serializer, writer):
        """
        Create a server, report its address and public key, and run it.
        """
        # create server
        server = cls._Server(registry, address, authkey, serializer)

        # inform parent process of the server's address
        writer.send(server.address)
        if authkey == 'PublicKey':
            writer.send(server.public_key)
        writer.close()

        # run the manager
        util.info('manager serving at %r', server.address)
        server.serve_forever()

    @staticmethod
    def _finalize_manager(process, address, authkey, state, _Client):
        """
        Shutdown the manager process; will be registered as a finalizer
        Uses relaxed timing.
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
            if process.is_alive():
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
        except KeyError:
            pass


class OpenMDAO_Proxy(BaseProxy):
    """ Sends credentials and provides dynamic result proxy generation. """

    def _callmethod(self, methodname, args=None, kwds=None):
        """
        Try to call a method of the referrent and return a copy of the result
        Sends current thread's credentials.
        """
        args = args or ()
        kwds = kwds or {}
        try:
            conn = self._tls.connection
        except AttributeError:
            curr_thread = threading.current_thread()
            util.debug('thread %r does not own a connection', curr_thread.name)
            self._connect()
            conn = self._tls.connection

            if self._authkey == 'PublicKey':
                # Send client public key, receive session key.
                server_key = self._manager._pubkey
                chunk_size = server_key.size() / 8
                key_pair = _generate_key_pair()
                public_key = key_pair.publickey()
                text = _encode_public_key(public_key)
                chunks = []
                while text:
                    chunks.append(server_key.encrypt(text[:chunk_size], ''))
                    text = text[chunk_size:]
                conn.send(chunks)
                if not hasattr(curr_thread, 'session_keys'):
                    curr_thread.session_keys = {}
                bytes = conn.recv()
                session_key = key_pair.decrypt(bytes)
                curr_thread.session_keys[id(conn)] = session_key

        if self._authkey == 'PublicKey':
            session_key = threading.current_thread().session_keys[id(conn)]
        else:
            session_key = ''

        credentials = get_credentials()

        # Incredibly bizarre problem evidenced by test_extcode.py (Python 2.6.1)
        # For some reason pickling the env_vars dictionary caused:
        #    PicklingError: Can't pickle <class 'openmdao.main.mp_support.ObjServer'>: attribute lookup openmdao.main.mp_support.ObjServer failed
        new_args = []
        for arg in args:
            if isinstance(arg, dict):
                new_args.append(dict(arg))
            else:
                new_args.append(arg)
# Cause error
#        new_args = args

        try:
            conn.send(_encrypt((self._id, methodname, new_args, kwds, credentials),
                               session_key))
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
                self._manager.register(token.typeid, None, auto_proxy)
                proxytype = self._manager._registry[token.typeid][-1]
            proxy = proxytype(
                token, self._serializer, manager=self._manager,
                authkey=self._authkey, exposed=exposed
                )
            conn = self._Client(token.address, authkey=self._authkey)
            dispatch(conn, None, 'decref', (token.id,))
            return proxy
        raise convert_to_error(kind, result)

    def __reduce__(self):
        """ For unpickling, uses :func:`auto_proxy`. """
        kwds = {}
        if Popen.thread_is_spawning():
            kwds['authkey'] = self._authkey

        if getattr(self, '_isauto', False):
            kwds['exposed'] = self._exposed_
            return (RebuildProxy,
                    (auto_proxy, self._token, self._serializer, kwds))
        else:
            return (RebuildProxy,
                    (type(self), self._token, self._serializer, kwds))


def make_proxy_type(name, exposed):
    """
    Return a proxy type whose methods are given by `exposed`.
    Supports special attribute access methods.
    """
    exposed = tuple(exposed)
    try:
        return _PROXY_CACHE[(name, exposed)]
    except KeyError:
        pass

    dic = {}

    for meth in exposed:
        # Special handling for specials...
        if meth == '__getattr__':
            pass  # Avoid recursion loop, but must be in 'exposed'.

        elif meth == '__getattribute__':
            # Call remote if not private or defined locally (proxied methods).
            exec """
def __getattr__(self, *args, **kwds):
    if args[0][0] == '_':
        return object.%s(self, *args, **kwds)
    try:
        return object.%s(self, *args, **kwds)
    except AttributeError:
        try:
            return self._callmethod(%r, args, kwds)
        except AttributeError:
            return self._callmethod('__getattr__', args, kwds)
""" % (meth, meth, meth) in dic

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


def auto_proxy(token, serializer, manager=None, authkey=None,
               exposed=None, incref=True):
    """
    Return an auto-proxy for `token`
    Uses :func:`make_auto_proxy`.
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

    ProxyType = make_proxy_type('OpenMDAO_AutoProxy[%s]' % token.typeid,
                                exposed)
    proxy = ProxyType(token, serializer, manager=manager, authkey=authkey,
                      incref=incref)
    proxy._isauto = True
    return proxy

