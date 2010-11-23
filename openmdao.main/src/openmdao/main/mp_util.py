"""
Multiprocessing support utilities.
"""

import base64
import ConfigParser
import cPickle
import getpass
import hashlib
import inspect
import logging
import os.path
import socket
import sys
import threading

from Crypto.Cipher import AES
from Crypto.PublicKey import RSA
from Crypto.Util.randpool import RandomPool

from multiprocessing import current_process, connection
from multiprocessing.managers import BaseProxy, dispatch, listener_client

if sys.platform == 'win32':  #pragma no cover
    from _multiprocessing import win32
    try:
        import win32api
        import win32security
        import ntsecuritycon
    except ImportError:
        _HAVE_PYWIN32 = False
    else:
        _HAVE_PYWIN32 = True

from openmdao.main.interfaces import obj_has_interface
from openmdao.main.rbac import rbac_methods

# Names of attribute access methods requiring special handling.
SPECIALS = ('__getattribute__', '__getattr__', '__setattr__', '__delattr__')

# Cache of client key pairs indexed by user (from credentials).
_KEY_CACHE = {}
_KEY_CACHE_LOCK = threading.Lock()


def keytype(authkey):
    """
    Just returns a string showing the type of `authkey`.

    authkey: string
        Key to report type of.
    """
    if authkey is None:
        return '%s (inherited)' % keytype(current_process().authkey)
    else:
        return authkey if authkey == 'PublicKey' else 'AuthKey'


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


def generate_key_pair(credentials, logger=None):
    """
    Returns RSA key containing both public and private keys for the user
    identified in `credentials`.  This can be an expensive operation, so
    we avoid generating a new key pair whenever possible.

    credentials: :class:`rbac.Credentials`
        Credentials of user.

    logger: :class:`logging.Logger`
        Used for debug messages.
    """
    with _KEY_CACHE_LOCK:
        # Look in previously generated keys.
        try:
            key_pair = _KEY_CACHE[credentials.user]
        except KeyError:
            # If key for current user (typical), check filesystem.
# TODO: file lock to protect from separate processes.
            user, host = credentials.user.split('@')
            if user == getpass.getuser():
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
            # Difficult to run test as non-current user.
            else:  #pragma no cover
                current_user = False
                generate = True

            if generate:
                logger = logger or logging.getLogger()
                logger.debug('generating public key...')
                pool = RandomPool(2048, hash=_SHA1)
                pool.stir()
                key_pair = RSA.generate(2048, pool.get_bytes)
                logger.debug('    done')

                if current_user:
                    # Save in protected file.
                    if sys.platform == 'win32' and not _HAVE_PYWIN32: #pragma no cover
                        logger.debug('No pywin32, not saving keyfile')
                    else:
                        if not os.path.exists(key_dir):
                            os.mkdir(key_dir)
                        _make_private(key_dir)  # Private while writing keyfile.
                        with open(key_file, 'wb') as out:
                            cPickle.dump(key_pair, out)
                        try:
                            _make_private(key_file)
                        # Hard to cause (recoverable) error here.
                        except Exception:  #pragma no cover
                            os.remove(key_file)  # Remove unsecured file.
                            raise

            _KEY_CACHE[credentials.user] = key_pair

    return key_pair

def _make_private(path):
    """ Make `path` accessible only by 'owner'. """
    if sys.platform == 'win32':  #pragma no cover
        # Find the SIDs for user and system.
        user, domain, type = \
            win32security.LookupAccountName('', win32api.GetUserName())
        system, domain, type = \
            win32security.LookupAccountName('', 'System')

        # Find the DACL part of the Security Descriptor for the file
        sd = win32security.GetFileSecurity(path,
                                        win32security.DACL_SECURITY_INFORMATION)

        # Create a blank DACL and add the ACEs we want.
        dacl = win32security.ACL()
        dacl.AddAccessAllowedAce(win32security.ACL_REVISION,
                                 ntsecuritycon.FILE_ALL_ACCESS, user)
        dacl.AddAccessAllowedAce(win32security.ACL_REVISION, 
                                 ntsecuritycon.FILE_ALL_ACCESS, system)

        # Put our new DACL into the Security Descriptor and update the file
        # with the updated SD.
        sd.SetSecurityDescriptorDacl(1, dacl, 0)
        win32security.SetFileSecurity(path,
                                      win32security.DACL_SECURITY_INFORMATION,
                                      sd)
    else:
        os.chmod(path, 0700)  # Read/Write/Execute


def encode_public_key(key):
    """
    Return base64 text representation of public key `key`.

    key: public key
        Public part of key pair.
    """
    # Just being defensive, this should never happen.
    if key.has_private():  #pragma no cover
        key = key.publickey()
    return base64.b64encode(cPickle.dumps(key, cPickle.HIGHEST_PROTOCOL))

def decode_public_key(text):
    """
    Return public key from text representation.

    text: string
        base64 encoded key data.
    """
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
    if connection.address_type(server.address) == 'AF_INET':
        parser.set(section, 'address', str(server.address[0]))
        parser.set(section, 'port', str(server.address[1]))
    else:
        parser.set(section, 'address', server.address)
        parser.set(section, 'port', '-1')
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
        raise IOError('No such file %r' % filename)
    parser = ConfigParser.ConfigParser()
    parser.read(filename)
    section = 'ServerInfo'
    address = parser.get(section, 'address')
    port = parser.getint(section, 'port')
    key = parser.get(section, 'key')
    if key:
        key = decode_public_key(key)
    return (address, port, key)


def encrypt(obj, session_key):
    """
    If `session_key` is specified, returns ``(length, data)`` of encrypted,
    pickled, `obj`. Otherwise `obj` is returned.

    obj: object
        Object to be pickled and encrypted.

    session_key: string
        Key used for encryption. Should be at least 16 bytes long.
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

def decrypt(msg, session_key):
    """
    If `session_key` is specified, returns object from encrypted pickled data
    contained in `msg`. Otherwise `msg` is returned.

    msg: string
        Encrypted text of pickled object.

    session_key: string
        Key used for encryption. Should be at least 16 bytes long.
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


def public_methods(obj):
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
    methods.extend([name for name in SPECIALS if hasattr(obj, name)])

    # Add special __is_instance__ and __has_interface__ methods.
    methods.append('__is_instance__')
    methods.append('__has_interface__')
    return methods


def make_typeid(obj):
    """
    Returns a type ID string from `obj`'s module and class names
    by replacing '.' with '_'.
    """
    typeid = '%s.%s' % (obj.__class__.__module__, obj.__class__.__name__)
    return typeid.replace('.', '_')


def is_legal_connection(conn, allowed_hosts, logger):
    """
    Return True if `conn` is from an allowed host.

    conn: :class:`_multiprocessing.Connection`
        Connection to be checked.

    allowed_hosts: list(string)
        IPv4 address patterns to check against. If a pattern ends with '.'
        (a domain) then the host address must begin with the pattern.
        Otherwise the host address must completely match the pattern.

    logger: :class:`logging.Logger`
        Used to output warnings about rejected connections.
    """
    # This attempt at creating a socket object won't necessarily work.
    sock = socket.fromfd(conn.fileno(), socket.AF_INET, socket.SOCK_STREAM)
    try:
        address = sock.getpeername()
    except socket.error:
        # Presumably a pipe (AF_UNIX, AF_PIPE).
        return True

    if address and connection.address_type(address) == 'AF_INET':
        logger.debug('Checking connection from %r', address)
        logger.debug('    allowed_hosts %r', allowed_hosts)
        host_addr = address[0]
        for pattern in allowed_hosts:
            if pattern[-1] == '.':  # Any host in domain.
                if host_addr.startswith(pattern):
                    return True
            elif host_addr == pattern:
                return True
        logger.warning('Rejecting connection from %r', address)
        return False
    else:
        # Presumably a pipe (AF_UNIX, AF_PIPE).
        return True

def read_allowed_hosts(path):
    """ Return allowed hosts data read from `path`. """
    allowed_hosts = []
    return allowed_hosts

