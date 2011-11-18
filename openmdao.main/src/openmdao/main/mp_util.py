"""
Multiprocessing support utilities.
"""

import atexit
import ConfigParser
import cPickle
import errno
import inspect
import logging
import os.path
import re
import socket
import sys
import time

from Crypto.Cipher import AES

from multiprocessing import current_process, connection
from multiprocessing.managers import BaseProxy

from openmdao.main.rbac import rbac_methods

from openmdao.util.publickey import decode_public_key
from openmdao.util.shellproc import ShellProc, STDOUT

# Names of attribute access methods requiring special handling.
SPECIALS = ('__getattribute__', '__getattr__', '__setattr__', '__delattr__')


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


# This happens on the remote server side and we'll check when connecting.
def write_server_config(server, filename, real_ip=None):  #pragma no cover
    """
    Write server connection information.

    server: OpenMDAO_Server
        Server to be recorded.

    filename: string
        Path to file to be written.

    real_ip: string
        If specified, the IP address to report (rather than possible tunnel)

    Connection information including IP address, port, and public key is
    written using :class:`ConfigParser`.
    """
    parser = ConfigParser.ConfigParser()
    section = 'ServerInfo'
    parser.add_section(section)
    if connection.address_type(server.address) == 'AF_INET':
        parser.set(section, 'address', str(real_ip or server.address[0]))
        parser.set(section, 'port', str(server.address[1]))
        tunnel = real_ip is not None and \
            socket.gethostbyname(real_ip) != server.address[0]
        parser.set(section, 'tunnel', str(tunnel))
    else:
        parser.set(section, 'address', server.address)
        parser.set(section, 'port', '-1')
        parser.set(section, 'tunnel', str(False))
    parser.set(section, 'key', server.public_key_text)
    with open(filename, 'w') as cfg:
        parser.write(cfg)

def read_server_config(filename):
    """
    Read a server's connection information.

    filename: string
        Path to file to be read.

    Returns ``(address, port, tunnel, key)``.
    """
    if not os.path.exists(filename):
        raise IOError('No such file %r' % filename)
    parser = ConfigParser.ConfigParser()
    parser.read(filename)
    section = 'ServerInfo'
    address = parser.get(section, 'address')
    port = parser.getint(section, 'port')
    tunnel = parser.getboolean(section, 'tunnel')
    key = parser.get(section, 'key')
    if key:
        key = decode_public_key(key)
    return (address, port, tunnel, key)


def setup_tunnel(address, port):
    """
    Setup tunnel to `address` and `port` assuming:

    - The remote login name matches the local login name.
    - `port` is available on the local host.
    - 'putty' is available on Windows, 'ssh' on other platforms.

    address: string
        IPv4 address to tunnel to.

    port: int
        Port at `address` to tunnel to.

    Returns ``(local_address, local_port)``.
    """
    logname = 'tunnel-%s-%d.log' % (address, port)
    logname = os.path.join(os.getcwd(), logname)
    stdout = open(logname, 'w')

    if sys.platform == 'win32':  # pragma no cover
        stdin = open('nul:', 'r')
        args = ['putty', '-L', '%d:localhost:%d' % (port, port), address]
    else:
        stdin = open('/dev/null', 'r')
        args = ['ssh', '-L', '%d:localhost:%d' % (port, port), address]

    tunnel_proc = ShellProc(args, stdin=stdin, stdout=stdout, stderr=STDOUT)
    sock = socket.socket(socket.AF_INET)
    address = ('127.0.0.1', port)
    for retry in range(20):
        time.sleep(.5)
        exitcode = tunnel_proc.poll()
        if exitcode is not None:
            msg = 'ssh tunnel process exited with exitcode %d,' \
                  ' output in %s' % (exitcode, logname)
            logging.error(msg)
            raise RuntimeError(msg)
        try:
            sock.connect(address)
        except socket.error as exc:
            if exc.args[0] != errno.ECONNREFUSED and \
               exc.args[0] != errno.ENOENT:
                raise
        else:
            atexit.register(_cleanup_tunnel, tunnel_proc, logname)
            sock.close()
            return address

    _cleanup_tunnel(tunnel_proc, logname)
    raise RuntimeError('Timeout trying to connect through tunnel to %s'
                       % address)

def _cleanup_tunnel(tunnel_proc, logname):
    """ Try to terminate `tunnel_proc` if it's still running. """
    if tunnel_proc.poll() is None:
        tunnel_proc.terminate(timeout=10)
    if os.path.exists(logname):
        os.remove(logname)


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


def is_legal_connection(address, allowed_hosts, logger):
    """
    Return True if `address` is from an allowed host.

    address: string
        Connection address to be checked.

    allowed_hosts: list(string)
        IPv4 address patterns to check against. If a pattern ends with '.'
        (a domain) then the host address must begin with the pattern.
        Otherwise the host address must completely match the pattern.

    logger: :class:`logging.Logger`
        Used to output warnings about rejected connections.
    """
    if address and connection.address_type(address) == 'AF_INET':
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
    """
    Return allowed hosts data read from `path`.

    path: string
        Path to allowed hosts file (typically 'hosts.allow').

    The file should contain IPv4 host addresses, IPv4 domain addresses,
    or hostnames, one per line. Blank lines are ignored, and '#' marks the
    start of a comment which continues to the end of the line.
    """
    ipv4_host = re.compile(r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$')
    ipv4_domain = re.compile(r'([0-9]+\.){1,3}$')

    count = 0
    allowed_hosts = []
    with open(path, 'r') as inp:
        for line in inp:
            count += 1
            sharp = line.find('#')
            if sharp >= 0:
                line = line[:sharp]
            line = line.strip()
            if not line:
                continue

            if ipv4_host.match(line):
                logging.debug('%s line %d: IPv4 host %r',
                              path, count, line)
                allowed_hosts.append(line)
            elif ipv4_domain.match(line):
                logging.debug('%s line %d: IPv4 domain %r',
                              path, count, line)
                allowed_hosts.append(line)
            else:
                try:
                    addr = socket.gethostbyname(line)
                except socket.gaierror:
                    logging.error('%s line %d: unrecognized host %r',
                                  path, count, line)
                else:
                    logging.debug('%s line %d: host %r at %r',
                                  path, count, line, addr)
                    allowed_hosts.append(addr)

    return allowed_hosts

