"""
Multiprocessing support utilities.
"""

import ConfigParser
import copy
import cPickle
import errno
import getpass
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
from openmdao.main.releaseinfo import __version__

from openmdao.util.publickey import decode_public_key, is_private, HAVE_PYWIN32
from openmdao.util.shellproc import ShellProc, STDOUT, PIPE

# Names of attribute access methods requiring special handling.
SPECIALS = ('__getattribute__', '__getattr__', '__setattr__', '__delattr__')


# Mapping from remote addresses to local tunnel addresses.
_TUNNEL_MAP = {}
# Log files that haven't been cleaned up yet due to Windows issue.
_TUNNEL_PENDING = []


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
    Write server configuration information.

    server: OpenMDAO_Server
        Server to be recorded.

    filename: string
        Path to file to be written.

    real_ip: string
        If specified, the IP address to report (rather than possible tunnel).

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
    logfile = os.path.join(os.getcwd(), 'openmdao_log.txt')
    parser.set(section, 'logfile', '%s:%s' % (socket.gethostname(), logfile))
    parser.set(section, 'version', __version__)

    with open(filename, 'w') as cfg:
        parser.write(cfg)

def read_server_config(filename):
    """
    Read a server's configuration information.

    filename: string
        Path to file to be read.

    Returns a dictionary containing 'address', 'port', 'tunnel', 'key', and
    'logfile' information.
    """
    if not os.path.exists(filename):
        raise IOError('No such file %r' % filename)
    parser = ConfigParser.ConfigParser()
    parser.read(filename)
    section = 'ServerInfo'
    cfg = {}
    cfg['address'] = parser.get(section, 'address')
    cfg['port'] = parser.getint(section, 'port')
    cfg['tunnel'] = parser.getboolean(section, 'tunnel')
    key = parser.get(section, 'key')
    if key:
        key = decode_public_key(key)
    cfg['key'] = key
    cfg['logfile'] = parser.get(section, 'logfile')
    cfg['version'] = parser.get(section, 'version')
    return cfg


def setup_tunnel(address, port, user=None, identity=None):
    """
    Setup tunnel to `address` and `port` assuming:

    - `port` is available on the local host.
    - 'plink' is available on Windows, 'ssh' on other platforms.
    - No user interaction is required to connect via 'plink'/'ssh'.

    address: string
        IPv4 address to tunnel to.

    port: int
        Port at `address` to tunnel to.

    user: string
        Remote username, if different than local name.
        Not needed if `address` is of the form ``user@host``.

    identity: string
        Path to optional identity file.

    Returns ``((local_address, local_port), (cleanup-info))``, where
    `cleanup-info` contains a cleanup function and its arguments.
    """
    # Try to grab an unused local port (ssh doesn't support allocation here).
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(('localhost', 0))
    local_port = sock.getsockname()[1]
    sock.close()

    args = ['-L', '%d:localhost:%d' % (local_port, port)]
    cleanup_info = None
    try:
        cleanup_info = _start_tunnel(address, port, args, user, identity,
                                     'ftunnel')
        tunnel_proc = cleanup_info[1]
        sock = socket.socket(socket.AF_INET)
        local_address = ('127.0.0.1', local_port)
        for retry in range(20):
            exitcode = tunnel_proc.poll()
            if exitcode is not None:
                raise RuntimeError('ssh tunnel %s:%s process exited with'
                                   ' exitcode %s' % (address, port, exitcode))
            try:
                sock.connect(local_address)
            except socket.error as exc:
                if exc.args[0] != errno.ECONNREFUSED and \
                   exc.args[0] != errno.ENOENT:
                    raise RuntimeError("Can't connect to ssh tunnel %s:%s: %s"
                                       % (address, port, exc))
                time.sleep(.5)
            else:
                sock.close()
                connected = True
                register_tunnel(('127.0.0.1', port), local_address)
                return (local_address, cleanup_info)
        raise RuntimeError('Timeout trying to connect through tunnel to %s:%s'
                           % (address, port))
    except Exception as exc:
        logging.error("Can't setup tunnel to %s:%s: %s", address, port, exc)
        if cleanup_info is not None:
            cleanup_info[0](*cleanup_info[1:], **dict(keep_log=True))
        raise

def setup_reverse_tunnel(remote_address, local_address, port, user=None,
                         identity=None):
    """
    Setup reverse tunnel to `local_address`:`port` from `remote_address`
    assuming:

    - 'plink' is available on Windows, 'ssh' on other platforms.
    - No user interaction is required to connect via 'plink'/'ssh'.

    remote_address: string
        IPv4 address connecting to tunnel.

    local_address: string
        IPv4 address of tunnel.

    port: int
        Local port.

    user: string
        Remote username, if different than local name.
        Not needed if `remote_address` is of the form ``user@host``.

    identity: string
        Path to optional identity file.

    Returns ``(('localhost', remote_port), (cleanup-info))`` where
    `cleanup-info` contains a cleanup function and its arguments.
    """
    if sys.platform != 'win32':  # Windows/plink doesn't report anything :-(
        args = ['-R', '%d:%s:%d' % (0, local_address, port)]
        try:
            cleanup_info = _start_tunnel(remote_address, port, args, user,
                                         identity, 'rtunnel')
        except Exception as exc:
            logging.error("Can't setup reverse tunnel from %s to %s:%s: %s",
                          remote_address, local_address, port, exc)
            raise

        # Look for the port allocated by ssh. Hopefully this is portable.
        with open(cleanup_info[3], 'r') as out:
            for line in out:
                if line.startswith('Allocated port'):
                    remote_port = int(line.split()[2])
                    logging.debug('Allocated remote port %s on %s',
                                  remote_port, remote_address)
                    return (('localhost', remote_port), cleanup_info)

        # Apparently nothing allocated. Retry with explicit port.
        logging.debug('No remote port allocated by ssh for %s', remote_address)
        cleanup_info[0](*cleanup_info[1:]) #, **dict(keep_log=True))

    remote_port = _unused_remote_port(remote_address, port, user, identity)
    args = ['-R', '%d:%s:%d' % (remote_port, local_address, port)]
    try:
        cleanup_info = _start_tunnel(remote_address, port, args, user, identity,
                                     'rtunnel2')
    except Exception:
        logging.error("Can't setup reverse tunnel from %s to %s:%s",
                      remote_address, local_address, port)
        raise

    return (('localhost', remote_port), cleanup_info)

def _unused_remote_port(address, port, user, identity):
    """ Return a (currently) unused port on `address`, default to `port`. """
    if '@' in address:
        user, host = address.split('@')
    else:
        user = user or getpass.getuser()
        host = address

    if sys.platform == 'win32':  # pragma no cover
        cmd = ['plink', '-batch', '-ssh']
    else:
        cmd = ['ssh']

    cmd += ['-l', user]
    if identity:
        cmd += ['-i', identity]
    cmd += ['-x', '-T']

# FIXME: this currently won't work for Windows if ssh doesn't connect to a
# UNIX-like shell (cygwin, etc.)
    code = '''"import socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.bind(('localhost', 0))
port = sock.getsockname()[1]
sock.close()
print 'port', port"'''

    cmd += [host, 'python', '-c', code.replace('\n', ';')]
    try:
        proc = ShellProc(cmd, stdout=PIPE, stderr=PIPE, universal_newlines=True)
    except Exception as exc:
        logging.warning("Can't get unused port on %s from %s (forcing %s): %s",
                        host, cmd, port, exc)
        return port

    output = proc.stdout.read()
    for line in output.split('\n'):
        if line.startswith('port'):
            remote_port = int(line.split()[1])
            logging.debug('Unused remote port %s on %s', remote_port, host)
            return remote_port
    else:
        logging.warning("Can't get unused port on %s from %s (forcing %s):\n"
                        "[stdout]\n%s\n[stderr]\n%s",
                        host, cmd, port, output, proc.stderr.read())
        return port

def _start_tunnel(address, port, args, user, identity, prefix):
    """ Start an ssh tunnel process. """
    if '@' in address:
        user, host = address.split('@')
    else:
        user = user or getpass.getuser()
        host = address

    if sys.platform == 'win32':  # pragma no cover
        cmd = ['plink', '-batch', '-ssh']
    else:
        cmd = ['ssh']

    cmd += ['-l', user]
    if identity:
        cmd += ['-i', identity]
    cmd += ['-N', '-x', '-T']  # plink doesn't support '-n' (no stdin)
    cmd += args + [host]

    logname = '%s-%s-%s.log' % (prefix, host, port)
    logname = os.path.join(os.getcwd(), logname)
    stdout = open(logname, 'w')

    tunnel_proc = None
    try:
        tunnel_proc = ShellProc(cmd, stdout=stdout, stderr=STDOUT)
    except Exception as exc:
        raise RuntimeError("Can't create ssh tunnel process from %s: %s"
                           % (cmd, exc))
    time.sleep(1)
    exitcode = tunnel_proc.poll()
    if exitcode is not None:
        raise RuntimeError('ssh tunnel process for %s:%s exited with exitcode'
                           ' %d, output in %s'
                           % (address, port, exitcode, logname))

    return (_cleanup_tunnel, tunnel_proc, stdout, logname, os.getpid())

def _cleanup_tunnel(tunnel_proc, stdout, logname, pid, keep_log=False):
    """ Try to terminate `tunnel_proc` if it's still running. """
    logging.debug('cleanup %s PID %s', os.path.basename(logname),
                  tunnel_proc.pid)

    if pid != os.getpid():
        return  # We're a forked process.
    if tunnel_proc.poll() is None:
        tunnel_proc.terminate(timeout=10)
    stdout.close()

    # Cleanup of log files is problematic on Windows. Reverse tunnel logs
    # are somehow 'in use by another process' (due to multiprocessing?).
    # It appears the last tunnel cleanup is able to actually remove the logs.
    if not keep_log and os.path.exists(logname):
        try:
            os.remove(logname)
        except WindowsError:
            _TUNNEL_PENDING.append(logname)

    for logname in copy.copy(_TUNNEL_PENDING):
        if os.path.exists(logname):
            try:
                os.remove(logname)
            except WindowsError:
                pass
            else:
                _TUNNEL_PENDING.remove(logname)
        else:
            _TUNNEL_PENDING.remove(logname)

def register_tunnel(remote, local):
    """ Register `local` as the address to use to access `remote`. """
    _TUNNEL_MAP[remote] = local

def tunnel_address(remote):
    """ Return address to use to access `remote`. """
    return _TUNNEL_MAP.get(remote, remote)


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
    if not os.path.exists(path):
        raise RuntimeError('%r does not exist' % path)

    if not is_private(path):
        if sys.platform != 'win32' or HAVE_PYWIN32:
            raise RuntimeError('%r is not private' % path)
        else:  #pragma no cover
            logging.warning('Allowed hosts file %r is not private', path)

    ipv4_host = re.compile(r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$')
    ipv4_domain = re.compile(r'([0-9]+\.){1,3}$')

    errors = 0
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
                logging.debug('%s line %d: IPv4 host %r', path, count, line)
                allowed_hosts.append(line)

            elif ipv4_domain.match(line):
                logging.debug('%s line %d: IPv4 domain %r', path, count, line)
                allowed_hosts.append(line)

            else:
                try:
                    addr = socket.gethostbyname(line)
                except socket.gaierror:
                    logging.error('%s line %d: unrecognized host %r',
                                  path, count, line)
                    errors += 1
                else:
                    logging.debug('%s line %d: host %r at %r',
                                  path, count, line, addr)
                    allowed_hosts.append(addr)
    if errors:
        raise RuntimeError('%d errors in %r, check log for details'
                           % (errors, path))
    return allowed_hosts

