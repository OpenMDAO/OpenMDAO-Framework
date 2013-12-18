"""
Support for generation, use, and storage of public/private key pairs.
The :func:`pk_encrypt`, :func:`pk_decrypt`, :func:`pk_sign`, and
:func:`pk_verify` functions provide a thin interface over
:class:`Crypto.PublicKey.RSA` methods for easier use and to work around some
issues found with some keys read from ssh ``id_rsa`` files.
"""

import base64
import cPickle
import getpass
import logging
import os.path
import socket
import sys
import threading

from Crypto.PublicKey import RSA
from Crypto.Random import get_random_bytes
from Crypto.Util.number import bytes_to_long

if sys.platform == 'win32':  #pragma no cover
    try:
        import win32api
        import win32con
        import win32security
        import ntsecuritycon
    except ImportError:
        HAVE_PYWIN32 = False
    else:
        HAVE_PYWIN32 = True
else:
    HAVE_PYWIN32 = False

from openmdao.util.log import NullLogger

# Cache of client key pairs indexed by user.
_KEY_CACHE = {}
_KEY_CACHE_LOCK = threading.Lock()


def get_key_pair(user_host, logger=None,
                 overwrite_cache=False, ignore_ssh=False):
    """
    Returns RSA key containing both public and private keys for the user
    identified in `user_host`.  This can be an expensive operation, so
    we avoid generating a new key pair whenever possible.
    If ``~/.ssh/id_rsa`` exists and is private, that key is returned.

    user_host: string
        Format ``user@host``.

    logger: :class:`logging.Logger`
        Used for debug messages.

    overwrite_cache: bool
        If True, a new key is generated and forced into the cache of existing
        known keys.  Used for testing.

    ignore_ssh: bool
        If True, ignore any existing ssh id_rsa key file.  Used for testing.

    .. note::

        To avoid unnecessary key generation, the public/private key pair for
        the current user is stored in the private file ``~/.openmdao/keys``.
        On Windows this requires the pywin32 extension.  Also, the public
        key is stored in ssh form in ``~/.openmdao/id_rsa.pub``.

    """
    logger = logger or NullLogger()

    with _KEY_CACHE_LOCK:
        if overwrite_cache:
            key_pair = _generate(user_host, logger)
            _KEY_CACHE[user_host] = key_pair
            return key_pair

        # Look in previously generated keys.
        try:
            key_pair = _KEY_CACHE[user_host]
        except KeyError:
            # If key for current user (typical), check filesystem.
# TODO: file lock to protect from separate processes.
            user, host = user_host.split('@')
            if user == getpass.getuser():
                current_user = True
                key_pair = None

                # Try to re-use SSH key. Exceptions should *never* be exercised!
                if not ignore_ssh:
                    id_rsa = \
                        os.path.expanduser(os.path.join('~', '.ssh', 'id_rsa'))
                    if is_private(id_rsa):
                        try:
                            with open(id_rsa, 'r') as inp:
                                key_pair = RSA.importKey(inp.read())
                        except Exception as exc:  #pragma no cover
                            logger.warning('ssh id_rsa import: %r', exc)
                        else:
                            generate = False
                    else:  #pragma no cover
                        logger.warning('Ignoring insecure ssh id_rsa.')

                if key_pair is None:
                    # Look for OpenMDAO key.
                    key_file = \
                        os.path.expanduser(os.path.join('~', '.openmdao', 'keys'))
                    if is_private(key_file):
                        try:
                            with open(key_file, 'rb') as inp:
                                key_pair = cPickle.load(inp)
                        except Exception:
                            generate = True
                        else:
                            generate = False
                    else:
                        logger.warning('Insecure keyfile! Regenerating keys.')
                        os.remove(key_file)
                        generate = True

            # Difficult to run test as non-current user.
            else:  #pragma no cover
                current_user = False
                generate = True

            if generate:
                key_pair = _generate(user_host, logger)
                if current_user:
                    key_dir = os.path.dirname(key_file)
                    if not os.path.exists(key_dir):
                        os.mkdir(key_dir)

                    # Save key pair in protected file.
                    if sys.platform == 'win32' and not HAVE_PYWIN32: #pragma no cover
                        logger.debug('No pywin32, not saving keyfile')
                    else:
                        make_private(key_dir)  # Private while writing keyfile.
                        with open(key_file, 'wb') as out:
                            cPickle.dump(key_pair, out,
                                         cPickle.HIGHEST_PROTOCOL)
                        try:
                            make_private(key_file)
                        # Hard to cause (recoverable) error here.
                        except Exception:  #pragma no cover
                            os.remove(key_file)  # Remove unsecured file.
                            raise

                    # Save public key in ssh form.
                    users = {user_host: key_pair.publickey()}
                    filename = os.path.join(key_dir, 'id_rsa.pub')
                    write_authorized_keys(users, filename, logger)

            _KEY_CACHE[user_host] = key_pair

    return key_pair

def _generate(user_host, logger):
    """ Return new key. """
    logger.debug('generating public key for %r...', user_host)
    if sys.platform == 'win32' and not HAVE_PYWIN32: #pragma no cover
        strength = 1024  # Much quicker to generate.
    else:
        strength = 2048
    key_pair = RSA.generate(strength, get_random_bytes)
    logger.debug('    done')
    return key_pair


def pk_encrypt(data, public_key):
    """
    Return list of chunks of `data` encrypted by `public_key`.

    data: string
        The message to be encrypted.

    public_key: :class:`Crypto.PublicKey.RSA`
        Public portion of key pair.
    """
    # Normally we would use 8 rather than 16 here, but for some reason at least
    # some keys read from ssh id_rsa files don't work correctly with 8.
    chunk_size = public_key.size() / 16
    chunks = []
    while data:
        chunks.append(public_key.encrypt(data[:chunk_size], ''))
        data = data[chunk_size:]
    return chunks

def pk_decrypt(encrypted, private_key):
    """
    Return `encrypted` decrypted by `private_key` as a string.

    encrypted: list
        Chunks of encrypted data returned by :func:`pk_encrypt`.

    private_key: :class:`Crypto.PublicKey.RSA`
        Private portion of key pair.
    """
    data = ''
    for chunk in encrypted:
        data += private_key.decrypt(chunk)
    return data


def pk_sign(hashed, private_key):
    """
    Return signature for `hashed` using `private_key`.

    hashed: string
        A hash value of the data to be signed.

    private_key: :class:`Crypto.PublicKey.RSA`
        Private portion of key pair.
    """
    # Normally we would just do:
    #    return private_key.sign(hashed, '')
    # But that fails for at least some keys from ssh id_rsa files.
    # Instead, use the 'slowmath' method:
    c = bytes_to_long(hashed)
    m = pow(c, private_key.d, private_key.n)
    return (m,)

def pk_verify(hashed, signature, public_key):
    """
    Verify `hashed` based on `signature` and `public_key`.

    hashed: string
        A hash for the data that is signed.

    signature: tuple
        Value returned by :func:`pk_sign`.

    public_key: :class:`Crypto.PublicKey.RSA`
        Public portion of key pair.
    """
    return public_key.verify(hashed, signature)


def is_private(path):
    """
    Return True if `path` is accessible only by 'owner'.

    path: string
        Path to file or directory to check.

    .. note::

        On Windows this requires the pywin32 extension.

    """
    if not os.path.exists(path):
        return True  # Nonexistent file is secure ;-)

    if sys.platform == 'win32':  #pragma no cover
        if not HAVE_PYWIN32:
            return False  # No way to know.

        # Find the SIDs for user and system.
        username = win32api.GetUserNameEx(win32con.NameSamCompatible)

        # Map Cygwin 'root' to 'Administrator'. Typically these are intended
        # to be identical, but /etc/passwd might configure them differently.
        if username.endswith('\\root'):
            username = username.replace('\\root', '\\Administrator')
        user, domain, type = win32security.LookupAccountName('', username)
        system, domain, type = win32security.LookupAccountName('', 'System')

        # Find the DACL part of the Security Descriptor for the file
        sd = win32security.GetFileSecurity(path,
                                        win32security.DACL_SECURITY_INFORMATION)
        dacl = sd.GetSecurityDescriptorDacl()
        if dacl is None:
            logging.warning('is_private: No DACL for %r', path)
            return False  # Happened on a user's XP system.

        # Verify the DACL contains just the two entries we expect.
        count = dacl.GetAceCount()
        if count != 2:
            return False
        for i in range(count):
            ace = dacl.GetAce(i)
            if ace[2] != user and ace[2] != system:
                return False
        return True
    else:
        return (os.stat(path).st_mode & 0077) == 0


def make_private(path):
    """
    Make `path` accessible only by 'owner'.

    path: string
        Path to file or directory to be made private.

    .. note::

        On Windows this requires the pywin32 extension.

    """
    if sys.platform == 'win32':  #pragma no cover
        if not HAVE_PYWIN32:
            raise ImportError('No pywin32')

        # Find the SIDs for user and system.
        username = win32api.GetUserNameEx(win32con.NameSamCompatible)

        # Map Cygwin 'root' to 'Administrator'. Typically these are intended
        # to be identical, but /etc/passwd might configure them differently.
        if username.endswith('\\root'):
            username = username.replace('\\root', '\\Administrator')
        user, domain, type = win32security.LookupAccountName('', username)
        system, domain, type = win32security.LookupAccountName('', 'System')

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
        # Normal chmod() works on test machines with ACLs enabled, but a user
        # in the field reported a situation where it didn't. This code tries
        # using libacl if it can. Doesn't seem to cause any problems, not
        # verifed that it helps though.
        try:
            # From pylibacl, which requires 'libacl1-dev'.
            import posix1e
        except ImportError:
            mode = 0700 if os.path.isdir(path) else 0600
            os.chmod(path, mode)  # Read/Write/Execute
        else:
            if os.path.isdir(path):
                acl = posix1e.ACL(text='u::rwx,g::-,o::-')
            else:
                acl = posix1e.ACL(text='u::rw,g::-,o::-')
            acl.applyto(path)

    if not is_private(path):
        raise RuntimeError("Can't make %r private" % path)


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


def read_authorized_keys(filename=None, logger=None):
    """
    Return dictionary of public keys, indexed by user, read from `filename`.
    The file must be in ssh format, and only RSA keys are processed.
    If the file is not private, then no keys are returned.

    filename: string
        File to read from. The default is ``~/.ssh/authorized_keys``.

    logger: :class:`logging.Logger`
        Used for log messages.
    """
    if not filename:
        filename = \
            os.path.expanduser(os.path.join('~', '.ssh', 'authorized_keys'))

    logger = logger or NullLogger()

    if not os.path.exists(filename):
        raise RuntimeError('%r does not exist' % filename)

    if not is_private(filename):
        if sys.platform != 'win32' or HAVE_PYWIN32:
            raise RuntimeError('%r is not private' % filename)
        else:  #pragma no cover
            logger.warning('Allowed users file %r is not private', filename)

    errors = 0
    keys = {}
    with open(filename, 'r') as inp:
        for line in inp:
            line = line.rstrip()
            sharp = line.find('#')
            if sharp >= 0:
                line = line[:sharp]
            if not line:
                continue

            key_type, blank, rest = line.partition(' ')
            if key_type != 'ssh-rsa':
                logger.error('unsupported key type: %r', key_type)
                errors += 1
                continue

            key_data, blank, user_host = rest.partition(' ')
            if not key_data:
                logger.error('bad line (missing key data):')
                logger.error(line)
                errors += 1
                continue

            try:
                user, host = user_host.split('@')
            except ValueError:
                logger.error('bad line (require user@host):')
                logger.error(line)
                errors += 1
                continue

            logger.debug('user %r, host %r', user, host)
            try:
                ip_addr = socket.gethostbyname(host)
            except socket.gaierror:
                logger.warning('unknown host %r', host)
                logger.warning(line)

            data = base64.b64decode(key_data)
            start = 0
            name_len = _longint(data, start, 4)
            start += 4
            name = data[start:start+name_len]
            if name != 'ssh-rsa':
                logger.error('name error: %r vs. ssh-rsa', name)
                logger.error(line)
                errors += 1
                continue

            start += name_len
            e_len = _longint(data, start, 4)
            start += 4
            e = _longint(data, start, e_len)
            start += e_len
            n_len = _longint(data, start, 4)
            start += 4
            n = _longint(data, start, n_len)
            start += n_len
            if start != len(data):
                logger.error('length error: %d vs. %d', start, len(data))
                logger.error(line)
                errors += 1
                continue

            try:
                pubkey = RSA.construct((n, e))
            except Exception as exc:
                logger.error('key construct error: %r', exc)
                errors += 1
            else:
                keys[user_host] = pubkey

    if errors:
        raise RuntimeError('%d errors in %r, check log for details'
                           % (errors, filename))
    return keys

def _longint(buf, start, length):
    """ Return long value from binary string. """
    value = long(0)
    for i in range(length):
        value = (value << 8) + ord(buf[start])
        start += 1
    return value


def write_authorized_keys(allowed_users, filename, logger=None):
    """
    Write `allowed_users` to `filename` in ssh format.
    The file will be made private if supported on this platform.

    allowed_users: dict
        Dictionary of public keys indexed by user.

    filename: string
        File to write to.

    logger: :class:`logging.Logger`
        Used for log messages.
    """
    logger = logger or NullLogger()

    with open(filename, 'w') as out:
        for user in sorted(allowed_users.keys()):
            pubkey = allowed_users[user]
            buf = 'ssh-rsa'
            key_data  = _longstr(len(buf), 4)
            key_data += buf
            buf = _longstr(pubkey.e)
            key_data += _longstr(len(buf), 4)
            key_data += buf
            buf = _longstr(pubkey.n)
            key_data += _longstr(len(buf), 4)
            key_data += buf
            data = base64.b64encode(key_data)
            out.write('ssh-rsa %s %s\n\n' % (data, user))

    if sys.platform == 'win32' and not HAVE_PYWIN32: #pragma no cover
        logger.warning("Can't make authorized keys file %r private", filename)
    else:
        make_private(filename)

def _longstr(num, length=0):
    """ Return binary string representation of `num`. """
    buf = chr(num & 0xff)
    num >>= 8
    while num:
        buf = chr(num & 0xff) + buf
        num >>= 8
    while len(buf) < length:
        buf = chr(0) + buf
    return buf

