import base64
import cPickle
import getpass
import logging
import os.path
import sys
import threading

from Crypto.PublicKey import RSA
from Crypto.Random import get_random_bytes

if sys.platform == 'win32':  #pragma no cover
    try:
        import win32api
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


def generate_key_pair(user_host, logger=None, overwrite_cache=False):
    """
    Returns RSA key containing both public and private keys for the user
    identified in `credentials`.  This can be an expensive operation, so
    we avoid generating a new key pair whenever possible.

    user_host: string
        Format ``user@host``.

    logger: :class:`logging.Logger`
        Used for debug messages.

    overwrite_cache: bool
        If True, a new key is generated and forced into the cache of existing
        known keys.  Used for testing.
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
                key_file = \
                    os.path.expanduser(os.path.join('~', '.openmdao', 'keys'))
                if _is_private(key_file):
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
                    # Save in protected file.
                    if sys.platform == 'win32' and not HAVE_PYWIN32: #pragma no cover
                        logger.debug('No pywin32, not saving keyfile')
                    else:
                        key_dir = os.path.dirname(key_file)
                        if not os.path.exists(key_dir):
                            os.mkdir(key_dir)
                        _make_private(key_dir)  # Private while writing keyfile.
                        with open(key_file, 'wb') as out:
                            cPickle.dump(key_pair, out,
                                         cPickle.HIGHEST_PROTOCOL)
                        try:
                            _make_private(key_file)
                        # Hard to cause (recoverable) error here.
                        except Exception:  #pragma no cover
                            os.remove(key_file)  # Remove unsecured file.
                            raise

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


def _is_private(path):
    """ Return True if `path` is accessible only by 'owner'. """
    if not os.path.exists(path):
        return True  # Nonexistent file is secure ;-)

    if sys.platform == 'win32':  #pragma no cover
        if not HAVE_PYWIN32:
            return False  # No way to know.

        # Find the SIDs for user and system.
        user, domain, type = \
            win32security.LookupAccountName('', win32api.GetUserName())
        system, domain, type = \
            win32security.LookupAccountName('', 'System')

        # Find the DACL part of the Security Descriptor for the file
        sd = win32security.GetFileSecurity(path,
                                        win32security.DACL_SECURITY_INFORMATION)
        dacl = sd.GetSecurityDescriptorDacl()

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


def authorized_keys(filename=None, logger=None):
    """
    Return dictionary of public keys, indexed by user, read from `filename`.
    The default is '~/.ssh/authorized_keys'.  The file must be in ssh form,
    and only RSA keys are processed.
    """
    if not filename:
        filename = \
            os.path.expanduser(os.path.join('~', '.ssh', 'authorized_keys'))

    logger = logger or NullLogger()

    keys = {}
    if not os.path.exists(filename):
        logger.debug('%r does not exist', filename)
        return keys

    with open(filename, 'r') as inp:
        for line in inp:
            line = line.rstrip()
            sharp = line.find('#')
            if sharp >= 0:
                line = line[:sharp]
            if not line:
                continue

            fields = line.split()
            if len(fields) != 3:
                logger.debug('bad line (require exactly 3 fields):')
                logger.debug(line)
                continue

            key_type, key_data, user = fields
            if key_type != 'ssh-rsa':
                logger.debug('unsupported key type: %r', key_type)
                continue

            logger.debug('user: %r', user)

            data = base64.b64decode(key_data)
            start = 0
            name_len = _longint(data, start, 4)
            start += 4
            name = data[start:start+name_len]
            if name != 'ssh-rsa':
                logger.debug('    name error: %r vs. ssh-rsa', name)
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
                logger.debug('    length error: %d vs. %d', start, len(data))
                continue

            try:
                pubkey = RSA.construct((n, e))
            except Exception as exc:
                logger.debug('    construct error: %r', exc)
            else:
                keys[user] = pubkey

    return keys

def _longint(buf, start, length):
    value = long(0)
    for i in range(length):
        value = (value << 8) + ord(buf[start])
        start += 1
    return value

