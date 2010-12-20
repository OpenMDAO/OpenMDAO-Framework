"""
Test mp_util.py
"""

import logging
import os.path
import getpass
import socket
import sys
import unittest
import nose

from openmdao.util.publickey import generate_key_pair, _KEY_CACHE, \
                                    _is_private, HAVE_PYWIN32, \
                                    read_authorized_keys, write_authorized_keys

class TestCase(unittest.TestCase):
    """ Test mp_util.py """

    def test_keyfile(self):
        logging.debug('')
        logging.debug('test_keyfile')

        # Force a key generation.
        key_file = os.path.expanduser(os.path.join('~', '.openmdao', 'keys'))
        if os.path.exists(key_file):
            os.remove(key_file)
        user = '%s@%s' % (getpass.getuser(), socket.gethostname())
        if user in _KEY_CACHE:
            del _KEY_CACHE[user]
        key_pair = generate_key_pair(user, logging.getLogger())

        # Again, this time with insecure key file.
        if sys.platform != 'win32':
            os.chmod(key_file, 0644)
            del _KEY_CACHE[user]
            key_pair = generate_key_pair(user, logging.getLogger())

        # Check privacy.
        if sys.platform != 'win32' or HAVE_PYWIN32:
            self.assertTrue(_is_private(key_file))
            if sys.platform == 'win32':
                public_file = os.environ['COMSPEC']
            else:
                public_file = '/bin/sh'
            self.assertFalse(_is_private(public_file))

    def test_authorized_keys(self):
        logging.debug('')
        logging.debug('test_authorized_keys')

        # Try various line formats.
        hostname = socket.gethostname()

        key_data = """
ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAt9gTm9qX3pKOvFbn8vdkWL/W4kAdtNxRQQXO6QXX7ihuYxv09ZMuqkFPCD1ZxwZNZG0BYstSytPyYQDAGbOglmsjfQ0PRtwDLvK4utGiGLuRsf8ig/cS8NDfSJ/I1B+DBlV1uMaGmzamsFDsavv4Qxf/X50Fl5JTBiPp9W17xkz+JyDCsNMaQd2iSx+GjLbxT/QG2xM9/qrF8bQAAMLdNoKHwVNW+lLXyww6YI9pPj7Tep/dg3xk5Ggf5L6eJGRzmJVMYSfFK+TIX4r49SNddo3Vy/K2H02Yxu6dIBXUTwa+AUC+Mfh5LisAJiM/Oj4NBngWVRgDjN9NH6nQD08R8Q== user1@%(host)s
ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAv7QM8MwxkX9yCKIebEH0o14b6Uik3KZnkQo2uF0NyuzDeeZntFym7v0mx7HV4KncjA5Ix2UBw4VKB2virDInO/YKYOC3ZqEJH/CvJkBFggPaZyJyzrEname0+NRXg+PnB2yIDKH0dpwEKVDkwAhEaAqcb9xoahEgXmd4kOmNGylJcwAJhSNqAC9BJO+gAdukGmKodM3nkwKo1BJc2ozqoYar8MYH/FQK8GPBOp4w2LHlm2yXuPB/dqd9/b9N4/ivf5LEthNMn1AnLS37tZIbQ4rSaxLGb72p0iBHSM5oHh1JKDn3mGDKIGxR1cxQ6PuuH6wNB5giNU9U76M4y2QGvw== user2@%(host)s

# another user entry
ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAIEA4hKDhZ7g/qlNrZuCG4EmYIfeUJLpsJQ4JOHylFwahJEy/A8VQEZpZADynouAhkM4AN96dYfyIRFxLR7EiO9ZSIg5FTF8qcpz2VuV0RBKjwO3R7GD966oRqZ6cz4Otx7LcZfDEVw2ybfe+uYnZZCF69ZpdVkNpg6IUjEqw/VZtpM= user3@%(host)s

# missing host
ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAIEA4hKDhZ7g/qlNrZuCG4EmYIfeUJLpsJQ4JOHylFwahJEy/A8VQEZpZADynouAhkM4AN96dYfyIRFxLR7EiO9ZSIg5FTF8qcpz2VuV0RBKjwO3R7GD966oRqZ6cz4Otx7LcZfDEVw2ybfe+uYnZZCF69ZpdVkNpg6IUjEqw/VZtpM= user4

# insufficient fields
ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAIBalxS0OHm1J3QB7WjtInEhqdMO7cqjqw0yVCfHqb8VU/nXJWQZPJAom8ey3uYWqrjVKuHPSgEaqqtJxwVIeJ5oBDOQbAT9WY4n7+mx8I+bhpdVsZQvtQE3BUgYh0/GUbRgSx+F/1efrwcRHCRb9QO+9DrIg1q2NeY6OR2bSiYA5Q==

# too many fields
command="dump /home",no-pty,no-port-forwarding ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAvD2QBWO4pSgMTkuGo9AqCBNTlAvnDSfKXPnwZZsHSNiWDJcSR+uMBATtdbBfYp039sudx3p+Mhm7IA70G61PiPgRebs8h/XC8gv7bUhDr7tMuG/kngSA61mId65+WtIbTJUnyLyAnRGv1uK4CcpCLAt0SrAbe9l+YAOnit6UQLIaysyrafjgbXgQDC6vFffxP9idJAhPveVV9jVoGvrf6XTAGByRKZzuPlKLIlHunIOOryOLl9FK0IbA7jYeoZ/ESt9mrheECcpAzW4jrEuU0LccN57ODKtT3Mc/sOnBVWIcIJ+5nv2dPsI2fphGrtZuyu+ckIcqhM5ydHHBius8IQ== user5@%(host)s

# unsupported key type
ssh-dsa AAAAB3NzaC1yc2EAAAABIwAAAQEAvD2QBWO4pSgMTkuGo9AqCBNTlAvnDSfKXPnwZZsHSNiWDJcSR+uMBATtdbBfYp039sudx3p+Mhm7IA70G61PiPgRebs8h/XC8gv7bUhDr7tMuG/kngSA61mId65+WtIbTJUnyLyAnRGv1uK4CcpCLAt0SrAbe9l+YAOnit6UQLIaysyrafjgbXgQDC6vFffxP9idJAhPveVV9jVoGvrf6XTAGByRKZzuPlKLIlHunIOOryOLl9FK0IbA7jYeoZ/ESt9mrheECcpAzW4jrEuU0LccN57ODKtT3Mc/sOnBVWIcIJ+5nv2dPsI2fphGrtZuyu+ckIcqhM5ydHHBius8IQ== user6@%(host)s

# munged data
ssh-rsa ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZA61mId65+WtIbTJUnyLyAnRGv1uK4CcpCLAt0SrAbe9l+YAOnit6UQLIaysyrafjgbXgQDC6vFffxP9idJAhPveVV9jVoGvrf6XTAGByRKZzuPlKLIlHunIOOryOLl9FK0IbA7jYeoZ/ESt9mrheECcpAzW4jrEuU0LccN57ODKtT3Mc/sOnBVWIcIJ+5nv2dPsI2fphGrtZuyu+ckIcqhM5ydHHBius8IQ== user7@%(host)s
""" % {'host': hostname}
        with open('key_data', 'w') as out:
            out.write(key_data)
        try:
            keys = read_authorized_keys('key_data', logging.getLogger())
            for name, key in keys.items():
                logging.debug('    %s: %r', name, key)
            self.assertEqual(sorted(keys.keys()),
                             ['user1@'+hostname,
                              'user2@'+hostname,
                              'user3@'+hostname])
        finally:
            os.remove('key_data')

        # Write and read-back.
        try:
            write_authorized_keys(keys, 'users.allow')
            new_keys = read_authorized_keys('users.allow')
            self.assertEqual(len(keys), len(new_keys))
            for user in sorted(keys.keys()):
                pubkey = keys[user]
                try:
                    new_pubkey = new_keys[user]
                except KeyError:
                    self.fail('new_keys is missing %r', user)
                self.assertEqual(new_pubkey.n, pubkey.n)
                self.assertEqual(new_pubkey.e, pubkey.e)
        finally:
            if os.path.exists('users.allow'):
                os.remove('users.allow')

        # Try default file, which may or may not exist.
        keys = read_authorized_keys(logger=logging.getLogger())

        # Try nonexistent file.
        keys = read_authorized_keys('no-such-file', logging.getLogger())
        self.assertEqual(keys.keys(), [])


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.util')
    sys.argv.append('--cover-erase')
    nose.runmodule()

