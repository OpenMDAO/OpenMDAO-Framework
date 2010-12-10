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
                                    authorized_keys

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
        key_pair = generate_key_pair(user)

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
        keys = authorized_keys()
        for name, key in keys.items():
            logging.debug('    %s: %r', name, key)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.util')
    sys.argv.append('--cover-erase')
    nose.runmodule()

