"""
Test mp_util.py
"""

import logging
import os.path
import socket
import sys
import unittest
import nose

from openmdao.main.mp_util import read_server_config, \
                                  read_allowed_hosts, is_legal_connection, \
                                  generate_key_pair, _KEY_CACHE, \
                                  _is_private, HAVE_PYWIN32
from openmdao.main.rbac import Credentials

from openmdao.util.testutil import assert_raises, assert_rel_error


class TestCase(unittest.TestCase):
    """ Test mp_util.py """

    def test_config(self):
        logging.debug('')
        logging.debug('test_config')

        # Try to read config from non-existent file.
        assert_raises(self, "read_server_config('no-such-file')",
                      globals(), locals(), IOError,
                      "No such file 'no-such-file'")

    def test_keyfile(self):
        logging.debug('')
        logging.debug('test_key')

        # Force a key generation.
        key_file = os.path.expanduser(os.path.join('~', '.openmdao', 'keys'))
        if os.path.exists(key_file):
            os.remove(key_file)
        credentials = Credentials()
        if credentials.user in _KEY_CACHE:
            del _KEY_CACHE[credentials.user]
        key_pair = generate_key_pair(credentials)

        # Check privacy.
        if sys.platform != 'win32' or HAVE_PYWIN32:
            self.assertTrue(_is_private(key_file))
            if sys.platform == 'win32':
                public_file = os.environ['COMSPEC']
            else:
                public_file = '/bin/sh'
            self.assertFalse(_is_private(public_file))

    def test_allowed_hosts(self):
        logging.debug('')
        logging.debug('test_allowed_hosts')

        hostname = socket.gethostname()
        host_ipv4 = socket.gethostbyname(hostname)
        dot = host_ipv4.rfind('.')
        domain_ipv4 = host_ipv4[:dot+1]

        with open('hosts.allow', 'w') as out:
            out.write("""
# Local host IPv4.
%s

# Local domain IPv4.
%s

# Local host name.
%s

# Gibberish.
$^&*
""" % (host_ipv4, domain_ipv4, hostname))

        try:
            allowed_hosts = read_allowed_hosts('hosts.allow')
        finally:
            os.remove('hosts.allow')

        # Check read data.
        self.assertEqual(len(allowed_hosts), 3)
        self.assertEqual(allowed_hosts[0], host_ipv4)
        self.assertEqual(allowed_hosts[1], domain_ipv4)
        self.assertEqual(allowed_hosts[2], host_ipv4)

        # Check AF_INET addresses.
        logger = logging.getLogger()
        self.assertTrue(is_legal_connection((host_ipv4, 0),
                                            allowed_hosts, logger))
        domain_host = domain_ipv4 + '123'
        self.assertTrue(is_legal_connection((domain_host, 0),
                                            allowed_hosts, logger))
        self.assertFalse(is_legal_connection(('0.0.0.0', 0),
                                            allowed_hosts, logger))

        # Check AF_UNIX address.
        self.assertTrue(is_legal_connection('/tmp/pipe',
                                            allowed_hosts, logger))


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

