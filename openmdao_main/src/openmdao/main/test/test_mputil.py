"""
Test mp_util.py
"""

import logging
import os.path
import socket
import sys
import unittest
import nose

from openmdao.main.mp_util import read_server_config, read_allowed_hosts, \
                                  is_legal_connection

from openmdao.util.publickey import make_private, HAVE_PYWIN32
from openmdao.util.testutil import assert_raises


class TestCase(unittest.TestCase):
    """ Test mp_util.py """

    def test_config(self):
        logging.debug('')
        logging.debug('test_config')

        # Try to read config from non-existent file.
        assert_raises(self, "read_server_config('no-such-file')",
                      globals(), locals(), IOError,
                      "No such file 'no-such-file'")

    def test_allowed_hosts(self):
        logging.debug('')
        logging.debug('test_allowed_hosts')

        hostname = socket.gethostname()
        host_ipv4 = socket.gethostbyname(hostname)
        dot = host_ipv4.rfind('.')
        domain_ipv4 = host_ipv4[:dot+1]

        good_data = """
# Local host IPv4.
%s

# Local domain IPv4.
%s

# Local host name.
%s

""" % (host_ipv4, domain_ipv4, hostname)

        gibberish = '$^&*'
        bad_data = good_data + """
# Gibberish.
%s
""" % gibberish

        # Try good data.
        with open('hosts.allow', 'w') as out:
            out.write(good_data)
        if sys.platform != 'win32' or HAVE_PYWIN32:
            make_private('hosts.allow')
        try:
            allowed_hosts = read_allowed_hosts('hosts.allow')
        finally:
            os.remove('hosts.allow')
        self.assertEqual(len(allowed_hosts), 3)
        self.assertEqual(allowed_hosts[0], host_ipv4)
        self.assertEqual(allowed_hosts[1], domain_ipv4)
        self.assertEqual(allowed_hosts[2], host_ipv4)

        # Try bad data.
        with open('hosts.allow', 'w') as out:
            out.write(bad_data)
        if sys.platform != 'win32' or HAVE_PYWIN32:
            make_private('hosts.allow')
        try:
            allowed_hosts = read_allowed_hosts('hosts.allow')
        except RuntimeError as exc:
            self.assertEqual(str(exc),
                             "1 errors in 'hosts.allow', check log for details")
        else:
            expected = 3
            try:
                # This actally resolves in some environments.
                socket.gethostbyname(gibberish)
            except socket.gaierror:
                pass
            else:
                expected += 1
            self.assertEqual(len(allowed_hosts), expected)
            self.assertEqual(allowed_hosts[0], host_ipv4)
            self.assertEqual(allowed_hosts[1], domain_ipv4)
            self.assertEqual(allowed_hosts[2], host_ipv4)
        finally:
            os.remove('hosts.allow')

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
        # Try nonexistant file.
        assert_raises(self, "read_allowed_hosts('no-such-file')",
                      globals(), locals(), RuntimeError,
                      "'no-such-file' does not exist")

        # Try insecure file.
        if sys.platform != 'win32' or HAVE_PYWIN32:
            with open('hosts.allow', 'w') as out:
                out.write('\n')
            os.chmod('hosts.allow', 0666)
            try:
                assert_raises(self, "read_allowed_hosts('hosts.allow')",
                              globals(), locals(), RuntimeError,
                              "'hosts.allow' is not private")
            finally:
                os.remove('hosts.allow')


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

