"""
Test Network Functions
"""
import unittest

from openmdao.util.network import get_unused_ip_port

class NetworkTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_get_unused_ip_port(self):
        ip_port = get_unused_ip_port()
        self.assertTrue( isinstance( ip_port, int ) )
        # actually, in practice the range does not go that low but it varies from system to system
        self.assertTrue( ip_port >= 1024 ) 
        self.assertTrue( ip_port <= 65535 )
        
        
if __name__ == '__main__':
    unittest.main()

