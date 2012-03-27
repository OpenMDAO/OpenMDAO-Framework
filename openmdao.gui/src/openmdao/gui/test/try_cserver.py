import unittest, os, time

from openmdao.gui.zmqservermanager import ZMQServerManager
from openmdao.gui.outstream import OutStreamRedirector

class test_cserver(unittest.TestCase):
    
    def setUp(self):
        self.server_mgr = ZMQServerManager('openmdao.gui.consoleserver.ConsoleServer')
        self.cserver = self.server_mgr.server('test')
        
        out_url = self.server_mgr.get_out_socket_url('test')
        self.out_rdr = OutStreamRedirector('OUT',out_url,'test.out')
        self.out_rdr.start()
            
        pub_url = self.server_mgr.get_pub_socket_url('test')
        self.pub_rdr = OutStreamRedirector('PUB',pub_url,'test.pub')
        self.pub_rdr.start()
        
        time.sleep(5)   # give them time to start up 

    def test_conmin(self):
        div = '-'*70
        
        print div
        print 'dir()'
        print self.cserver.onecmd('dir()')
        
        print div
        print 'LOAD PROJECT'
        self.cserver.load_project(os.getcwd()+'/simple_1.proj')
            
        print div
        print 'CHECK FILES (note paraboloid.py defining Paraboloid)'
        print self.cserver.get_files()
        
        print div
        print 'ADD CONMIN DRIVER'
        print self.cserver.add_component('driver',
           'openmdao.lib.drivers.conmindriver.CONMINdriver','');
        
        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print self.cserver.get_attributes('driver')
        
        print div
        print 'IMPORT PARABOLOID'
        self.cserver.onecmd('from paraboloid import Paraboloid')

        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print self.cserver.get_attributes('driver')

        print div
        print 'CREATE PARABOLOID'
        self.cserver.add_component('p','Paraboloid');
        
        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print self.cserver.get_attributes('driver')

    def tearDown(self):
        time.sleep(5)
        self.out_rdr.terminate()
        self.pub_rdr.terminate()
            
if __name__ == "__main__":
    unittest.main()
