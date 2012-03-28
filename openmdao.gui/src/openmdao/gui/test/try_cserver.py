import unittest, os, os.path, time

from openmdao.gui.zmqservermanager import ZMQServerManager
from openmdao.gui.outstream import OutStreamRedirector
from openmdao.gui.util import print_list, print_dict, print_json

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
        
        time.sleep(2)   # give them time to start up 

    def test_conmin(self):
        div = '-'*70
        
        print div
        print 'LOAD PROJECT'
        path = os.path.dirname(os.path.abspath(__file__))
        self.cserver.load_project(os.path.join(path,'simple_1.proj'))
            
        print div
        print 'CHECK FILES (note paraboloid.py defining Paraboloid)'
        print_dict(self.cserver.get_files())
        
        print div
        print 'IMPORT PARABOLOID'
        self.cserver.onecmd('from paraboloid import Paraboloid')

        print div
        print 'WORKING TYPES'
        print_dict(self.cserver.get_workingtypes())

        print div
        print 'CREATE ASSEMBLY'
        print self.cserver.add_component('prob','openmdao.main.assembly.Assembly','');
        
        print div
        print 'COMPONENTS'
        print_json(self.cserver.get_components())
        
        print div
        print 'ADD CONMIN DRIVER'
        print self.cserver.add_component('driver',
           'openmdao.lib.drivers.conmindriver.CONMINdriver','prob');
        
        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print_json(self.cserver.get_attributes('prob.driver'))

        print div
        print 'CREATE PARABOLOID'
        self.cserver.add_component('p','Paraboloid','prob');
        
        print div
        print 'COMPONENTS'
        print_json(self.cserver.get_components())

        print div
        print 'STRUCTURE'
        print print_json(self.cserver.get_structure('prob'))

        print div
        print 'dir()'
        print self.cserver.onecmd('dir()')
        
    def tearDown(self):
        time.sleep(2)
        self.out_rdr.terminate()
        self.pub_rdr.terminate()
        self.cserver.terminate()
            
if __name__ == "__main__":
    unittest.main()
