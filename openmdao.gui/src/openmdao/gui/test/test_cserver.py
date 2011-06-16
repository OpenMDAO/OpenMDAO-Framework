import unittest, os

from openmdao.gui.consoleserverfactory import *

class test_cserver(unittest.TestCase):
    
    def setUp(self):
        self.server_mgr = ConsoleServerFactory()
        self.cserver = self.server_mgr.console_server('test')
        self.print_output()

    def print_output(self):
        print 'server output:'
        print self.cserver.get_output()
        
    def test_conmin(self):
        div = '-'*70
        
        print div
        print 'LOAD PROJECT'
        self.cserver.load_project(os.getcwd()+'/simple_1.proj')
        self.print_output()
            
        print div
        print 'CHECK FILES (note paraboloid.py defining Paraboloid)'
        print self.cserver.get_files()
        self.print_output()
        
        print div
        print 'ADD CONMIN DRIVER'
        print self.cserver.add_component('driver',
           'openmdao.lib.drivers.conmindriver.CONMINdriver');
        self.print_output()
        
        # NOTE: this is not legit, but demonstrates that the error doesn't occur
        #       if you don't add() CONMINdriver to top
        # print 'SET DRIVER TO CONMIN DRIVER'
        # self.cserver.onecmd('from openmdao.main.factorymanager import *')
        # self.cserver.onecmd('top.driver=create("openmdao.lib.drivers.conmindriver.CONMINdriver")')
        # self.print_output()

        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print self.cserver.get_attributes('driver')
        self.print_output()
        
        print div
        print 'IMPORT PARABOLOID'
        self.cserver.onecmd('from paraboloid import Paraboloid')
        self.print_output()

        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print self.cserver.get_attributes('driver')
        self.print_output()

        print div
        print 'CREATE PARABOLOID'
        self.cserver.add_component('p','Paraboloid');
        self.print_output()
        
        print div
        print 'CHECK DRIVER ATTRIBUTES'
        print self.cserver.get_attributes('driver')
        self.print_output()

    def tearDown(self):
        self.server_mgr.delete_server('test')        
        self.server_mgr.cleanup()
            
if __name__ == "__main__":
    unittest.main()
