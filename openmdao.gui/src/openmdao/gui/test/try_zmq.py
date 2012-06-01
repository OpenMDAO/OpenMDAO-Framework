import os, sys, traceback, json
from cStringIO import StringIO

import zmq
from zmq.eventloop import ioloop
from zmq.eventloop.zmqstream import ZMQStream
ioloop.install()

from openmdao.main.zmqcomp import *
from openmdao.main.zmqrpc import *

from openmdao.util.network import get_unused_ip_port

from openmdao.gui.zmqservermanager import ZMQServerManager
from openmdao.gui.outstream import OutStreamRedirector
from openmdao.gui.util import print_dict, print_list, print_json


def run_simple():
    ''' create a ZMQServer for simple model, run & query it
    '''
    server_id = 'OptimizationUnconstrained'
    classpath = 'openmdao.examples.simple.optimization_unconstrained.OptimizationUnconstrained'    
    mgr = ZMQServerManager(classpath)
    srv = mgr.server(server_id)
    if srv is not None:
        out_url = mgr.get_out_socket_url(server_id)
        out_rdr = OutStreamRedirector('OPT OUT',out_url,'OptimizationUnconstrained.out')
        out_rdr.start()
            
        pub_url = mgr.get_pub_socket_url(server_id)
        pub_rdr = OutStreamRedirector('OPT PUB',pub_url,'OptimizationUnconstrained.pub')
        pub_rdr.start()
        
        time.sleep(5)   # give them time to start up 
        
        srv.dir()
        srv.register_published_vars(['paraboloid.x', 'paraboloid.y', 'paraboloid.f_xy'])
        srv.run()
        
        print 'x =',srv.get('paraboloid.x')
        print 'y =',srv.get('paraboloid.y')
        print 'f_xy =',srv.get('paraboloid.f_xy')
        
        time.sleep(15)   # give it time to finish
        mgr.delete_server('OptimizationUnconstrained')
        out_rdr.terminate()
        pub_rdr.terminate()

def run_cserver():
    ''' create a ZMQ-based console server & exercise it
    '''
    server_id = 'ConsoleServer'
    classpath = 'openmdao.gui.consoleserver.ConsoleServer'   
    mgr = ZMQServerManager(classpath)
    cserver = mgr.server(server_id)
    if cserver is not None:
        print '=============================================================='
        print '================= started cserver ============================'
        print '=============================================================='
        #print cserver.onecmd("!!!!!!!!!!!!!!!!!!!  I'm Alive   !!!!!!!!!!!!!")
        print cserver.getcwd()
        #print 'types:'
        #print cserver.get_available_types()
        
        print '=============================================================='
        print '================= redirecting streams ========================'
        print '=============================================================='
        # subscriber to the cserver output & print to stdout
        out_url = mgr.get_out_socket_url(server_id)
        out_rdr = OutStreamRedirector('CSERVER OUT',out_url,'cserver.out')
        out_rdr.start()
        
        pub_url = mgr.get_pub_socket_url(server_id)
        pub_rdr = OutStreamRedirector('CSERVER PUB',pub_url,'cserver.pub')
        pub_rdr.start()
        
        time.sleep(5)   # give them time to start up 
        
        print '=============================================================='
        print '================ load project ================================'
        print '=============================================================='
        path = os.path.dirname(os.path.abspath(__file__))
        proj_file = os.path.join(path,'sellar.proj')
        print cserver.load_project(proj_file)
        print_dict(cserver.get_files())
        
        print '=============================================================='
        print '================ import from  file ==========================='
        print '=============================================================='
        print cserver.onecmd('from sellar_CO import SellarCO')
        print_dict(cserver.get_workingtypes())
        #print cserver.onecmd('trace')
        
        print '=============================================================='
        print '=============== execute script to set up the problem ========='
        print '=============================================================='
        script = """
top = SellarCO()
top.z1_t = 5.0
top.dis1.z1 = 5.0
top.dis2.z1 = 5.0

top.z2_t = 2.0
top.dis1.z2 = 2.0
top.dis2.z2 = 2.0

top.x1_t = 1.0
top.dis1.x1 = 1.0

top.y1_t = 3.16
top.y2_t = 0.0
top.dis1.y2 = 0.0
top.dis2.y1 = 3.16
"""
        cserver.onecmd(script)
        #print cserver.onecmd('trace')
        
        print cserver.onecmd('print dir(top)')
        print_json(cserver.get_structure(''))
        print_json(cserver.get_workflow('top'))
        print cserver.get_value('top.dis1.z1')
        print cserver.get_value('top.dis1.z2')
        print cserver.get_value('top.dis1.x1')
        
        print '=============================================================='
        print '================= register published vars and run ============'
        print '=============================================================='
        cserver.onecmd("top.register_published_vars(['dis1.z1', 'dis1.z2', 'dis1.x1'])")
        cserver.run()
        
        print '=============================================================='
        print '================ get results ================================='
        print '=============================================================='
        print cserver.onecmd('trace')
        print cserver.get_value('top.dis1.z1')
        print cserver.get_value('top.dis1.z2')
        print cserver.get_value('top.dis1.x1')
        
        print '=============================================================='
        print '================ delete server ==============================='
        print '=============================================================='
        time.sleep(15)   # give it time to finish
        mgr.delete_server(server_id)
        out_rdr.terminate()
        pub_rdr.terminate()
    
def main():
    run_simple()
    run_cserver()
    

if __name__ == '__main__':
    main()
