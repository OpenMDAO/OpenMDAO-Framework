import os, sys, traceback

from multiprocessing import Process

from openmdao.main.zmqcomp import *
from openmdao.main.zmqrpc import *

from openmdao.util.network import get_unused_ip_port

import zmq
from zmq.eventloop import ioloop
from zmq.eventloop.zmqstream import ZMQStream
ioloop.install()


class ZMQServerManager(object):
    ''' creates and keeps track of ZMQ servers
    '''
    def __init__(self,classpath):
        self.server_dict = {}
        self.classpath = classpath
        
    def server(self,server_id):
        ''' get server associated with an id or create one if none exists
        '''
        try:
            if server_id in self.server_dict:
                (server, rep_url, pub_url) = self.server_dict[server_id]
                return ZMQ_RPC(rep_url)
            else:
                rep_url = "tcp://127.0.0.1:%i" % get_unused_ip_port()
                print "%s serving requests on %s" % (server_id, rep_url)
                pub_url = "tcp://127.0.0.1:%i" % get_unused_ip_port()
                print "%s publishing on %s" % (server_id, pub_url)
                server = ZMQServer(self.classpath,rep_url,pub_url)
                server.start()
                self.server_dict[server_id] = (server, rep_url, pub_url)
                return ZMQ_RPC(rep_url)
        except Exception, err:
            print 'Error getting server',server_id
            print str(err.__class__.__name__), ":", err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)     
            return None
            
    def delete_server(self,server_id):
        ''' delete the server associated with an id
        '''
        if server_id in self.server_dict:
            (server, rep_url, pub_url) = self.server_dict[server_id]
            del self.server_dict[server_id]
            try:
                server.cleanup()
            except Exception, err:
                pass
            server.terminate()
            del server

class ZMQServer(Process):
    def __init__(self,classpath,rep_url,pub_url):
        parts = classpath.split('.')
        modpath = '.'.join(parts[:-1])
        __import__(modpath)
        try:
            mod = sys.modules[modpath]
            self.ctor = getattr(mod, parts[-1])
        except KeyError, AttributeError:
            print "ZMQServer can't locate %s" % classpath
            sys.exit(-1)
        self.obj = self.ctor()
        print 'ZMQServer obj:',self.obj

        self.rep_url = rep_url
        self.pub_url = pub_url
        
        super(ZMQServer, self).__init__()
    
    def run(self):
        print 'Starting ZMQServer:',self
        ZmqCompWrapper.serve(self.obj, rep_url=self.rep_url, pub_url=self.pub_url)
                         

class ConsoleServerFactory(ZMQServerManager):
    ''' creates and keeps track of console servers
    '''
    def __init__(self):
        super(ConsoleServerFactory, self).__init__('openmdao.gui.consoleserverfactory.ConsoleServer')

    def console_server(self,server_id):
        return self.server(server_id)
        
def main():
    ''' create server, run it and get results
    '''
    
    # create the OptimizationUnconstrained server
    classpath = 'openmdao.examples.simple.optimization_unconstrained.OptimizationUnconstrained'
    mgr = ZMQServerManager(classpath)
    srv = mgr.server('OptimizationUnconstrained')
    if srv is not None:
        #srv.register_published_vars(['paraboloid.x', 'paraboloid.y', 'paraboloid.f_xy'])
        srv.run()
        print srv.get('paraboloid.x')
        print srv.get('paraboloid.y')
        print srv.get('paraboloid.f_xy')
        mgr.delete_server('OptimizationUnconstrained')
        
    
    mgr = ConsoleServerFactory()
    cserver = mgr.server('cserver')
    if srv is not None:
        print 'started cserver:',cserver
        print 'cwd:'
        print cserver.getcwd()
        #print 'types:'
        #print cserver.get_available_types()
        
        from openmdao.gui.settings import MEDIA_ROOT
        proj_file = MEDIA_ROOT+'/projects/swryan/sellar auto.proj'
        print 'loading project:',proj_file
        print cserver.load_project(proj_file)
        
        print cserver.execfile('sellar_CO.py')
        print cserver.onecmd('print dir()')
        print 'working_types:',cserver.get_workingtypes()
        print cserver.get_structure('')
        print cserver.get_workflow('prob')
        print cserver.get_value('prob.dis1.z1')
        print cserver.get_value('prob.dis1.z2')
        print cserver.get_value('prob.dis1.x1')
        cserver.onecmd("prob.register_published_vars(['prob.dis1.z1', 'prob.dis1.z2', 'prob.dis1.x1'])")
        cserver.onecmd('prob.dis1.z1 = 0')
        cserver.run()
        print cserver.get_value('trace')
        print cserver.get_value('prob.dis1.z1')
        
        mgr.delete_server('cserver')
        

if __name__ == '__main__':
    main()
    
    