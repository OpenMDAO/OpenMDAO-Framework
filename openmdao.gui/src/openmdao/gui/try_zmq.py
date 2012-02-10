import os, sys, traceback, json
from cStringIO import StringIO

from multiprocessing import Process

from openmdao.main.zmqcomp import *
from openmdao.main.zmqrpc import *

from openmdao.util.network import get_unused_ip_port

from openmdao.gui.util import print_dict, print_list, RepeatTimer

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
                (server, rep_url, pub_url, out_url) = self.server_dict[server_id]
                return ZMQ_RPC(rep_url)
            else:
                rep_url = "tcp://127.0.0.1:%i" % get_unused_ip_port()
                print "%s serving requests on %s" % (server_id, rep_url)
                pub_url = "tcp://127.0.0.1:%i" % get_unused_ip_port()
                print "%s publishing on %s" % (server_id, pub_url)
                out_url = "tcp://127.0.0.1:%i" % get_unused_ip_port()
                print "%s outputting on %s" % (server_id, out_url)
                server = ZMQServer(self.classpath,rep_url,pub_url,out_url)
                server.start()
                self.server_dict[server_id] = (server, rep_url, pub_url, out_url)
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
            (server, rep_url, pub_url, out_url) = self.server_dict[server_id]
            del self.server_dict[server_id]
            try:
                server.cleanup()
            except Exception, err:
                pass
            server.terminate()
            del server

class ZMQServer(Process):
    def __init__(self,classpath,rep_url,pub_url,out_url):
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
        self.out_url = out_url
        
        super(ZMQServer, self).__init__()
    
    def get_output(self):
        ''' get any pending output and clear the outputput buffer
        '''
        output = self.cout.getvalue()
        self.cout.truncate(0)
        return output

    def publish_output(self):
        ''' publish any pending output and clear the outputput buffer
        '''
        try:
            output = self.cout.getvalue()
            if len(output) > 0:
                self.cout_socket.send(output)
                self.cout.truncate(0)
        except Exception, err:
            print err,sys.exc_info()

    def run(self):
        # open a ZMQ socket on out_url and start publishing cout
        # self.sysout = sys.stdout
        # self.syserr = sys.stderr
        # self.cout = StringIO()
        # sys.stdout = self.cout
        # sys.stderr = self.cout
        # try:
        # context = zmq.Context()
        # self.cout_socket = context.socket(zmq.PUB)            
        # print "binding output to %s" % self.out_url
        # self.cout_socket.bind(self.out_url)
        # self.cout_timer = RepeatTimer(2,self.publish_output)
        # self.cout_timer.start()
        # except Exception, err:
        # print err,sys.exc_info()

        print 'Starting ZMQServer:',self
        ZmqCompWrapper.serve(self.obj, rep_url=self.rep_url, pub_url=self.pub_url)
                         

class ConsoleServerFactory(ZMQServerManager):
    ''' creates and keeps track of console servers
    '''
    def __init__(self):
        super(ConsoleServerFactory, self).__init__('openmdao.gui.consoleserverfactory.ConsoleServer')

    def console_server(self,server_id):
        return self.server(server_id)

def print_json(data):
    print json.dumps(json.loads(str(data)),indent=2)
    
def run_simple():
    # create the OptimizationUnconstrained server
    classpath = 'openmdao.examples.simple.optimization_unconstrained.OptimizationUnconstrained'
    mgr = ZMQServerManager(classpath)
    srv = mgr.server('OptimizationUnconstrained')
    if srv is not None:
        print dir(srv)
        print srv.dir()
        #srv.register_published_vars(['paraboloid.x', 'paraboloid.y', 'paraboloid.f_xy'])
        srv.run()
        print srv.get('paraboloid.x')
        print srv.get('paraboloid.y')
        print srv.get('paraboloid.f_xy')
        mgr.delete_server('OptimizationUnconstrained')

def run_cserver():
    mgr = ConsoleServerFactory()
    cserver = mgr.server('cserver')
    if cserver is not None:
        print '=============================================================='
        print '================= started cserver ============================'
        print cserver.getcwd()
        #print 'types:'
        #print cserver.get_available_types()
        
        print '=============================================================='
        print '================ load project ================================'
        from openmdao.gui.settings import MEDIA_ROOT
        proj_file = MEDIA_ROOT+'/projects/swryan/sellar auto.proj'
        print cserver.load_project(proj_file)
        print_dict(cserver.get_files())
        
        print '=============================================================='
        print '================ import from  file ==========================='
        print cserver.onecmd('from sellar_CO import SellarCO')
        print_dict(cserver.get_workingtypes())
        print cserver.onecmd('trace')
        
        print '=============================================================='
        print '=============== execute script to set up the problem ========='
        script = """
prob = SellarCO()

prob.dis1.z1 = 5.0
prob.dis2.z1 = 5.0

prob.dis1.z2 = 2.0
prob.dis2.z2 = 2.0

prob.dis1.x1 = 1.0

prob.dis1.y2 = 0.0
prob.dis2.y1 = 3.16
"""
        cserver.onecmd(script)
        print cserver.onecmd('trace')
        
        print cserver.onecmd('print dir(prob)')
        print_json(cserver.get_structure(''))
        print_json(cserver.get_workflow('prob'))
        print cserver.get_value('prob.dis1.z1')
        print cserver.get_value('prob.dis1.z2')
        print cserver.get_value('prob.dis1.x1')
        
        print '=============================================================='
        print '================= register published vars and run ============'
        #cserver.onecmd("prob.register_published_vars(['dis1.z1', 'dis1.z2', 'dis1.x1'])")
        cserver.run()
        
        print '=============================================================='
        print '================ get results ================================='
        print cserver.onecmd('trace')
        print cserver.get_value('prob.dis1.z1')
        print cserver.get_value('prob.dis1.z2')
        print cserver.get_value('prob.dis1.x1')
        
        print '=============================================================='
        print '================ delete server ==============================='
        mgr.delete_server('cserver')
    
def main():
    #run_simple()
    run_cserver()

if __name__ == '__main__':
    main()
    
    