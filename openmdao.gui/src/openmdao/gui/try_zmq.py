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

from outstream import OutStream

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
                url_fmt = "tcp://127.0.0.1:%i"
                rep_url = url_fmt % get_unused_ip_port()
                pub_url = url_fmt % get_unused_ip_port()
                out_url = url_fmt % get_unused_ip_port()
                print "%s access RPC on %s" % (server_id, rep_url)
                print "%s publishing on %s" % (server_id, pub_url)
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

    def get_rep_url(self,server_id):
        if server_id in self.server_dict:
            (server, rep_url, pub_url, out_url) = self.server_dict[server_id]
            return rep_url
        else:
            return None
        
    def get_pub_url(self,server_id):
        if server_id in self.server_dict:
            (server, rep_url, pub_url, out_url) = self.server_dict[server_id]
            return pub_url
        else:
            return None
        
    def get_out_url(self,server_id):
        if server_id in self.server_dict:
            (server, rep_url, pub_url, out_url) = self.server_dict[server_id]
            return out_url            
        else:
            return None


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
        
    def run(self):
        import zmq
        from zmq.eventloop import ioloop
        from zmq.eventloop.zmqstream import ZMQStream
        ioloop.install()
        
        # redirect stdout/stderr to a ZMQ socket
        try:
            context = zmq.Context()
            socket = context.socket(zmq.PUB)            
            print "pid %d binding output to %s" % (os.getpid(), self.out_url)
            socket.bind(self.out_url)
            sys.stdout = OutStream(socket,'stdout')
            sys.stderr = sys.stdout
        except Exception, err:
            print err,sys.exc_info()

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
    ''' pretty print json data
    '''
    print json.dumps(json.loads(str(data)),indent=2)
    
def add_listener(addr):
    ''' listen for output on the given port and dump it to stderr
    '''
    def _write_message(msg):
        print  >> sys.stderr, 'output>>',msg
        
    stream = None
    try:
        context = zmq.Context()
        socket = context.socket(zmq.SUB)
        print 'listening for output on',addr
        socket.connect(addr)
        socket.setsockopt(zmq.SUBSCRIBE, '')
        stream = ZMQStream(socket)
    except Exception, err:
        print '    error getting outstream:',err
        exc_type, exc_value, exc_traceback = sys.exc_info()
        traceback.print_exception(exc_type, exc_value, exc_traceback)
        traceback.print_tb(exc_traceback, limit=30)   
        if stream and not stream.closed():
            stream.close()
    else:
        stream.on_recv(_write_message)

def start_loop():
    loop = ioloop.IOLoop.instance()
    loop.start()
        
def run_simple():
    ''' create a ZMQServer for simple model, run & query it
    '''
    server_id = 'OptimizationUnconstrained'
    classpath = 'openmdao.examples.simple.optimization_unconstrained.OptimizationUnconstrained'
    print '%d starting %s server...' % (os.getpid(), server_id)
    mgr = ZMQServerManager(classpath)
    srv = mgr.server(server_id)
    if srv is not None:
        out_url = mgr.get_out_url(server_id)
        add_listener(out_url)
            
        #print dir(srv)
        #print srv.dir()
        srv.register_published_vars(['paraboloid.x', 'paraboloid.y', 'paraboloid.f_xy'])
        srv.run()
        
        print srv.get('paraboloid.x')
        print srv.get('paraboloid.y')
        print srv.get('paraboloid.f_xy')
        mgr.delete_server('OptimizationUnconstrained')

def run_cserver():
    ''' create a ZMQ-based console server & exercise it
    '''
    server_id = 'sellar'
    mgr = ConsoleServerFactory()
    cserver = mgr.server(server_id)
    if cserver is not None:
        out_url = mgr.get_out_url(server_id)
        add_listener(out_url)

        
        print '=============================================================='
        print '================= started cserver ============================'
        print '=============================================================='
        #print cserver.onecmd("!!!!!!!!!!!!!!!!!!!  I'm Alive   !!!!!!!!!!!!!")
        print cserver.getcwd()
        #print 'types:'
        #print cserver.get_available_types()
        
        print '=============================================================='
        print '================ load project ================================'
        print '=============================================================='
        from openmdao.gui.settings import MEDIA_ROOT
        proj_file = MEDIA_ROOT+'/projects/swryan/sellar auto.proj'
        print cserver.load_project(proj_file)
        print_dict(cserver.get_files())
        
        print '=============================================================='
        print '================ import from  file ==========================='
        print '=============================================================='
        print cserver.onecmd('from sellar_CO import SellarCO')
        print_dict(cserver.get_workingtypes())
        print cserver.onecmd('trace')
        
        print '=============================================================='
        print '=============== execute script to set up the problem ========='
        print '=============================================================='
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
        print '=============================================================='
        #cserver.onecmd("prob.register_published_vars(['dis1.z1', 'dis1.z2', 'dis1.x1'])")
        cserver.run()
        
        print '=============================================================='
        print '================ get results ================================='
        print '=============================================================='
        print cserver.onecmd('trace')
        print cserver.get_value('prob.dis1.z1')
        print cserver.get_value('prob.dis1.z2')
        print cserver.get_value('prob.dis1.x1')
        
        print '=============================================================='
        print '================ delete server ==============================='
        print '=============================================================='
        mgr.delete_server('cserver')
    
def main():
    import threading
    loop_thread = threading.Thread(target=start_loop)
    loop_thread.start()

    #run_simple()
    run_cserver()
    loop_thread.join()

if __name__ == '__main__':
    main()
