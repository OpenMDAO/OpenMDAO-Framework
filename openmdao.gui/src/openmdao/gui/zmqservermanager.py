import os, sys, time, traceback

from openmdao.main.zmqrpc import ZMQ_RPC
from openmdao.util.network import get_unused_ip_port
from openmdao.gui.zmqserver import ZMQServer
from openmdao.gui.zmqstreamserver import ZMQStreamServer

debug = True
def DEBUG(msg):
    if debug:
        print '<<<'+str(os.getpid())+'>>> ZMQServerManager --',msg

class ZMQServerManager(object):
    ''' creates and keeps track of ZMQ servers for the given class
    '''
    def __init__(self,classpath):
        self.server_dict = {}
        self.classpath = classpath
        
    def server(self,server_id):
        ''' get server associated with an id, create one if none exists
        '''
        try:
            if server_id in self.server_dict:
                server_info = self.server_dict[server_id]
                return server_info['proxy']
            else:
                url_fmt = "tcp://127.0.0.1:%i"
                rep_url = url_fmt % get_unused_ip_port()
                pub_url = url_fmt % get_unused_ip_port()
                out_url = url_fmt % get_unused_ip_port()
                DEBUG("%s RPC on %s" % (server_id, rep_url))
                DEBUG("%s pub on %s" % (server_id, pub_url))
                DEBUG("%s out on %s" % (server_id, out_url))
                server = ZMQServer.spawn_server(self.classpath,rep_url,pub_url,out_url)
                proxy = ZMQ_RPC(rep_url)
                self.server_dict[server_id] = {
                    'server':     server,
                    'proxy':      proxy,
                    'rep_url':    rep_url,
                    'pub_url':    pub_url,
                    'out_url':    out_url                 
                }
                return proxy
        except Exception, err:
            print 'Error getting server',server_id
            print str(err.__class__.__name__),":", err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)     
            return None
            
    def delete_server(self,server_id):
        ''' delete the server associated with an id
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            server = server_info['server']
            del self.server_dict[server_id]
            try:
                server.cleanup()
            except Exception, err:
                pass
            server.terminate()
            del server

    def get_pub_url(self,server_id):
        ''' get the url of the publisher socket for the server associated with
            an id
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            return server_info['pub_url']
        else:
            return None
        
    def get_out_url(self,server_id):
        ''' get the url of the output socket for the server associated with
            an id
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            return server_info['out_url']
        else:
            return None

    def get_pubstream_server(self,server_id,ws_url):
        ''' get the publisher socket web server for the server associated with
            an id, create one if none exists
        '''
        server_info = self.server_dict[server_id]
        if 'pub_server' in server_info:
            return server_info['pub_server']
        else:
            ws_port = get_unused_ip_port()
            ws_addr = 'ws://localhost:%d%s' % (ws_port, ws_url)
            server_info['pub_server'] = ws_addr
            ZMQStreamServer.spawn_process(server_info['pub_url'],ws_port,ws_url)
            time.sleep(2)  # give server a chance to spool up
            return ws_addr
    
    def get_outstream_server(self,server_id,ws_url):
        ''' get the output socket web server for the server associated with
            an id, create one if none exists
        '''
        server_info = self.server_dict[server_id]
        if 'out_server' in server_info:
            return server_info['out_server']
        else:
            ws_port = get_unused_ip_port()
            ws_addr = 'ws://localhost:%d%s' % (ws_port, ws_url)
            server_info['out_server'] = ws_addr
            ZMQStreamServer.spawn_process(server_info['out_url'],ws_port,ws_url)
            time.sleep(2)  # give server a chance to spool up
            return ws_addr
    