import os, sys, traceback

from openmdao.main.zmqrpc import ZMQ_RPC
from openmdao.util.network import get_unused_ip_port
from openmdao.gui.zmqserver import ZMQServer

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
        ''' get server associated with an id or create one if none exists
        '''
        try:
            if server_id in self.server_dict:
                (server, proxy, rep_url, pub_url, out_url) = self.server_dict[server_id]
                return proxy
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
                self.server_dict[server_id] = (server, proxy, rep_url, pub_url, out_url)
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
            (server, proxy, rep_url, pub_url, out_url) = self.server_dict[server_id]
            del self.server_dict[server_id]
            try:
                server.cleanup()
            except Exception, err:
                pass
            server.terminate()
            del server

    def get_pub_url(self,server_id):
        if server_id in self.server_dict:
            (server, proxy, rep_url, pub_url, out_url) = self.server_dict[server_id]
            return pub_url
        else:
            return None
        
    def get_out_url(self,server_id):
        if server_id in self.server_dict:
            (server, proxy, rep_url, pub_url, out_url) = self.server_dict[server_id]
            return out_url            
        else:
            return None
