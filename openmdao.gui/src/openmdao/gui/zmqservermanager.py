import os
import sys
import traceback

from openmdao.main.zmqrpc import ZMQ_RPC
from openmdao.util.network import get_unused_ip_port
from openmdao.gui.zmqserver import ZMQServer
from openmdao.gui.zmqstreamserver import ZMQStreamServer

debug = True


def DEBUG(msg):
    if debug:
        print '<<<'+str(os.getpid())+'>>> ZMQServerManager --', msg


class ZMQServerManager(object):
    ''' creates and keeps track of ZMQ servers for the given class
    '''

    def __init__(self, classpath):
        self.server_dict = {}
        self.classpath = classpath

    def server(self, server_id):
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
                DEBUG("%s \n\t RPC on %s \n\t pub on %s \n\t out on %s" % (server_id, rep_url, pub_url, out_url))
                server = ZMQServer.spawn_server(self.classpath, rep_url, pub_url, out_url)
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
            print 'Error getting server', server_id
            print str(err.__class__.__name__),":", err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)
            return None

    def delete_server(self, server_id):
        ''' delete the server(s) associated with an id
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            del self.server_dict[server_id]

            if 'out_server' in server_info:
                server_info['out_server'].terminate()
                server_info['out_server'].wait()
            if 'pub_server' in server_info:
                server_info['pub_server'].terminate()
                server_info['pub_server'].wait()

            server = server_info['server']
            proxy = server_info['proxy']
            try:
                proxy.cleanup()
            except Exception:
                pass
            server.terminate()
            server.wait()

    def get_pub_socket_url(self, server_id):
        ''' get the url of the publisher socket for the server associated with
            an id
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            return server_info['pub_url']
        else:
            return None

    def get_out_socket_url(self, server_id):
        ''' get the url of the output socket for the server associated with
            an id
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            return server_info['out_url']
        else:
            return None

    def get_pub_server_url(self, server_id, ws_url):
        ''' get the publisher socket web server for the server associated with
            an id, create one if none exists
        '''
        server_info = self.server_dict[server_id]
        if 'pub_server' in server_info:
            return server_info['pub_server_url']
        else:
            ws_port = get_unused_ip_port()
            ws_addr = 'ws://localhost:%d%s' % (ws_port, ws_url)
            server_info['pub_server'] = ZMQStreamServer.spawn_process(server_info['pub_url'], ws_port, ws_url)
            server_info['pub_server_url'] = ws_addr
            return ws_addr

    def get_out_server_url(self, server_id, ws_url):
        ''' get the output socket web server for the server associated with
            an id, create one if none exists
        '''
        server_info = self.server_dict[server_id]
        if 'out_server' in server_info:
            return server_info['out_server_url']
        else:
            ws_port = get_unused_ip_port()
            ws_addr = 'ws://localhost:%d%s' % (ws_port, ws_url)
            server_info['out_server'] = ZMQStreamServer.spawn_process(server_info['out_url'], ws_port, ws_url)
            server_info['out_server_url'] = ws_addr
            return ws_addr

    def cleanup(self):
        ''' delete all servers
        '''
        keys = self.server_dict.keys()
        for server_id in keys:
            self.delete_server(server_id)
