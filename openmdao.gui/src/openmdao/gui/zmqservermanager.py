import os
import sys
import threading
import traceback
import socket

from openmdao.main.zmqrpc import ZMQ_RPC
from openmdao.util.network import get_unused_ip_port
from openmdao.gui.zmqserver import ZMQServer
from openmdao.gui.zmqstreamserver import ZMQStreamServer

debug = True


def DEBUG(msg):
    if debug:
        print '<<<' + str(os.getpid()) + '>>> ZMQServerManager --', msg
        sys.stdout.flush()


class ZMQServerManager(object):
    ''' Creates and keeps track of ZMQ servers for the given class.
    '''

    def __init__(self, classpath, external=False):
        self.server_dict = {}
        self.classpath = classpath
        self.external = external
        if (external):
            self.address = socket.gethostbyaddr(socket.gethostname())[0]
        else:
            self.address = 'localhost'

    def server(self, server_id):
        ''' Get server associated with an id; create one if none exists.
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
                DEBUG("%s \n\t RPC on %s \n\t pub on %s \n\t out on %s"
                      % (server_id, rep_url, pub_url, out_url))
                server = ZMQServer.spawn_server(self.classpath, rep_url,
                                                pub_url, out_url)
                proxy = ZMQ_RPC(rep_url)
                self.server_dict[server_id] = {
                    'server':     server,
                    'proxy':      proxy,
                    'rep_url':    rep_url,
                    'pub_url':    pub_url,
                    'out_url':    out_url
                }
                return proxy
        except Exception as err:
            print 'Error getting server', server_id
            print str(err.__class__.__name__), ":", err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)
            return None

    def delete_server(self, server_id):
        ''' Delete the server(s) associated with an id.
        '''
        if server_id in self.server_dict:
            server_info = self.server_dict[server_id]
            del self.server_dict[server_id]

            self._terminate(server_info, 'out_ws_server')
            self._terminate(server_info, 'pub_ws_server')

            # Call proxy.cleanup() in a separate thread so we can
            # recover if it hangs (happens sometimes on Windows/EC2).
            proxy = server_info['proxy']
            cleaner = threading.Thread(target=self._cleanup_proxy,
                                       args=(proxy,), name='Proxy Cleaner')
            cleaner.daemon = True
            cleaner.start()
            cleaner.join(10)
            if cleaner.is_alive():
                print 'Timeout waiting for proxy cleaner'

            self._terminate(server_info, 'server')

    @staticmethod
    def _terminate(server_info, name):
        ''' Terminate process `name` in `server_info`. '''
        proc = server_info.get(name)
        if proc is not None:
            DEBUG('terminating %s' % name)
            try:
                proc.terminate()
                proc.wait()
                DEBUG('    terminated')
            except Exception as exc:
                print 'Error terminating', name, exc
        else:
            DEBUG("Can't terminate %s, no process" % name)

    @staticmethod
    def _cleanup_proxy(proxy):
        ''' Try to invoke proxy.cleanup(). This hangs sometimes on Windows. '''
        try:
            proxy.cleanup()
        except Exception as exc:
            print 'Error cleaning up proxy', exc

    def get_websocket_url(self, server_id, stream_name, target_url):
        ''' Get the url of the websocket for the specified stream of the
            specified server; if websocket server does not exist, start it.

                server_id:      The id of the server.

                stream_name:    The name of the stream ('out' or 'pub').

                target_url:     The relative url at which to serve the websocket.
        '''
        if stream_name not in ['out', 'pub']:
            print >>sys.stderr, \
                "ZMQServerManager - Invalid stream requested:", stream_name
            return None

        server_info = self.server_dict[server_id]

        ws_url_key = stream_name + '_ws_url'
        if ws_url_key in server_info:
            return server_info[ws_url_key]
        else:
            ws_port = get_unused_ip_port()
            ws_url = 'ws://%s:%d%s' % (self.address, ws_port, target_url)
            server_info[stream_name + '_ws_server'] = \
                ZMQStreamServer.spawn_process(server_info[stream_name+'_url'],
                                              ws_port, target_url, self.external)
            server_info[ws_url_key] = ws_url
            return ws_url

    def cleanup(self):
        ''' Delete all servers.
        '''
        keys = self.server_dict.keys()
        for server_id in keys:
            self.delete_server(server_id)
