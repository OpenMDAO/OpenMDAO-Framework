
import os, os.path
import tempfile
from shutil import rmtree

from mdao_util import singleton
from consoleserverfactory import ConsoleServerFactory

@singleton
class ServerManager:
    ''' keeps track of console servers & temporary files
    '''
    def __init__(self):
        self.factory = ConsoleServerFactory()
        self.cserver_dict = {}
        self.temp_files = {}
        
    def __del__(self):
        ''' make sure we clean up on exit
        '''
        #self.cleanup()  # this locks up python for some reason
        
    def console_server(self,server_id):
        ''' create a new console server associated with a server id
        '''
        if not self.cserver_dict.has_key(server_id):
            cserver = self.factory.create('mdao-'+server_id)
            self.cserver_dict[server_id] = cserver;
        else:
            cserver = self.cserver_dict[server_id]
        return cserver
        
    def delete_server(self,server_id):
        ''' delete the console server associated with a server id
        '''
        if self.cserver_dict.has_key(server_id):
            cserver = self.cserver_dict[server_id]
            del self.cserver_dict[server_id]
            cserver.cleanup()
            del cserver
        
    def get_tempdir(self,name):
        ''' create a temporary file prefixed with the given name
        '''
        if not name in self.temp_files:
            self.temp_files[name] = tempfile.mkdtemp(prefix='mdao-'+name+'.')
        return self.temp_files[name]
        
    def cleanup(self):        
        ''' clean up temporary files, etc
        '''
        for server_id, cserver in self.cserver_dict:
            del self.cserver_dict[server_id]
            cserver.cleanup()
            del cserver            
        for name in self.temp_files:
            f = self.temp_files[name]
            if os.path.exists(f):
                rmtree(f)

    
