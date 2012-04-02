
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.Model=function() {

    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        outstream_topic = 'outstream',
        outstream_socket = false,
        publisher_socket = false,
        subscribers = {};
 
    /** initialize a websocket
           url:        the URL of the address on which to open the websocket
           handler:    the message handler for the websocket
    */
    function open_websocket(url,handler) {
        // make ajax call to get outstream websocket
        jQuery.ajax({
            type: 'GET',
            url:  url,
            success: function(addr) {
                openmdao.Util.openWebSocket(addr,handler);
            },
            error: function(jqXHR, textStatus, errorThrown) {
                debug.error("Error getting websocket (status="+jqXHR.status+"): "+jqXHR.statusText);
                debug.error(jqXHR,textStatus,errorThrown);
            }
        });
    };
    
    
    /** initialize the outstream websocket */
    function open_outstream_socket(topic) {
        open_websocket('outstream', function(data) {
            var callbacks = subscribers[topic];
            if (callbacks) {
                for (i = 0; i < callbacks.length; i++) {
                    if (typeof callbacks[i] === 'function') {
                        callbacks[i](data);
                    }
                    else {
                        debug.error('Model: invalid callback function for topic:',topic,callbacks[i]);
                    };
                };
            };
        });
    };

    /** initialize the publisher websocket */
    function open_publisher_socket() {
        open_websocket('pubstream', function(message) {
            message = jQuery.parseJSON(message);
            var callbacks = subscribers[message[0]];
            if (callbacks) {
                for (i = 0; i < callbacks.length; i++) {
                    if (typeof callbacks[i] === 'function') {
                        callbacks[i](message);
                    }
                    else {
                        debug.error('Model: invalid callback function for topic:',topic,callbacks[i]);
                    };
                };
            };
        });
    };
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** add a subscriber (i.e. a function to be called) for messgages with the given topic */
    this.addListener = function(topic, callback) {
        if (topic in subscribers) {
            subscribers[topic].push(callback);
        }
        else {
            subscribers[topic] = [ callback ]
            if (topic === outstream_topic && !outstream_socket) {
                outstream_socket = true;
                open_outstream_socket(outstream_topic);
            }
            else if (!publisher_socket) {
                publisher_socket = true;
                open_publisher_socket();
            };
        };
    };
    
   /** notify all generic listeners that something may have changed  */
    this.updateListeners = function() {        
        //debug.info('updateListeners',subscribers)
        var callbacks = subscribers[''];
        //debug.info('updateListeners',callbacks)
        if (callbacks) {
            for (i = 0; i < callbacks.length; i++) {
                debug.info('updating',callbacks[i])
                if (typeof callbacks[i] === 'function') {
                    callbacks[i]();
                }
                else {
                    debug.error('Model: invalid callback function for topic:',topic,callbacks[i]);
                }
            };
        };
    };

    /** get the list of object types that are available for creation */
    this.getTypes = function(callback, errorHandler) {
        if (typeof callback != 'function')
            return;

        jQuery.ajax({
            type: 'GET',
            url:  'types',
            dataType: 'json',
            success: callback,
            error: errorHandler
        })
    }

    /** get a new (empty) model */
    this.newModel = function() {
        jQuery.ajax({
            type: 'POST',
            url:  'model',
            success: self.updateListeners
        })
    }

    /** save the current project */
    this.saveProject = function(callback,errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'project',
            success: callback,
            error: errorHandler
        })
    }
   
    /** get list of components in the top driver workflow */
    this.getWorkflow = function(pathname,callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'workflow/'+pathname,
                dataType: 'json',
                success: callback,
                error: errorHandler
            })
        }
    }
    
    /** get the data structure for an assembly */
    this.getStructure = function(pathname,callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            if (!pathname) {
                pathname = '';
            };
            jQuery.ajax({
                type: 'GET',
                url:  'structure/'+pathname,
                dataType: 'json',
                success: callback,
                error: errorHandler
            })
        }
    }
 
    /** get  hierarchical list of components*/
    this.getComponents = function(callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'components',
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            })
        }
    }
    
    /** get  attributes of a component*/
    this.getComponent = function(name,callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'component/'+name,
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            })
        }
    }

    /** get connections between two components in an assembly */
    this.getConnections = function(pathname,src_name,dst_name,callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'connections/'+pathname,
                dataType: 'json',
                data: { 'src_name': src_name, 'dst_name': dst_name },
                success: callback,
                error: errorHandler
            })
        }
    }

    /** set connections between two components in an assembly */
    this.setConnections = function(pathname,src_name,dst_name,connections,callback,errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'connections/'+pathname,
            dataType: 'json',
            data: { 'src_name': src_name, 'dst_name': dst_name, 'connections': connections },
            success: callback,
            error: errorHandler
        })
    }
    
    /** add an object of the specified type & name to the specified parent */
    this.addComponent = function(typepath,name,parent,callback) {
        if (!parent) {
            parent = '';
        };
       
        if (/driver/.test(typepath)&&(openmdao.Util['$'+name])){openmdao.Util['$'+name]();return;};

        jQuery.ajax({
            type: 'POST',
            url:  'component/'+name,
            data: {'type': typepath, 'parent': parent },
            success: function(text) {
                        if (typeof callback == 'function') {
                            callback(text);
                        };
                        self.updateListeners();
            }
        });
    }

    /** remove the component with the given pathname */
    this.removeComponent = function(pathname) {
        var parent = openmdao.Util.getPath(pathname);
        if (parent.length > 0 ) {
            self.issueCommand(parent+'.remove("'+openmdao.Util.getName(pathname)+'")');
        }
        else {
            self.issueCommand('del('+openmdao.Util.getName(pathname)+')');
        }
    }
    
    /** issue the specified command against the model */
    this.issueCommand = function(cmd, callback, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'command',
            data: { 'command': cmd },
            success: function(txt) { 
                        if (typeof callback == 'function') {
                            callback(txt)
                        };
                        self.updateListeners()
                     },
            error: errorHandler
        })
    }

    /** get any queued output from the model */
    this.getOutput = function(callback, errorHandler) {
        jQuery.ajax({
            url: 'output',
            success: function(text) { 
                        if (typeof callback == 'function') {
                            callback(text)
                        };
                     },
            error: errorHandler
        })
    }

    /** get a recursize file listing of the model working directory (as JSON) */
    this.getFiles = function(callback, errorHandler) {
        if (typeof callback != 'function')
            return

        jQuery.ajax({
            type: 'GET',
            url:  'files',
            dataType: 'json',
            data: {},
            success: callback,
            error: errorHandler
        })
    }

    /** get the contents of the specified file */
    this.getFile = function(filepath, callback, errorHandler) {
        if (typeof callback != 'function')
            return;

        jQuery.ajax({
            type: 'GET',
            url:  'file'+filepath.replace(/\\/g,'/'),
            dataType: 'text',
            success: callback,
            error: errorHandler
        })
    }

    /** set the contents of the specified file */
    this.setFile = function(filepath, contents, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+filepath.replace(/\\/g,'/'),
            data: { 'contents': contents},
            success: self.updateListeners,
            error: errorHandler
        })
    }

    /** create a new folder in the model working directory with the specified path */
    this.createFolder = function(folderpath, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+folderpath.replace(/\\/g,'/'),
            data: { 'isFolder': true},
            success: self.updateListeners,
            error: errorHandler
        })
    }

    /** create a new file in the model working directory with the specified path  */
    this.newFile = function(folderpath) {
        openmdao.Util.promptForValue('Specify a name for the new file',function(name) {
            if (folderpath)
                name = folderpath+'/'+name
            var contents = ''
            if (/.py$/.test(name))
                contents = '"""\n   '+name+'\n"""\n\n'
            if (/.json$/.test(name))
                contents = '[]'                
            self.setFile(name,contents)
        })
    }

    /** prompt for name & create a new folder */
    this.newFolder = function(folderpath) {
        openmdao.Util.promptForValue('Specify a name for the new folder',function(name) {
            if (folderpath) {
                name = folderpath+'/'+name;
            }
            self.createFolder(name);
        })
    }

    /** upload a file to the model working directory */
    this.uploadFile = function() {
        // TODO: make this an AJAX call so we can updateListeners afterwards
        openmdao.Util.popupWindow('upload','Add File',150,400);
    }

    /** delete the file in the model working directory with the specified path */
    this.removeFile = function(filepath) {
        jQuery.ajax({
            type: 'DELETE',
            url:  'file'+filepath.replace(/\\/g,'/'),
            data: { 'file': filepath },
            success: self.updateListeners,
            error: function(jqXHR, textStatus, errorThrown) {
                        // not sure why this always returns a false error
                       debug.warn("model.removeFile",jqXHR,textStatus,errorThrown);
                       self.updateListeners();
                   }
            })
    }
    
    /** import the contents of the specified file into the model */
    this.importFile = function(filepath, callback, errorHandler) {
        // change path to package notation and import
        var path = filepath.replace(/\.py$/g,'').
                            replace(/\\/g,'.').
                            replace(/\//g,'.');
        self.issueCommand("from "+path+" import *", callback, errorHandler);
    }

    /** execute the model */
    this.runModel = function() {
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { },
            success: function(data, textStatus, jqXHR) {
                         self.issueCommand('print "'+data.replace('\n','\\n') +'"')
                     },
            error: function(jqXHR, textStatus, errorThrown) {
                       debug.error("Error running model (status="+jqXHR.status+"): "+jqXHR.statusText)
                       debug.error(jqXHR,textStatus,errorThrown)
                   }
        })
    }
    
    /** execute the specified file */
    this.execFile = function(filepath) {
        // convert to relative path with forward slashes
        var path = filepath.replace(/\\/g,'/')
        if (path[0] == '/')
            path = path.substring(1,path.length)

        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { 'filename': path },
            success: self.updateListeners,
        })
    }

    /** exit the model */
    this.exit = function() {
        jQuery.ajax({
            type: 'POST',
            url: 'exit',
        })
    }
    
}
