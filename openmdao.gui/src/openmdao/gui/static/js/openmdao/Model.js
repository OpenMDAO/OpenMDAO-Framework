
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.Model=function() {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        outstream_topic = 'outstream',
        outstream_opened = false,
        outstream_socket = null,
        pubstream_opened = false,
        pubstream_socket = null,
        subscribers = {};

    /** initialize a websocket
           url:        the URL of the address on which to open the websocket
           handler:    the message handler for the websocket
    */
    function open_websocket(url,socket,handler) {
        // make ajax call to get outstream websocket
        jQuery.ajax({
            type: 'GET',
            url:  url,
            success: function(addr) {
                socket = openmdao.Util.openWebSocket(addr,handler);
            },
            error: function(jqXHR, textStatus, err) {
                debug.error('Error getting websocket url',jqXHR,textStatus,err);
            }
        });
    }

    /** initialize the outstream websocket */
    function open_outstream_socket(topic) {
        open_websocket('outstream', this.outstream_socket,
            function(data) {
                var callbacks = subscribers[topic];
                if (callbacks) {
                    for (i = 0; i < callbacks.length; i++) {
                        if (typeof callbacks[i] === 'function') {
                            callbacks[i](data);
                        }
                        else {
                            debug.error('Model: invalid callback for topic:',
                                        topic,callbacks[i]);
                        }
                    }
                }
            }
        );
    }

    /** initialize the publisher websocket */
    function open_pubstream_socket() {
        open_websocket('pubstream', this.pubstream_socket,
            function(message) {
                message = jQuery.parseJSON(message);
                var callbacks = subscribers[message[0]];
                if (callbacks) {
                    for (i = 0; i < callbacks.length; i++) {
                        if (typeof callbacks[i] === 'function') {
                            callbacks[i](message);
                        }
                        else {
                            debug.error('Model: invalid callback for topic:',
                                        topic,callbacks[i]);
                        }
                    }
                }
            }
        );
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** add a subscriber (i.e. a function to be called)
        for messages with the given topic
    */
    this.addListener = function(topic, callback) {
        if (subscribers.hasOwnProperty(topic)) {
            subscribers[topic].push(callback);
        }
        else {
            if (topic === outstream_topic && !outstream_opened) {
                outstream_opened = true;
                open_outstream_socket(outstream_topic);
            }
            else {
                if (!pubstream_opened) {
                    pubstream_opened = true;
                    open_pubstream_socket();
                }
                if (topic.length > 0 && ! /.exec_state$/.test(topic)) {
                    jQuery.ajax({
                        type: 'GET',
                        url:  'publish',
                        dataType: 'json',
                        data: {'topic': topic}
                    });
                }
            }
            subscribers[topic] = [ callback ];
        }
    };

   /** notify all generic listeners that something may have changed  */
    this.updateListeners = function() {
        //debug.info('updateListeners',subscribers)
        var callbacks = subscribers[''];
        //debug.info('updateListeners',callbacks)
        if (callbacks) {
            for (i = 0; i < callbacks.length; i++) {
                if (typeof callbacks[i] === 'function') {
                    callbacks[i]();
                }
                else {
                    debug.error('Model: invalid callback for topic:',
                                topic,callbacks[i]);
                }
            }
        }
    };

    /** get the list of object types that are available for creation */
    this.getTypes = function(callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }

        jQuery.ajax({
            type: 'GET',
            url:  'types',
            dataType: 'json',
            success: callback,
            error: errorHandler
        });
    };

    /** get a new (empty) model */
    this.newModel = function() {
        jQuery.ajax({
            type: 'POST',
            url:  'model',
            success: self.updateListeners
        });
    };

    /** save the current project */
    this.saveProject = function(callback,errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'project',
            success: callback,
            error: errorHandler
        });
    };

    /** get list of components in the top driver workflow */
    this.getWorkflow = function(pathname,callback,errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'workflow/'+pathname,
                dataType: 'json',
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get the structure (data flow)) of an assembly */
    this.getDataflow = function(pathname,callback,errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            if (!pathname) {
                pathname = '';
            }
            jQuery.ajax({
                type: 'GET',
                url:  'dataflow/'+pathname,
                dataType: 'json',
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get  hierarchical list of components*/
    this.getComponents = function(callback,errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'components',
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get  attributes of a component*/
    this.getComponent = function(name,callback,errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'component/'+name,
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get connections between two components in an assembly */
    this.getConnections = function(pathname,src_name,dst_name,callback,errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'connections/'+pathname,
                dataType: 'json',
                data: { 'src_name': src_name, 'dst_name': dst_name },
                success: callback,
                error: errorHandler
            });
        }
    };

    /** set connections between two components in an assembly */
    this.setConnections = function(pathname,src_name,dst_name,connections,callback,errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'connections/'+pathname,
            dataType: 'json',
            data: { 'src_name': src_name, 'dst_name': dst_name,
                    'connections': connections },
            success: callback,
            error: errorHandler
        });
    };

    /** add an object of the specified type & name to the specified parent */
    this.addComponent = function(typepath,name,parent,callback) {
        if (!parent) {
            parent = '';
        }

        if (/driver/.test(typepath)&&(openmdao.Util['$'+name])){openmdao.Util['$'+name]();return;}

        jQuery.ajax({
            type: 'POST',
            url:  'component/'+name,
            data: {'type': typepath, 'parent': parent },
            success: function(text) {
                        if (typeof callback === 'function') {
                            callback(text);
                        }
                        self.updateListeners();
            }
        });
    };

    /** remove the component with the given pathname */
    this.removeComponent = function(pathname) {
        var parent = openmdao.Util.getPath(pathname);
        if (parent.length > 0 ) {
            cmd = parent+'.remove("'+openmdao.Util.getName(pathname)+'")';
        }
        else {
            cmd = 'del('+openmdao.Util.getName(pathname)+')';
        }
        self.issueCommand(cmd);
    };

    /** issue the specified command against the model */
    this.issueCommand = function(cmd, callback, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'command',
            data: { 'command': cmd },
            success: function(txt) {
                        if (typeof callback === 'function') {
                            callback(txt);
                        }
                        self.updateListeners();
                     },
            error: errorHandler
        });
    };

    /** get any queued output from the model */
    this.getOutput = function(callback, errorHandler) {
        jQuery.ajax({
            url: 'output',
            success: function(text) {
                        if (typeof callback === 'function') {
                            callback(text);
                        }
                     },
            error: errorHandler
        });
    };

    /** get a recursize file listing of the model working directory (as JSON) */
    this.getFiles = function(callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }

        jQuery.ajax({
            type: 'GET',
            url:  'files',
            dataType: 'json',
            data: {},
            success: callback,
            error: errorHandler
        });
    };

    /** get the contents of the specified file */
    this.getFile = function(filepath, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }

        jQuery.ajax({
            type: 'GET',
            url:  'file'+filepath.replace(/\\/g,'/'),
            dataType: 'text',
            success: callback,
            error: errorHandler
        });
    };

    /** set the contents of the specified file */
    this.setFile = function(filepath, contents, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+filepath.replace(/\\/g,'/'),
            data: { 'contents': contents},
            success: self.updateListeners,
            error: errorHandler
        });
    };

    /** create new folder with  specified path in the model working directory */
    this.createFolder = function(folderpath, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+folderpath.replace(/\\/g,'/'),
            data: { 'isFolder': true},
            success: self.updateListeners,
            error: errorHandler
        });
    };

    /** create new file with  specified path in the model working directory */
    this.newFile = function(folderpath) {
        openmdao.Util.promptForValue('Specify a name for the new file',
            function(name) {
                if (folderpath) {
                    name = folderpath+'/'+name;
                }
                var contents = '';
                if (/.py$/.test(name)) {
                    contents = '"""\n   '+name+'\n"""\n\n';
                }
                if (/.json$/.test(name)) {
                    contents = '[]';
                }
                self.setFile(name,contents);
            }
        );
    };

    /** prompt for name & create a new folder */
    this.newFolder = function(folderpath) {
        openmdao.Util.promptForValue('Specify a name for the new folder',
            function(name) {
                if (folderpath) {
                    name = folderpath+'/'+name;
                }
                self.createFolder(name);
            }
        );
    };

    /** upload a file to the model working directory */
    this.uploadFile = function() {
        // TODO: make this an AJAX call so we can updateListeners afterwards
        openmdao.Util.popupWindow('upload','Add File',150,400);
    };

    /** delete file with specified path from the model working directory */
    this.removeFile = function(filepath) {
        jQuery.ajax({
            type: 'DELETE',
            url:  'file'+filepath.replace(/\\/g,'/'),
            data: { 'file': filepath },
            success: self.updateListeners,
            error: function(jqXHR, textStatus, errorThrown) {
                        // not sure why this always returns a false error
                       debug.warn("model.removeFile",
                                  jqXHR,textStatus,errorThrown);
                       self.updateListeners();
                   }
            });
    };

    /** import the contents of the specified file into the model */
    this.importFile = function(filepath, callback, errorHandler) {
        // change path to package notation and import
        var path = filepath.replace(/\.py$/g,'').
                            replace(/\\/g,'.').
                            replace(/\//g,'.');
        self.issueCommand("from "+path+" import *", callback, errorHandler);
    };

    /** execute the model */
    this.runModel = function() {
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { },
            success: function(data, textStatus, jqXHR) {
                         cmd = 'print "'+data.replace('\n','\\n') +'"';
                         self.issueCommand(cmd);
                     },
            error: function(jqXHR, textStatus, errorThrown) {
                       debug.error("Error running model " +
                                   "(status=" + jqXHR.status+"): " +
                                   jqXHR.statusText);
                       debug.error(jqXHR,textStatus,errorThrown);
                   }
        });
    };

    /** execute the specified file */
    this.execFile = function(filepath) {
        // convert to relative path with forward slashes
        var path = filepath.replace(/\\/g,'/');
        if (path[0] === '/') {
            path = path.substring(1,path.length);
        }
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { 'filename': path },
            success: self.updateListeners
        });
    };

    /** reload the model */
    this.reload = function() {
        if (this.outstream_socket) {
            this.outstream_socket.close(1000,'reload');
        }
        if (this.pubstream_socket) {
            this.pubstream_socket.close(1000,'reload');
        }
        window.location.replace('/workspace/project');
    };

    /** exit the model */
    this.close = function() {
        if (this.outstream_socket) {
            this.outstream_socket.close(1000,'close');
        }
        if (this.pubstream_socket) {
            this.pubstream_socket.close(1000,'close');
        }
        window.location.replace('/workspace/close');
    };

    /** exit the model */
    this.exit = function() {
        if (this.outstream_socket) {
            this.outstream_socket.close(1000,'exit');
        }
        if (this.pubstream_socket) {
            this.pubstream_socket.close(1000,'exit');
        }
        window.location.replace('/exit');
    };

};
