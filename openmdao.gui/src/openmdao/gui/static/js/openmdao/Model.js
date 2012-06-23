
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.Model=function() {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        outstream_opened = false,
        pubstream_opened = false,
        sockets = {},
        subscribers = {},
        windows = [];

    /** initialize a websocket
           url:        the URL of the address on which to open the websocket
           handler:    the message handler for the websocket
    */
    function open_websocket(url,handler) {
        // make ajax call (to url) to get the address of the websocket
        jQuery.ajax({
            type: 'GET',
            url:  url,
            success: function(addr) {
                sockets[url] = openmdao.Util.openWebSocket(addr,handler);
            },
            error: function(jqXHR, textStatus, err) {
                debug.error('Error getting websocket url',jqXHR,textStatus,err);
            }
        });
    }

    /** close all websockets */
    function close_websockets(reason) {
        jQuery.each(sockets,function(idx,socket) {
            socket.close(1000,reason);
        });
    }

    /** handle an output message, which is just passed on to all subscribers */
    function handleOutMessage(message) {
        var callbacks = subscribers.outstream;
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

    /** handle a published message, which has a topic
        the message is passed only to subscribers of that topic
    */
    function handlePubMessage(message) {
        if (typeof message === 'string' || message instanceof String) {
            try {
                message = jQuery.parseJSON(message);
            }
            catch(err) {
                debug.error('Model.handlePubMessage Error:',err,message);
            }
        }
        var topic = message[0],
            callbacks = subscribers[message[0]];
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
            subscribers[topic] = [ callback ];
        }
        if (topic === 'outstream' && !outstream_opened) {
            // if outstream socket is not opened yet, open it
            outstream_opened = true;
            open_websocket('outstream', handleOutMessage);
        }
        else {
            // if pubstream socket is not opened yet, open it
            if (!pubstream_opened) {
                pubstream_opened = true;
                open_websocket('pubstream', handlePubMessage);
            }
            // tell server there's a new subscriber to the topic
            if (topic.length > 0 && ! /.exec_state$/.test(topic)) {
                jQuery.ajax({
                    type: 'GET',
                    url:  'publish',
                    data: {'topic': topic, 'publish': true}
                });
            }
        }
    };

    /** remove a subscriber (i.e. a function to be called)
        for messages with the given topic
    */
    this.removeListener = function(topic, callback) {
        if (subscribers.hasOwnProperty(topic)) {
            var listeners = subscribers[topic];
            while (listeners.indexOf(callback) !== -1) {
              listeners.splice(listeners.indexOf(callback), 1);
            }
            // tell server there's one less subscriber to the topic
            if (topic.length > 0 && ! /.exec_state$/.test(topic)) {
                jQuery.ajax({
                    type: 'GET',
                    url:  'publish',
                    data: {'topic': topic, 'publish': false}
                });
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
            url:  'model'
        });
    };

    /** save the current project */
    this.saveProject = function(callback,errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'project',
            success: callback,
            error: errorHandler,
            complete: function(jqXHR, textStatus) {
                          if (typeof openmdao_test_mode !== 'undefined') {
                              openmdao.Util.notify('Save complete: ' +textStatus);
                          }
                      }
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
            // src and dst names are optional
            // (no src or dst means the src or dst is the assembly itself)
            var args = {};
            if (src_name) {
                args.src_name = src_name;
            }
            if (dst_name) {
                args.dst_name = dst_name;
            }
            jQuery.ajax({
                type: 'GET',
                url:  'connections/'+pathname,
                dataType: 'json',
                data: args,
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

        if (/driver/.test(typepath) && (openmdao.Util['$'+name])) {
            openmdao.Util['$'+name]();
            return;
        }

        jQuery.ajax({
            type: 'POST',
            url:  'component/'+name,
            data: {'type': typepath, 'parent': parent },
            success: function(text) {
                        if (typeof callback === 'function') {
                            callback(text);
                        }
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
    this.issueCommand = function(cmd, callback, errorHandler, completeHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'command',
            data: { 'command': cmd },
            success: function(txt) {
                        if (typeof callback === 'function') {
                            callback(txt);
                        }
                     },
            error: errorHandler,
            complete: completeHandler
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
    this.setFile = function(filepath, contents, callback, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+filepath.replace(/\\/g,'/'),
            data: { 'contents': contents},
            success: function(text) {
                        if (typeof callback === 'function') {
                            callback(text);
                        }
                     },
            error: errorHandler
        });
    };

    /** create new folder with  specified path in the model working directory */
    this.createFolder = function(folderpath, callback, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+folderpath.replace(/\\/g,'/'),
            data: { 'isFolder': true},
            success: function(text) {
                        if (typeof callback === 'function') {
                            callback(text);
                        }
                     },
            error: errorHandler
        });
    };

    /** create a new file in the model working directory with the specified path  */
    this.newFile = function(name, folderpath) {
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
    };

    /** prompt for name & create a new folder */
    this.newFolder = function(name, folderpath) {
            if (folderpath) {
                name = folderpath+'/'+name;
            }
            self.createFolder(name);
    };

    /** delete file with specified path from the model working directory */
    this.removeFile = function(filepath, callback) {
        jQuery.ajax({
            type: 'DELETE',
            url:  'file'+filepath.replace(/\\/g,'/'),
            data: { 'file': filepath },
            success: callback,
            error: function(jqXHR, textStatus, errorThrown) {
                        // not sure why this always returns a false error
                       debug.warn("model.removeFile",
                                  jqXHR,textStatus,errorThrown);
                   }
            });
    };

    /** import the contents of the specified file into the model */
    this.importFile = function(filepath, callback, errorHandler) {
        // change path to package notation and import
        var path = filepath.replace(/\.py$/g,'').
                            replace(/\\/g,'.').
                            replace(/\//g,'.');
        cmd = 'from '+path+' import *';
        self.issueCommand(cmd, callback, errorHandler, null);
    };

    /** execute the model */
    this.runModel = function() {
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { },
            success: function(jqXHR, textStatus) {
                         if (typeof openmdao_test_mode !== 'undefined') {
                             openmdao.Util.notify('Run complete: '+textStatus);
                         }
                      },
            error: function(jqXHR, textStatus, errorThrown) {
                       debug.error("Error running model (status="+jqXHR.status+"): "+jqXHR.statusText);
                       debug.error(jqXHR,textStatus,errorThrown);
                   }
        });
    };

    /** execute the specified file */
    this.execFile = function(filepath, callback) {
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
            success: function(text) {
                        if (typeof callback === 'function') {
                            callback(text);
                        }
                     }
        });
    };

    /** reload the model */
    this.reload = function() {
        openmdao.Util.closeWebSockets('reload');
        self.closeWindows();
        window.location.replace('/workspace/project');
    };

    /** exit the model */
    this.close = function() {
        openmdao.Util.closeWebSockets('close');
        self.closeWindows();
        window.location.replace('/workspace/close');
    };

    /** exit the model */
    this.exit = function() {
        openmdao.Util.closeWebSockets('exit');
        self.closeWindows();
        window.location.replace('/exit');
    };

    /** add window to window list. */
    this.addWindow = function(win) {
        if (! windows) {
            windows = [];
        }
        windows.push(win);
    };

    /** close all windows on the window list */
    this.closeWindows = function() {
        if ( windows) {
            for (i = 0; i < windows.length; i++) {
                windows[i].close();
            }
        }
    };

};

