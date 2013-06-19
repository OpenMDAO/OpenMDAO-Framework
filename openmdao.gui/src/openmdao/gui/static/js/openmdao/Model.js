
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.Model=function(listeners_ready) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        _modified = false,
        outstream_opened = false,
        pubstream_opened = false,
        sockets = {},
        subscribers = {},
        windows = [];

    this.model_ready = jQuery.Deferred();

    /** initialize a websocket
           stream_name: the name of the stream for which to open the websocket
           handler:     the message handler for the websocket
    */
    function open_websocket(stream_name, handler) {
        // make ajax call (to url) to get the address of the websocket
       return jQuery.ajax({ type: 'GET', url:  'stream/'+stream_name })
               .fail(function(jqXHR, textStatus, err) {
                   debug.error('Error getting websocket url',jqXHR,textStatus,err);
               })
               .pipe(function(addr) {
                   return openmdao.Util.openWebSocket(addr,handler);
               })
               .done(function(sock) {
                   sockets[stream_name] = sock;
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
                    debug.error('Model: invalid callback for output message:',
                                callbacks[i]);
                }
            }
        }
        else {
            debug.info("no callbacks for out message:",message);
        }
    }

    /** handle a published message, which has a topic
        the message is passed only to subscribers of that topic
    */
    function handlePubMessage(message) {
        if (typeof message === 'string' || message instanceof String) {
            try {
                message = jQuery.parseJSON(message);
                self.publish(message);
            }
            catch(err) {
                debug.error('Model.handlePubMessage Error:',err,message);
            }
        }
        else {
            // binary message, assume it uses our simple framing protocol:
            //      msg starts with a null padded routing string of size
            //      openmdao.NAME_SIZE, followed by the actual binary msg
            var namearr = new Uint8Array(message, 0, openmdao.NAME_SIZE-1),
                name = String.fromCharCode.apply(null, namearr),
                idx = name.indexOf("\0");
            if (idx > 0) {
                name = name.substr(0, idx);
            }
            self.publish([name, message]);  // send the whole message to save on some copying later...
        }
    }

    var ws_ready = jQuery.when(open_websocket('out', handleOutMessage),
                               open_websocket('pub', handlePubMessage));

    if (! listeners_ready) { // to keep js_unit_test from failing
        listeners_ready = jQuery.Deferred();
        listeners_ready.resolve();
    }

    // this makes project loading wait until after the listeners have
    // been registered and the websockets opened
    jQuery.when(ws_ready, listeners_ready).done(function() {
        jQuery.ajax({ type: 'GET', url: 'project_load' })
        .done(function() {
             self.model_ready.resolve();
        })
        .fail(function() {
             self.model_ready.reject();
        });
    });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** add a subscriber (i.e. a function to be called)
        for messages with the given topic.
        Topics beginning with '@' are for messaging within the GUI.
    */
    this.addListener = function(topic, callback) {
        //console.debug("added a listener for topic "+topic);
        if (subscribers.hasOwnProperty(topic)) {
            subscribers[topic].push(callback);
        }
        else {
            subscribers[topic] = [callback];
        }
        // tell server there's a new subscriber to the topic
        if (topic !== 'outstream' && topic.length > 0 &&
            ! /.exec_state$/.test(topic) && topic.charAt(0) !== '@') {
            jQuery.ajax({
                type: 'GET',
                url:  'subscription/'+encodeURIComponent(topic)
            });
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
            if (topic.length > 0 && ! /.exec_state$/.test(topic) &&
                topic.charAt(0) !== '@') {
                jQuery.ajax({
                    type: 'DELETE',
                    url:  'subscription/'+encodeURIComponent(topic)
                });
            }
        }
    };

    /** publish message to subscribed listeners. */
    this.publish = function(message) {
        var topic = message[0],
            callbacks,
            i;
        if (subscribers.hasOwnProperty(topic) && subscribers[topic].length > 0) {
            // Need a copy in case subscriber removes itself during callback.
            callbacks = subscribers[topic].slice();
            for (i = 0; i < callbacks.length; i++) {
                if (typeof callbacks[i] === 'function') {
                    callbacks[i](message);
                }
                else {
                    debug.error('Model.publish: invalid callback for topic:',
                                topic, callbacks[i]);
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

    /** get constructor signature for a type */
    this.getSignature = function(typename, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        jQuery.ajax({
            type: 'GET',
            url:  'type/'+typename+'/signature',
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

    /** I split this function out from commit so I could call it directly
       from js_unit_test */
    this.commit_with_comment = function(comment) {
        defrd = jQuery.ajax({
            type: 'POST',
            url:  'project',
            data: { 'comment': comment },
            complete: function(jqXHR, textStatus) {
                          if (typeof openmdao_test_mode !== 'undefined') {
                              openmdao.Util.notify('Commit complete: ' +textStatus);
                          }
                      }
        });
        self.setModified(false);
        return defrd.promise();
    };

    /** commit the current project to the repository (after supplying a comment)*/
    this.commit = function(callback, errorHandler) {
        openmdao.Util.promptForValue("Enter a commit comment", self.commit_with_comment);
    };

    /** revert back to the most recent commit of the project */
    this.revert = function(errorHandler) {
        openmdao.Util.confirm("Remove all uncommitted changes?",
            function() {
                jQuery.ajax({
                    type: 'POST',
                    url:  'project_revert',
                    success: function(data, textStatus, jqXHR) {
                        self.reload();
                    },
                    error: errorHandler,
                    complete: function(jqXHR, textStatus) {
                                  if (typeof openmdao_test_mode !== 'undefined') {
                                      openmdao.Util.notify('Revert complete: ' +textStatus);
                                  }
                              }
                });
                self.setModified(false);
        });
    };

    /** get list of components in the top driver workflow */
    this.getWorkflow = function(pathname, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            if (!pathname) {
                pathname = 'None';
            }
            jQuery.ajax({
                type: 'GET',
                url:  'object/'+pathname+'/workflow',
                dataType: 'json',
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get the structure (data flow)) of an assembly */
    this.getDataflow = function(pathname, callback ,errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            if (!pathname) {
                pathname = 'None';
            }
            jQuery.ajax({
                type: 'GET',
                url:  'object/'+pathname+'/dataflow',
                dataType: 'json',
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get  hierarchical list of components */
    this.getComponents = function(callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'objects',
                dataType: 'json',
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get the inputs and outputs of the assembly's child components and
        an indicator for each whether or not it is a passthrough variable */
    this.getPassthroughs = function(pathname, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'object/'+pathname+'/passthroughs',
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get  attributes of any slotable object */
    this.getObject = function(name, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'object/'+name,
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get all available events in a workflow */
    this.getAvailableEvents = function(pathname, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'object/'+pathname+'/events',
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            });
        }
    };


    /** get value for pathname */
    this.getValue = function(pathname, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'value/'+pathname,
                success: callback,
                error: errorHandler
            });
        }
    };

    /** get connections between two components in an assembly */
    this.getConnections = function(pathname, src_name, dst_name, callback, errorHandler) {
        if (typeof callback !== 'function') {
            return;
        }
        else {
            // src and dst names are optional
            // (no src or dst means the src or dst is the assembly itself)
            var args = {};
            if (src_name) {
                args.source = src_name;
            }
            if (dst_name) {
                args.target = dst_name;
            }
            jQuery.ajax({
                type: 'GET',
                url:  'object/'+pathname+'/connections',
                dataType: 'json',
                data: args,
                success: callback,
                error: errorHandler
            });
        }
    };

    /** create or replace object with pathname with a new object of the specified type */
    this.putObject = function(pathname, typepath, args, callback, errorHandler) {
        jQuery.ajax({
            type: 'PUT',
            url:  'object/'+pathname,
            data: {'type': typepath, 'args': args},
            success: callback,
            error: errorHandler
        });
        self.setModified(true);
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
        self.setModified(true);
    };

    /** issue the specified command against the model */
    this.issueCommand = function(cmd, callback, errorHandler, completeHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'command',
            data: { 'command': cmd },
            success: callback,
            error: errorHandler,
            complete: completeHandler
        });
        self.setModified(true);
    };

    /** set the value of the variable with to rhs */
    this.setVariableValue = function(pathname, rhs, type, callback, errorHandler, completeHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'variable/'+encodeURIComponent(pathname),   // escape any brackets, etc.
            data: {
                'rhs': rhs,
                'type': type   // need to know if it's a string vs. an expression
            },
            success: callback,
            error: errorHandler,
            complete: completeHandler
        });
        self.setModified(true);
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
            success: callback,
            error: errorHandler
        });
    };

    /** set the contents of the specified file */
    this.setFile = function(filepath, contents, force, callback, errorHandler, handler409) {
        jQuery.ajax({
            type: 'PUT',
            url:  'file/'+filepath.replace(/\\/g,'/'),
            data: { 'contents': contents, 'force': force },
            success: callback,
            error: errorHandler,
            statusCode: {
                409: handler409
             }
        });
        self.setModified(true);
    };

    /** create new folder with specified path in the model working directory */
    this.createFolder = function(folderpath, callback, errorHandler) {
        jQuery.ajax({
            type: 'PUT',
            url:  'file/'+folderpath.replace(/\\/g,'/'),
            data: { 'isFolder': true},
            success: callback,
            error: errorHandler
        });
        self.setModified(true);
    };

    /** create a new file in the model working directory with the specified path  */
    this.newFile = function(name, folderpath, callback) {
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
            self.setFile(name, contents, undefined, callback);
            self.setModified(true);
    };

    /** prompt for name & create a new folder */
    this.newFolder = function(name, folderpath) {
            if (folderpath) {
                name = folderpath+'/'+name;
            }
            self.createFolder(name);
            self.setModified(true);
    };

    /** rename file with specified path. */
    this.renameFile = function(filepath, newname, callback) {
        // convert to relative path with forward slashes
        var path = filepath.replace(/\\/g,'/');
        if (path[0] === '/') {
            path = path.substring(1, path.length);
        }
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+path,
            data: { 'rename': newname },
            success: callback,
            error: function(jqXHR, textStatus, errorThrown) {
                       debug.warn("model.renameFile",
                                  jqXHR, textStatus, errorThrown);
                   }
        });
        self.setModified(true);
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
            self.setModified(true);
    };

    /** delete files with specified path from the model working directory */
    this.removeFiles = function(filepaths, callback) {

        jQuery.ajax({
            type: 'DELETE',
            url:  'files',
            data: JSON.stringify( { 'filepaths': filepaths } ),
            contentType: 'application/json; charset=utf-8',
            dataType: 'json',
            success: callback,
            error: function(jqXHR, textStatus, errorThrown) {
                        // not sure why this always returns a false error
                       debug.warn("model.removeFiles",
                                  jqXHR,textStatus,errorThrown);
                   }
            });
            self.setModified(true);
    };

    /** execute a component */
    this.runComponent = function(pathname) {
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'object/'+pathname,
            success: function(data, textStatus, jqXHR) {
                         if (typeof openmdao_test_mode !== 'undefined') {
                             openmdao.Util.notify('Run complete: '+textStatus);
                         }
                      },
            error: function(jqXHR, textStatus, errorThrown) {
                       debug.error("Error running component (status="+jqXHR.status+"): "+jqXHR.statusText);
                       debug.error(jqXHR,textStatus,errorThrown);
                   }
        });
        self.setModified(true);
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
            url:  'file/'+path,
            success: callback
        });
        self.setModified(true);
    };

    /** reload the model */
    this.reload = function() {
        self.setModified(false);
        openmdao.Util.closeWebSockets('reload');
        self.closeWindows();
        window.location.replace('/workspace/project');
    };

    /** close the model */
   this.close = function() {
       openmdao.Util.closeWebSockets('close');
       self.closeWindows();
       window.location.replace('/workspace/close');
   };

   /** exit the gui */
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
        if ( windows ) {
            for (i = 0; i < windows.length; i++) {
                windows[i].close();
            }
        }
    };

    /** return if the model has changed since last save */
    this.getModified = function(){
        return _modified;
    };

    /** Set '_modified' flag and publish to '@model-modified' topic. */
    this.setModified = function(value) {
        _modified = value;
        this.publish(['@model-modified', _modified]);
    };

};

