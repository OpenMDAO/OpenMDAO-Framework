
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.Model=function(listeners_ready) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var _self = this,
        _modified = false,
        _windows = [],
        _websockets = [],
        _subscribers = {};

    _self.model_ready = jQuery.Deferred();

    /** close all windows associated with this model */
    function closeWindows() {
        if (_windows) {
            for (i=0; i<_windows.length; i++) {
                _windows[i].close();
            }
        }
    }

    /** connect to websocket at specified address */
    function openWebSocket(addr, handler, errHandler, retry, delay) {
        // if retry is true and connection fails, try again to connect after delay
        retry = typeof retry !== 'undefined' ? retry : true;
        delay = typeof delay !== 'undefined' ? delay : 2000;

        var socket = null,
            defrd = jQuery.Deferred();

        function connect_after_delay() {
            tid = setTimeout(connect, delay);
        }

        function displaySockets() {
            debug.info('WebSockets:');
            for (var i=0 ; i<_websockets.length; ++i) {
                debug.info('    '+i+': state '+_websockets[i].readyState);
            }
        }

        function connect() {
            if (socket === null || socket.readyState > 0) {
                socket = new WebSocket(addr);
                socket.binaryType = "arraybuffer"; // when binary msgs are received, treat as ArrayBuffers
                _websockets.push(socket);
                socket.onopen = function (e) {
                    defrd.resolve(socket);
                    //debug.info('websocket opened '+socket.readyState,socket,e); displaySockets();
                };
                socket.onclose = function (e) {
                    //debug.info('websocket closed',socket,e); displaySockets();
                    index = _websockets.indexOf(this);
                    if (index >= 0) {
                        _websockets.splice(index, 1);
                        if (typeof openmdao_test_mode !== 'undefined') {
                            if (_websockets.length === 0) {
                                openmdao.Util.notify('WebSockets closed',
                                                     'closed', 'ws_closed');
                            }
                        }
                    }
                    else {
                        debug.info('websocket not found!');
                    }
                    if ((e.code === 1006) && (retry === true)) {
                        // See RFC 6455 for error code definitions.
                        connect_after_delay();
                    }
                };
                socket.onmessage = function(e) {
                    //debug.info('websocket message',socket,e);
                    handler(e.data);
                };

                socket.onerror = function (e) {
                    if (typeof errHandler === 'function') {
                        errHandler(e);
                    }
                    else {
                        debug.error('websocket error',socket,e);
                    }
                };
            }
        }

        connect();
        /*
        debug.info('websocket connected');
        debug.info('addr='+addr);
        debug.info('retry='+retry);
        debug.info('delay='+delay);
        debug.info('handler:');
        debug.info(handler);
        debug.info('errhandler:');
        debug.info(errHandler);
        */
        return defrd.promise();
    }

    /** Close all WebSockets. */
    function closeWebSockets(reason) {
        var i;
        if (_websockets) {
            for (i = 0 ; i < _websockets.length ; ++i) {
             _websockets[i].close(1000, reason);
            }
        }
    }

    /** initialize a message stream
           stream_name: the name of the stream
           handler:     the message handler for the stream
    */
    function openStream(stream_name, handler) {
        // make ajax call (to url) to get the address of the websocket
        return  jQuery.ajax({
                    type: 'GET',
                    url:  'stream/'+stream_name
                })
                .fail(function(jqXHR, textStatus, err) {
                    debug.error('Error getting websocket url', jqXHR ,textStatus, err);
                })
                .pipe(function(addr) {
                    return openWebSocket(addr, handler);
                });
    }

    /** publish message to subscribed listeners. */
    function publish(message) {
        var topic = message[0],
            callbacks;
        if (_subscribers.hasOwnProperty(topic) && _subscribers[topic].length > 0) {
            // Need a copy in case subscriber removes itself during callback.
            callbacks = _subscribers[topic].slice();
            for (var i=0; i<callbacks.length; i++) {
                if (typeof callbacks[i] === 'function') {
                    callbacks[i](message);
                }
                else {
                    debug.error('Model.publish: invalid callback for topic:',
                                topic, callbacks[i]);
                }
            }
        }
    }

    /** Set 'modified' flag and publish to '@model-modified' topic. */
    function setModified(value) {
        _modified = value;
        publish(['@model-modified', _modified]);
    };

    /** handle an output message, which is just passed on to all _subscribers */
    function handleOutMessage(message) {
        var callbacks = _subscribers.outstream;
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
            debug.info("no callbacks for out message:", message);
        }
    }

    /** handle a published message, which has a topic
        the message is passed only to _subscribers of that topic
    */
    function handlePubMessage(message) {
        if (typeof message === 'string' || message instanceof String) {
            try {
                message = jQuery.parseJSON(message);
                publish(message);
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
            publish([name, message]);  // send the whole message to save on some copying later...
        }
    }

    var ws_ready = jQuery.when(openStream('out', handleOutMessage),
                               openStream('pub', handlePubMessage));

    if (! listeners_ready) { // to keep js_unit_test from failing
        listeners_ready = jQuery.Deferred();
        listeners_ready.resolve();
    };

    // this makes project loading wait until after the listeners have
    // been registered and the websockets opened
    jQuery.when(ws_ready, listeners_ready).done(function() {
        jQuery.ajax({ type: 'GET', url: 'project_load' })
              .done(function() { _self.model_ready.resolve(); })
              .fail(function() { _self.model_ready.reject();  });
    });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** add a listener (i.e. a function to be called)
        for messages with the given topic.
        Topics beginning with '@' are for messaging within the GUI.
    */
    this.addListener = function(topic, callback) {
        //console.debug("added a listener for topic "+topic);
        if (_subscribers.hasOwnProperty(topic)) {
            _subscribers[topic].push(callback);
        }
        else {
            _subscribers[topic] = [callback];
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

    /** remove a listener (i.e. a function to be called)
        for messages with the given topic
    */
    this.removeListener = function(topic, callback) {
        if (_subscribers.hasOwnProperty(topic)) {
            var listeners = _subscribers[topic];
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

    /** get the list of object types that are available for creation */
    this.getTypes = function() {
        return jQuery.ajax({
            type: 'GET',
            url:  'types',
            dataType: 'json',
        })
    };

    /** get constructor signature for a type */
    this.getSignature = function(typename, callback, errorHandler) {
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
        return jQuery.ajax({
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
        setModified(false);
        return defrd.promise();
    };

    /** commit the current project to the repository (after supplying a comment)*/
    this.commit = function() {
        openmdao.Util.promptForValue("Enter a commit comment", _self.commit_with_comment);
    };

    /** revert back to the most recent commit of the project */
    this.revert = function(errorHandler) {
        openmdao.Util.confirm("Remove all uncommitted changes?",
            function() {
                jQuery.ajax({
                    type: 'POST',
                    url:  'project_revert',
                    success: function(data, textStatus, jqXHR) {
                        _self.reload();
                    },
                    error: errorHandler,
                    complete: function(jqXHR, textStatus) {
                                  if (typeof openmdao_test_mode !== 'undefined') {
                                      openmdao.Util.notify('Revert complete: ' +textStatus);
                                  }
                              }
                });
                setModified(false);
        });
    };

    /** get list of components in the top driver workflow */
    this.getWorkflow = function(pathname) {
        if (!pathname) {
            pathname = 'None';
        }
        return jQuery.ajax({
            type: 'GET',
            url:  'object/'+pathname+'/workflow',
            dataType: 'json',
        });
    };

    /** get the structure (data flow)) of an assembly */
    this.getDataflow = function(pathname) {
        if (!pathname) {
            pathname = 'None';
        }
        return jQuery.ajax({
            type: 'GET',
            url:  'object/'+pathname+'/dataflow',
            dataType: 'json',
        });
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
        setModified(true);
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
        _self.issueCommand(cmd);
        setModified(true);
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
        setModified(true);
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
        setModified(true);
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
        setModified(true);
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
        setModified(true);
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
            _self.setFile(name, contents, undefined, callback);
            setModified(true);
    };

    /** prompt for name & create a new folder */
    this.newFolder = function(name, folderpath) {
            if (folderpath) {
                name = folderpath+'/'+name;
            }
            _self.createFolder(name);
            setModified(true);
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
        setModified(true);
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
            setModified(true);
    };

    /** delete files with specified path from the model working directory */
    this.removeFiles = function(filepaths, callback) {
        jQuery.ajax({
            type: 'DELETE',
            url:  'files',
            data: JSON.stringify({'filepaths': filepaths}),
            contentType: 'application/json; charset=utf-8',
            success: callback,
            error: function(jqXHR, textStatus, errorThrown) {
                debug.error("model.removeFiles", jqXHR, textStatus, errorThrown);
            }
        });
        setModified(true);
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
        setModified(true);
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
        setModified(true);
    };

    /** reload the model */
    this.reload = function() {
        setModified(false);
        closeWebSockets('reload');
        closeWindows();
        window.location.replace('/workspace/project');
    };

    /** close the model */
   this.close = function() {
       closeWebSockets('close');
       closeWindows();
       window.location.replace('/workspace/close');
   };

   /** exit the gui */
   this.exit = function() {
       closeWebSockets('exit');
       closeWindows();
       window.location.replace('/exit');
   };

    /** add window to the list of windows accociated with this model. */
    this.addWindow = function(win) {
        if (! _windows) {
            _windows = [];
        }
        _windows.push(win);
    };

    /** edit file associated with this model */
    this.editFile = function(filename) {
        if (_self.codeEditor) {
            _self.codeEditor.editFile(filename);
        }
        else {
            var url = 'tools/editor';
            if (filename) {
                url += '?filename='+filename;
            }
            w = openmdao.Util.popupWindow(url, 'Code Editor');
            _self.addWindow(w);
        }
    };

    /** view geometry associated with this model */
    this.viewGeometry = function(pathname) {
        function popupGeom(pathname) {
            w = openmdao.Util.popupWindow('tools/geometry?path='+pathname,
                                          'Geometry of '+pathname);
            _self.addWindow(w);
        }
        if (typeof pathname === "undefined" || !pathname) {
            openmdao.Util.promptForValue('Enter pathname of geometry object to view:',
                                           popupGeom);
        }
        else {
            popupGeom(pathname);
        }
    };

    /** For functional testing: Notify when `nSockets` are open. */
    this.webSocketsReady = function(nSockets) {
        function doPoll() {
            setTimeout(poll, 500);
        }

        function poll() {
            var i;
            if (_websockets.length >= nSockets) {
                for (i = 0 ; i < _websockets.length ; ++i) {
                    if (_websockets[i].readyState !== 1) {
                        doPoll();
                        return;
                    }
                }
                openmdao.Util.notify('WebSockets open', 'open',
                                      'ws_open');
            }
            else {
                doPoll();
            }
        }
        poll();
    }

    /** For functional testing: Close all WebSockets. */
    this.closeWebSockets = function(reason) {
        closeWebSockets(reason);
    }
};

