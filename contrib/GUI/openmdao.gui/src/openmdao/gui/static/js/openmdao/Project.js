
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.Project=function(listeners_ready) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var _self = this,
        _modified = false,
        _windows = [],
        _websockets = [],
        _subscribers = {};

    _self.project_ready = jQuery.Deferred();

    /** close all windows associated with this project */
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
                socket.onopen = function(e) {
                    defrd.resolve(socket);
                    //debug.info('websocket opened '+socket.readyState,socket,e); displaySockets();
                };
                socket.onclose = function(e) {
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
                    // debug.info('websocket message', socket, e);
                    handler(e.data);
                };

                socket.onerror = function(e) {
                    if (typeof errHandler === 'function') {
                        errHandler(e);
                    }
                    else {
                        debug.error('websocket error', socket, e);
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
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'stream/'+stream_name
                    })
                    .fail(function(jqXHR, textStatus, err) {
                        debug.error('Error getting websocket url', jqXHR ,textStatus, err);
                    })
                    .pipe(function(addr) {
                        return openWebSocket(addr, handler);
                    });
        return jqXHR.promise();
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
                    debug.error('Project.publish: invalid callback for topic:',
                                topic, callbacks[i]);
                }
            }
        }
    }

    /** Set 'modified' flag and publish to '@project-modified' topic. */
    function setModified(value) {
        if (value !== _modified) {
            _modified = value;
            publish(['@project-modified', _modified]);
        }
    }

    /** handle an output message, which is just passed on to all subscribers */
    function handleOutMessage(message) {
        var callbacks = _subscribers.outstream;
        if (callbacks) {
            for (i = 0; i < callbacks.length; i++) {
                if (typeof callbacks[i] === 'function') {
                    callbacks[i](message);
                }
                else {
                    debug.error('Project: invalid callback for output message:',
                                callbacks[i]);
                }
            }
        }
        else {
            debug.warn("no callbacks for out message:", message);
        }
    }

    /** handle a published message, which has a topic
        the message is passed only to subscribers of that topic
    */
    function handlePubMessage(message) {
        if (typeof message === 'string' || message instanceof String) {
            try {
                message = jQuery.parseJSON(message);
                publish(message);
            }
            catch(err) {
                debug.error('Project.handlePubMessage Error:',err,message);
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
    }

    // this makes project loading wait until after the listeners have
    // been registered and the websockets opened
    jQuery.when(ws_ready, listeners_ready).done(function() {
        jQuery.ajax({ type: 'POST', url: 'project', data: {'action': 'load'} })
              .done(function() { _self.project_ready.resolve(); })
              .fail(function() { _self.project_ready.reject();  });
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
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'types',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get constructor signature for a type */
    this.getSignature = function(typename) {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'type/'+typename+'/signature',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** commit project to the repository with comment */
    this.commit_with_comment = function(comment) {
        jQuery.ajax({
            type: 'POST',
            url:  'project',
            data: { 'action': 'commit', 'comment': comment },
            complete: function(jqXHR, textStatus) {
                          if (typeof openmdao_test_mode !== 'undefined') {
                              openmdao.Util.notify('Commit complete: ' +textStatus);
                          }
                      }
        });
        setModified(false);
    };

    /** commit project to the repository after prompting for comment */
    this.commit = function() {
        openmdao.Util.promptForValue("Enter a commit comment", _self.commit_with_comment);
    };

    /** revert back to the most recent commit of the project */
    this.revert = function(errorHandler) {
        var confirmation = openmdao.Util.confirm("Remove all uncommitted changes");
        confirmation.done(function() {
            jqXHR = jQuery.ajax({
                type: 'POST',
                url:  'project',
                data: {'action': 'revert'}
            })
            .done(function(data, textStatus, jqXHR) {
                _self.reload();
            })
            .fail(errorHandler)
            .always(function(jqXHR, textStatus) {
                  if (typeof openmdao_test_mode !== 'undefined') {
                      openmdao.Util.notify('Revert complete: ' +textStatus);
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
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'object/'+pathname+'/workflow',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get the structure (data flow)) of an assembly */
    this.getDataflow = function(pathname) {
        if (!pathname) {
            pathname = 'None';
        }
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'object/'+pathname+'/dataflow',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get  hierarchical list of components */
    this.getComponents = function() {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'objects',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get the inputs and outputs of the assembly's child components and
        an indicator for each whether or not it is a passthrough variable */
    this.getPassthroughs = function(pathname) {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'object/'+pathname+'/passthroughs',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get  attributes of an object */
    this.getObject = function(name) {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'object/'+name,
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get all available events in a workflow */
    this.getAvailableEvents = function(pathname) {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'object/'+pathname+'/events',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get value for pathname */
    this.getValue = function(pathname) {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'value/'+pathname,
                        success: callback,
                        error: errorHandler
                    });
        return jqXHR.promise();
    };

    /** get connections within an assembly */
    this.getConnectivity = function(pathname) {
        // src and dst names are optional
        // (no src or dst means the src or dst is the assembly itself)
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'object/'+pathname+'/connectivity',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** create or replace object with pathname with a new object of the specified type */
    this.putObject = function(pathname, typepath, args) {
        var jqXHR = jQuery.ajax({
            type: 'PUT',
            url:  'object/'+pathname,
            data: {'type': typepath, 'args': args}
        });
        setModified(true);
        return jqXHR.promise();
    };

    /**
     * Get name and arguments for new object, add it to parent object,
     * and optionally invoke callback.
     *
     * typePath:   python path for type of object.
     * typeName:   last component of typePath.
     * parentPath: pathname for new object's parent.
     * prompt:     optional prompt use when requesting name.
     * callback:   optional callback invoked after adding object to project.
     */
    this.addObject = function(typePath, typeName, parentPath, prompt) {
        prompt = prompt || 'Enter name for new '+ typeName;
        _self.getSignature(typePath).done(function(signature) {
            openmdao.Util.promptForArgs(prompt, signature, function(name, args) {
                if (parentPath) {
                    name = parentPath + '.' + name;
                }
                _self.putObject(name, typePath, args);
            });
        });
    },

    /**
     * Confirm and then replace object.
     *
     * typePath: python path for type of replacement object.
     * typeName: last component of typePath.
     * objPath: pathname for replaced object.
     */
    this.replaceObject = function(typePath, typeName, objPath) {
        prompt = 'Replace '+objPath+' with '+typeName;
        openmdao.Util.confirm(prompt, function() {
            openmdao.project.getSignature(typePath).done(function(signature) {
                if (signature.args.length) {
                    prompt = 'Replacement '+typeName;
                    openmdao.Util.promptForArgs(prompt, signature, function(nm, args) {
                        openmdao.project.putObject(objPath, typePath, args);
                    }, true);
                }
                else {
                    openmdao.project.putObject(objPath, typePath, '');
                }
            });
        });
    },

    /** remove the object with the given pathname */
    this.removeObject = function(pathname) {
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

    /** issue the specified command against the project */
    this.issueCommand = function(cmd) {
        var jqXHR = jQuery.ajax({
                        type: 'POST',
                        url:  'command',
                        data: { 'command': cmd }
                    });
        setModified(true);
        return jqXHR.promise();
    };

    /** set the value of the variable with to rhs */
    this.setVariableValue = function(pathname, rhs, type) {
        var jqXHR = jQuery.ajax({
                        type: 'POST',
                        url:  'variable/'+encodeURIComponent(pathname),   // escape any brackets, etc.
                        data: { 'rhs': rhs, 'type': type }
                    });
        setModified(true);
        return jqXHR.promise();
    };

    /** get a recursize file listing of the project working directory (as JSON) */
    this.getFiles = function() {
        var jqXHR = jQuery.ajax({
                        type:     'GET',
                        url:      'files',
                        dataType: 'json'
                    });
        return jqXHR.promise();
    };

    /** get the contents of the specified file */
    this.getFile = function(filepath) {
        var jqXHR = jQuery.ajax({
                        type: 'GET',
                        url:  'file'+filepath.replace(/\\/g,'/')
                    });
        return jqXHR.promise();
    };

    /** set the contents of the specified file */
    this.setFile = function(filepath, contents, force) {
        var jqXHR = jQuery.ajax({
                        type: 'PUT',
                        url:  'file/'+filepath.replace(/\\/g,'/'),
                        data: { 'contents': contents, 'force': force }
                    });
        setModified(true);
        return jqXHR.promise();
    };

    /** create new folder with specified path in the project working directory */
    this.createFolder = function(folderpath) {
        var jqXHR = jQuery.ajax({
                        type: 'PUT',
                        url:  'file/'+folderpath.replace(/\\/g,'/'),
                        data: { 'isFolder': true}
                    });
        setModified(true);
        return jqXHR.promise();
    };

    /** create a new file in the project working directory with the specified path  */
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
        return _self.setFile(name, contents, undefined);
    };

    /** prompt for name & create a new folder */
    this.newFolder = function(name, folderpath) {
        if (folderpath) {
            name = folderpath+'/'+name;
        }
        return _self.createFolder(name);
    };

    /** add one or more files, optionally specifying a dest folder */
    this.addFiles = function(files, path) {
        var formData = new FormData();
        for (var filename in files) {
            formData.append('file', files[filename]);
        }
        if (path) {
            formData.append('path', path);
        }
        var jqXHR = jQuery.ajax({
                        type: 'POST',
                        url:  'tools/upload',
                        data: formData,
                        processData: false, // Don't assume default data format.
                        contentType: false  // Don't use default content type.
                    });
        setModified(true);
        return jqXHR.promise();
    };

    /** rename file with specified path. */
    this.renameFile = function(filepath, newname) {
        // convert to relative path with forward slashes
        var path = filepath.replace(/\\/g,'/');
        if (path[0] === '/') {
            path = path.substring(1, path.length);
        }
        // make the call
        var jqXHR = jQuery.ajax({
                        type: 'POST',
                        url:  'file/'+path,
                        data: { 'rename': newname }
                    });
        setModified(true);
        return jqXHR.promise();
    };

    /** delete file with specified path from the project working directory */
    this.removeFile = function(filepath) {
        var jqXHR = jQuery.ajax({
                    type: 'DELETE',
                    url:  'file'+filepath.replace(/\\/g,'/'),
                    data: { 'file': filepath }
                })
                .fail(function(jqXHR, textStatus, errorThrown) {
                    alert('Error removing file: ' + textStatus);
                    debug.error('Error removing file', path, name,
                                jqXHR, textStatus, errorThrown);
                });

        setModified(true);
        return jqXHR.promise();
    };

    /** delete files with specified path from the project working directory */
    this.removeFiles = function(filepaths) {
        var jqXHR = jQuery.ajax({
                type: 'DELETE',
                url:  'files',
                data: JSON.stringify({'filepaths': filepaths}),
                contentType: 'application/json; charset=utf-8'
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
                alert('Error removing files: ' + textStatus);
                debug.error('Error removing files', path, name,
                            jqXHR, textStatus, errorThrown);
            });

        setModified(true);
        return jqXHR.promise();
    };

    /** execute a component */
    this.runComponent = function(pathname) {
        var jqXHR = jQuery.ajax({
                        type: 'POST',
                        url:  'object/'+pathname
                    })
            .done(function(data, textStatus, jqXHR) {
                     if (typeof openmdao_test_mode !== 'undefined') {
                         openmdao.Util.notify('Run complete: '+textStatus);
                     }
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
                debug.error("Error running component (status="+jqXHR.status+"): "+jqXHR.statusText);
                debug.error(jqXHR,textStatus,errorThrown);
            });

        setModified(true);
        return jqXHR.promise();
    };

    /** execute the specified file */
    this.execFile = function(filepath) {
        // convert to relative path with forward slashes
        var path = filepath.replace(/\\/g,'/');
        if (path[0] === '/') {
            path = path.substring(1,path.length);
        }

        // make the call
        var jqXHR = jQuery.ajax({
                        type: 'POST',
                        url:  'file/'+path
                    });

        setModified(true);
        return jqXHR.promise();
    };

    /** reload the project */
    this.reload = function() {
        setModified(false);
        closeWebSockets('reload');
        closeWindows();
        window.location.replace('/workspace/project');
    };

    /** close the project and redirect to the specified url */
    this.close = function(url) {
        closeWindows();
        closeWebSockets('close');
        jQuery.ajax({
            type: 'POST',
            url:  'project',
            data: {'action': 'close'}
        })
        .done(function() {
            window.location.replace(url);
        })
        .fail(function(jqXHR, textStatus, errorThrown) {
            debug.error('Error closing project',
                        jqXHR, textStatus, errorThrown);
            alert('Error closing project: '+textStatus+'\n'+errorThrown);
        });
    };

    /** add window to the list of windows accociated with this project. */
    this.addWindow = function(win) {
        if (! _windows) {
            _windows = [];
        }
        _windows.push(win);
    };

    /** edit file associated with this project */
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

    /** open file in a new browser window (possibly not in a useful format) */
    this.viewFile = function(pathname) {
        pathname = pathname.replace(/\\/g,'/');
        if (pathname[0] != '/') {
            pathname = '/' + pathname;
        }
        openmdao.Util.popupWindow('file'+pathname, pathname);
    };

    /** view geometry with specified pathname, prompt if not specified */
    this.viewGeometry = function(pathname) {
        function popupGeom(pathname) {
            pathname = pathname.replace(/\\/g,'/');
            var w = openmdao.Util.popupWindow('tools/geometry?path='+pathname,
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

    /** view image with specified pathname, or all images if not specified */
    this.viewImages = function(pathname) {
        var w;
        if (pathname) {
            w = openmdao.Util.popupWindow('tools/images?path='+pathname, 'Images');
        }
        else {
            w = openmdao.Util.popupWindow('tools/images', 'Images');
        }
        _self.addWindow(w);
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
    };

    /** For functional testing: Close all WebSockets. */
    this.closeWebSockets = function(reason) {
        closeWebSockets(reason);
    };
};

