
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

/**
 * utility functions used in the openmdao gui
 */
openmdao.Util = {

    /**
     * function to toggle visibility of an element
     *
     * id:   id of the element to hide/show
     */
    toggle_visibility: function(id) {
        var e = document.getElementById(id);
        if (e.style.display === 'block') {
            e.style.display = 'none';
        }
        else {
            e.style.display = 'block';
        }
    },

    /**
     * function to block all input on the page
     * (by covering it with a semi-transparent div)
     */
    toggle_screen: function() {
        var id = '_smokescreen_',
            el = document.getElementById(id);
        if (el === null) {
            el = document.createElement('div');
            el.setAttribute('id',id);
            el.style.cssText='position:fixed;top:0px;left:0px;'+
                             'height:100%;width:100%;'+
                             'background:#EEE;opacity:.4;' +
                             'z-index:99999;display:none';
            document.body.appendChild(el);
        }
        if (el.style.display === 'block') {
            el.style.display = 'none';
        }
        else {
            el.style.display = 'block';
        }
    },

    /**
     * open a popup window to view a URL
     *
     * url:     the url to open in the new window
     * title:   the title of the window
     * options: window options
     */
    popupWindow: function(url,title,options) {
        var specs = {
            height:     3/4*screen.height,
            width:      1/2*screen.width,
            top:        10,
            left:       10,
            location:   0,
            menubar:    0,
            resizable:  1,
            scrollbars: 0,
            status:     1,
            titlebar:   1,
            toolbar:    0
        },
        spec_string = '',
        spec = null,
        win = null;

        if (options) {
            jQuery.extend(specs, options);
        }

        for (spec in specs) {
            if (specs.hasOwnProperty(spec)) {
                if (spec_string.length > 0) {
                    spec_string = spec_string + ',';
                }
                spec_string = spec_string + spec + '=' + specs[spec];
            }
        }

        win = window.open(url, title, spec_string);
        win.document.title = title;
    },

    /**
     * open a popup window to view HTML
     *
     * html:    the html to display in the new window
     * title:   the title of the window
     * options: window options
     */
    htmlWindow: function(html,title,options) {
        var win =  openmdao.Util.popupWindow('',title,options);
        win.document.open();
        win.document.write(html);
        win.document.close();
    },

    /**
     * open a popup window initialized by the given script
     *
     * title:   the title of the window
     * script:  script to initialize the window
     * options: window options
     */
    popupScript: function (title,script,options) {
        var url = "/workspace/base?head_script='"+script+"'",
            win = openmdao.Util.popupWindow(url,title,options);
    },


    /**
     *  escape anything in the text that might look like HTML, etc.
     */
    escapeHTML: function(text) {
        var i = 0,
            result = "";
        for(i = 0; i < text.length; i++){
            if(text.charAt(i) === "&"
                  && text.length-i-1 >= 4
                  && text.substr(i, 4) !== "&amp;"){
                result = result + "&amp;";
            } else if(text.charAt(i) === "<"){
                result = result + "&lt;";
            } else if(text.charAt(i) === ">"){
                result = result + "&gt;";
            } else if(text.charAt(i) === " "){
                result = result + "&nbsp;";
            } else {
                result = result + text.charAt(i);
            }
        }
        return result;
    },

    /**
     * add a handler to the onload event
     * ref: http://simonwillison.net/2004/May/26/addLoadEvent/
     *
     * func:    the function to add
     */
    addLoadEvent: function(func) {
        var oldonload = window.onload;
        if (typeof window.onload !== 'function') {
            window.onload = func;
        }
        else {
            window.onload = function() {
                if (oldonload) {
                    oldonload();
                }
                func();
            };
        }
    },

    /**
     * function to scroll to the bottom of an element (FIXME: doesn't work)
     *
     * el:      the element to scroll
     */
    scrollToBottom: function(el) {
        el.scrollTop = el.scrollHeight;
        el.scrollTop = el.scrollHeight - el.clientHeight;
    },

    /**
     * prompt for a value
     *
     * prompt:      prompt string
     * callback:    the function to call with the provided value
     * baseId:      optional id, default ``get-value``, used for element ids
     */
    promptForValue: function(prompt, callback, baseId) {
        baseId = baseId || 'get-value';

        // if the user didn't specify a callback, just return
        if (typeof callback !== 'function') {
            return;
        }

        var promptId = baseId+'-prompt',
            inputId = baseId+'-input',
            okId = baseId+'-ok',
            cancelId = baseId + '-cancel',
            element = document.getElementById(baseId),
            win = null;
            userInput = null;

        function handleResponse(ok) {
            // close dialog
            win.dialog('close');
            // if response was 'Ok' then invoke the callback
            if (ok && callback) {
                callback(userInput.val());
            }
            // remove from DOM
            win.remove();
        }

        win = jQuery('<div id="'+baseId+'"><div id="'+promptId+'" /></div>');

        userInput = jQuery('<input type="text" id="'+inputId+'" style="width:100%"></input>');
        userInput.bind('keypress.enterkey', function(e) {
            if (e.which === 13) {
                handleResponse(true);
            }
        });
        userInput.appendTo(win);

        win.dialog({
            autoOpen: false,
            modal: true,
            buttons: [
                {
                    text: 'Ok',
                    id: okId,
                    click: function() { handleResponse(true); }
                },
                {
                    text: 'Cancel',
                    id: cancelId,
                    click: function() { handleResponse(false); }
                }
            ]
        });

        jQuery('#'+promptId).html(prompt+':');

        win.dialog('open');
    },

    /**
     * Notify user with `msg`.  Typically used when testing to catch
     * completion of 'background' processing.
     *
     * msg:     message to display.
     * title:   optional title, default ``Note:``.
     * baseId:  optional id, default ``notify``, used for element ids.
     */
    notify: function(msg, title, baseId) {
        title = title || 'Note:';
        baseId = baseId || 'notify';

        var msgId = baseId+'-msg',
            element = document.getElementById(msgId),
            win = null;

        if (element === null) {
            win = jQuery('<div id="'+msgId+'"></div>');
            win.dialog({
                autoOpen: false,
                modal: true,
                title: title,
                buttons: [
                    {
                        text: 'Ok',
                        id: baseId+'-ok',
                        click: function() {
                            win.dialog('close');
                        }
                    }
                ]
            });
        }
        else {
            win = jQuery('#'+msgId);
        }

        win.text(msg);
        win.dialog('open');
    },

    /**
     * show the properties of an object on the log (debug only)
     *
     * obj: the object for which properties are to be displayed
     */
    dumpProps: function(obj) {
        var prop;
        for (prop in obj) {
            if (obj.hasOwnProperty(prop)) {
                debug.log(prop + ": " + obj[prop]);
            }
        }
    },

    /**
     * close the browser window
     */
    closeWindow: function() {
        jQuery('body').html('<center><b>Thank you for using OpenMDAO, You may close your browser now!');
        window.open('','_self');
        window.close();
    },

    /**
     * The purge function takes a reference to a DOM element as an argument. It loops through the
     * element's attributes. If it finds any functions, it nulls them out. This breaks the cycle,
     * allowing memory to be reclaimed. It will also look at all of the element's descendent
     * elements, and clear out all of their cycles as well. The purge function is harmless on
     * Mozilla and Opera. It is essential on IE. The purge function should be called before removing
     * any element, either by the removeChild method, or by setting the innerHTML property.
     *
     * http://www.crockford.com/javascript/memory/leak.html
     */
    purge: function(d) {
        var a = d.attributes, i, l, n;
        if (a) {
            l = a.length;
            for (i = 0; i < l; i += 1) {
                n = a[i].name;
                if (typeof d[n] === 'function') {
                    d[n] = null;
                }
            }
        }
        a = d.childNodes;
        if (a) {
            l = a.length;
            for (i = 0; i < l; i += 1) {
                purge(d.childNodes[i]);
            }
        }
    },

    /** get the path from the pathname */
    getPath: function(pathname) {
        path = '';
        if (pathname) {
            var lastdot = pathname.lastIndexOf('.');
            if (lastdot > 0) {
                path = pathname.substring(0,lastdot);
            }
        }
        return path;
    },

    /** get the name from the pathname */
    getName: function(pathname) {
        var name = pathname,
            tok = pathname.split('.');
        if (tok.length > 1) {
            name = tok[tok.length-1];
        }
        return name;
    },

    /** find the element with the highest z-index of those specified by the jQuery selector */
    getHighest: function (selector) {
        var elems = jQuery(selector),
            highest_elm = null,
            highest_idx = 0,
            i = 0;
        for (i = 0; i < elems.length; i++)  {
            var elem = elems[i][0];
            var zindex = document.defaultView.getComputedStyle(elem,null).getPropertyValue("z-index");
            if ((zindex > highest_idx) && (zindex !== 'auto')) {
                highest_elm = elem;
                highest_idx = zindex;
            }
        }
        return highest_elm;
    },

    /** rotate the page */
    rotatePage: function (x) {
        x = parseInt(x,10);
        var rotateCSS = ' -moz-transform: rotate('+x+'deg); -moz-transform-origin: 50% 50%;'
                      + ' -webkit-transform: rotate('+x+'deg);-webkit-transform-origin: 50% 50%;'
                      + ' -o-transform: rotate('+x+'deg); -o-transform-origin: 50% 50%;'
                      + ' -ms-transform: rotate('+x+'deg); -ms-transform-origin: 50% 50%;'
                      + ' transform: rotate('+x+'deg); transform-origin: 50% 50%;';
        document.body.setAttribute('style',rotateCSS);
    },$doabarrelroll:function(){for(i=0;i<=360;i++){setTimeout("openmdao.Util.rotatePage("+i+")",i*40);} return;},

    /** connect to websocket at specified address */
    openWebSocket: function(addr,handler,errHandler,retry,delay) {
        // if retry is true and connection fails, try again to connect after delay
        retry = typeof retry !== 'undefined' ? retry : true;
        delay = typeof delay !== 'undefined' ? delay : 2000;

        var socket = null;

        if (!openmdao.sockets) {
            openmdao.sockets = [];
        }

        function connect_after_delay() {
            tid = setTimeout(connect, delay);
        }

        function displaySockets() {
            debug.info('WebSockets:');
            var i = 0;
            for (i = 0 ; i < openmdao.sockets.length ; ++i) {
                debug.info('    '+i+': state '+openmdao.sockets[i].readyState);
            }
        }

        function connect() {
            if (socket === null || socket.readyState > 0) {
                socket = new WebSocket(addr);
                openmdao.sockets.push(socket);
                socket.onopen = function (e) {
                    //debug.info('websocket opened '+socket.readyState,socket,e);
                    //displaySockets();
                };
                socket.onclose = function (e) {
                    //debug.info('websocket closed',socket,e);
                    //displaySockets();
                    index = openmdao.sockets.indexOf(this);
                    if (index >= 0) {
                        openmdao.sockets.splice(index, 1);
                        if (typeof openmdao_test_mode !== 'undefined') {
                            if (openmdao.sockets.length === 0) {
                                openmdao.Util.notify('WebSockets closed');
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

        return socket;
    },

    /** Close all WebSockets. */
    closeWebSockets: function(reason) {
        var i = 0;
       if (openmdao.sockets) {
          for (i = 0 ; i < openmdao.sockets.length ; ++i) {
             openmdao.sockets[i].close(1000, reason);
          }
       }
    },

    /** Notify when `nSockets` are open (used for testing). */
    webSocketsReady: function(nSockets) {
        function doPoll() {
            setTimeout(poll, 1000);
        }

        function poll() {
            var i = 0;
            debug.info('polling for '+nSockets+' open WebSockets');
            if (openmdao.sockets.length >= nSockets) {
                for (i = 0 ; i < openmdao.sockets.length ; ++i) {
                    if (openmdao.sockets[i].readyState !== 1) {
                        debug.info('socket '+i+' not open: '
                                   +openmdao.sockets[i].readyState);
                        doPoll();
                        return;
                    }
                }
                openmdao.Util.notify('WebSockets open');
            }
        }
        poll();
    }

};


