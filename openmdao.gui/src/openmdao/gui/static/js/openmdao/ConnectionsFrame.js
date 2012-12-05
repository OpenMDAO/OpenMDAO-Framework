
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConnectionsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('ConnectionsFrame-'+pathname).replace(/\./g,'-');
    openmdao.ConnectionsFrame.prototype.init.call(this, id,
        'Connections: '+openmdao.Util.getName(pathname));

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        // component selectors
        componentsHTML = '<div style="width:100%;background:grey"><table>'
                       +        '<tr><td>Source Component:</td>'
                       +            '<td>Target Component:</td>'
                       +        '</tr>'
                       +        '<tr><td><input id="src_cmp_list" /></td>'
                       +            '<td><input id="dst_cmp_list" /></td>'
                       +        '</tr>'
                       + '</table></div>',
        componentsDiv = jQuery(componentsHTML)
            .appendTo(self.elm),
        src_cmp_selector = componentsDiv.find('#src_cmp_list'),
        dst_cmp_selector = componentsDiv.find('#dst_cmp_list'),
        component_list = [],
        // connections diagram
        connectionsCSS = 'background:grey; position:relative; top:0px; width:100%; overflow-x:hidden; overflow-y:auto;',
        connectionsDiv = jQuery('<div id="'+id+'-connections" style="'+connectionsCSS+'">')
            .appendTo(self.elm),
        r = Raphael(connectionsDiv.attr('id')),
        // variable selectors and connect button
        variablesCSS = 'background:grey; position:relative; bottom:5px; width:100%;',
        variablesHTML = '<div style="'+variablesCSS+'"><table>'
                      +        '<tr><td>Source Variable:</td>'
                      +        '    <td>Target Variable:</td>'
                      +        '</tr>'
                      +        '<tr><td><input  id="src_var_list" /></td>'
                      +        '    <td><input  id="dst_var_list" /></td>'
                      +        '    <td><button id="connect" class="button">Connect</button></td>'
                      +        '</tr>'
                      + '</table></div>',
        variablesDiv = jQuery(variablesHTML)
            .appendTo(self.elm),
        src_var_selector = variablesDiv.find('#src_var_list'),
        dst_var_selector = variablesDiv.find('#dst_var_list'),
        connect_button = variablesDiv.find('#connect')
                        .click(function() {
                            var src = src_var_selector.val();
                            var dst = dst_var_selector.val();
                            model.issueCommand(self.pathname+'.connect("'+src+'","'+dst+'")');
                            src_var_selector.val('');
                            dst_var_selector.val('');
                        }),
        assemblyKey = '<Assembly>',
        assemblyCSS = {'font-style':'italic', 'opacity':'0.5'},
        normalCSS = {'font-style':'normal', 'opacity':'1.0'},
        // context menu
        contextMenu = jQuery("<ul class='context-menu'>")
            .appendTo(connectionsDiv),
        showAllVariables = true;  // show all vars vs only connected vars

    self.elm.css({'position':'relative', 'height':'100%',
                  'overflow':'hidden', 'max-height': 0.8*jQuery(window).height()});
    self.pathname = null;

    // set the connections pane height to dynamically fill the space between the
    // component and variable selectors
    function resize_contents() {
        connectionsDiv.height(self.elm.innerHeight()
                            - componentsDiv.outerHeight()
                            - variablesDiv.outerHeight());
    }

    // resize contents when told to do so (e.g. by BaseFrame when dialog is resized)
    self.elm.on('resize_contents', function(e) {
        resize_contents();
    });

    connectionsDiv.on('selectstart',false);

    // create context menu for toggling the showAllVariables option
    contextMenu.uniqueId();
    contextMenu.append(jQuery('<li>Toggle All Variables</li>').click(function(e) {
        if (showAllVariables) {
            showAllVariables = false;
            showConnections();
        }
        else {
            showAllVariables = true;
            showConnections();
        }
    }));
    ContextMenu.set(contextMenu.attr('id'), connectionsDiv.attr('id'));

    function setupSelector(selector) {
        // if selector gains focus with assemblyKey then clear it
        selector.focus(function() {
            selector = jQuery(this);
            if (selector.val() === assemblyKey) {
                selector.val('').css(normalCSS);
            }
        });

        // process new selector value when selector loses focus
        selector.on('blur', function(e) {
            selector.autocomplete('close');

            if (e.target.value === '' || e.target.value === assemblyKey) {
                selector.val(assemblyKey);
                selector.css(assemblyCSS);
            }

            if (selector.attr('id') === src_cmp_selector.attr('id')) {
                if (e.target.value === assemblyKey) {
                    self.src_comp = '';
                }
                else {
                    if (jQuery.inArray(e.target.value, component_list) >= 0) {
                        self.src_comp = e.target.value;
                    }
                    else {
                        selector.val(self.src_comp);
                    }
                }
            }
            else {
                if (e.target.value === assemblyKey) {
                    self.dst_comp = '';
                }
                else {
                    if (jQuery.inArray(e.target.value, component_list) >= 0) {
                        self.dst_comp = e.target.value;
                    }
                    else {
                        selector.val(self.dst_comp);
                    }
                }
            }

            if (e.target.value === '' || e.target.value === assemblyKey) {
                selector.val(assemblyKey);
                selector.css(assemblyCSS);
            }

            showConnections();
        });

        // set autocomplete to trigger blur (remove focus)
        selector.autocomplete({
            select: function(event, ui) {
                if (ui.item.value === '') {
                    selector.val(assemblyKey);
                    selector.css(assemblyCSS);
                }
                else {
                    selector.val(ui.item.value);
                    selector.css(normalCSS);
                }
                selector.blur();
            },
            delay: 0,
            minLength: 0
        });

        // set enter key to trigger blur (remove focus)
        selector.on('keypress.enterkey', function(e) {
            if (e.which === 13) {
                selector.blur();
            }
        });
    }

    setupSelector(src_cmp_selector);
    setupSelector(dst_cmp_selector);

    function loadData(data) {
        if (!data || !data.Dataflow || !data.Dataflow.components
                  || !data.Dataflow.components.length) {
            // don't have what we need, probably something got deleted
            debug.warn('ConnectionFrame.loadData(): Invalid data',data);
            self.close();
        }
        else {
            component_list = jQuery.map(data.Dataflow.components,
                                   function(comp,idx){ return comp.name; });
            component_list.push(assemblyKey);

            // update the output & input selectors with component list
            src_cmp_selector.html('');
            src_cmp_selector.autocomplete({source: component_list});

            dst_cmp_selector.html('');
            dst_cmp_selector.autocomplete({source: component_list});
        }

        if (self.src_comp) {
            src_cmp_selector.val(self.src_comp);
            src_cmp_selector.css(normalCSS);
        }
        else {
            src_cmp_selector.val(assemblyKey);
            src_cmp_selector.css(assemblyCSS);
        }
        if (self.dst_comp) {
            dst_cmp_selector.val(self.dst_comp);
            dst_cmp_selector.css(normalCSS);
        }
        else {
            dst_cmp_selector.val(assemblyKey);
            dst_cmp_selector.css(assemblyCSS);
        }
        showConnections();
    }

    function loadConnectionData(data) {
        if (!data || !data.sources || !data.destinations) {
            // don't have what we need, probably something got deleted
            debug.warn('ConnectionFrame.loadConnectionData(): Invalid data',data);
            self.close();
        }
        else {
            r.clear();
            figures = {};
            var i = 0,
                x = 15,
                y = 10,
                conn_list = jQuery.map(data.connections, function(n) {
                    return n;
                }),
                src_list  = jQuery.map(data.sources, function(n) {
                    return self.src_comp ? self.src_comp+'.'+n.name : n.name;
                }),
                dst_list   = jQuery.map(data.destinations, function(n) {
                    return self.dst_comp ? self.dst_comp+'.'+n.name : n.name;
                });

            for (i = 0; i <conn_list.length; i++) {
                if (conn_list[i].indexOf('.') >= 0) {
                    conn_list[i]=conn_list[i].split('.')[1];
                }
            }

            jQuery.each(data.sources, function(idx,srcvar) {
                if (showAllVariables || conn_list.contains(srcvar.name)) {
                    var src_name = self.src_comp ? self.src_comp+'.'+srcvar.name : srcvar.name;
                    figures[src_name] = r.variableNode(r, x, y, src_name, srcvar, true);
                    y = y + 40;  // add height of fig (30 px) plus 10 px of space
                }
            });
            var end_outputs = y;

            x = 195;
            y = 10;
            jQuery.each(data.destinations, function(idx,dstvar) {
                if (showAllVariables || conn_list.contains(dstvar.name)) {
                    var dst_name = self.dst_comp ? self.dst_comp+'.'+dstvar.name : dstvar.name;
                    figures[dst_name] = r.variableNode(r, x, y, dst_name, dstvar, false);
                    y = y + 40;  // add height of fig (30 px) plus 10 px of space
                }
            });
            var end_inputs = y;

            var height = Math.max(end_inputs, end_outputs, 25);
            r.setSize(connectionsDiv.width(), height);

            connectionsDiv.show();
            variablesDiv.show();

            resize_contents();

            jQuery.each(data.connections,function(idx,conn) {
                var src_name = conn[0],
                    dst_name = conn[1],
                    src_fig = figures[src_name],
                    dst_fig = figures[dst_name],
                    c = r.connection(src_fig, dst_fig, "#000", "#fff");
            });

            // update the output & input selectors to current outputs & inputs
            src_var_selector.html('');
            src_var_selector.autocomplete({ source: src_list ,minLength:0});

            dst_var_selector.html('');
            dst_var_selector.autocomplete({ source: dst_list ,minLength:0});
        }
    }

    function drawLine(startX, startY, endX, endY) {
        var start = {
            x: startX,
            y: startY
        };
        var end = {
            x: endX,
            y: endY
        };
        var getPath = function() {
            return "M" + start.x + " " + start.y + " L" + end.x + " " + end.y;
        };
        var redraw = function() {
            node.attr("path", getPath());
        };

        var node = r.path(getPath());
        return {
            element: node,
            updateStart: function(x, y) {
                start.x = x;
                start.y = y;
                redraw();
                return this;
            },
            updateEnd: function(x, y) {
                end.x = x;
                end.y = y;
                redraw();
                return this;
            }
        };
    }

    connectionsDiv.on('mousedown', function(e) {
        var offset = connectionsDiv.offset(),
            x = e.clientX - offset.left,
            y = e.clientY - offset.top,
            source = r.getElementByPoint(e.clientX, e.clientY),
            line = drawLine(x, y, x, y),
            target;

        if (source !== null) {
            connectionsDiv.on({
                'mousemove': function(e) {
                    x = e.clientX - offset.left;
                    y = e.clientY - offset.top;
                    line.updateEnd(x, y);
                },
                'mouseup': function(e) {
                    connectionsDiv.off('mousemove mouseup');
                    line.element.remove();
                    target = r.getElementByPoint(e.clientX, e.clientY);
                    if (target !== null && target !== source) {
                        var cmd = self.pathname + '.connect("',
                            src_name = source.data('name'),
                            tgt_name = target.data('name');
                        if (src_name && tgt_name) {
                            if (source.data('connected')) {
                                openmdao.Util.notify(src_name+' is already connected to something!');
                            }
                            else if (target.data('connected')) {
                                openmdao.Util.notify(tgt_name+' is already connected to something!');
                            }
                            else if (source.data('input') && !target.data('input')) {
                                cmd = cmd+src_name+'","'+tgt_name+'")';
                                model.issueCommand(cmd);
                            }
                            else if (source.data('input') && !target.data('input')) {
                                cmd = cmd+tgt_name+'","'+src_name+'")';
                                model.issueCommand(cmd);
                            }
                        }
                        else {
                            //debug.warn('ConnectionsFrame: Invalid source or target',
                            //           src_name, source, tgt_name, target);
                        }
                    }
                }
            });
        }
    });

    /** edit connections between the source and destination objects in the assembly */
    function showConnections() {
        if (self.src_comp !== null && self.dst_comp !== null) {
            model.getConnections(self.pathname, self.src_comp, self.dst_comp,
                loadConnectionData,
                function(jqXHR, textStatus, errorThrown) {
                    debug.error(jqXHR,textStatus,errorThrown);
                    self.close();
                }
            );
        }
        else {
            connectionsDiv.hide();
            variablesDiv.hide();
        }
    }

    /** handle message about the assembly */
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            loadData(message[1]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** if there is an object loaded, update it from the model */
    this.update = function() {
        if (self.pathname && self.pathname.length > 0) {
            self.editAssembly(self.pathname,self.src_comp,self.dst_comp);
        }
    };

    /** get the specified assembly from model */
    this.editAssembly = function(path, src_comp, dst_comp) {
        if (self.pathname !== path) {
           if (self.pathname !== null) {
                model.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            model.addListener(self.pathname, handleMessage);
        }

        self.src_comp = src_comp;
        self.dst_comp = dst_comp;

        model.getComponent(path, loadData,
            function(jqXHR, textStatus, errorThrown) {
                debug.warn('ConnectionsFrame.editAssembly() Error:',
                            jqXHR, textStatus, errorThrown);
                // assume component has been deleted, so close frame
                self.close();
            }
        );
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

    this.editAssembly(pathname, src_comp, dst_comp);
};

/** set prototype */
openmdao.ConnectionsFrame.prototype = new openmdao.BaseFrame();
openmdao.ConnectionsFrame.prototype.constructor = openmdao.ConnectionsFrame;
