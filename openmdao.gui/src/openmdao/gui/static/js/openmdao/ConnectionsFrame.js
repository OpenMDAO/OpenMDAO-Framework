/**
 *  ConnectionsFrame: a frame for viewing/editing connections in an assembly
 *
 *  Source and destination components are selected via input boxes at the top
 *  of the frame. Source (output) and destination (input) variables are
 *  rendered as SVG figures with curves between them representing connections.
 *  Source and destination variables can be selected via input boxes at the
 *  bottom of the frame and connected by clicking on the 'connect' button.
 *  Alternatively, dragging from one variable figure to another will connect
 *  them if they are eligible to be connected. Input variables can be
 *  disconnected by right clicking on them and choosing 'Disconnect' from the
 *  context menu.
 *
 *  Arguments:
 *      project:  object that provides access to the openmdao project
 *      pathname: the pathname of the assembly
 *      src_comp: (optional) the source component to select initially
 *      dst_comp: (optional) the destination component to select initially
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConnectionsFrame = function(project, pathname, src_comp, dst_comp) {
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
                       +        '<tr><td style="width:200px">Source Component:</td>'
                       +        '    <td style="width:200px"></td>'
                       +        '    <td style="width:200px">Target Component:</td>'
                       +        '</tr>'
                       +        '<tr><td style="width:200px"><select id="src_cmp_list" /></td>'
                       +        '    <td style="width:200px"></td>'
                       +        '    <td style="width:200px"><select id="dst_cmp_list" /></td>'
                       +        '</tr>'
                       + '</table></div>',
        componentsDiv = jQuery(componentsHTML)
            .appendTo(self.elm),
        src_cmp_selector = componentsDiv.find('#src_cmp_list').combobox(),
        dst_cmp_selector = componentsDiv.find('#dst_cmp_list').combobox(),
        src_cmp_input = src_cmp_selector.siblings('.ui-autocomplete-input').attr('id','src_cmp_input'),
        dst_cmp_input = dst_cmp_selector.siblings('.ui-autocomplete-input').attr('id','dst_cmp_input'),
        component_list = [],
        // connections diagram
        connectionsCSS = 'background:grey; position:relative; top:0px; width:100%; overflow-x:hidden; overflow-y:auto;',
        connectionsDiv = jQuery('<div id="'+id+'-connections" style="'+connectionsCSS+'">')
            .appendTo(self.elm),
        r = Raphael(connectionsDiv.attr('id')),
        // variable selectors and connect button
        variablesCSS = 'background:grey; position:relative; bottom:5px; width:100%;',
        variablesHTML = '<div style="'+variablesCSS+'"><table>'
                      +        '<tr><td style="width:200px">Source Variable:</td>'
                      +        '    <td style="width:200px">'
                      +        '    <td style="width:200px">Target Variable:</td>'
                      +        '</tr>'
                      +        '<tr><td style="width:200px"><select id="src_var_list" /></td>'
                      +        '    <td style="width:200px; text-align:center"><button id="expression" class="button">Enter Source Expression</button></td>'
                      +        '    <td style="width:200px"><select id="dst_var_list" /></td>'
                      +        '    <td style="width:100px"><button id="connect" class="button">Connect</button></td>'
                      +        '</tr>'
                      + '</table></div>',
        variablesDiv = jQuery(variablesHTML)
            .appendTo(self.elm),
        src_var_selector = variablesDiv.find('#src_var_list').combobox(),
        dst_var_selector = variablesDiv.find('#dst_var_list').combobox(),
        src_var_input = src_var_selector.siblings('.ui-autocomplete-input').attr('id','src_var_input'),
        dst_var_input = dst_var_selector.siblings('.ui-autocomplete-input').attr('id','dst_var_input'),
        expression_button = variablesDiv.find('#expression')
                        .click(function() {
                            openmdao.Util.promptForValue('Enter an expression',
                                function(expr) {
                                    if (expr) {
                                        src_var_input.val(expr);
                                    }
                                });
                        }),
        connect_button = variablesDiv.find('#connect')
                        .click(function() {
                            var src = src_var_input.val(),
                                dst = dst_var_input.val();
                            if (src === '') {
                                openmdao.Util.notify('Invalid source variable');
                            }
                            else if (dst === '') {
                                openmdao.Util.notify('Invalid target variable');
                            }
                            else {
                                project.issueCommand(self.pathname+'.connect("'+src+'","'+dst+'")');
                                src_var_selector.val('');
                                dst_var_selector.val('');
                            }
                        }),
        allKey = '-- All --',
        assemblyKey = '-- Assembly --',
        assemblyCSS = {'font-style':'italic', 'opacity':'0.5'},
        normalCSS   = {'font-style':'normal', 'opacity':'1.0'},
        // context menu
        contextMenu = jQuery("<ul class='context-menu'>")
            .appendTo(connectionsDiv),
        busyCSS = 'position:absolute;left:150px;top:25px;color:black;background-color:DarkGray;border:1px solid black;',
        busyDiv = jQuery('<div class="busy" style="'+busyCSS+'">&nbsp;&nbsp;Updating... Please wait&nbsp;&nbsp;</div>')
            .appendTo(connectionsDiv).hide(),
        src_figures = {},
        dst_figures = {},
        xpr_figures = {},
        expanded_src = {},          // expanded state for expandable source nodes
        expanded_dst = {},          // expanded state for expandable dest nodes
        connection_data = {},       // cache of most recently fetched connection data
        showAllVariables = true;    // show all vars vs only connected vars

    self.elm.css({'position':'relative', 'height':'100%',
                  'overflow':'hidden', 'max-height': 0.8*jQuery(window).height()});

    self.pathname = null;

    // set the connections pane height to dynamically fill the space between the
    // component and variable selectors
    function resize_contents() {
        connectionsDiv.height(self.elm.innerHeight()
                            - componentsDiv.outerHeight()
                            - variablesDiv.outerHeight());
        r.setSize(connectionsDiv.width(), connectionsDiv.height());
    }

    // resize contents when told to do so (e.g. by BaseFrame when dialog is resized)
    self.elm.on('resize_contents', function(e) {
        resize_contents();
    });

    // disallow annoying text selection in the connections div
    connectionsDiv.on('selectstart',false);

    // create context menu for toggling the showAllVariables option
    contextMenu.uniqueId();
    contextMenu.append(jQuery('<li>Show Connected Variables Only</li>').click(function(e) {
        showAllVariables = !showAllVariables;
        if (showAllVariables) {
            jQuery(this).text('Show Connected Variables Only');
        }
        else {
            jQuery(this).text('Show All Variables');
        }
        showConnections();
    }));
    ContextMenu.set(contextMenu.attr('id'), connectionsDiv.attr('id'));

    /** set up a component selector */
    function setupSelector(selector) {
        // get a reference to the INPUT element, which is a sibling to the SELECT
        selector.input = selector.siblings('.ui-autocomplete-input');

        // when input gains focus, clear it
        selector.input.focus(function(e) {
            selector.input.val('').css(normalCSS);
        });

        // process new selector value on change
        selector.change(function(e) {
            var src_prev = self.src_comp,
                dst_prev = self.dst_comp;

            // make sure the input field shows the proper value with the proper style
            if (this.value === assemblyKey) {
                selector.input.val(assemblyKey);
                selector.input.css(assemblyCSS);
            }
            else {
                selector.input.val(this.value);
                selector.input.css(normalCSS);
            }

            // set active source/destination component to current selection if valid
            if (selector.attr('id') === src_cmp_selector.attr('id')) {
                if (this.value === assemblyKey) {
                    self.src_comp = '';
                }
                else {
                    if (jQuery.inArray(this.value, component_list) >= 0) {
                        self.src_comp = this.value;
                    }
                    else {
                        selector.input.val(self.src_comp);
                    }
                }
            }
            else {
                if (this.value === assemblyKey) {
                    self.dst_comp = '';
                }
                else {
                    if (jQuery.inArray(this.value, component_list) >= 0) {
                        self.dst_comp = this.value;
                    }
                    else {
                        selector.input.val(self.dst_comp);
                    }
                }
            }

            // if the selection has changed, re-render the connections
            if (self.src_comp !== src_prev || self.dst_comp !== dst_prev) {
                showConnections();
            }
        });

        // trigger selector change event when input loses focus,
        // if input field is empty, then select the assembly
        selector.input.blur(function(e) {
            if (this.value === '') {
                this.value = assemblyKey;
                selector.val(assemblyKey);
            }
            selector.change();
        });

        // trigger selector change event when enter key is pressed in input field
        selector.input.on('keypress.enterkey', function(e) {
            if (e.which === 13) {
                this.blur();
            }
        });
    }

    // set up source and destination component selector behaviors
    setupSelector(src_cmp_selector);
    setupSelector(dst_cmp_selector);

    /** populate component selectors from dataflow data and show connections */
    function loadComponentData(data) {
        if (!data || !data.Dataflow || !data.Dataflow.components
                  || !data.Dataflow.components.length) {
            // don't have what we need, probably something got deleted
            debug.warn('ConnectionFrame.loadComponentData(): Invalid data',data);
            self.close();
        }
        else {
            component_list = jQuery.map(data.Dataflow.components,
                                   function(comp,idx){ return comp.name; });
            component_list.push(assemblyKey);
            component_list.push(allKey);

            // update the output & input selectors with component list
            src_cmp_selector.html('');
            dst_cmp_selector.html('');
            jQuery.each(component_list, function(idx, comp_name) {
                src_cmp_selector.append('<option value="'+comp_name+'">'+comp_name+'</option>');
                dst_cmp_selector.append('<option value="'+comp_name+'">'+comp_name+'</option>');
            });

            if (self.src_comp) {
                src_cmp_selector.val(self.src_comp);
                src_cmp_selector.input.val(self.src_comp);
                src_cmp_selector.input.css(normalCSS);
            }
            else {
                src_cmp_selector.val(assemblyKey);
                src_cmp_selector.input.val(assemblyKey);
                src_cmp_selector.input.css(assemblyCSS);
            }
            if (self.dst_comp) {
                dst_cmp_selector.val(self.dst_comp);
                dst_cmp_selector.input.val(self.dst_comp);
                dst_cmp_selector.input.css(normalCSS);
            }
            else {
                dst_cmp_selector.val(assemblyKey);
                dst_cmp_selector.input.val(assemblyKey);
                dst_cmp_selector.input.css(assemblyCSS);
            }

            showConnections();
        }
    }

    /** populate connections and variable selectors with source and dest variables */
    function loadConnectionData(data) {
        var i = 0,
            x = 15,
            y = 10,
            connected_vars = jQuery.map(data.edges, function(n) {
                return n;
            }),
            src_list = [],
            dst_list = [],
            xpr_list = [];

        jQuery.each(data.nodes, function(name, attr) {
            if (attr.type === 'expr') {
                xpr_list.push(name);
            }
            else if (attr.io === 'output') {
                if (!self.src_comp) {
                    src_list.push(name);
                }
                else if (openmdao.Util.getPath(name) === self.src_comp) {
                    src_list.push(name);
                }
            }
            else if (attr.io === 'input') {
                if (!self.dst_comp) {
                    dst_list.push(name);
                }
                else if (openmdao.Util.getPath(name) === self.dst_comp) {
                    dst_list.push(name);
                }
            }
            else {
                debug.warn('Unexpected connection data:', name, attr);
            }
        });

        src_list.sort();
        dst_list.sort();
        xpr_list.sort();

        src_figures = {};
        dst_figures = {};
        xpr_figures = {};
        r.clear();

        jQuery.each(src_list, function(idx, src_name) {
            var attr = data.nodes[src_name],
                connected = connected_vars.contains(src_name),
                parent_name, parent_fig,
                dot_brkt = -1;
            if (showAllVariables || connected) {
                // add some attribute for rendering the figure
                attr.name = openmdao.Util.getName(src_name);
                attr.input = false;
                attr.connected = connected;
                // some of this logic may be redundant (all names should be qualified, etc.)
                dot_brkt = src_name.search(/\.|\[/);
                if (dot_brkt > 0) {
                    parent_name = src_name.substring(0, dot_brkt);
                    if (self.src_comp && self.src_comp === parent_name) {
                        src_figures[src_name] = r.variableNode(r, x, y, src_name, attr);
                        y = y + 40;  // add height of fig (30 px) plus 10 px of space
                    }
                    else {
                        parent_fig = src_figures[parent_name];
                        if (!self.src_comp && !parent_fig) {
                            // no component selected, add a parent fig for the component
                            parent_fig = r.variableNode(r, x, y, parent_name, {
                                name:  parent_name,
                                type:  'Component',
                                units: '',
                                input: false
                            });
                            src_figures[parent_name] = parent_fig;
                            y = y + 40;  // add height of fig (30 px) plus 10 px of space
                        }
                        if (parent_fig) {
                            attr.parent = parent_fig;
                            if (expanded_src[parent_name]) {
                                src_figures[src_name] = r.variableNode(r, x, y, src_name, attr);
                                y = y + 40;  // add height of fig (30 px) plus 10 px of space
                                parent_fig.expanded();
                            }
                            else {
                                expanded_src[parent_name] = false;  // default to collapsed
                                parent_fig.collapsed();
                            }
                        }
                    }
                }
                else {
                    src_figures[src_name] = r.variableNode(r, x, y, src_name, attr);
                    y = y + 40;  // add height of fig (30 px) plus 10 px of space
                }
            }
        });
        var end_outputs = y;


        x = 202 + 15;  // second column
        y = 10;

        jQuery.each(xpr_list, function(idx, xpr) {
            var attr = data.nodes[xpr];
            // add some attribute for rendering the figure
            attr.name = xpr;
            attr.input = true;
            attr.connected = true;
            xpr_figures[xpr] = r.variableNode(r, x, y, xpr, attr);
            y = y + 40;  // add height of fig (30 px) plus 10 px of space
        });


        x = 202*2 + 15;  // third column
        y = 10;
        jQuery.each(dst_list, function(idx, dst_name) {
            var attr = data.nodes[dst_name],
                connected = connected_vars.contains(dst_name),
                dot_brkt = -1;
            if (showAllVariables || connected) {
                // add some attribute for rendering the figure
                attr.name = openmdao.Util.getName(dst_name);
                attr.input = true;
                attr.connected = connected;
                // some of this logic may be redundant (all names should be qualified, etc.)
                dot_brkt = dst_name.search(/\.|\[/);
                if (dot_brkt > 0) {
                    parent_name = dst_name.substring(0, dot_brkt);
                    if (self.dst_comp && self.dst_comp === parent_name) {
                        dst_figures[dst_name] = r.variableNode(r, x, y, dst_name, attr);
                        y = y + 40;  // add height of fig (30 px) plus 10 px of space
                    }
                    else {
                        parent_fig = dst_figures[parent_name];
                        if (!parent_fig) {
                            // no component selected, add a parent fig for the component
                            parent_fig = r.variableNode(r, x, y, parent_name, {
                                name:  parent_name,
                                type:  'Component',
                                units: '',
                                input: true
                            });
                            dst_figures[parent_name] = parent_fig;
                            y = y + 40;  // add height of fig (30 px) plus 10 px of space
                        }
                        if (parent_fig) {
                            attr.parent = parent_fig;
                            if (expanded_dst[parent_name]) {
                                dst_figures[dst_name] = r.variableNode(r, x, y, dst_name, attr);
                                y = y + 40;  // add height of fig (30 px) plus 10 px of space
                                parent_fig.expanded();
                            }
                            else {
                                expanded_dst[parent_name] = false;  // default to collapsed
                                parent_fig.collapsed();
                            }
                        }
                    }
                }
                else {
                    dst_figures[dst_name] = r.variableNode(r, x, y, dst_name, attr);
                    y = y + 40;  // add height of fig (30 px) plus 10 px of space
                }
            }
        });
        var end_inputs = y;

        var height = Math.max(end_inputs, end_outputs, 25);
        r.setSize(connectionsDiv.width(), height);

        connectionsDiv.show();
        variablesDiv.show();

        jQuery.each(data.edges,function(idx, conn) {
            var src_fig = src_figures[conn[0]],
                dst_fig = dst_figures[conn[1]],
                parent_name,
                dot_brkt;

            if (/[\+\-\*\/]/.test(conn[0])) {
                src_fig = xpr_figures[conn[0]];
            }

            if (/[\+\-\*\/]/.test(conn[1])) {
                dst_fig = xpr_figures[conn[1]];
            }

            // if src or dst fig is not found then check for collapsed parent and link to that
            if (!src_fig) {
                parent_name = self.src_comp ? conn[0].substr(self.src_comp.length+1) : conn[0];
                dot_brkt = parent_name.search(/\.|\[/);
                if (dot_brkt > 0) {
                    parent_name = parent_name.substring(0, dot_brkt);
                    src_fig = src_figures[parent_name];
                }
            }
            if (!dst_fig) {
                parent_name = self.dst_comp ? conn[1].substr(self.dst_comp.length+1) : conn[1];
                dot_brkt = parent_name.search(/\.|\[/);
                if (dot_brkt > 0) {
                    parent_name = parent_name.substring(0, dot_brkt);
                    dst_fig = dst_figures[parent_name];
                }
            }

            if (src_fig && dst_fig) {
                r.connection(src_fig, dst_fig, "#000", "#fff")
                    .line.node.className.baseVal += ' variable-connection';
            }
            else {
                debug.error('Cannot draw connection between '+conn[0]+' and '+conn[1]);
            }
        });

        // update the output & input selectors to current outputs & inputs
        src_var_input.val('');
        src_var_selector.html('');
        jQuery.each(src_list, function(idx, var_name) {
            src_var_selector.append('<option value="'+var_name+'">'+var_name+'</option>');
        });

        dst_var_input.val('');
        dst_var_selector.html('');
        jQuery.each(dst_list, function(idx, var_name) {
            dst_var_selector.append('<option value="'+var_name+'">'+var_name+'</option>');
        });

        busyDiv.hide();
    }

    /** draw a line on the connections div */
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
            line.attr("path", getPath());
        };

        var line = r.path(getPath());
        return {
            element: line,
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

    // double clicking on an expandable node will toggle expansion of that node
    connectionsDiv.on('dblclick', function(e) {
        var offset = connectionsDiv.offset(),
            x = e.clientX - offset.left,
            y = e.clientY - offset.top,
            source = r.getElementByPoint(e.clientX, e.clientY);
        if (source) {
            var name = source.data('name'),
                input = source.data('input');
            if (input) {
                if (expanded_dst.hasOwnProperty(name)) {
                    expanded_dst[name] = !expanded_dst[name];
                    loadConnectionData(connection_data);
                }
            }
            else if (expanded_src.hasOwnProperty(name)) {
                expanded_src[name] = !expanded_src[name];
                loadConnectionData(connection_data);
            }

        }
    });

    // configure mouse handlers to connect source and dest variables when a
    // line is drawn from one to the other and to add a 'disconnect' option
    // to the context menu when right clicking on a connected input variable
    connectionsDiv.on('mousedown', function(e) {
        var offset = connectionsDiv.offset(),
            x = e.clientX - offset.left,
            y = e.clientY - offset.top,
            source = r.getElementByPoint(e.clientX, e.clientY),
            target,
            line;

        if (source !== null) {
            if (e.button === 2) {
                // context menu option to disconnect the variable
                if (source.data('input') && source.data('connected')) {
                    var src_name = source.data('name'),
                        menuItem = jQuery('<li>Disconnect '+src_name+'</li>')
                            .click(function(e) {
                                cmd = self.pathname+'.disconnect("'+src_name+'")';
                                project.issueCommand(cmd);
                            });
                        removeItem = function() {
                            menuItem.remove();
                            ContextMenu._removeEvent(document,"click", this);
                        };
                    contextMenu.append(menuItem);
                    ContextMenu._addEvent(document,'click', removeItem);
                }
            }
            else {
                line = drawLine(x, y + connectionsDiv.scrollTop(),
                                x, y + connectionsDiv.scrollTop());
                connectionsDiv.on({
                    'mousemove': function(e) {
                        x = e.clientX - offset.left;
                        y = e.clientY - offset.top + connectionsDiv.scrollTop();
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
                                if (!source.data('input') && target.data('input')) {
                                    if (target.data('connected')) {
                                        openmdao.Util.notify('Input variable ('+
                                            tgt_name+') is already connected to something!');
                                    }
                                    else {
                                        cmd = cmd+src_name+'","'+tgt_name+'")';
                                        project.issueCommand(cmd);
                                    }
                                }
                                else if (source.data('input') && !target.data('input')) {
                                    if (source.data('connected')) {
                                        openmdao.Util.notify('Input variable ('+
                                            src_name+') is already connected to something!');
                                    }
                                    else {
                                        cmd = cmd+tgt_name+'","'+src_name+'")';
                                        project.issueCommand(cmd);
                                    }
                                }
                            }
                        }
                    }
                });
            }
        }
    });

    /** show connections between the source and destination components */
    function showConnections() {
        if (self.src_comp !== null && self.dst_comp !== null) {
            busyDiv.show();
            project.getConnectivity(self.pathname)
                .done(function(data) {
                    if (!data) {
                        // don't have what we need, probably something got deleted
                        debug.warn('ConnectionFrame.showConnections(): Invalid data', data);
                        self.close();
                    }
                    else {
                        connection_data = data;
                        loadConnectionData(connection_data);
                    }
                })
                .fail(function(jqXHR, textStatus, errorThrown) {
                    debug.error('Error getting connections for',
                                self.pathname, self.src_comp, self.dst_comp,
                                jqXHR,textStatus,errorThrown);
                    self.close();
                });
        }
        else {
            connectionsDiv.hide();
            variablesDiv.hide();
        }
    }

    /** handle message containing the assembly connection data (dataflow) */
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            loadComponentData(message[1]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** if there is an object loaded, update it from the project */
    this.update = function() {
        if (self.pathname && self.pathname.length > 0) {
            self.editAssembly(self.pathname,self.src_comp,self.dst_comp);
        }
    };

    /** populate frame with connection data for the specified assembly */
    this.editAssembly = function(path, src_comp, dst_comp) {
        if (self.pathname !== path) {
           if (self.pathname !== null) {
                project.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            project.addListener(self.pathname, handleMessage);
        }

        self.src_comp = src_comp;
        self.dst_comp = dst_comp;

        project.getObject(path)
            .done(loadComponentData)
            .fail(function(jqXHR, textStatus, errorThrown) {
                debug.warn('ConnectionsFrame.editAssembly() Error:',
                            jqXHR, textStatus, errorThrown);
                // assume component has been deleted, so close frame
                self.close();
            });
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            project.removeListener(self.pathname, handleMessage);
        }
    };

    this.editAssembly(pathname, src_comp, dst_comp);
};

/** set prototype */
openmdao.ConnectionsFrame.prototype = new openmdao.BaseFrame();
openmdao.ConnectionsFrame.prototype.constructor = openmdao.ConnectionsFrame;
