
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowTreeFrame = function(id, model, select_fn, dblclick_fn, workflow_fn, dataflow_fn) {
    openmdao.WorkflowTreeFrame.prototype.init.call(this, id, 'Workflow');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        tree = jQuery('<div>')
            .appendTo('<div style="height:100%">')
            .appendTo("#"+id);

    self.pathname = false;

    /** convert model.json to structure required for jstree */
    function convertJSON(json, path, openNodes) {
        var data = [];
        jQuery.each(json, function(idx,item) {
            var pathname   = item.pathname,
                type       = item.type,
                interfaces = item.interfaces,
                name = openmdao.Util.getName(pathname);
            
            // Figure out what type we are (lacking interfaces)
            if (item.workflow) {
                interfaces = ['IDriver'];
            }
            else if (item.driver) {
                interfaces = ['IAssembly'];
                name = name + '.driver';
            }
            else {
                interfaces = ['IComponent'];
            }
            
            // if my name is just 'driver', qualify with parent (assembly) name
            if (name === 'driver') {
                interfaces = ['IAssembly'];
                parent_assy = openmdao.Util.getPath(pathname);
                name = openmdao.Util.getName(parent_assy) + '.driver';
            }            
            
            interfaces = JSON.stringify(interfaces);
            var node = { 'data': name  };
            node.attr = {
                 'type'  : type,
                 'path'  : pathname,
                 'interfaces' : interfaces,
                 'parent' : path
            };
            // Driver recursion
            if (item.workflow) {
                node.children = convertJSON(item.workflow, pathname,
                                            openNodes);
            }
            // Assembly recursion
            else if (item.driver) {
                node.children = convertJSON(item.driver.workflow,
                                            pathname + '.driver',
                                            openNodes);
            }
            
            if (openNodes.indexOf(pathname) >= 0) {
                node.state = 'open';
            }
            data.push(node);
        });

        return data;
    }

    /** update the tree with JSON model data */
    function updateTree(json) {
    
        // Grab paths of currently open nodes.
        var openNodes = [];
        self.elm.find("li.jstree-open").each(function () {
            openNodes.push(this.getAttribute("path"));
        });
        
        // We may get a single workflow object or a list of workflow objects
        // if we get a single workflow, stick it in a list for consistency
        if (!jQuery.isArray(json)) {
            json = [json];
        }

        tree.empty();
        tree.jstree({
            plugins     : [ "json_data", "themes", "types", "cookies", "contextmenu", "ui", "crrm" ],
            json_data   : { "data": convertJSON(json, '', openNodes) },
            themes      : { "theme":  "openmdao" },
            cookies     : { "prefix": "objtree", opts : { path : '/' } },
            contextmenu : { "items":  contextMenu },
            core        : { "animation" : 0 },
            crrm        : { "move" : {
                                // don't allow moving within the tree (for now anyway)
                                "check_move" : function (m) {
                                    return false;
                                }
                            }
                          }
        })
        /*.bind("select_node.jstree", function(e, data) {
            if (typeof select_fn === 'function') {
                var path = data.rslt.obj.attr("path");
                select_fn(path);
            }
        })*/
        .bind("dblclick.jstree", function (e, data) {
            if (typeof dblclick_fn === 'function') {
                var node = jQuery(e.target).closest("li"),
                    path = node.attr("path");
                dblclick_fn(path);
            }
        })
        .bind("loaded.jstree", function (e, data) {
            jQuery('#'+id+' li').each(function() {
                jQuery(this.children[1]).droppable({
                    accept: '.DataflowFigure',
                    greedy: true,
                    tolerance: 'pointer',
                    out: function(ev, ui){
                        target_iface = this.parentElement.getAttribute('interfaces');
                        if (target_iface.indexOf("IComponent") >= 0) {
                            //this.parentElement.parentElement.parentElement.children[1].removeClass('jstree-hovered');
                        }
                        else {
                            this.removeClass('jstree-hovered');
                        }
                    },
                    over: function(ev, ui){
                        target_iface = this.parentElement.getAttribute('interfaces');
                        if (target_iface.indexOf("IComponent") >= 0) {
                            //this.parentElement.parentElement.parentElement.children[1].addClass('jstree-hovered');
                        }
                        else {
                            this.addClass('jstree-hovered');
                        }
                    },
                    drop: function(ev, ui) {
                        
                        var droppedObject = jQuery(ui.draggable).clone(),
                            source_path = droppedObject.attr('pathname'),
                            target_iface = this.parentElement.getAttribute('interfaces');
                        
                        // If we dropped on a driver, add to its workflow.
                        // Otherwise, add to the parent driver's workflow.
                        if (target_iface.indexOf("IComponent") >= 0) {
                            target_path = this.parentElement.getAttribute('parent');
                        }
                        else {
                            target_path = this.parentElement.getAttribute('path');
                        }
                        cmd = target_path + '.workflow.add("' + openmdao.Util.getName(source_path) + '", check=True)';
                        model.issueCommand(cmd);                        
                    }
                });
            });

            /* add classes so that the items in the component tree are specific
               to what they are: assembly, driver or component */
            jQuery('#'+id+' li').each(function () {
            
                if (this.getAttribute("interfaces").indexOf("IAssembly") >= 0) {
                    this.children[1].children[0].addClass("jstree-assembly");
                }
                else if (this.getAttribute("interfaces").indexOf("IDriver") >= 0) {
                    this.children[1].children[0].addClass("jstree-driver");
                }
                else if (this.getAttribute("interfaces").indexOf("IComponent") >= 0) {
                    this.children[1].children[0].addClass("jstree-component");
                }
            });
        });
    }

    /** get a context menu for the specified node */
    function contextMenu(node) {
        var path = node.attr('path'),
            name = openmdao.Util.getName(path);
            type = node.attr('type'),
            parent = node.attr('parent'),
            interfaces = jQuery.parseJSON(node.attr('interfaces')),
            menu = {};

        menu.properties = {
            "label"  : 'Properties',
            "action" :  function(node) {
                            var id = (path+'-properties').replace(/\./g,'-');
                            new openmdao.PropertiesFrame(id, model).editObject(path);
                        }
        };
        /*if (jQuery.inArray('IAssembly',interfaces) >= 0) {
            menu.show_dataflow = {
                "label"  : 'Show Dataflow',
                "action" :  function(node) {
                                dataflow_fn(path);
                            }
            };
            // shortcut to driver workflow
            menu.show_workflow = {
                "label"  : 'Show Workflow',
                "action" :  function(node) {
                                workflow_fn(path+'.driver');
                            }
            };
        }*/
        if (jQuery.inArray('IDriver',interfaces) >= 0) {
            menu.show_workflow = {
                "label"  : 'Show Workflow',
                "action" :  function(node) {
                                workflow_fn(path);
                            }
            };
        }
        menu.run = {
            "label"  : 'Run this Component',
            "action" :  function(node) {
                            model.runComponent(path);
                        }
        };
        if (parent !== '') {
            menu.remove = {
                "label"  : 'Remove from Workflow',
                "action" :  function(node) {
                                var cmd = parent +".workflow.remove('" + name + "')";
                                model.issueCommand(cmd);
                            }
            };
        };
        return menu;
    }

    function handleMessage(message) {
        if (message.length !== 2) {
            debug.warn('Invalid component data for:', self.pathname, message);
        }
        else {
            if (message[1].hasOwnProperty('Workflow')) {
                var workflow = message[1].Workflow;
                if (typeof workflow === 'string') {
                    workflow = jQuery.parseJSON(workflow);
                }
                updateTree(workflow);
            }
        }
    }

    // listen for 'workflow' messages and update object tree accordingly
    model.addListener('', handleMessage);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        model.removeListener('', handleMessage);
    };

    /** update the tree, with data from the model  */
    this.update = function() {
        model.getWorkflow('',
                          updateTree,
                          function(jqXHR, textStatus, errorThrown) {
                              debug.error("Error getting workflow ", jqXHR);
                          });
    };

    // load initial component data
    model.model_ready.always(function() {
       self.update();
    });
};

/** set prototype */
openmdao.WorkflowTreeFrame.prototype = new openmdao.BaseFrame();
openmdao.WorkflowTreeFrame.prototype.constructor = openmdao.WorkflowTreeFrame;

