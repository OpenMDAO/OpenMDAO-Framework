
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowTreePane = function(elm, project, select_fn, dblclick_fn, workflow_fn, dataflow_fn) {
    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this;

    self.pathname = false;

    /** convert json to structure required for jstree */
    function convertJSON(json, parent, openNodes) {
        var data = [];
        jQuery.each(json, function(idx, item) {
            var pathname   = item.pathname,
                type       = item.type,
                interfaces = item.interfaces,
                name = openmdao.Util.getName(pathname);

            // Figure out what type we are (lacking interfaces)
            if (item.workflow) {
                interfaces = ['IDriver'];
                subpath = pathname;

                // Exception: top is always an assembly
                if (name === 'driver') {
                    interfaces = ['IAssembly'];
                    pathname = openmdao.Util.getPath(pathname);
                    name = openmdao.Util.getName(pathname) + '.driver';
                    parent = name;
                    subpath = pathname + '.driver';
                }
            }
            else if (item.driver) {
                interfaces = ['IAssembly'];
                name = name + '.driver';
                subpath = pathname + '.driver';
            }
            else {
                interfaces = ['IComponent'];
            }

            interfaces = JSON.stringify(interfaces);
            var node = { 'data': name  };
            node.attr = {
                 'type'  : type,
                 'path'  : pathname,
                 'interfaces' : interfaces,
                 'parent' : parent
            };

            // Driver recursion
            if (item.workflow) {
                node.children = convertJSON(item.workflow, subpath,
                                            openNodes);
            }
            // Assembly recursion
            else if (item.driver) {
                node.children = convertJSON(item.driver.workflow,
                                            subpath, openNodes);
            }

            if (openNodes.indexOf(pathname) >= 0) {
                node.state = 'open';
            }
            data.push(node);
        });

        return data;
    }

    /** update the tree with JSON project data */
    function updateTree(json) {

        // Grab paths of currently open nodes.
        var openNodes = [];
        elm.find("li.jstree-open").each(function () {
            openNodes.push(this.getAttribute("path"));
        });

        // We may get a single workflow object or a list of workflow objects
        // if we get a single workflow, stick it in a list for consistency
        if (!jQuery.isArray(json)) {
            json = [json];
        }

        elm.empty();
        elm.jstree({
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
                    path = node.attr("path"),
                    iface = node.attr('interfaces');
                if (iface.indexOf("IAssembly") >= 0) {
                    path = path + '.driver';
                }
                dblclick_fn(path);
            }
        })
        .bind("loaded.jstree", function (e, data) {
            elm.find('li').each(function() {
                // add classes so that the items in the tree are specific
                // to what they are: assembly, driver or component
                if (this.getAttribute("interfaces").indexOf("IAssembly") >= 0) {
                    this.children[1].children[0].addClass("jstree-assembly");
                }
                else if (this.getAttribute("interfaces").indexOf("IDriver") >= 0) {
                    this.children[1].children[0].addClass("jstree-driver");
                }
                else if (this.getAttribute("interfaces").indexOf("IComponent") >= 0) {
                    this.children[1].children[0].addClass("jstree-component");
                }

                // make drop targets
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

                        if (target_iface.indexOf("IComponent") >= 0) {
                            // Don't do anything if we drop on a component.
                            cmd='';
                        }
                        if (target_iface.indexOf("IAssembly") >= 0) {
                            // Assemblies require some extra care.
                            target_path = this.parentElement.getAttribute('path');
                            cmd = target_path + '.driver.workflow.add("' + openmdao.Util.getName(source_path) + '", check=True)';
                        }
                        else {
                            target_path = this.parentElement.getAttribute('path');
                            cmd = target_path + '.workflow.add("' + openmdao.Util.getName(source_path) + '", check=True)';
                        }
                        if (cmd) {
                            //console.log(cmd);
                            project.issueCommand(cmd);
                        }
                    }
                });
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
                            if (jQuery.inArray('IAssembly',interfaces) >= 0) {
                                subpath = path + '.driver';
                            }
                            else {
                                subpath = path;
                            }
                            var id = (subpath+'-properties').replace(/\./g,'-');
                            new openmdao.PropertiesFrame(id, project).editObject(subpath);
                        }
        };
        if (jQuery.inArray('IAssembly',interfaces) >= 0) {
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
        }
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
                            project.runComponent(path);
                        }
        };
        if (parent !== '') {
            menu.remove = {
                "label"  : 'Remove from Workflow',
                "action" :  function(node) {
                                var cmd = parent +".workflow.remove('" + name + "')";
                                //console.log(cmd);
                                project.issueCommand(cmd);
                            }
            };
        }
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
    project.addListener('', handleMessage);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        project.removeListener('', handleMessage);
    };

    /** update the tree, with data from the project  */
    this.update = function() {
        project.getWorkflow('')
            .done(updateTree)
            .fail(function(jqXHR, textStatus, err) {
                debug.error("Error getting workflow", jqXHR, textStatus, err);
            });
    };

    this.update();
};
