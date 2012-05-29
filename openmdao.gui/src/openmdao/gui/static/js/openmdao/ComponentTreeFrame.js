
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ComponentTreeFrame = function(id,model,select_fn,dblclick_fn,workflow_fn,dataflow_fn) {
    var menu =  [
                    {   "text": "Component",
                        "items": [
                            { "text": "Add Component", "onclick": "alert('Sorry, not implemented yet :(');" }
                        ]
                    }
                ];
    openmdao.ComponentTreeFrame.prototype.init.call(this,id,'Objects',menu);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        filter_beg = '_',
        tree = jQuery('<div>').appendTo('<div style="height:100%">').appendTo("#"+id);

    /** make the parent pane droppable */
    /** TODO: handle this with jstree (i.e. drop into appropriate place in tree)
    tree.parent().droppable({
        accept: '.objtype',
        drop: function(ev,ui) {
                // get the object that was dropped
                var droppedObject = jQuery(ui.draggable).clone();
                // get the type name and path
                var typename = droppedObject.text();
                var typepath = droppedObject.attr("path");
                openmdao.Util.promptForValue('Specify a name for the new '+typename,function(name) {
                    model.addComponent(typepath,name);
                });
            }
    });
    **/

    /** convert model.json to structure required for jstree */
    function convertJSON(json, path) {
        var data = [];

        jQuery.each(json, function(idx,item) {
            var pathname   = item.pathname,
                type       = item.type,
                interfaces = item.interfaces,
                name = openmdao.Util.getName(pathname);

            if (filter_beg.indexOf(name[0])<0) {
                interfaces = JSON.stringify(interfaces);
                var node = { 'data': name  };
                node.attr = {
                     'type'  : type,
                     'path'  : pathname,
                     'interfaces' : interfaces
                };
                if (item.children) {
                    node.children = convertJSON(item.children,pathname);
                }
                data.push(node);
            }
        });
        return data;
    }

    /** update the tree with JSON model data  */
    function updateTree(json) {
        tree.empty();
        tree.jstree({
            plugins     : [ "json_data", "sort", "themes", "types", "cookies", "contextmenu", "ui", "crrm", "dnd"],
            json_data   : { "data": convertJSON(json,'') },
            themes      : { "theme":  "classic" },
            cookies     : { "prefix": "objtree", opts : { path : '/' } },
            contextmenu : { "items":  contextMenu },
            crrm        : { "move" : {
                                // don't allow moving within the tree (for now anyway)
                                "check_move" : function (m) {
                                    return false;
                                }
                            }
                          },
            dnd         : { /* drop_check: false means move is invalid, otherwise true */
                            "drop_check" : function (data) {
                                // data.o - the object being dragged
                                // data.r - the drop target
                                //debug.info("ComponentTreeFrame: drop_check:",data);
                                if (data.r.hasClass('WorkflowFigure')) {
                                    return true;
                                }
                                else {
                                    return false;
                                }
                            },

                            /* drop_target: jquery selector matching all drop targets */
                            "drop_target" : "*",

                            /* drop_finish: executed after a valid drop */
                            "drop_finish" : function (data) {
                                // data.o - the object being dragged
                                // data.r - the drop target
                                debug.info("ComponentTreeFrame: drop_finish:",data);
                                data.e.stopPropagation();
                                if (data.r.hasClass('WorkflowFigure')) {
                                    var component = openmdao.Util.getName(data.o.attr('path')),
                                        pathname  = data.r.data('pathname'),
                                        cmd = pathname+'.workflow.add("'+component+'")';
                                    debug.info(cmd);
                                    model.issueCommand(cmd);
                                }
                            },

                            /* drag_target: jquery selector matching all foreign nodes that can be dropped on the tree */
                            "drag_target" : ".objtype",

                            /* drag_check: */
                            "drag_check" : function (data) {
                                debug.info("ComponentTreeFrame: drag_check:",data);
                                // data.o - the foreign object being dragged
                                // data.r - the hovered node
                                return {
                                    after  : false,
                                    before : false,
                                    inside : false
                                };
                            },

                            /* drag_finish:  executed after a dropping a foreign element on a tree item */
                            "drag_finish" : function (data) {
                                // data.o - the foreign object being dragged
                                // data.r - the target node
                                debug.info("ComponentTreeFrame: drag_finish:",data);
                            }
                          }
        })
        .bind("select_node.jstree", function(e,data) {
            if (typeof select_fn === 'function') {
                var path = data.rslt.obj.attr("path");
                select_fn(path);
            }
        })
        .bind("dblclick.jstree", function (e,data) {
            if (typeof dblclick_fn === 'function') {
                var node = jQuery(e.target).closest("li"),
                    path = node.attr("path");
                dblclick_fn(path);
            }
        });
        // .bind("loaded.jstree", function (e, data) {
            // jQuery('#'+id+' a').draggable({ helper: 'clone', appendTo: 'body' })    // doesn't work ?
        // })
        // .one("reselect.jstree", function (e, data) { });
    }

    /** get a context menu for the specified node */
    function contextMenu(node) {

        var path = node.attr('path'),
            type = node.attr('type'),
            interfaces = jQuery.parseJSON(node.attr('interfaces'));

        // now create the menu
        var menu = {};

        menu.properties = {
            "label"  : 'Properties',
            "action" :  function(node) {
                            var id = (path+'-properties').replace(/\./g,'-');
                            new openmdao.PropertiesFrame(id,model).editObject(path);
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
                            model.issueCommand(path+'.run()');
                        }
        };
        menu.toggle = {
            "label"  : 'Toggle Hidden Components',
            "action" :  function(node) {
                            if (filter_beg.length === 0) {
                                filter_beg = '_';
                            }
                            else {
                                filter_beg = '';
                            }
                            update();
                        }
        };
        menu.remove = {
            "label"  : 'Remove',
            "action" :  function(node) {
                            model.removeComponent(path);
                        }
        };
        return menu;
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== 'components') {
            debug.warn('Invalid components data:',message);
        }
        else {
            components = jQuery.parseJSON(message[1]);
            updateTree(components);
        }
    }

    // listen for 'components' messages and update object tree accordingly
    model.addListener('components', handleMessage);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        model.removeListener('components', handleMessage);
    };

    /** update the tree, with data from the model  */
    this.update = function() {
        model.getComponents(updateTree);
    };

    // load initial component data
    this.update();
};

/** set prototype */
openmdao.ComponentTreeFrame.prototype = new openmdao.BaseFrame();
openmdao.ComponentTreeFrame.prototype.constructor = openmdao.ComponentTreeFrame;

