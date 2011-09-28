
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ObjectTree = function(id,model,select_fn,dblclick_fn,workflow_fn,dataflow_fn) {
    var menu =  [
                    {   "text": "Component", 
                        "items": [
                            { "text": "Add Component", "onclick": "alert('Sorry, not implemented yet :(');" },
                        ]
                    },
                ];
    openmdao.ObjectTree.prototype.init.call(this,id,'Objects',menu);
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this,
        filterChars = '_',
        tree = jQuery('<div>').appendTo('<div style="height:100%">').appendTo("#"+id)

    /**  make the parent pane droppable * /
    tree.parent().droppable({
    accept: '.objtype',
    drop: function(ev,ui) { 
            debug.info("objtree drop:",ev,ui)
            // get the object that was dropped
            var droppedObject = jQuery(ui.draggable).clone();
            // get the type name and path
            var typename = droppedObject.text();
            var typepath = droppedObject.attr("path");
            openmdao.Util.promptForName(function(name) { 
                model.addComponent(typepath,name);
            })
        }
    })
    /**/
        
    // ask model for an update whenever something changes
    model.addListener(update)
    
    /** convert model.json to structure required for jstree */
    function convertJSON(json, path) {
        var data = [];
            
        jQuery.each(json, function(idx,item) {
            var pathname   = item['pathname'],
                type       = item['type'],
                interfaces = item['interfaces'],
                name = openmdao.Util.getName(pathname);
                
            if (filterChars.indexOf(name[0])<0) {
                var node = { 'title': name  };
                node['data'] = { 
                     'type'  : type,
                     'path'  : pathname,
                     'title' : type+': '+name,
                     'interfaces' : interfaces
                };
                if (item['children']) {
                    node['children'] = convertJSON(item['children'],pathname);
                };
                data.push(node)
            }
        })
        return data
    }
  
    /** update the tree with JSON model data  */
    function updateTree(json) {
        tree.empty()        
        tree.jstree({
            plugins     : [ "json", "sort", "themes", "types", "cookies", "contextmenu", "ui", "dnd" ],
            json        : { "data": convertJSON(json,'') },
            themes      : { "theme":  "classic-dark" },
            cookies     : { "prefix": "objtree", opts : { path : '/' } },
            contextmenu : { "items":  contextMenu },
            dnd         : { 
                            /* drop_check: false means move is invalid, otherwise true */
                            "drop_check" : function (data) {
                                // data.o - the object being dragged
                                // data.r - the drop target                                
                                debug.info("drop_check:",data);
                                return true;
                            },
                            
                            /* drop_target: jquery selector matching all drop targets */
                            "drop_target" : ".jstree-drop",
                            
                            /* drop_finish: executed after a valid drop */
                            "drop_finish" : function (data) { 
                                // data.o - the object being dragged
                                // data.r - the drop target                                
                                debug.info("drop_finish:",data)
                                debug.info(data.o,"was dropped on",data.r);
                            },
                            
                            /* drag_target: jquery selector matching all foreign nodes that can be dropped on the tree */
                            "drag_target" : ".objtype, .ui-draggable, .jstree-draggable, .ui-droppable",
                            
                            /* drag_check: */
                            "drag_check" : function (data) {
                                debug.info("drag_check:",data);
                                // data.o - the foreign object being dragged
                                // data.r - the hovered node
                                return { 
                                    after : true, 
                                    before : true, 
                                    inside : true 
                                };
                            },
                            
                            /* drag_finish:  executed after a dropping a foreign element on a tree item */
                            "drag_finish" : function (data) { 
                                // data.o - the foreign object being dragged
                                // data.r - the target node
                                debug.info("drag_finish:",data)
                            }
                          },            
        })
        .bind("select_node.jstree", function(e,data) {
            if (typeof select_fn == 'function') {
                var meta = data.rslt.obj.data(),
                    path = meta["path"];
                select_fn(path)
            }
        })
        .bind("dblclick.jstree", function (e,data) {
            if (typeof dblclick_fn == 'function') {
                var node = jQuery(e.target).closest("li"),
                    meta = node.data(),
                    path = meta["path"];
                dblclick_fn(model,path)
            }
        })
        .bind("loaded.jstree", function (e, data) {
            jQuery('#'+id+' .obj').draggable({ helper: 'clone', appendTo: 'body' })
        })
        .one("reselect.jstree", function (e, data) { });
    }

    /** get a context menu for the specified node */
    function contextMenu(node) {
        // first let's see what was clicked on
        var isAssembly = false;  // there's no "IAssembly" interface, so..
        if (node.is('.jstree-leaf')) {
            debug.log('ObjectTree.contextMenu: clicked on leaf node');
            debug.log(node);
        }
        else {
            debug.log('ObjectTree.contextMenu: clicked on non-leaf node');
            debug.log(node);
            isAssembly = true;
        }
        
        var metadata = node.data();
        debug.info(metadata)
        var path = metadata['path'],
            type = metadata['type'],
            interfaces = metadata['interfaces']
            
        
        // now create the menu
        var menu = {}
        
        menu.properties = {
            "label"  : 'Properties',
            "action" :  function(node) { 
                            new openmdao.PopupPropertiesEditor(model,path)
                        }
        };
        if (jQuery.inArray('IDriver',interfaces) >= 0) {
            menu.show_workflow = {
                "label"  : 'Show Workflow',
                "action" :  function(node) { 
                                workflow_fn(path);
                            }
            }
        };
        if (isAssembly) {
            menu.show_dataflow = {
                "label"  : 'Show Data Connections',
                "action" :  function(node) { 
                                dataflow_fn(path);
                            }
            }
            menu.set_top = {
                "label"  : 'Set as Top',
                "action" :  function(node) { 
                                model.setTop(path)
                            }
            };
        };
        menu.add_to_workflow = {
            "label"  : 'Add to Workflow',
            "action" :  function(node) { 
                            // TODO: need to show list of workflows and allow user to pick one
                            model.issueCommand('top.driver.workflow.add("'+path+'")');
                        }
        };
        menu.run = {
            "label"  : 'Run this Component',
            "action" :  function(node) { 
                            model.issueCommand('top.'+path+'.run()');
                        }
        };
        menu.toggle = {
            "label"  : 'Toggle Underscores',
            "action" :  function(node) { 
                            if (filterChars.length == 0)
                                filterChars = '_';
                            else
                                filterChars = '';
                            model.getJSON(updateTree);
                        }
        };
        menu.remove = {
            "label"  : 'Remove',
            "action" :  function(node) { 
                            model.issueCommand('top.'+openmdao.Util.getParentPath(path)+
                                '.remove("'+openmdao.Util.getName(path)+'")');
                        }
        };        
        return menu;
    }
    
    /** update the tree, with data from the model  */
    function update() {
        model.getComponents(updateTree)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
 
}

/** set prototype */
openmdao.ObjectTree.prototype = new openmdao.BasePane();
openmdao.ObjectTree.prototype.constructor = openmdao.ObjectTree;