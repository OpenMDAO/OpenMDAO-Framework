
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
        filter_beg = '_',
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
                
            if (filter_beg.indexOf(name[0])<0) {
                interfaces = JSON.stringify(interfaces)
                var node = { 'data': name  };
                node['attr'] = { 
                     'type'  : type,
                     'path'  : pathname,
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
            plugins     : [ "json_data", "sort", "themes", "types", "cookies", "contextmenu", "ui", "dnd" ],
            json_data   : { "data": convertJSON(json,'') },
            themes      : { "theme":  "classic" },
            cookies     : { "prefix": "objtree", opts : { path : '/' } },
            contextmenu : { "items":  contextMenu },
            dnd         : { /* drop_check: false means move is invalid, otherwise true */
                            "drop_check" : function (data) {
                                // data.o - the object being dragged
                                // data.r - the drop target                                
                                //debug.info("ObjectTree: drop_check:",data);
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
                                debug.info("ObjectTree: drop_finish:",data)
                                data.e.stopPropagation();
                                if (data.r.hasClass('WorkflowFigure')) {
                                    var component = openmdao.Util.getName(data.o.attr('path')),
                                        pathname  = data.r.data('pathname')
                                        cmd = 'top.'+pathname+'.workflow.add("'+component+'")';
                                    debug.info(cmd);
                                    model.issueCommand(cmd);
                                }
                            },
                            
                            /* drag_target: jquery selector matching all foreign nodes that can be dropped on the tree */
                            "drag_target" : ".objtype",
                            
                            /* drag_check: */
                            "drag_check" : function (data) {
                                debug.info("ObjectTree: drag_check:",data);
                                // data.o - the foreign object being dragged
                                // data.r - the hovered node
                                return true;
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
                                debug.info("ObjectTree: drag_finish:",data)
                            }
                          },            
        })
        .bind("select_node.jstree", function(e,data) {
            if (typeof select_fn == 'function') {
                var path = data.rslt.obj.attr("path");
                select_fn(path);
            }
        })
        .bind("dblclick.jstree", function (e,data) {
            if (typeof dblclick_fn == 'function') {
                var node = jQuery(e.target).closest("li"),
                    path = node.attr("path");
                dblclick_fn(model,path);
            }
        })
        // .bind("loaded.jstree", function (e, data) {
            // jQuery('#'+id+' a').draggable({ helper: 'clone', appendTo: 'body' })    // doesn't work ?
        // })
        // .one("reselect.jstree", function (e, data) { });
    }

    /** get a context menu for the specified node */
    function contextMenu(node) {
        
        var isAssembly = false;  // there's no "IAssembly" interface, so..
        if (! node.is('.jstree-leaf')) {
            isAssembly = true;
        }
        
        var path = node.attr('path'),
            type = node.attr('type'),
            interfaces = jQuery.parseJSON(node.attr('interfaces'));
            
        // now create the menu
        var menu = {}
        
        menu.properties = {
            "label"  : 'Properties',
            "action" :  function(node) { 
                            var id = (path+'-properties').replace(/\./g,'-')
                            new openmdao.PropertiesEditor(id,model).editObject(path)
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
                "label"  : 'Show Structure',
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
        menu.run = {
            "label"  : 'Run this Component',
            "action" :  function(node) { 
                            model.issueCommand('top.'+path+'.run()');
                        }
        };
        menu.toggle = {
            "label"  : 'Toggle Hidden Components',
            "action" :  function(node) { 
                            if (filter_beg.length == 0) {
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
                            var parent = openmdao.Util.getParentPath(path);
                            if (parent.length > 0 ) {
                                parent = 'top.'+parent;
                            }
                            else {
                                parent = 'top';
                            }
                            model.issueCommand(parent+'.remove("'+openmdao.Util.getName(path)+'")');
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
openmdao.ObjectTree.prototype = new openmdao.BaseFrame();
openmdao.ObjectTree.prototype.constructor = openmdao.ObjectTree;