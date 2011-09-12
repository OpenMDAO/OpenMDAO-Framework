
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ObjectTree = function(id,model,select_fn,dblclick_fn) {
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
        tree.parent().droppable({
            accept: '.objtype',
            drop: function(ev,ui) { 
                // get the object that was dropped
                var droppedObject = jQuery(ui.draggable).clone();
                debug.info('ObjectTree drop',droppedObj)
                // get the type name and path
                var typename = droppedObject.text();
                var typepath = droppedObject.attr("path");
                openmdao.Util.promptForName(function(name) { 
                    model.addComponent(typepath,name);
                })
            }
        })
        
    // ask model for an update whenever something changes
    model.addListener(update)
    
    /** convert model.json to structure required for jstree */
    function convertJSON(json, path) {
        var data = []
        jQuery.each(json, function(name,item) {
            //debug.info(name,item)
            var showObj = filterChars.indexOf(name[0])<0 
                        && name.indexOf('py/')<0 
            if (showObj) {
                var pathname = path.length>0 ? path+'.'+name : name,
                    node = { 'data': name }
                    node['attr'] = { 
                         'class' : 'obj',
                         'path'  : pathname,
                         'title' : name
                    }
                    node['children'] = convertJSON(item,pathname)
                //debug.info(node)
                data.push(node)
            }
        })
        return data
    }
  
    /** update the tree with JSON model data  */
    function updateTree(json) {
        jQuery.jstree._themes = "/static/css/jstree/";
        tree.empty()        
        tree.jstree({
            plugins     : [ "json_data", "sort", "themes", "types", "cookies", "contextmenu", "ui" ],
            json_data   : { "data": convertJSON(json,'') },
            themes      : { "theme":  "classic" },
            cookies     : { "prefix": "objtree", opts : { path : '/' } },
            contextmenu : { "items":  contextMenu },
        })
        .bind("select_node.jstree", function(e,data) {
            if (typeof select_fn == 'function') {
                var pathname = data.rslt.obj.attr("path")
                select_fn(pathname)
            }
        })
        .bind("dblclick.jstree", function (e,data) {
            if (typeof dblclick_fn == 'function') {
                var node = jQuery(e.target).closest("li")
                var pathname = node.attr("path")
                dblclick_fn(model,pathname)
            }
        })
        .bind("loaded.jstree", function (e, data) {
            jQuery('#'+id+' .obj').draggable({ helper: 'clone', appendTo: 'body' })
        })
        
    }

    /** get a context menu for the specified node */
    function contextMenu(node) {
        // first let's see what was clicked on
        var isAssembly = false;
        if (node.is('.jstree-leaf')) {
            debug.log('ObjectTree.contextMenu: clicked on leaf node');
            debug.log(node);
        }
        else {
            debug.log('ObjectTree.contextMenu: clicked on non-leaf node');
            debug.log(node);
            isAssembly = true;
        }
        var path = node.attr('path')
        
        // now create the menu
        var menu = {}
        
        // TODO: implement stuff
        menu.properties = {
            "label"  : 'Properties',
            "action" :  function(node) { 
                            new openmdao.PopupPropertiesEditor(model,path)
                        }
        };
        if (isAssembly) {
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