/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.ObjectTree = function(id,model,select_fn,dblclick_fn) {
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    if (arguments.length > 0) {
        // initialize private variables
        var self = this,
            tree = null,
            filterChars = '_'
        // build it
        init()
    }

    function init() {
        // initialize the base pane
        self.prototype = Object.create(openmdao.BasePane, {
            id:     { value: id },
            title:  { value: "Objects" },
            menu:   { value: 
                        [
                            {   "text": "Component", 
                                "items": [
                                    { "text": "Add Component", "onclick": "alert('Sorry, not implemented yet :(');" },
                                ]
                            },
                        ],
                    }
        })
        self.prototype.init()
        
        // add a div for the tree and make it droppable
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
    }
    
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
        if (node.is('.jstree-leaf')) {
            debug.log('ObjectTree.contextMenu: clicked on leaf node')
            debug.log(node)
        }
        else {
            debug.log('ObjectTree.contextMenu: clicked on non-leaf node')
            debug.log(node)
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
        }
        menu.add_to_workflow = {
            "label"  : 'Add to Workflow',
            "action" :  function(node) { 
                            // TODO: need to show list of workflows and allow user to pick one
                            model.issueCommand('top.driver.workflow.add("'+path+'")')
                        }
        }
        menu.toggle = {
            "label"  : 'Toggle Underscores',
            "action" :  function(node) { 
                            if (filterChars.length == 0)
                                filterChars = '_'
                            else
                                filterChars = ''
                            model.getJSON(updateTree)
                        }
        }
        
        return menu
    }
    
    /** update the tree, with data from the model  */
    function update() {
        model.getComponents(updateTree)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
 
}
