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
openmdao.ObjectTree = function(id,model,edit_function) {
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        elm = jQuery('#'+id),
        filterChars = '_' // filter objects with names that start with these chars
        
    /** make the tree pane droppable */
    elm.parent().droppable({
        accept: '.objtype',
        drop: function(ev,ui) { 
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
    
    /** convert model.json to structure required for jstree */
    function convertJSON(json, path) {
        var data = []
        jQuery.each(json, function(name,item) {
            var showObj = filterChars.indexOf(name[0])<0 
                        && name.indexOf('py/')<0 
                        && !(item && item['py/type'])
                        && !(item && item['py/repr'])
            if (showObj) {
                var pathname = path.length>0 ? path+'.'+name : name,
                    node = { 'data': name },
                    itemType = typeof item
                    
                if (itemType !== "undefined") {
                    if (item == null) {
                        node['attr'] = { 
                             'class' : 'null',
                             'path'  : pathname,
                             'title' : 'null' 
                        }
                        node['value'] = null
                    }
                    else if (itemType=='number' || itemType=='string' || itemType=='boolean' ) {
                        node['attr'] = { 
                             'class' : 'var',
                             'path'  : pathname,
                             'title' : itemType+': '+item
                        }
                        node['value'] = item
                    }
                    else if (item instanceof Array) {
                        node['attr'] = { 
                             'class' : 'array', 
                             'path'  : pathname,
                             'title' : 'array: '+item.join("")
                        }
                        node['value'] = item
                    }
                    else if (item['py/object']) {
                        var tokens = item['py/object'].split('.'),
                            typename = tokens[tokens.length-1]
                        node['attr'] = { 
                             'class' : 'obj',
                             'path'  : pathname,
                             'title' : typename
                        }
                        node['objtype'] = item['py/object']
                        // TODO: may want to do something special with traits in general
                        if (item['py/seq']) // array/list object (traits)
                            node['value'] = item['py/seq']
                        else if (item['py/state'])
                            node['children'] = convertJSON(item['py/state'],pathname)
                        else {
                            debug.warn('ObjectTree.convertJSON: '+name+' is a py/object with no py/state..')
                            // TODO? what's the diff between this and a py/object with a py/state?
                            node['children'] = convertJSON(item,pathname)
                        }
                    }
                    else if (item['py/ref']) {
                        node['attr'] = { 
                             'class' : 'ref',
                             'path'  : pathname,
                             'title' : 'link to: '+item['py/ref']
                        }
                        node['value'] = item['py/ref']
                        //node['data'] = node['data'] + ' --> ' + node['ref']
                    }
                    else {  // just a plain old container I guess
                        node['attr'] = { 
                             'class' : 'obj',
                             'path'  : pathname,
                             'title' : 'object'
                        }
                        node['children'] = convertJSON(item,pathname)
                    }
                }
                data.push(node)
            }
        })
        return data
    }

    /** update the tree with JSON model data  */
    function updateTree(json) {
        jQuery.jstree._themes = "/static/css/jstree/";
        elm.empty()        
        elm.jstree({
            plugins     : [ "json_data", "sort", "themes", "types", "cookies", "contextmenu", "ui" ],
            json_data   : { "data": convertJSON(json,''), "progressive_render": true },
            themes      : { "theme":  "classic" },
            cookies     : { "prefix": "objtree", opts : { path : '/' } },
            contextmenu : { "items":  contextMenu }
        })
        .bind("select_node.jstree", function(e,data) {
            if (typeof edit_function == 'function') {
                var pathname = data.rslt.obj.attr("path")
                edit_function(pathname)
            }
        }); 
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
                            var tableID = 'PE-'+path.replace(/\./g,'-'),
                                table = jQuery('<table id='+tableID+'></table>')
                            table.dialog({
                                'modal': false,
                                'title': 'Properties: '+path,
                                'close': function(ev, ui) { jQuery(tableID).remove() }
                            })
                            new openmdao.PropertiesEditor(tableID,model).editObject(path)
                        }
        }
        menu.add_to_workflow = {
            "label"  : 'Add to Workflow',
            "action" :  function(node) { 
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
    
    // ask model for an update whenever something changes
    model.addListener(update)
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
 
}
