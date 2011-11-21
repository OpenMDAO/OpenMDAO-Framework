
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.FileTree = function(id,model,code_fn,geom_fn) {
    var menu = [  
                    {   "text": "File",
                        "items": [
                            { "text": "New File",      "onclick": "openmdao.model.newFile();" },
                            { "text": "New Folder",    "onclick": "openmdao.model.newFolder();" },
                            { "text": "Add File",      "onclick": "openmdao.model.uploadFile();" }
                        ]
                    }
                ];
    openmdao.PropertiesEditor.prototype.init.call(this,id,'Files',menu);
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this,
        tree = jQuery('<div>').appendTo('<div style="height:100%">').appendTo("#"+id),
        filter_beg = '_.',
        filter_ext = [ 'pyc', 'pyd' ],
        filter_active = true;
        
    // ask model for an update whenever something changes
    model.addListener(update);
        
    /** recursively build an HTML representation of a JSON file structure */
    function getFileHTML(path,val) {
        // get the file name and extension 
        var path = path.replace(/\\/g,'/'),
            name = path.split('/'),
            name = name[name.length-1],
            ext = name.split('.'),
            ext = ext[ext.length-1];
            
        if (!filter_active || ((filter_beg.indexOf(name[0])<0 && filter_ext.indexOf(ext)<0))) {
            var html = "<li><a"
            if (typeof val === 'object') {    // a folder
                html += " class='folder' path='"+path+"'>"+name+"</a>"
                html += "<ul>"
                jQuery.each(val,function(path,val) {
                    html += getFileHTML(path,val)
                })
                html += "</ul>"
            }
            else
                html += " class='file' path='"+path+"'>"+name+"</a>"
            html += "</li>"
        }
        return html
    }
    
    /** if we have an edit function, then call it on the specified file */
    function editFile(pathname) {
        if (typeof code_fn == 'function')
            code_fn(pathname)
        else
            alert("Edit function is not defined")
    }

    /** display the file in a new window (probably not in a useful format) */
    function viewFile(pathname) {
        openmdao.Util.popupWindow('file'+pathname.replace(/\\/g,'/'),pathname,600,800)
    }


    /** if we have a view geometry function, then call it on the specified file */
    function viewGeometry(pathname) {
        if (typeof geom_fn == 'function')
            geom_fn(pathname)
        else
            alert("View Geometry function is not defined")
    }

    /** get a context menu for the specified node */
    function contextMenu(node) {
        // first let's see what was clicked on
        var isFolder = false, isEmptyFolder = false
        if (node.is('.jstree-leaf')) {
            filenode = node.find('.file')
            if (filenode.length == 0) {
                filenode = node.find('.folder')
                if (filenode.length > 0) {
                    isFolder = true
                    isEmptyFolder = true
                }
                else
                    alert("WTF?!") // leaf node that's not a file or empty folder?
            }
        }
        else {
            filenode = node.find('.folder')
            if (filenode.length > 0) {
                isFolder = true
                isEmptyFolder = false
            }
            else
                alert("WTF?!") // non-leaf node that's not a folder?
        }
        
        var path = filenode.attr('path')

        // now create the menu
        var menu = {}
        
        // if they clicked on a folder then create new files inside that folder
        if (isFolder) {
            menu.create = {
                "label"  : 'New File',
                "action" : function(node) { model.newFile(path) }
            }
            menu.add = {
                "label"  : 'Add File',
                "action" : function(node) { model.uploadFile(path) }
            }
            menu.createFolder = {
                "label"  : 'New Folder',
                "action" : function(node) { model.newFolder(path) }
            }
        }
        else {
            menu.create = {
                "label"  : 'New File',
                "action" : function(node) { model.newFile() }
            }
            menu.add = {
                "label"  : 'Add File',
                "action" : function(node) { model.uploadFile() }
            }
            menu.createFolder = {
                "label"  : 'New Folder',
                "action" : function(node) { model.newFolder() }
            }
        }

        // TODO: implement rename()
        menu.rename = {
            "label"  : 'Rename',
            "action" : function(node) { alert("Rename") }
        }
        
        // if it's not a folder, 
        if (!isFolder) {
            // view file in another window (TODO: make this useful, e.g. display image, format text or w/e)
            menu.viewFile = {
                "label"  : 'View File',
                "action" : function(node) { viewFile(path) }
            };
            // let them edit it (TODO: filter out non-text files?)
            menu.editFile = {
                "label"  : 'Edit File',
                "action" : function(node) { editFile(path) }
            };
            // if it's a py file, let them import or execute it
            if (/.py$/.test(path)) {
                menu.importfile = {
                    "label"  : 'Import * from File',
                    "action" : function(node) { model.importFile(path) }
                };          
                menu.execfile = {
                    "label"  : 'Execute File',
                    "action" : function(node) { model.execFile(path) }
                };           
            };
            // if it's a geometry file, let them load it into viewer
            if (/.geom$/.test(path)) {
                menu.viewGeometry = {
                    "label"  : 'View Geometry',
                    "action" : function(node) { viewGeometry('file'+path.replace(/\\/g,'/')) }
                };            
            };
        };

        // delete only files and empty folders
        if (!isFolder) {
            menu.deleteFile = {
                "label"  : 'Delete File',
                "action" : function(node) { model.removeFile(path); }
            }
        }
        else if (isEmptyFolder) {
            menu.deleteFolder = {
                "label"  : 'Delete Empty Folder',
                "action" : function(node) { model.removeFile(path); }
            }
        }
        
        // set WD
        if (isFolder) {
            menu.setWD = {
                "label"  : 'Set as WD',
                "action" : function(node) { model.setFolder(path) }
            }    
        }
        menu.toggle = {
            "label"  : 'Toggle Hidden Files',
            "action" :  function(node) { filter_active = !filter_active; update(); }
        };
        
        return menu
    }
        
    /** update the tree from JSON file structure */
    function updateFiles(files) {
        // generate HTML for the file tree
        var html = "<ul>";
        jQuery.each(files,function(path,val) {
            html += getFileHTML(path,val);
        })
        html += "</ul>";
        
        // replace old html
        tree.html(html);
        
        // convert to a jstree
        tree.jstree({
            "plugins" :     [ "html_data", "sort", "themes", "types", "cookies", "contextmenu", "ui" ],
            "themes" :      { "theme":  "classic" },
            "cookies" :     { "prefix": "filetree", opts : { path : '/' } },
            "contextmenu" : { "items":  contextMenu }
        })
        .bind("loaded.jstree", function (e) {
            jQuery('#'+id+' .file').draggable({ helper: 'clone', appendTo: 'body' })
        })
        .bind("dblclick.jstree", function (e,tree) {
            var node = jQuery(e.target),            
                path = node.attr("path");
            if (node.hasClass('file')) {
                editFile(path);
            }
            else if (node.hasClass('folder')) {
                // what do, what do
            }
            else {
                debug.warn("node in file tree does not seem to be a file or a folder:",node);
            }
        })
    }

    /** update the display, with data from the model */
    function update() {
        tree.html("<div>Updating...</div>")
            .effect('highlight',{color:'#ffd'},1000);
        model.getFiles(updateFiles);
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
}

/** set prototype */
openmdao.FileTree.prototype = new openmdao.BaseFrame();
openmdao.FileTree.prototype.constructor = openmdao.FileTree;
