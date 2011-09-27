
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.FileTree = function(id,model,edit_function,view_function) {
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
        tree = jQuery('<div>').appendTo('<div style="height:100%">').appendTo("#"+id)
        
    // ask model for an update whenever something changes
    model.addListener(update)
        
    /** recursively build an HTML representation of a JSON file structure */
    function getFileHTML(path,val) {
        // get the file name without the path for display 
        if (path[0]==='/')
            var names = path.split('/')
        else
            var names = path.split('\\')
        var name = names[names.length-1]
        
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
        return html
    }
    
    /** if we have an edit function, then call it on the specified file */
    function editFile(pathname) {
        if (typeof edit_function == 'function')
            edit_function(pathname)
        else
            alert("Edit function is not defined")
    }

    /** if we have a view function, then call it on the specified file */
    function viewFile(pathname) {
        if (typeof view_function == 'function')
            view_function(pathname)
        else
            alert("View function is not defined")
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
            // let them edit it (TODO: filter out non-text files?)
            menu.update = {
                "label"  : 'Edit File',
                "action" : function(node) { editFile(path) }
            }
            // if it's a py file, let them import or execute it
            if (/.py$/.test(path)) {
                menu.importfile = {
                    "label"  : 'Import * from File',
                    "action" : function(node) { model.importFile(path) }
                }            
                menu.execfile = {
                    "label"  : 'Execute File',
                    "action" : function(node) { model.execFile(path) }
                }            
            }
            // if it's a json file, assume for now it's geometry and let them load it into viewer
            if (/.json$/.test(path)) {
                menu.viewGeometry = {
                    "label"  : 'View Geometry',
                    "action" : function(node) { viewFile('file'+path.replace(/\\/g,'/')) }
                }            
            }
        }

        // delete only files and empty folders
        if (!isFolder) {
            menu.delete = {
                "label"  : 'Delete File',
                "action" : function(node) { model.removeFile(path) }
            }
        }
        else if (isEmptyFolder) {
            menu.delete = {
                "label"  : 'Delete Empty Folder',
                "action" : function(node) { model.removeFile(path) }
            }
        }
        
        // set WD
        if (isFolder) {
            menu.setWD = {
                "label"  : 'Set as WD',
                "action" : function(node) { model.setFolder(path) }
            }    
        }
        
        return menu
    }
        
    /** update the tree from JSON file structure */
    function updateFiles(files) {
        // generate HTML for the file tree
        var html = "<ul>"
        jQuery.each(files,function(path,val) {
            html += getFileHTML(path,val)
        })
        html += "</ul>"
        
        // replace old html
        tree.html(html)
        
        // convert to a jstree
        tree.jstree({
            "plugins" :     [ "html", "sort", "themes", "types", "cookies", "contextmenu", "ui" ],
            "themes" :      { "theme":  "classic-dark" },
            "cookies" :     { "prefix": "filetree", opts : { path : '/' } },
            "contextmenu" : { "items":  contextMenu }
        })
        .bind("loaded.jstree", function (e, data) {
            jQuery('#'+id+' .file').draggable({ helper: 'clone', appendTo: 'body' })
        })
    }

    /** update the display, with data from the model */
    function update() {
        tree.html("<div>Updating...</div>")
                 .effect('highlight',{color:'#ffd'},1000)
        model.getFiles(updateFiles)
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
}

/** set prototype */
openmdao.FileTree.prototype = new openmdao.BasePane();
openmdao.FileTree.prototype.constructor = openmdao.FileTree;
