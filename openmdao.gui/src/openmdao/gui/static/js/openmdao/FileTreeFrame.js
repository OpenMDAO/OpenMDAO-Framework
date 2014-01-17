
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.FileTreeFrame = function(id, project) {
    var _menu = [
        {   "text": "File",
            "items": [
                { "text": "New File",     "onclick": "openmdao.FileTreeFrame.prototype.newFile();" },
                { "text": "New Folder",   "onclick": "openmdao.FileTreeFrame.prototype.newFolder();" },
                { "text": "Add Files",    "onclick": "openmdao.FileTreeFrame.prototype.addFile();" },
                { "text": "Delete Files", "onclick": "openmdao.FileTreeFrame.prototype.deleteFiles();" }
            ]
        }
    ];
    openmdao.FileTreeFrame.prototype.init.call(this, id, 'Files', _menu);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var _self = this,
        _tree = jQuery('<div>')
            .appendTo(_self.elm),
        _filter_beg = '_.',
        _filter_ext = [ 'pyc', 'pyd' ],
        _filter_active = true,
        _contextMenu = jQuery("<ul id="+id+"-context-menu class='context-menu'>")
            .appendTo(_self.elm);
         _updates = [];  // queue for updates

    // Enable dropping of files onto file tree frame to add them to project
    _self.elm.bind({
        dragenter: function() {
            _self.elm.addClass('hover ui-state-highlight');
            return false;
        },
        dragover: function() {
            return false;
        },
        dragleave: function() {
            _self.elm.removeClass('hover ui-state-highlight');
            return false;
        },
        dragend: function() {
            _self.elm.removeClass('hover ui-state-highlight');
            return false;
        },
        drop: function(e) {
            _self.elm.removeClass('hover ui-state-highlight');
            e = e || window.event;
            e.preventDefault();
            e = e.originalEvent || e;

            var i = 0,
                files = (e.files || e.dataTransfer.files);
            for (i = 0; i < files.length; i++) {
                (function(i) {
                    var reader = new FileReader();
                    reader.onload = function(e) {
                       project.setFile(files[i].name, e.target.result);
                    };
                    reader.readAsText(files[i]);
                })(i);
            }
            return false;
        }
    });

    // add background pane context menu items
    _contextMenu.append(jQuery('<li title="Create new file">New File</li>').click(function(e) {
        openmdao.FileTreeFrame.prototype.newFile();
    }));
    _contextMenu.append(jQuery('<li title="Create new folder">New Folder</li>').click(function(e) {
        openmdao.FileTreeFrame.prototype.newFolder();
    }));
    _contextMenu.append(jQuery('<li title="Add existing files to project">Add Files</li>').click(function(e) {
        openmdao.FileTreeFrame.prototype.addFile();
    }));
    _contextMenu.append(jQuery('<li title="Toggle visibility of hidden files">Toggle Hidden Files</li>').click(function(e) {
        toggleFilter();
    }));
    ContextMenu.set(_contextMenu.attr('id'), _self.elm.attr('id'));

    /** recursively build an HTML representation of a JSON file structure */
    function getFileHTML(path, val) {
        path = path.replace(/\\/g,'/');

        // get the file name and extension
        var name = path.split('/'),
            name = name[name.length-1],
            ext = name.split('.'),
            ext = ext[ext.length-1],
            url = "application/octet-stream:"+name+":file"+path+"'",
            html = '';

        if (!_filter_active || ((_filter_beg.indexOf(name[0])<0 && _filter_ext.indexOf(ext)<0))) {
            if (typeof val === 'object') {    // a folder
                html += "<li><a class='folder' path='"+path+"'>"+name+"</a><ul>";
                jQuery.each(val,function(path,val) {
                    html += getFileHTML(path, val);
                });
                html += "</ul></li>";
            }
            else {
                html += "<li><a class='file' path='"+path+"' draggable='true'"
                     +  " data-downloadurl='"+url+"'>"+name+"</a></li>";
            }
        }
        return html;
    }

    /** save a copy of the file to the local file system (download) */
    function saveCopy(pathname) {
        jQuery.fileDownload('file'+pathname+'?download=True');
    }

    /** delete file after confirmation **/
    function deleteFile(filepath) {
        openmdao.FileTreeFrame.prototype.confirmDeleteFile(filepath)
            .done(function() {
                project.removeFile(filepath);
            });
    }

    /** delete folder and contents after confirmation **/
    function deleteFolder(node) {
        var filenodes = node.find('.folder, .file'),
            filepaths = jQuery.map(filenodes,
                function(n, i) { return jQuery(n).attr('path'); });

        filepaths.sort();

        openmdao.FileTreeFrame.prototype.confirmDeleteFiles(filepaths)
            .done(function() {
                project.removeFiles(filepaths.reverse());
            });
    }

    /** delete selected files after confirmation **/
    function deleteSelectedFiles() {
        var filepaths = [];
        _self.elm.find('a.file.jstree-clicked').each(function() {
            filepaths.push(this.getAttribute("path"));
        });
        _self.elm.find('a.folder.jstree-clicked').each(function() {
            jQuery(this).parent().find('a.file, a.folder').each(function() {
                filepaths.push(this.getAttribute("path"));
            });
        });
        if (filepaths.length > 0) {
            filepaths = filepaths.sort();
            openmdao.FileTreeFrame.prototype.confirmDeleteFiles(filepaths)
                .done(function() {
                    project.removeFiles(filepaths.reverse());
                });
        }
    }

    /** toggle the hidden files filter */
    function toggleFilter() {
        _filter_active = !_filter_active;
        _self.update();
    }

    /** get a context menu for the specified node */
    function nodeMenu(node) {
        // first let's see what was clicked on
        var isFolder = false,
            isEmptyFolder = false;
        if (node.is('.jstree-leaf')) {
            filenode = node.find('.file');
            if (filenode.length === 0) {
                filenode = node.find('.folder');
                if (filenode.length > 0) {
                    isFolder = true;
                    isEmptyFolder = true;
                }
                else {
                    debug.error("FileTreeFrame: leaf node that's not a file or empty folder?",node);
                }
            }
        }
        else {
            filenode = node.find('.folder');
            if (filenode.length > 0) {
                isFolder = true;
                isEmptyFolder = false;
            }
            else {
                debug.error("FileTreeFrame: non-leaf node that's not a folder?",node);
            }
        }

        var path = filenode.attr('path');

        // now create the menu
        var menu = {};

        if (isFolder) {
            // if they clicked on a folder then create new files inside that folder
            menu.createFile = {
                "label"  : 'New File',
                "action" : function(node) {
                    if (isFolder) {
                        openmdao.FileTreeFrame.prototype.newFile(path);
                    }
                    else {
                        openmdao.FileTreeFrame.prototype.newFile();
                    }
                }
            };
            menu.createFolder = {
                "label"  : 'New Folder',
                "action" : function(node) {
                    if (isFolder) {
                        openmdao.FileTreeFrame.prototype.newFolder(path);
                    }
                    else {
                        openmdao.FileTreeFrame.prototype.newFolder();
                    }
                }
            };

            menu.addFile = {
                "label"  : 'Add Files',
                "action" : function(node) {
                    if (isFolder) {
                        openmdao.FileTreeFrame.prototype.addFile(path);
                    }
                    else {
                        openmdao.FileTreeFrame.prototype.addFile();
                    }
                }
            };
        }

        // if it's not a folder
        if (!isFolder) {
            // let them edit it (TODO: filter out non-text files?)
            menu.editFile = {
                "label"  : 'Edit File',
                "action" : function(node) { openmdao.project.editFile(path); }
            };

            // if it's a py file, let them execute it
            if (/.py$/.test(path)) {
                menu.execFile = {
                    "label"  : 'Execute File',
                    "action" : function(node) { project.execFile(path); }
                };
            }

            // if it's an image file, let them load it into image viewer
            if (openmdao.Util.hasImageExtension(path)) {
                menu.viewImage = {
                    "label"  : 'View Image',
                    "action" : function(node) { openmdao.project.viewImages(path); }
                };
            }

            // if it's a geometry file, let them load it into geometry viewer
            if (openmdao.Util.hasGeometryExtension(path)) {
                menu.viewGeometry = {
                    "label"  : 'View Geometry',
                    "action" : function(node) { openmdao.project.viewGeometry(path); }
                };
            }

            // view file in another window
            menu.viewFile = {
               "label"  : 'Open in Browser',
               "action" : function(node) { openmdao.project.viewFile(path); }
            };

            // save a copy (i.e. download)
            menu.saveCopy = {
                "label"  : 'Save a Copy',
                "action" : function(node) {
                                var name = path.split('/');
                                name = name[name.length-1];
                                saveCopy(path);
                           }
            };

            // rename file
            menu.renameFile = {
                "label"  : 'Rename',
                "action" : function(node) {
                                var old = path.split('/');
                                old = old[old.length-1];
                                openmdao.Util.promptForValue('New name for '+old, function(name) {
                                    project.renameFile(path, name)
                                        .fail(function(jqXHR, textStatus, errorThrown) {
                                            alert('Error renaming file: ' + textStatus);
                                            debug.error('Error renaming file', path, name,
                                                        jqXHR, textStatus, errorThrown);
                                        });
                                });
                           }
            };
        }

        // delete files and folders
        if (!isFolder) {
            menu.deleteFile = {
                "label"  : 'Delete File',
                "action" : function(node) { deleteFile(path); }
            };
        }
        else if (isEmptyFolder) {
            menu.deleteEmptyFolder = {
                "label"  : 'Delete Empty Folder',
                "action" : function(node) { deleteFile(path); }
            };
        }
        else {
            menu.deleteFolder = {
                "label"  : 'Delete Folder and Contents',
                "action" : function(node) { deleteFolder(node); }
            };
        }

        // if they have selected multiple files, offer multiple file delete
        if (_self.elm.find('a.jstree-clicked').length > 1) {
            menu.deleteSelectedFiles = {
                "label"  : 'Delete Selected Files',
                "action" : function(node) { deleteSelectedFiles(); }
            };
        }

        menu.toggle = {
            "label"  : 'Toggle Hidden Files',
            "action" :  function(node) { toggleFilter(); }
        };

        return menu;
    }

    /** update the tree from JSON file structure */
    function updateFiles(files, deferred) {
        // highlight the tree per user preference
        if (openmdao.preferences.FileTreeFrame.highlightOnUpdate) {
            _tree.html("<div>Updating...</div>")
                .effect('highlight', {color:'#ffd'}, 1000);
        }

        // Grab paths of currently open nodes.
        var openNodes = [];
        _self.elm.find("li.jstree-open").each(function () {
            openNodes.push(jQuery(this).find('a:first').attr('path'));
        });

        // generate HTML for the file tree
        var html = "<ul>";
        jQuery.each(files, function(path, val) {
            html += getFileHTML(path, val);
        });
        html += "</ul>";

        // replace old html
        _tree.html(html);

        // convert to a jstree
        _tree.jstree({
            "plugins" :     [ "html_data", "sort", "themes", "types", "contextmenu", "ui" ],
            "themes" :      { "theme":  "classic" },
            "contextmenu" : { "items":  nodeMenu }
        })
        .bind("dblclick.jstree", function(e) {
            var node = jQuery(e.target),
                path = node.attr("path");
            if (node.hasClass('file')) {
                if (openmdao.Util.hasImageExtension(path)) {
                    openmdao.project.viewImages(path);
                }
                else if (openmdao.Util.hasGeometryExtension(path)) {
                    openmdao.project.viewGeometry(path);
                }
                else {
                    openmdao.project.editFile(path);
                }
            }
            else if (node.hasClass('folder')) {
                // what do, what do
            }
            else {
                debug.warn("node in file tree does not seem to be a file or a folder:", node);
            }
        })
        .bind("loaded.jstree", function(e) {
            _tree.find('.folder').each(function () {
                if (openNodes.indexOf(this.getAttribute('path')) >= 0) {
                    this.parentNode.removeClass('jstree-closed');
                    this.parentNode.addClass('jstree-open');
                }
            });

            _tree.find('.file').draggable({ helper: 'clone', appendTo: 'body' });

            _tree.find('li').each(function() {
                // children[1] is the A tag inside the LI
                // children[1].children[0] is the INS tag inside the A tag and
                // that is the icon, which is set via the appropriate class
                if (this.children[1].getAttribute("class") === "folder") {
                    this.children[1].children[0].addClass("jstree-folder");
                }
                else {
                    if (this.children[1].text.match("\.py$")) {
                        this.children[1].children[0].addClass("jstree-python-file");
                    }
                    else {
                        this.children[1].children[0].addClass("jstree-file");
                    }
                }
            });

            deferred.resolve();
        });
    }

    function queueUpdate(files) {
        var old_update = _updates.shift(),
            new_update = jQuery.Deferred();

        _updates.push(new_update);

        if (old_update) {
            // make sure old update is done first
            jQuery.when(old_update).done(function() {
                updateFiles(files, new_update);
            });
        }
        else {
            updateFiles(files, new_update);
        }
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== 'files') {
            debug.warn('Invalid files data:', message);
        }
        else {
            queueUpdate(message[1]);
        }
    }

    // listen for 'files' messages and update file data accordingly
    project.addListener('files', handleMessage);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        project.removeListener('files', handleMessage);
    };

    /** update the display, with data from the project */
    this.update = function() {
        project.getFiles()
            .done(queueUpdate)
            .fail(function(jqXHR, textStatus, errorThrown) {
                debug.error('Error getting files',
                            jqXHR, textStatus, errorThrown);
            });
    };

    // load initial file data
    project.project_ready.always(function() {
       _self.update();
    });

};

/** set prototype */
openmdao.FileTreeFrame.prototype = new openmdao.BaseFrame();
openmdao.FileTreeFrame.prototype.constructor = openmdao.FileTreeFrame;

/** create a new file in the current project and edit it */
openmdao.FileTreeFrame.prototype.newFile = function(path) {
    openmdao.Util.promptForValue('Specify a name for the new file',
        function(name) {
            openmdao.project.newFile(name, path)
                .done(function() {
                    var pathname = '/'+name;
                    if (path) {
                        pathname = path + name;
                    }
                    // if the editor is already open, load the new file
                    if (openmdao.project.codeEditor) {
                        openmdao.project.editFile(pathname);
                    }
                    if (typeof openmdao_test_mode !== 'undefined') {
                        openmdao.Util.notify('New file created');
                    }
                })
                .fail(function(jqXHR, textStatus, errorThrown) {
                    debug.error('Error creating file', name, path,
                                jqXHR, textStatus, errorThrown);
                });
        });
};

/** create a new folder in the current project */
openmdao.FileTreeFrame.prototype.newFolder = function(path) {
    openmdao.Util.promptForValue('Specify a name for the new folder',
        function(name) { openmdao.project.newFolder(name, path); } );
};

/** choose & add one or more files, optionally specifying a dest folder */
openmdao.FileTreeFrame.prototype.addFile = function(path) {
    filechooser = jQuery('<input id="filechooser" type="file" multiple="true"' +
                         ' style="position:absolute;top:-500;left:-500" />')
        .appendTo('body');

    function uploadFiles(files, path) {
        openmdao.project.addFiles(files, path)
            .fail(function(jqXHR, textStatus, errorThrown) {
                alert('error uploading files ('+textStatus+', '+errorThrown+')');
                debug.error('error uploading files', files, path,
                            jqXHR, textStatus, errorThrown);
            });
        filechooser.remove();  // self destruct, one use only
    }

    filechooser.bind({
        'change': function(e) {
            uploadFiles(this.files, path);
            e.preventDefault();
            e.stopPropagation();
        }
    });

    filechooser.show();
    filechooser.focus();
    if (typeof openmdao_test_mode !== 'undefined') {
        // if testing, make the file chooser visible for selenium
        filechooser.css({'left':'100px', 'top':'100px'});
    }
    else {
        filechooser.click();
        filechooser.hide();
    }
};

/** confirm whether to delete file with specified path */
openmdao.FileTreeFrame.prototype.confirmDeleteFile = function(filepath) {
    var confirmation = 'The following file will be deleted:<br><br>';
    confirmation = confirmation + '    ' + filepath + '<br>';
    confirmation = confirmation + '<br>Proceed';

    confirmation = openmdao.Util.confirm(confirmation, "Confirm Delete File");
    return confirmation;
};

/** confirm whether to delete files with specified paths */
openmdao.FileTreeFrame.prototype.confirmDeleteFiles = function(filepaths) {
    var confirmation = 'The following files will be deleted:<br><br>';
    for (var i=0 ; i<filepaths.length; i++) {
        confirmation = confirmation + '    ' + filepaths[i] + '<br>';
    }
    confirmation = confirmation + '<br>Proceed';

    confirmation = openmdao.Util.confirm(confirmation, "Confirm Delete Files");
    return confirmation;
};

/** delete selected files **/
openmdao.FileTreeFrame.prototype.deleteFiles = function() {
    var filepaths = [];
    // FIXME: hard coded element ID
    jQuery('#ftree_pane a.file.jstree-clicked').each(function() {
        filepaths.push(this.getAttribute("path"));
    });
    jQuery('#ftree_pane a.folder.jstree-clicked').each(function() {
        jQuery(this).parent().find('a.file, a.folder').each(function() {
            filepaths.push(this.getAttribute("path"));
        });
    });
    if (filepaths.length > 0) {
        filepaths = filepaths.sort();
        openmdao.FileTreeFrame.prototype.confirmDeleteFiles(filepaths)
            .done(function(filepath) {
                openmdao.project.removeFiles(filepaths.reverse());
            });
    }
};
