
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
        _filter_active = true;

    // Enable dropping of files onto file tree frame to add them to project
    _self.elm.bind({
        dragenter: function () {
            _self.elm.addClass('hover ui-state-highlight');
            return false;
        },
        dragover: function () {
            return false;
        },
        dragleave: function () {
            _self.elm.removeClass('hover ui-state-highlight');
            return false;
        },
        dragend: function () {
            _self.elm.removeClass('hover ui-state-highlight');
            return false;
        },
        drop: function (e) {
            _self.elm.removeClass('hover ui-state-highlight');
            e = e || window.event;
            e.preventDefault();
            e = e.originalEvent || e;

            var i = 0,
                files = (e.files || e.dataTransfer.files);
            for (i = 0; i < files.length; i++) {
                (function (i) {
                    var reader = new FileReader();
                    reader.onload = function (e) {
                       project.setFile(files[i].name, e.target.result);
                    };
                    reader.readAsText(files[i]);
                })(i);
            }
            return false;
        }
    });

    /** recursively build an HTML representation of a JSON file structure */
    function getFileHTML(path, val) {
        path = path.replace(/\\/g,'/');

        // get the file name and extension
        var name = path.split('/'),
            name = name[name.length-1],
            ext = name.split('.'),
            ext = ext[ext.length-1],
            url = "application/octet-stream:"+name+":file"+path+"'";

        var html = '';
        if (!_filter_active || ((_filter_beg.indexOf(name[0])<0 && _filter_ext.indexOf(ext)<0))) {
            html = "<li><a";
            if (typeof val === 'object') {    // a folder
                html += " class='folder' path='"+path+"'>"+name+"</a>";
                html += "<ul>";
                jQuery.each(val,function(path,val) {
                    html += getFileHTML(path,val);
                });
                html += "</ul>";
            }
            else {
                html += " class='file' path='"+path+"' draggable='true'"
                     +  " data-downloadurl='"+url+"'>"+name+"</a>";
            }
            html += "</li>";
        }
        else {
            html = '';
        }
        return html;
    }

    /** display the file in a new window (probably not in a useful format) */
    function viewFile(pathname) {
        openmdao.Util.popupWindow('file'+pathname.replace(/\\/g,'/'), pathname);
    }

    /** save a copy of the file to the local file system (download) */
    function saveCopy(pathname) {
        jQuery.fileDownload('file'+pathname+'?download=True');
    }

    /** delete selected files **/
    function deleteSelectedFiles() {
        var filepaths = [];
        _self.elm.find('a.jstree-clicked').each(function() {
            filepaths.push(this.getAttribute("path"));
        });
        project.removeFiles(filepaths)
            .fail(function(jqXHR, textStatus, errorThrown) {
                alert('Error removing files: ' + textStatus);
                debug.error('Error removing files', path, name,
                            jqXHR, textStatus, errorThrown);
            });
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
                    "action" : function(node) { openmdao.project.viewGeometry(path.replace(/\\/g,'/')); }
                };
            }

            // view file in another window
            menu.viewFile = {
               "label"  : 'Open in Browser',
               "action" : function(node) { viewFile(path); }
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

        // delete only files and empty folders
        if (!isFolder) {
            menu.deleteFile = {
                "label"  : 'Delete File',
                "action" : function(node) { project.removeFile(path)
                                                .fail(function(jqXHR, textStatus, errorThrown) {
                                                    alert('Error removing file: ' + textStatus);
                                                    debug.error('Error removing file', path, name,
                                                                jqXHR, textStatus, errorThrown);
                                                });
                                           }
            };
        }
        else if (isEmptyFolder) {
            menu.deleteFolder = {
                "label"  : 'Delete Empty Folder',
                "action" : function(node) { project.removeFile(path)
                                                .fail(function(jqXHR, textStatus, errorThrown) {
                                                    alert('Error removing folder: ' + textStatus);
                                                    debug.error('Error renaming folder', path, name,
                                                                jqXHR, textStatus, errorThrown);
                                                });
                                           }
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
    function updateFiles(files) {
        // highlight the tree per user preference
        if (openmdao.preferences.FileTreeFrame.highlightOnUpdate) {
            _tree.html("<div>Updating...</div>")
                .effect('highlight', {color:'#ffd'}, 1000);
        }

        // generate HTML for the file tree
        var html = "<ul>";
        jQuery.each(files, function(path,val) {
            html += getFileHTML(path,val);
        });
        html += "</ul>";

        // replace old html
        _tree.html(html);

        // convert to a jstree
        _tree.jstree({
            "plugins" :     [ "html_data", "sort", "themes", "types", "cookies", "contextmenu", "ui" ],
            "themes" :      { "theme":  "classic" },
            "cookies" :     { "prefix": "filetree", opts : { path : '/' } },
            "contextmenu" : { "items":  nodeMenu }
        })
        .bind("loaded.jstree", function (e) {
            _self.elm.find('.file').draggable({ helper: 'clone', appendTo: 'body' });

            // id is  "file_pane"
            _self.elm.find('.jstree li').each(function () {
                // children[1] is the a tag inside the li
                // children[1].children[0] is the ins tag inside the a tag and that is the
                //    icon that needs to be set, which we do by adding a class and
                //    adding some CSS into mdao-styles.css
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

        })
        .bind("dblclick.jstree", function (e) {
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
        });

        // enable dragging out to desktop (only supported under chrome)
        // ref: http://www.thecssninja.com/javascript/gmail-dragout
        // FIXME: doesn't work, handlers not getting added??
        _tree.find('.file').bind({
            'dragstart': function(e) {
                var url = jQuery(this).attr('data-download-url');
                e.dataTransfer.setData('DownloadURL',url);
                return false;
            }
        });
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== 'files') {
            debug.warn('Invalid files data:', message);
        }
        else {
            updateFiles(message[1]);
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
            .done(updateFiles)
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
        var formData = new FormData(),
            xhr = new XMLHttpRequest(),
            filename;
        for (filename in files) {
            formData.append('file', files[filename]);
        }
        // now post a new XHR request
        xhr.open('POST', '/workspace/tools/upload');
        xhr.onload = function () {
            if (xhr.status !== 200) {
                alert('error uploading files ('+xhr.status+', '+xhr.statusText+')');
                debug.error('error uploading files', xhr, files, path);
            }
        };
        if (path) {
            formData.append('path', path);
        }
        xhr.send(formData);

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

/** delete selected files **/
openmdao.FileTreeFrame.prototype.deleteFiles = function() {
    var filepaths = [];
    // FIXME: hard coded element ID
    jQuery('#ftree_pane a.jstree-clicked').each(function() {
        filepaths.push(this.getAttribute("path"));
    });
    openmdao.project.removeFiles(filepaths)
        .fail(function(jqXHR, textStatus, errorThrown) {
            alert('Error removing files: ' + textStatus);
            debug.error('Error removing files', path, name,
                        jqXHR, textStatus, errorThrown);
        });
};
