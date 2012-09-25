
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.CodeFrame = function(id,model) {
    openmdao.CodeFrame.prototype.init.call(this,id,'Code');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        filepath = "",
        // toolbar
        uiBarID = id+'-uiBar',
        uiBar = jQuery('<div id="'+uiBarID+'">')
            .css({ 'background-color':'gray', height:'25px', 'font-size': '80%'})
            .appendTo(self.elm),
        newBtn = jQuery("<button id='"+uiBarID+"-new'>New</button>")
            .button({icons: {primary:'ui-icon-document'}})
            .css({height:'25px'})
            .appendTo(uiBar),
        saveBtn = jQuery("<button id='"+uiBarID+"-save'>Save</button>")
            .button({icons: {primary:'ui-icon-disk'}})
            .css({height:'25px'})
            .appendTo(uiBar),
        //saveAllBtn = jQuery("<button id='"+uiBarID+"-saveAll'>Save All</button>")
        //    .button({icons: {primary:'ui-icon-disk'}})
        //    .css({height:'25px'})
        //    .appendTo(uiBar),
        findBtn = jQuery("<button id='"+uiBarID+"-find'>Find</button>")
            .button({icons: {primary:'ui-icon-search'}})
            .css({height:'25px'})
            .appendTo(uiBar),
        replBtn = jQuery("<button id='"+uiBarID+"-replace'>Replace</button>")
            .button({icons: {primary:'ui-icon-search'}})
            .css({height:'25px'})
            .appendTo(uiBar),
        rAllBtn = jQuery("<button id='"+uiBarID+"-replaceAll'>Replace All</button>")
            .button({icons: {primary:'ui-icon-search'}})
            .css({height:'25px'})
            .appendTo(uiBar),
        undoBtn = jQuery("<button id='"+uiBarID+"-undo'>Undo</button>")
            .button({icons: {primary:'ui-icon-arrowrefresh-1-n'}})
            .css({height:'25px'})
            .appendTo(uiBar),
        file_tabs = jQuery('<div id='+id+'-tabs>')
            .css({'font-size':'95%'})
            .appendTo(self.elm),
        file_inner = jQuery('<ul class="tabrow"></ul>')
            .appendTo(file_tabs),
        // editor
        editorID = id+'-textarea',
        overwriteID = id+'-overwrite',
        cancelID = id+'-cancel',
        closeID = id+'-close',
        saveID = id+'-save',
        editorArea = jQuery('<pre id="'+editorID+'">')
            .css({overflow:'hidden', position:'absolute'})
            .height('100%') //self.elm.height() - uiBar.height())
            .appendTo(self.elm),
        editor = ace.edit(editorID),
        EditSession = require("ace/edit_session").EditSession,
        UndoManager = require("ace/undomanager").UndoManager,
        selectedFile = "",
        waitClose=[],
        sessions={}, //container for ace editor sessions
        defaultSession = new EditSession(" ");

    file_tabs.tabs({
        closable: true,
        select: function(event, ui) { selectTab(event,ui); },
        closableClick: function(event, ui) {
            return closeTab(event,ui);
        }
    });
    //file_tabs.find( ".ui-tabs-nav" ).sortable();

    newBtn.click(function() {
        openmdao.Util.promptForValue('Specify a name for the new file',
            function(name) {
                if (name) {
                    newTab('"""\n   '+name+'\n"""\n\n',"/"+name);
                }
            });
    });
    saveBtn.click(function() { saveFile(); });
    //saveAllBtn.click(function() { saveAllFiles(); });
    findBtn.click(function() { editor.commands.commands.find.exec(editor); });
    replBtn.click(function() { editor.commands.commands.replace.exec(editor); });
    rAllBtn.click(function() { editor.commands.commands.replaceall.exec(editor); });
    undoBtn.click(function() { editor.commands.commands.undo.exec(editor); });

    // set theme/mode
    //editor.setTheme("ace/theme/chrome");

    editor.setSession(defaultSession);
    editor.setReadOnly(true);

    function selectTab(event, ui) { // switch ace session based on the selected tab
        fname_nodot = nameSplit(ui.tab.innerText);
        selectedFile = fname_nodot;
        editor.setSession(sessions[fname_nodot][0]);
    }

    function closeTab(event,ui) {
        tab_id = ui.index;
        pathname = ui.tab.innerText;
        fname_nodot = nameSplit(pathname); //remove periods and slashes from filename

        //file changed since last save?
        code_last=sessions[fname_nodot][1];
        code_now=sessions[fname_nodot][0].getValue();

        if (code_last === code_now) {  //nothing changed. close tab
            if (file_tabs.tabs("length") === 1) {
                editor.setSession(defaultSession);editor.setReadOnly(true);
            }
            delete sessions[fname_nodot];
            file_tabs.tabs("remove",tab_id);
        }
        else { //file changed. require user choice
            var win = jQuery('<div>"'+pathname+'" has been changed. Save before closing?</div>');
            jQuery(win).dialog({
                'modal': true,
                'title': 'Save',
                'buttons': [
                    {
                        text: 'Save file',
                        id: saveID,
                        click: function() {
                                   waitClose=[fname_nodot,tab_id];
                                   saveFile(fname_nodot);
                                   jQuery(this).dialog('close');
                               }
                    },
                    {
                        text: 'Close without saving',
                        id: closeID,
                        click: function() {
                                   if (file_tabs.tabs("length") === 1) {
                                       editor.setSession(defaultSession);
                                       editor.setReadOnly(true);
                                   }
                                   delete sessions[fname_nodot];
                                   jQuery(this).dialog('close');
                                   file_tabs.tabs("remove",tab_id);
                               }
                    },
                    {
                        text: 'Cancel',
                        id: cancelID,
                        click: function() {//just close dialog
                                   jQuery(this).dialog('close');
                                   return false;
                               }
                    }
                  ]
            });
        }
        return false;
    }

    // keyboard shortcut
    editor.commands.addCommand({
        name: "save",
        bindKey: {win: "Ctrl-S", mac: "Command-S"},
        exec: function() { saveFile(); }
    });

    // make the editor a drop target for file objects
    editorArea.droppable ({
        accept: '.file',
        drop: function(ev,ui) {
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedPath = droppedObject.attr("path");
            if (droppedPath) {
                self.editFile(droppedPath);
            }
        }
    });

    
    function failedSave(jqXHR, textStatus, errorThrown) {
        debug.error("file save failed: "+textStatus, jqXHR, errorThrown);
        if (jqXHR.status != 409) {
            openmdao.Util.notify(jqXHR.responseXML, 'File Error', 'file-error');
        }
    }

    function handle409(jqXHR, textStatus, errorThrown) {
        var win = jQuery('<div>You have modified a class that may already have instances in the model. '+
                         'If you save the file, you must save and reload the project.</div>');
        jQuery(win).dialog({
            'modal': true,
            'title': 'Save and Reload Project',
            'buttons': [
                {
                  text: 'Save and Reload',
                  id: overwriteID,
                  click: function() {
                           jQuery(this).dialog('close');
                           model.setFile(filepath,editor.getSession().getValue(), 1,
                                         function(data, textStatus, jqXHR) {
                                            model.saveProject(function(data, textStatus, jqXHR) {
                                                model.reload()
                                            })
                                         },
                                         failedSave);
                         }
                },
                {
                   text: 'Cancel',
                   id: cancelID,
                   click: function() {
                             jQuery(this).dialog('close');
                          }
                }
              ]
            }
        );
    }

    /** tell the model to save the current contents to current filepath */
    function saveFile(fname_nodot) {
        if (! fname_nodot) {
            fname_nodot=selectedFile;
        }
        code_last = sessions[fname_nodot][1];
        current_code = sessions[fname_nodot][0].getValue();
        filepath = sessions[fname_nodot][2];
        if (code_last !== current_code) {
            model.setFile(filepath, current_code, 0,
                          function (data, textStatus, jqXHR) { // success callback
                            sessions[fname_nodot][1] = current_code; // store saved file for comparison
                            renameTab("#"+fname_nodot,filepath); // mark as not modified
                            sessions[fname_nodot][3] = false;  
                            if (typeof openmdao_test_mode !== 'undefined') {
                                openmdao.Util.notify('Save complete: ' + textStatus);
                            }
                            if (waitClose.length !== 0) {
                                fname_nodot = waitClose[0];
                                tab_id = waitClose[1];
                                if (file_tabs.tabs("length") === 1) {
                                    editor.setSession(defaultSession);
                                    editor.setReadOnly(true);
                                }
                                delete sessions[fname_nodot];
                                file_tabs.tabs("remove", tab_id);
                                waitClose = [];
                            }
                         }, failedSave, handle409);
        }
    }

    function saveAllFiles() {
        for (key in sessions) {
            saveFile(key);
        }
    }

    function findMode(filepath) {
        chunks = filepath.split('.');
        if (chunks.length === 2){
            if (chunks[1] === "py") {
                return "ace/mode/python";
            }
            else if (chunks[1] === "js") {
                return "ace/mode/javascript";
            }
            else {
                return "ace/mode/python";
            }
        }
        else {
            return "ace/mode/python";
        }
    }

    function newTab(contents,filepath,fname_nodot,mode) {
        editor.setReadOnly(false);
        if (!fname_nodot) {
            fname_nodot = nameSplit(filepath);
        }
        if (!mode) {
            mode = findMode(filepath);
        }
        var newfile = new EditSession(contents); // new code session for ace
        newfile.setUseSoftTabs();
        newfile.setUndoManager(new UndoManager());
        newfile.setMode(mode);
        newfile.on('change', function(evt) {
            if (sessions[fname_nodot][0].getValue() != contents) {
                renameTab("#"+fname_nodot, filepath+"*");
                sessions[fname_nodot][3] = true;
            }
            else {
                renameTab("#"+fname_nodot, filepath);
                sessions[fname_nodot][3] = false;
            }
        });
        editor.setSession(newfile);
        sessions[fname_nodot] = [newfile,contents,filepath,false]; // store session for efficent switching
        
        jQuery('<div id="'+fname_nodot+'"></div>').appendTo(file_inner); // new empty div
        file_tabs.tabs("add",'#'+fname_nodot,filepath);
        file_tabs.tabs( 'select', "#"+fname_nodot);
        selectedFile=fname_nodot;
        self.resize();
        editor.resize();
    }

    function renameTab(selector, value) {
        jQuery('a[href="' + selector + '"]').text(value);
    }

    function nameSplit(pathname) {
        return pathname.split('.').join('').split('/').join('').split('*').join('');
    }

    /** display file error */
    function fileError(msg) {
        // topic = msg[0];
        text = msg[1];
        openmdao.Util.notify(text, 'File Error', 'file-error');
    }

    // ask model for an update whenever a file error occurs.
    model.addListener('file_errors', fileError);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    // for GUI testing
    this.editor = editor;

    this.currentTablabel = function() {
        return jQuery('#'+id+'-tabs .ui-tabs-selected a').text();
    };

    /** get contents of specified file from model, load into editor */
    this.editFile = function(pathname) {
        filepath = pathname;
        mode = findMode(filepath);
        fname_nodot= nameSplit(filepath);
        if (fname_nodot in sessions) {// file already has open tab? just switch to it
            editor.setSession(sessions[fname_nodot][0]);
            file_tabs.tabs("select","#"+fname_nodot);
        }
        else { // file not being edited; make new tab
            model.getFile(pathname,
                // success
                function(contents) {
                    newTab(contents,filepath,fname_nodot,mode);
                    if (filepath.charAt(0) === "/") {
                        //file_label.text(filepath.substr(1));
                    }
                    else {
                        //file_label.text(filepath);
                    }
                    //editor.session.doc.setValue(contents);
                    self.resize();
                    editor.resize();
                    editor.navigateFileStart();
                    var UndoManager = require("ace/undomanager").UndoManager;
                    editor.getSession().setUndoManager(new UndoManager());
                },
                // failure
                function(jqXHR, textStatus, errorThrown) {
                    alert("Error editing file: "+jqXHR.statusText);
                }
            );
        }
    };

    // method to resize the Ace code pane
    this.resize = function() {
        editorArea.width(self.elm.width());
        editorArea.height(self.elm.height()-editorArea.offset().top);
    };

    /** get the pathname for the current file */
    this.getPathname = function() {
        return filepath;
    };

};

/** set prototype */
openmdao.CodeFrame.prototype = new openmdao.BaseFrame();
openmdao.CodeFrame.prototype.constructor = openmdao.CodeFrame;

