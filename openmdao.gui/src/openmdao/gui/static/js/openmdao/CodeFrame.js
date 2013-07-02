
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.CodeFrame = function(id, project) {
    openmdao.CodeFrame.prototype.init.call(this, id, 'Code');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
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
        fileTabs = jQuery('<div id='+id+'-tabs>')
            .css({'font-size':'95%'})
            .appendTo(self.elm),
        fileInner = jQuery('<ul class="tabrow"></ul>')
            .appendTo(fileTabs),
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
        selectedTabName = "",
        sessions = {}, //container for ace editor sessions
        defaultSession = new EditSession(" ");

    editor.setSession(defaultSession);
    editor.setReadOnly(true);

    // set up buttons
    newBtn.click(function() {
        openmdao.Util.promptForValue('Specify a name for the new file',
            function(name) {
                if (name) {
                    newTab('"""\n   '+name+'\n"""\n\n',"/"+name);
                }
            });
    });
    saveBtn.click(function() { saveFile(); });
    findBtn.click(function() { editor.commands.commands.find.exec(editor); });
    replBtn.click(function() { editor.commands.commands.replace.exec(editor); });
    rAllBtn.click(function() { editor.commands.commands.replaceall.exec(editor); });
    undoBtn.click(function() { editor.commands.commands.undo.exec(editor); });

    // set up tabs
    fileTabs.tabs({
        tabTemplate: "<li><a href='#{href}'>#{label}</a> <span class='ui-icon ui-icon-close'></span></li>",
        beforeActivate: function(ev, ui) { selectTab(ev,ui); }
    });

    jQuery('#'+id+'-tabs span.ui-icon-close').live( "click", function(ev, ui) {
        return closeTab(jQuery(this).closest('li'));
    });

    /** switch ace session based on the selected tab */
    function selectTab(ev, ui) {
        var tabName = nameSplit(ui.newTab[0].innerText);
        selectedTabName = tabName;
        editor.setSession(sessions[tabName].editSession);
    }

    /** close tab, prompt to save file if contents have changed */
    function closeTab(tab) {
        var tabIndex = tab.index(),
            tabName  = tab.attr('aria-controls'),
            filepath = tab.text(),
            session  = sessions[tabName];

        function closeIt() {
            if (fileTabs.tabs("length") === 1) {
                editor.setSession(defaultSession);
                editor.setReadOnly(true);
            }
            delete sessions[tabName];
            tab.remove();
            fileTabs.tabs("refresh");
        }

        if (session.editSession.getValue() === session.prevContent) {
            //nothing changed, close tab
            closeIt();
        }
        else {
            //file changed. require user choice
            var win = jQuery('<div>"'+filepath+'" has been changed. Save before closing?</div>');
            jQuery(win).dialog({
                'modal': true,
                'title': 'Save',
                'buttons': [
                    {
                        text: 'Save file',
                        id: saveID,
                        click: function() {
                                    saveFile(tabName, closeIt);
                                    jQuery(this).dialog('close');
                               }
                    },
                    {
                        text: 'Close without saving',
                        id: closeID,
                        click: function() {
                                   closeIt();
                                   jQuery(this).dialog('close');
                               }
                    },
                    {
                        text: 'Cancel',
                        id: cancelID,
                        click: function() {
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
        greedy: true,
        drop: function(ev,ui) {
            var droppedObject = jQuery(ui.draggable).clone(),
                droppedPath = droppedObject.attr("path");
            if (droppedPath) {
                self.editFile(droppedPath);
            }
        }
    });

    /** Save the contents of the specified file tab. If no tabname is specified,
        save the contents of the currently selected tab. If a callback function
        is specified, call that function after successfully saving the file.
    **/
    function saveFile(tabName, callback) {
        if (! tabName) {
            tabName = selectedTabName;
        }

        var session     = sessions[tabName],
            currentCode = session.editSession.getValue(),
            lastCode    = session.prevContent,
            filepath    = session.filepath;

        /** display error message if file save failed */
        function failedSave(jqXHR, textStatus, errorThrown) {
            // 409 gets special handling.
            if (jqXHR.status === 409) {
                handle409(jqXHR, textStatus, errorThrown);
            }
            // 400 is (normally) related to msg reported via publisher.
            else if (jqXHR.status !== 400) {
                var msg = jqXHR.responseXML || textStatus;
                openmdao.Util.notify(msg, 'Save Failed');
                debug.error("file save failed: "+textStatus, jqXHR, errorThrown);
            }
        }

        /** 409 response when saving file indicates project reload will be necessary */
        function handle409(jqXHR, textStatus, errorThrown) {
            var win = jQuery('<div>Changing this file can alter the project configuration. '+
                             'If you save this file, you must reload the project.</div>');
            jQuery(win).dialog({
                'modal': true,
                'title': 'Save File and Reload Project',
                'buttons': [
                    {
                      text: 'Save File and Reload Project',
                      id:    overwriteID,
                      click: function() {
                                jQuery(this).dialog('close');
                                    project.setFile(filepath, currentCode, 1)
                                        .done(function() { project.reload(); })
                                        .fail(failedSave);
                             }
                    },
                    {
                       text: 'Cancel',
                       id:    cancelID,
                       click: function() { jQuery(this).dialog('close'); }
                    }
                  ]
                }
            );
        }

        if (currentCode !== lastCode) {
            project.setFile(filepath, currentCode, 0)
                .done(function(data, textStatus, jqXHR) {
                    // store saved file for comparison
                    session.prevContent = currentCode;
                    // mark as not modified
                    renameTab("#"+tabName, filepath);
                    session.modifed = false;
                    if (typeof openmdao_test_mode !== 'undefined') {
                        openmdao.Util.notify('Save complete: ' + textStatus);
                    }
                    if (typeof callback === 'function') {
                        callback();
                    }
                })
                .fail(failedSave);
        }
    }

    /** determine editor mode for file based on file extension */
    function findMode(filename) {
        chunks = filename.split('.');
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

    /** create a new tab for the given file */
    function newTab(contents, filepath, tabName, mode) {
        editor.setReadOnly(false);
        if (!tabName) {
            tabName = nameSplit(filepath);
        }
        if (!mode) {
            mode = findMode(filepath);
        }
        var newSession = new EditSession(contents); // new code session for ace
        newSession.setUseSoftTabs(true);
        newSession.setTabSize(4);
        newSession.setUndoManager(new UndoManager());
        newSession.setMode(mode);
        newSession.on('change', function(evt) {
            if (sessions[tabName].editSession.getValue() !== contents) {
                renameTab("#"+tabName, filepath+"*");
                sessions[tabName].modified = true;
            }
            else {
                renameTab("#"+tabName, filepath);
                sessions[tabName].modified = false;
            }
        });
        editor.setSession(newSession);

        // store session for efficent switching
        sessions[tabName] = {
            'editSession': newSession,
            'prevContent': contents,
            'filepath':    filepath,
            'modified':    false
        };

        jQuery('<div id="'+tabName+'"></div>').appendTo(fileInner); // new empty div
        fileTabs.tabs("add", '#'+tabName, filepath);
        fileTabs.tabs('select', "#"+tabName);
        selectedTabName = tabName;
        if (Object.keys(sessions) > 1) {
            // On OS X an initial self.resize() would result in a blank display.
            // Keeping original resize code for possibly handling lots 'o tabs.
            self.resize();
            editor.resize();
        }
    }

    /** rename tab */
    function renameTab(selector, value) {
        jQuery('a[href="' + selector + '"]').text(value);
    }

    /** remove periods and forward slashes from file path */
    function nameSplit(filepath) {
        return filepath.split('.').join('')
                       .split('/').join('')
                       .split('*').join('')
                       .trim();
    }

    /** display file error */
    function fileError(msg) {
        // topic = msg[0];
        text = msg[1];
        openmdao.Util.notify(text, 'File Error', 'file-error');
    }

    // ask project for an update whenever a file error occurs.
    project.addListener('file_errors', fileError);

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    // for GUI testing
    this.editor = editor;

    /** get the tab label for the currently selected tab */
    this.currentTablabel = function() {
        return fileTabs.find('.ui-tabs-active a').text();
    };

    /** get contents of specified file from project, load into editor */
    this.editFile = function(filepath) {
        if (! filepath) {
            alert("Error: file name not specified");
            return;
        }
        var tabName = nameSplit(filepath);
        if (sessions[tabName]) {
            // file already has open tab, just switch to it
            editor.setSession(sessions[tabName].editSession);
            fileTabs.tabs("select","#"+tabName);
        }
        else {
            // file not being edited, make new tab
            project.getFile(filepath)
                .done(function(contents) {
                    newTab(contents, filepath, tabName);
                    editor.navigateFileStart();
                    editor.getSession().setUndoManager(new UndoManager());
                })
                .fail(function(jqXHR, textStatus, errorThrown) {
                    alert("Error editing file: "+jqXHR.statusText);
                    debug.error('Error editing file', filepath,
                            jqXHR, textStatus, errorThrown);
                });
        }
    };

    /** method to resize the Ace code pane */
    this.resize = function() {
        editorArea.width(self.elm.width());
        editorArea.height(self.elm.height()-editorArea.offset().top);
    };

};

/** set prototype */
openmdao.CodeFrame.prototype = new openmdao.BaseFrame();
openmdao.CodeFrame.prototype.constructor = openmdao.CodeFrame;
