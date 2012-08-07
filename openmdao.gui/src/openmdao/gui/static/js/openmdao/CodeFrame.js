
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
            .css({ 'background-color':'gray', height:'25px' })
            .appendTo(self.elm),
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
        // file label (temporary until we have file tabs)
        file_label = jQuery('<div id='+id+'-label>')
            .css({ position:'absolute', top:20, right:0, padding:'5px 20px' })
            .appendTo(self.elm),
        // editor
        editorID = id+'-textarea',
        overwriteID = id+'-overwrite',
        cancelID = id+'-cancel',
        editorArea = jQuery('<pre id="'+editorID+'">')
            .css({overflow:'hidden', position:'absolute'})
            .height('100%') //self.elm.height() - uiBar.height())
            .appendTo(self.elm),
        editor = ace.edit(editorID);

    saveBtn.click(function() { saveFile(); });
    findBtn.click(function() { editor.commands.commands.find.exec(editor); });
    replBtn.click(function() { editor.commands.commands.replace.exec(editor); });
    rAllBtn.click(function() { editor.commands.commands.replaceall.exec(editor); });
    undoBtn.click(function() { editor.commands.commands.undo.exec(editor); });

    // set theme/mode
    //editor.setTheme("ace/theme/chrome");
    editor.getSession().setMode("ace/mode/python");

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

    function successful_save(data, textStatus, jqXHR) {
        if (typeof openmdao_test_mode !== 'undefined') {
            openmdao.Util.notify('Save complete: ' +textStatus);
        }
    }
    
    function failed_save(jqXHR, textStatus, errorThrown) {
        debug.info("file save failed: "+textStatus);
        debug.info(jqXHR);
        debug.info(errorThrown);
    }

    function handle409(jqXHR, textStatus, errorThrown) {
        var win = jQuery('<div>You have modified a class that may already have instances in the model. Do you want to continue?</div>');
        jQuery(win).dialog({
            'modal': true,
            'title': 'Overwrite Existing Classes',
            'buttons': [
                {
                  text: 'Overwrite',
                  id: overwriteID,
                  click: function() {
                           jQuery(this).dialog('close');
                           model.setFile(filepath,editor.getSession().getValue(), 1,
                                         successful_save, null, handle409);
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
    function saveFile() {
        model.setFile(filepath,editor.getSession().getValue(), 0,
                      successful_save, failed_save, handle409);
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** get contents of specified file from model, load into editor */
    this.editFile = function(pathname) {
        filepath = pathname;
        model.getFile(pathname,
            // success
            function(contents) {
                if (filepath.charAt(0) === "/") {
                    file_label.text(filepath.substr(1));
                }
                else {
                    file_label.text(filepath);
                }
                editor.session.doc.setValue(contents);
                self.resize();
                editor.resize();
                editor.navigateFileStart();
                var UndoManager = require("ace/undomanager").UndoManager;
                editor.getSession().setUndoManager(new UndoManager());
            },
            // failure
            function(jqXHR, textStatus, errorThrown) {
                debug.info(textStatus);
                debug.info(errorThrown);
                alert("Error editing file: "+jqXHR.statusText);
            }
        );
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

    /** display file error */
    function fileError(msg) {
        // topic = msg[0];
        text = msg[1];
        openmdao.Util.notify(text, 'File Error', 'file-error');
    }

    // ask model for an update whenever a file error occurs.
    model.addListener('file_errors', fileError);
};

/** set prototype */
openmdao.CodeFrame.prototype = new openmdao.BaseFrame();
openmdao.CodeFrame.prototype.constructor = openmdao.CodeFrame;

