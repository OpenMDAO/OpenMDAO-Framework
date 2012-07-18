
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.CodeFrame = function(id,model) {
    openmdao.CodeFrame.prototype.init.call(this,id,'Code');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        filepath = "",
        editorID = id+'-textarea',
        editorArea = jQuery('<pre id="'+editorID+'">')
            .appendTo(self.elm).width('100%').height('100%');

    var editor = ace.edit(editorID);

    //editor.setTheme("ace/theme/chrome");
    editor.getSession().setMode("ace/mode/python");

    editor.commands.addCommand({
        name: "save",
        bindKey: {win: "Ctrl-S", mac: "Command-S"},
        exec: function() {saveFile();}
    });

    // make the parent element (tabbed pane) a drop target for file objects
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

    /** tell the model to save the current contents to current filepath */
    function saveFile() {
        model.setFile(filepath,editor.session.doc.getValue());
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
                //editor.setValue(contents);
                editor.session.doc.setValue(contents);
                editor.navigateFileStart();
            },
            // failure
            function(jqXHR, textStatus, errorThrown) {
                debug.info(textStatus);
                debug.info(errorThrown);
                alert("Error editing file: "+jqXHR.statusText);
            }
        );
    };

    /** get the pathname for the current file */
    this.getPathname = function() {
        return filepath;
    };
};

/** set prototype */
openmdao.CodeFrame.prototype = new openmdao.BaseFrame();
openmdao.CodeFrame.prototype.constructor = openmdao.CodeFrame;

