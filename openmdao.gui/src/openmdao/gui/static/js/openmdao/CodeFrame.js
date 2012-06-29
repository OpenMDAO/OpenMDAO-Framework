
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
        editorArea = jQuery('<pre id="'+editorID+'">').appendTo("#"+id).width(screen.width).height(screen.height);
        
    var editor = ace.edit(editorID);
        
    //editor.setTheme("ace/theme/chrome");
    editor.getSession().setMode("ace/mode/python");
        
    editor.commands.addCommand({
        name: "save",
        bindKey: {win: "Ctrl-S", mac: "Command-S"},
        exec: function() {saveFile();}
    });    
            
    // make the parent element (tabbed pane) a drop target for file objects
    editorArea.parent().droppable ({
        accept: '.file .obj',
        drop: function(ev,ui) {
            var droppedObject = jQuery(ui.draggable).clone();
            debug.info('CodeFrame drop',droppedObj);
            if (droppedObject.hasClass('file')) {
                editFile(droppedObject.attr("path"));
            }
        }
    });

    function handle409(jqXHR, textStatus, errorThrown) {
        var win = jQuery('<div>You have modified a class that may already have instances in the model. Do you want to continue?</div>')
        jQuery(win).dialog({
            'modal': true,
            'title': 'Overwrite Existing Classes',
            'buttons': {
                'Overwrite': function() {
                    jQuery(this).dialog('close');
                    self.saveFile(true);  // force a save
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    }

    /** tell the model to save the current contents to current filepath */
    function saveFile(force) {
        debug.info("saveFile:  force = "+force)
        console.log(editor.getValue());
        model.setFile(filepath,editor.session.doc.getValue(),force,null,null,handle409);
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

