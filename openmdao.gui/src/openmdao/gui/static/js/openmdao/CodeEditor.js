
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.CodeEditor = function(id,model) {
    openmdao.CodeEditor.prototype.init.call(this,id,'Code');
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    // initialize private variables
    var self = this,
        filepath = "",
        editorID = id+'-textarea',
        editorArea = jQuery('<textarea id="'+editorID+'">').appendTo("#"+id).width(screen.width).height(screen.height),
        editor = CodeMirror.fromTextArea(editorID, {
            parserfile: ["../contrib/python/js/parsepython.js"],
            stylesheet: "/static/codemirror/contrib/python/css/pythoncolors.css",
            path:       "/static/codemirror/js/",
            lineNumbers: true,
            textWrapping: false,
            indentUnit: 4,
            parserConfig: {'pythonVersion': 2, 'strictErrors': true},
            saveFunction: function() { saveFile() }
        });
    
    // make the parent element (tabbed pane) a drop target for file objects
    editorArea.parent().droppable ({
        accept: '.file .obj',
        drop: function(ev,ui) { 
            var droppedObject = jQuery(ui.draggable).clone();
            debug.info('CodeEditor drop',droppedObj);
            if (droppedObject.hasClass('file')) {
                editFile(droppedObject.attr("path"));
            };
        }
    });

    /** tell the model to save the current contents to current filepath */
    function saveFile() {
        model.setFile(filepath,editor.getCode());
    };
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** get contents of specified file from model, load into editor */
    this.editFile = function(pathname) {
        filepath = pathname;
        model.getFile(pathname, 
            // success
            function(contents) {
                editor.setCode(contents);
            },
            // failure
            function(jqXHR, textStatus, errorThrown) {
                debug.info(textStatus)
                debug.info(errorThrown)
                alert("Error editing file: "+jqXHR.statusText)
            }
        );
    };
    
    /** get the pathname for the current file */
    this.getPathname = function() {
        return filepath;
    };    
}

/** set prototype */
openmdao.CodeEditor.prototype = new openmdao.BaseFrame();
openmdao.CodeEditor.prototype.constructor = openmdao.CodeEditor;