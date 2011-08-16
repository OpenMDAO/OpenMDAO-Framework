/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.CodeEditor = function(id,model) {
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    if (arguments.length > 0)
        // initialize private variables
        var self = this,
            filepath = "",
            editor = null
        // build it
        init()
    
    
    function init() {
        // initialize the base pane
        self.prototype = Object.create(openmdao.BasePane, {
            id:             { value: id },
            title:          { value: "Code Editor" },
        })
        self.prototype.init()
        
        var editorID = id+'-content',
            editorArea = jQuery('<textarea id="'+editorID+'">').appendTo("#"+id).width(screen.width).height(screen.height)
        editor = CodeMirror.fromTextArea(editorID, {
            parserfile: ["../contrib/python/js/parsepython.js"],
            stylesheet: "/static/codemirror/contrib/python/css/pythoncolors.css",
            path:       "/static/codemirror/js/",
            lineNumbers: true,
            textWrapping: false,
            indentUnit: 4,
            parserConfig: {'pythonVersion': 2, 'strictErrors': true},
            saveFunction: function() { saveFile() }
        })
        
        // make the parent element (tabbed pane) a drop target for file objects
        editorArea.parent().droppable ({
            accept: '.file .obj',
            drop: function(ev,ui) { 
                var droppedObject = jQuery(ui.draggable).clone();
                debug.info('CodeEditor drop',droppedObj)
                if (droppedObject.hasClass('file')) {
                    editFile(droppedObject.attr("path"));
                }
            }
        });
    }

    /** tell the model to save the current contents to current filepath */
    function saveFile() {
        model.setFile(filepath,editor.getCode()) 
    }
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    /** get contents of specified file from model, load into editor */
    this.editFile = function(pathname) {
        model.getFile(pathname, 
            // success
            function(contents) {
                filepath = pathname
                editor.setCode(contents)
            },
            // failure
            function(jqXHR, textStatus, errorThrown) {
                debug.info(textStatus)
                debug.info(errorThrown)
                alert("Error editing file: "+jqXHR.statusText)
            }
        )
    }
}

