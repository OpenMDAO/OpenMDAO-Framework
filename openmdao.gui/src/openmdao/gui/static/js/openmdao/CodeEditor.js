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
    this.prototype = new openmdao.BasePane()
    
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        elm  = null,
        filepath = null,
        editor = null
        
    if (arguments.length > 0)
        init()
    
    function init() {
        self.prototype.init(id,'CodeEditor')
        //elm = jQuery("#"+id).width(screen.width).height(screen.height),
        var editorID = id+'-content'
        elm = jQuery('<textarea id="'+editorID+'">').appendTo("#"+id).width(screen.width).height(screen.height)
        
        filepath = "",
        
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
        elm.droppable ({
            accept: '.file',
            drop: function(ev,ui) { 
                var droppedObject = jQuery(ui.draggable).clone();
                debug.info('CodeEditor drop')
                editFile(droppedObject.attr("path"));
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

