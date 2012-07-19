
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.CodeFrame = function(id,model) {
    openmdao.CodeFrame.prototype.init.call(this,id,'Code');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
	
	uiBarID=id+'-uiBar';
	uiparent=jQuery("#"+id).parent();
	uiparent.css({overflow:'hidden',position:'absolute'});
	uiBar= jQuery('<div id="'+uiBarID+'">').prependTo(uiparent).width(screen.width).height(15);	

	saveID=	uiBarID+'-save';
	findID=	uiBarID+'-find';
	replaceID=	uiBarID+'-replace';
	replaceAllID=	uiBarID+'-replaceAll';
	undoID=uiBarID+'-undo';
	
	jQuery("<button id='"+saveID+"'>Save</button>").button({icons: {primary:'ui-icon-disk'}}).css({height:'25px'}).appendTo("#"+uiBarID);    
	jQuery("<button id='"+findID+"'>Find</button>").button({icons: {primary:'ui-icon-search'}}).css({height:'25px'}).appendTo("#"+uiBarID);    
	jQuery("<button id='"+replaceID+"'>Replace</button>").button({icons: {primary:'ui-icon-search'}}).css({height:'25px'}).appendTo("#"+uiBarID);  
	jQuery("<button id='"+replaceAllID+"'>Replace All</button>").button({icons: {primary:'ui-icon-search'}}).css({height:'25px'}).appendTo("#"+uiBarID);  	
	jQuery("<button id='"+undoID+"'>Undo</button>").button({icons: {primary:'ui-icon-arrowrefresh-1-n'}}).css({height:'25px'}).appendTo("#"+uiBarID);  	
	
	jQuery("#"+saveID).click(function() { saveFile(); });
	jQuery("#"+findID).click(function() { editor.commands.commands.find.exec(editor); });
	jQuery("#"+replaceID).click(function() { editor.commands.commands.replace.exec(editor); });
	jQuery("#"+replaceAllID).click(function() { editor.commands.commands.replaceall.exec(editor); });
	jQuery("#"+undoID).click(function() { editor.commands.commands.undo.exec(editor); });
	
    var self = this,
        filepath = "",
        editorID = id+'-textarea',

        editorArea = jQuery('<pre id="'+editorID+'">').css({position:'absolute',overflow:'hidden'}).appendTo("#"+id);
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
    editorArea.width(jQuery(window).width()-210);
    editorArea.height(jQuery(window).height()-75);
    };
    
    /** get the pathname for the current file */
    this.getPathname = function() {
        return filepath;
    };
};

/** set prototype */
openmdao.CodeFrame.prototype = new openmdao.BaseFrame();
openmdao.CodeFrame.prototype.constructor = openmdao.CodeFrame;

