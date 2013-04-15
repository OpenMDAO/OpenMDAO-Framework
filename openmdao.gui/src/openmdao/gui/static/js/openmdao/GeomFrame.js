
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

g = {};                 // place to store "globals"  FIXME: this should be namespaced to WebViewer

g.animcount = 0;
g.NAME_SIZE=256;   // our messages have the first NAME_SIZE bytes allocated for a topic name
g.MSG_START = g.NAME_SIZE;

g.zNear =  1.0;             // have some values for Zscreen
g.zFar  = 11.0;

g.messageQ = [];              // a place to put the binary messages

openmdao.GeomFrame = function(id, model, pathname) {

    openmdao.GeomFrame.prototype.init.call(this, id, 'Geometry: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        geometry = null,
        contextMenu = jQuery('<ul id='+id+'-menu class="context-menu">'),
        canvas_id = id+"-canvas",
        status_id = id+"-status";

    var html = ['<canvas id='+canvas_id+'>',
                '  If you are seeing this your web browser does not support the &lt;canvas>&gt; element. Ouch!',
                '</canvas>',
                '<div id='+status_id+'></div>'].join();

    // replace old html
    self.elm.html(html);

    g.id = id;
    g.canvas = document.getElementById(id+"-canvas");

    // set up extra storage for matrix-matrix multiplies
    g.uiMatrix = new J3DIMatrix4();

    // ui cursor variables
    g.cursorX  = -1;    // current cursor position
    g.cursorY  = -1;
    g.keyPress = -1;    // last key pressed
    g.startX   = -1;    // start of dragging position
    g.startY   = -1;
    g.button   = -1;    // button pressed
    g.modifier =  0;    // modifier (shift,alt,cntl) bitflag
    g.offTop   =  0;    // offset to upper-left corner of the canvas
    g.offLeft  =  0;
    g.dragging = false;
  
  //var canvas = document.getElementById("WebViewer");
    g.canvas.addEventListener('mousemove',  getCursorXY,  false);
    g.canvas.addEventListener('mousedown',  getMouseDown, false);
    g.canvas.addEventListener('mouseup',    getMouseUp,   false);
    document.addEventListener('keypress',   getKeyPress,  false);

    g.statusline = new StatusLine(status_id);
    wvStart(); 

    // set the connections pane height to dynamically fill the space between the
    // component and variable selectors
    function resize_contents() {
        var title_height = self.elm.find('ui-dialog-titlebar').outerHeight();
        var status_height = self.elm.find("#statusline").outerHeight();
        g.canvas.height = self.elm.height()-title_height-status_height;
        g.canvas.width = self.elm.width() - 20;
    }

    // resize contents when told to do so (e.g. by BaseFrame when dialog is resized)
    self.elm.on('resize_contents', function(e) {
        resize_contents();
    });

    function handleMessage(message) {
        // NOTE: the message here is the entire ArrayBuffer containing the padded topic name + the
        // actual message being sent to the WebViewer.  This is done so we can avoid copying 
        // the buffer.
        g.messageQ.push(message[1]); // FIXME: add namespacing to WebViewer stuff
    }

    // subscribe to model for data
    function setGeometry(pathname) {
        if (geometry !== null) {
            model.removeListener(geometry, handleMessage);
        }
        geometry = pathname;
        model.addListener(pathname, handleMessage);
    }

    // prompt for a new geometry
    function changeGeometry() {
        openmdao.Util.promptForValue('Enter pathname of geometry object to view:',
            function(pathname) {
                setGeometry(pathname);
            }
        );
    }

    // create context menu
    contextMenu.append(jQuery('<li>View Different Geometry...</li>').click(function(ev) {
        changeGeometry();
    }));
    contextMenu.appendTo(this.elm);
    ContextMenu.set(contextMenu.attr('id'), id);

    if (pathname) {
        setGeometry(pathname);
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        if ( geometry !== null ) {
            model.removeListener(geometry, handleMessage);
        }
    };

    /** nothing to see here, we get our data elsewhere */
    this.update = function() {};

    g.getWidth = function() {
        return self.elm.height();
    };

    g.getHeight = function() {
        return self.elm.width();
    };

};

/** set prototype */
openmdao.GeomFrame.prototype = new openmdao.BaseFrame();

openmdao.GeomFrame.prototype.chooseVariable = function() {
    openmdao.Util.promptForValue('Enter pathname of geometry object to view:',
        function(pathname) {
            p=new openmdao.GeomFrame('geom-'+pathname, openmdao.model, pathname);
        }
    );
};

