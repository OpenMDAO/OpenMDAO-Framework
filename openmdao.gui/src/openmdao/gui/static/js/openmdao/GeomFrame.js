
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
        contextMenu = jQuery('<ul id='+id+'-menu class="context-menu">');

    var html = ['<div class="geom-frame">',
                '<canvas id="WebViewer">',
                '  If you are seeing this your web browser does not support the &lt;canvas>&gt; element. Ouch!',
                '</canvas>',
                '<div id="statusline"></div>',
                '</div>'].join();

    // replace old html
    self.elm.html(html);

    wvStart();  // FIXME

    function handleMessage(message) {
        console.debug("GeomFrame.handleMessage. pushing ArrayBuffer onto g.messageQ");
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

