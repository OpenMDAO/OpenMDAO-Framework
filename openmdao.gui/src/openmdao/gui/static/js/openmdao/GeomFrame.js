
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.GeomFrame = function(id, model, pathname) {
    openmdao.GeomFrame.prototype.init.call(this, id, 'Geometry: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        geometry = null,
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>");

    function handleMessage(message) {
        if (message.length === 2) {
            var newdata = {};
            newdata[message[0]] = message[1];
            updateData(newdata);
        }
    }

    // subscribe to model for data
    function setGeometry(pathname) {
        if (geometry != null) {
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
        model.removeListener(variables[i], handleMessage);
    };

    /** nothing to see here, we get our data elsewhere */
    this.update = function() {};

};

/** set prototype */
openmdao.GeomFrame.prototype = new openmdao.BaseFrame();

/*
openmdao.GeomFrame.prototype.chooseVariable = function() {
    openmdao.Util.promptForValue('Enter pathname of geometry object to view:',
        function(pathname) {
            p=new openmdao.GeomFrame('plot-'+pathname,openmdao.model,pathname);
        }
    );
*/
};

