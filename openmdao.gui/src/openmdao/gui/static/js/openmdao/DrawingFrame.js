
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

// requires flot.js

openmdao.DrawingFrame = function(id,model,pathname) {
    openmdao.DrawingFrame.prototype.init.call(this,id,'Drawing: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        css = 'height: 350px;width: 600px;', //position:relative;',
        drawing = jQuery('<div style="'+css+'";>')
           .appendTo(this.elm),
       contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>");

    /** update the Drawing with SVG data */
    function updateDrawing(svgdata) {
        drawing.html(svgdata);
    }

    // handle message with new drawing data
    function handleMessage(message) {
        debug.info('DrawingFrame received message',message);
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid drawing data for:', self.pathname, message);
            debug.warn('message length',message.length, 'topic', message[0]);
        }
        else {
            updateDrawing(message[1]);
        }
    }

    // subscribe to model for data
    function draw(pathname) {
        if (self.pathname && self.pathname.length>0) {
            if (self.pathname !== pathname) {
                model.removeListener(self.pathname, handleMessage);
                self.pathname = pathname;
            }
        }
        else {
            self.pathname = pathname;
        }
        model.addListener(self.pathname, handleMessage);
    }

    // create context menu
    contextMenu.append(jQuery('<li>Help</li>').click(function(ev) {
        alert("You're on your own Bub...");
    }));
    contextMenu.appendTo(this.elm);
    ContextMenu.set(contextMenu.attr('id'), id);

    if (pathname) {
        draw(pathname);
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/


    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            model.removeListener(self.pathname, handleMessage);
        }
    };

    /** nothing to see here, we get our data elsewhere */
    this.update = function() {};

};

/** set prototype */
openmdao.DrawingFrame.prototype = new openmdao.BaseFrame();

openmdao.DrawingFrame.prototype.chooseVariable = function() {
    openmdao.Util.promptForValue('Enter pathname of variable to Drawing:',
        function(pathname) {
            p=new openmdao.DrawingFrame('Drawing-'+pathname,openmdao.model,pathname);
        }
    );
};

