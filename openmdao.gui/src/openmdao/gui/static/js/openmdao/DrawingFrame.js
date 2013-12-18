
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

// requires flot.js

openmdao.DrawingFrame = function(id, project, pathname) {
    openmdao.DrawingFrame.prototype.init.call(this,id,'Drawing: '+pathname);

    /***********************************************************************
     *  private
     ***********************************************************************/
    // initialize private variables
    var self = this,
        css = 'height:100%; width:100%;', //position:relative;',
        drawing = jQuery('<div style="'+css+'";>')
           .appendTo(this.elm),
        contextMenu = jQuery("<ul id="+id+"-menu class='context-menu'>");

    /** update the Drawing with SVG data */
    function updateDrawing(svgdata) {
        if (svgdata.length > 0) {
            drawing.html(svgdata);
        }
        else {
            drawing.html('Drawing not available');
        }
    }

    // handle message with new drawing data
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid drawing data for:', self.pathname, message);
            debug.warn('message length',message.length, 'topic', message[0]);
        }
        else {
            updateDrawing(message[1]);
        }
    }

    // subscribe to project for data and set initial value
    function draw(pathname) {
        if (self.pathname && self.pathname.length>0) {
            if (self.pathname !== pathname) {
                project.removeListener(self.pathname, handleMessage);
                self.pathname = pathname;
            }
        }
        else {
            self.pathname = pathname;
        }

        if (self.pathname.length > 0) {
            project.addListener(self.pathname, handleMessage);
            project.getValue(self.pathname)
                .done(updateDrawing)
                .fail(function(jqXHR, textStatus, errorThrown) {
                    debug.error("Error getting drawing for:", self.pathname,
                        jqXHR, textStatus, errorThrown);
                    self.pathname = '';
                });
        }
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

    /** clean up listeners before going away */
    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            project.removeListener(self.pathname, handleMessage);
        }
    };

    /** update drawing */
    this.update = function() {
        if (self.pathname) {
            draw(self.pathname);
        }
        else {
            updateDrawing('');
        }
    };
};

/** set prototype */
openmdao.DrawingFrame.prototype = new openmdao.BaseFrame();

openmdao.DrawingFrame.prototype.chooseVariable = function() {
    var prompt = 'Enter pathname of variable to draw:<br>' +
                 '(a Str variable with an SVG value)';
    openmdao.Util.promptForValue(prompt,
        function(pathname) {
            p=new openmdao.DrawingFrame('Drawing-'+pathname,openmdao.project,pathname);
        }
    );
};

