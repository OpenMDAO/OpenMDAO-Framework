var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.update = function() {
    // tell all openmdao frames to update themselves
    jQuery.each(this.frames,function(id,frame) {
        frame.update();
    });
};

openmdao.BaseFrame = function() {
    this.id    = null;  // the id attribute of the element the frame is built on
    this.elm   = null;  // the element the frame is built on wrapped by jQuery
    this.par   = null;  // the parent element as a jQuery object
    this.title = "";    // the title to be used for this frame
    this.menu  = null;  // an optional menu
};

openmdao.BaseFrame.prototype.init = function (id,title,menu) {
/*  initialize a BaseFrame on the element with the given ID
    if the element doesn't exist it will be created as a popup
    any existing HTML under the element will be deleted
    if a menu is provided, then it will be built into the frame
 */
    this.id = id;
    this.title = title;
    this.menu = menu;

    if (this.id) {
        this.elm = jQuery("#"+this.id);
    }
    else {
        if (openmdao.uniqueID) {
            openmdao.uniqueID = openmdao.uniqueID + 1;
        }
        else {
            openmdao.uniqueID = 1;
        }
        this.id = "BaseFrame"+openmdao.uniqueID;
    }

    // add to list of frames
    if (! openmdao.hasOwnProperty('frames')) {
        openmdao.frames = { };
    }
    openmdao.frames[this.id] = this;

    // if the elm doesn't exist, create it as a popup
    if (this.elm && this.elm.length > 0) {
        this.par = this.elm.parent();
    }
    else {
        this.par = null;
        this.elm = jQuery('<div id='+this.id+'></div>');
        this.popup(this.title);
    }

    // delete any existing content and prevent browser context menu
    this.elm.html("")
            .bind("contextmenu", function(e) { return false; });

    // create menubar and add menu if one has been provided
    if (this.menu) {
        var menuID  = this.id+"-menu",
            menuDiv = this.elm.append("<nav2 id='"+menuID+"'>"),
            menuObj = new openmdao.Menu(menuID,this.menu),
            style   = "style='position:absolute;top:5px;right:5px;z-index:1001'",
            pop_btn = jQuery("<div title='Pop Out' "+style+">*</div>");
        pop_btn.click( function() { this.popup(this.title); }.bind(this) );
        menuDiv.append(pop_btn);
    }
};

openmdao.BaseFrame.prototype.popup = function (title) {
    /* put this frame in a popup */
    var dlg = this.elm;
    dlg.dialog({
        'modal': false,
        'title': title,
        'close': function(ev, ui) {
                    this.close();
                 }.bind(this),
        'height': 'auto',
        'width' : 'auto',
        'open': function(ev, ui) {
                    // make sure the popup fits in the window
                    if (dlg.height() > window.innerHeight*0.8) {
                        dlg.height(window.innerHeight*0.8);
                    }
                    if (dlg.width() > window.innerWidth*0.8) {
                        dlg.width(window.innerWidth*0.8);
                    }
                    // and is not off the edge of the window
                    var off  = dlg.offset(),
                        top  = off.top,
                        left = off.left;
                    if (top < 0) {
                        top = 0;
                    }
                    else if (top + dlg.outerHeight() > window.innerHeight) {
                        top = window.innerHeight - dlg.outerHeight();
                    }
                    if (left < 0) {
                        left = 0;
                    }
                    else if (left + dlg.outerWidth() > window.innerWidth) {
                        left = window.innerWidth - dlg.outerWidth();
                    }
                    if (top !== off.top || left !== off.left) {
                        dlg.dialog({ position: [top, left] });
                    }
                }
    });

};

openmdao.BaseFrame.prototype.setTitle = function (title) {
    if (title) {
        this.title = title;
        this.elm.dialog('option', 'title', title);
    }
};

openmdao.BaseFrame.prototype.close = function () {
    if ((this.hasOwnProperty('destructor')) &&
        (typeof this.destructor === 'function')) {
        this.destructor();
    }
    // assuming I'm a dialog: if I have a parent then re-dock with it, else self-destruct
    if (this.par) {
        this.elm.dialog('destroy');
        this.elm.appendTo(this.par);
        this.elm.show();
    }
    else {
        this.elm.dialog('destroy');
        this.elm.remove();
    }
};

openmdao.BaseFrame.prototype.update = function() {
    // place holder to update contents of the frame (optional)
    //debug.warn('BaseFrame.update - no update function defined for',this)
};

