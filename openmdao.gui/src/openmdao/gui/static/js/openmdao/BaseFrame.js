var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.BaseFrame = function () {
    id:         null;   // the id attribute of the element the frame is built on
    elm:        null;   // the element the frame is built on wrapped by jQuery
    par:        null;   // the parent element as a jQuery object
    title:      "";     // the title to be used for this frame
    menu:       null;   // an optional menu     
}
     
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
        this.id = "BaseFrame"+openmdao.uniqueID
    }
    
    // if the elm doesn't exist, create it as a popup 
    if (this.elm && this.elm.length > 0) {
        this.par = this.elm.parent();
    }
    else {
        this.par = null;
        this.elm = jQuery('<div id='+this.id+'></div>');
        this.popup(this.title);
    }
    
    // set the title
    if (this.title) {
        this.elm.attr('title',this.title);
    }
        
    // delete any existing content and prevent browser context menu
    this.elm.html("")
                 .bind("contextmenu", function(e) { return false; })
    
    // create menubar and add menu if one has been provided
    if (this.menu) {        
        var menuID = this.id+"-menu",
            menuDiv = this.elm.append("<nav2 id='"+menuID+"'>"),
            popButton = jQuery("<div title='Pop Out' style='position:absolute;top:5px;right:5px;z-index:1001'>*</div>")
                .click( function() { this.popup(this.title) }.bind(this)
            )
        new openmdao.Menu(menuID,this.menu)
        // FIXME: HACK, add button to make window pop out (TODO: alternately open in new browser window?)
        menuDiv.append(popButton)
    }                
},
    
openmdao.BaseFrame.prototype.popup = function (title) {
    /* put this frame in a popup */
    this.elm.dialog({
        'modal': false,
        'title': title,
        'close': function(ev, ui) {
                    this.close();
                 }.bind(this),
        width:   'auto',
        height:  'auto'
    })
}

openmdao.BaseFrame.prototype.setTitle = function (title) {
    if (title) {
        this.title = title
        this.elm.dialog('option', 'title', title);
    }
}

openmdao.BaseFrame.prototype.close = function () {
    // assuming I'm a dialog: if I have a parent then re-dock with it, else self-destruct
    if (this.par) {
        this.elm.dialog('destroy')
        this.elm.appendTo(this.par)
        this.elm.show()
    }
    else {
        this.elm.dialog('destroy')
        this.elm.remove(); 
    }
}