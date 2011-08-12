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
openmdao.BasePane = {
    
    /* reference to the instance */
    self: null,
    
    /* the DOM element the pane is built into */
    elm:  null,
    
    /* the element's parent */
    par: null,
    
    /*  initialize a BasePane on the element with the given ID
        if the element doesn't exist it will be created as a popup
        any existing HTML under the element will be deleted
        if a menu is provided, then it will be built into the pane
     */
    init: function (id,title,menu) {       
        this.self = this
        this.elm = jQuery("#"+id)
        
        // if the elm doesn't exist, create it as a popup 
        if (this.elm.length === 0) {
            this.elm = jQuery('<div id='+id+'></div>')
            this.popup(title+': '+id)
        }
        else {
            this.par = this.elm.parent()
        }

        // set the title
        if (title)
            this.elm.attr('title',title)
            
        // delete any existing content and prevent browser context menu
        this.elm.html("")
                .bind("contextmenu", function(e) { return false; })
        
        // create menubar and add menu if one has been provided
        if (menu) {        
            var menuID = id+"-menu",
                menuDiv = this.elm.append("<nav2 id='"+menuID+"'>"),
                popButton = jQuery("<div title='Pop Out' style='position:absolute;top:5px;right:5px;z-index:1001'>*</div>")
                    .click( function() { this.popup(title) }.bind(this) 
                          )
            new openmdao.Menu(menuID,menu)
            // FIXME: HACK, add button to make window pop out (TODO: alternately open in new browser window?)
            menuDiv.append(popButton)
        }                
    },
    
    /* put this pane in a popup */
    popup: function(title) {
        this.elm.dialog({
            'modal': false,
            'title': title,
            'close': function(ev, ui) {
                        if (this.par) {
                            this.elm.dialog('destroy')
                            this.elm.appendTo(this.par)
                            this.elm.show()
                        }
                        else {
                            this.elm.remove(); 
                        }
                    }.bind(this),
            width: 'auto', 
            height: 'auto'
        })
    }
}

