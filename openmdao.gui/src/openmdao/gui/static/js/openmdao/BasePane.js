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
openmdao.BasePane = function() {
    this.elm = null,
    this.par = null,
    
    this.init = function (id,title,menu) {       
        this.elm = jQuery("#"+id)
        
        // if the elm doesn't exist, create it as a popup 
        if (this.elm.length === 0) {
            this.elm = jQuery('<div id='+id+'></div>')
            this.popup(title+': '+id)
        }
        else {
            this.par = this.elm.parent()
            this.elm.html("")
        }

        // set the title
        if (title)
            this.elm.attr('title',title)
            
        // delete any existing content and prevent browser context menu
        this.elm.html("").bind("contextmenu", function(e) { return false; })
        
        // create menubar and add menu if one has been provided
        if (menu) {        
            var menuID = id+"-menu",
                menuDiv = this.elm.append("<nav2 id='"+menuID+"'>"),
                popButton = jQuery("<div title='Pop Out' style='position:absolute;top:5px;right:5px;z-index:1001'>*</div>")
                    .click(function() {this.popup(title) }.bind(this))
            new openmdao.Menu(menuID,menu)
            // FIXME: experimental HACK, add button to make window pop out (TODO: alternately open in new browser window?)
            menuDiv.append(popButton)
        }                
    },
    
    this.popup = function(title) {
        var elm = this.elm,
            par = this.par
        this.elm.dialog({
            'modal': false,
            'title': title,
            'close': function(ev, ui) { 
                if (par) {
                    elm.dialog('destroy')
                    elm.appendTo(par)
                    elm.show()
                }
                else {
                    elm.remove(); 
                }
            },
        })
    }
}

