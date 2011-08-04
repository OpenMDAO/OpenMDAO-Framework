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
        
        console.log("BasePane init",this.elm,"par=",this.par)

        // if the elm doesn't exist, create it as a popup 
        if (this.elm.length === 0) {
            this.elm = jQuery('<div id='+id+'></div>')
            this.popup(title+': '+id)
            console.log("BasePane created",this.elm,"par=",this.par)
        }
        else {
            this.par = this.elm.parent()
            this.elm.html("")
            console.log("BasePane erased",this.elm,"par=",this.par)
        }

        // set the title
        if (title)
            this.elm.attr('title',title)
            
        // delete any existing content and prevent browser context menu
        this.elm.html("").bind("contextmenu", function(e) { return false; })
        
        
        var menuBar = this.elm.append("<nav2>")
        if (menu) {        
            var menuID = id+"-menu"
            menuDiv = menuBar.append("<nav2 id='"+menuID+"'>")
            new openmdao.Menu(menuID,menu)
        }        
        
        var popButton = jQuery("<span title='Pop Out' style='float:right;color:grey'>*</span>").click(function() {this.popup(title) }.bind(this))          
        menuBar.append(popButton)
        
    },
    
    this.popup = function(title) {
        var elm = this.elm,
            par = this.par
        this.elm.dialog({
            'modal': false,
            'title': title,
            'close': function(ev, ui) { 
                if (par) {
                    console.log("trying to put",elm,"back on",par)
                    elm.dialog('destroy')
                    elm.appendTo(par)
                    elm.show()
                }
                else {
                    console.log("removing",elm,"par=",par)
                    elm.remove(); 
                }
            },
        })
    }
}

