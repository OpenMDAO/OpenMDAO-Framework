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
openmdao.Palette = function(id,model) {
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    if (arguments.length > 0) {
        // initialize private variables
        var self = this,
            libs = null
        // build it
        init()
    }

    function init() {
        // initialize the base pane
        self.prototype = Object.create(openmdao.BasePane, {
            id:     { value: id },
            title:  { value: "Libraries" },
        })        
        self.prototype.init()
    
        libs = jQuery('<div>').appendTo("#"+id)
        
        // dropping a filename onto the palette pane means import *
        libs.droppable ({
            accept: '.file',
            drop: function(ev,ui) { 
                debug.info('Palette drop: ',ev,ui)
                var droppedObject = jQuery(ui.draggable).clone()            
                debug.info('Palette drop: ',droppedObject)
                var path = droppedObject.attr("path")
                debug.info('Palette drop: '+path)
                if (/.py$/.test(path)) {
                    model.importFile(path)
                }
                else {
                    alert("Not a python file:\n"+path)
                }
            }
        })
    
        // ask model for an update whenever something changes
        model.addListener(update)
    }

    /** rebuild the Palette from an XML library list */
    function updatePalette(packages) {
        // remember what is expanded
        var expanded = jQuery('.library-list:visible')
    
        // build the new html
        var html="<div id='library'>"
        jQuery.each(packages, function(name,item) {
            html+= packageHTML(name,item)
        });
        html+="</div>"
        
        // replace old html        
        libs.html(html)
        
        // make everything draggable
        jQuery('.objtype').draggable({ helper: 'clone', appendTo: 'body' })
                
        // collapse all and add click functionality
        jQuery('.library-list').hide()
        jQuery('.library-header').click(function () {
            jQuery(this).next().toggle("normal")
            return false;
        });
        
        // restore previously expanded libraries, if any
        expanded.each(function() {
            jQuery('.library-list:[title='+this.title+']').show()
        })
    }

    /** build HTML string for a package */
    function packageHTML(name,item) {
        var html = ''
        // if item has a version it's an object type, otherwise it's a package
        if (item['version'])
            html+="<div class='objtype' path='"+item['path']+"' title='"+name+"'>"+name+"</div>"
        else {
            html+="<div class='library-header' title='"+name+"'>"
            html+="<h3>"+name+"</h3>"
            html+="</div>"
            html+="<ul class='library-list' title='"+name+"'>"
            jQuery.each(item, function(name,subitem) {
                html+= packageHTML(name,subitem)
            });
            html+="</ul>"
        }
        return html
    }

    /** update the display, with data from the model */
    function update() {
        libs.html("<div>Updating...</div>")
            .effect('highlight',{color:'#ffd'},1000)
        model.getTypes(updatePalette)
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/
 
}