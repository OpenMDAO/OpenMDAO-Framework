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
     *  private (available only to privileged methods) 
     ***********************************************************************/
     
    var self = this,
        elm = jQuery("#"+id)

    // dropping a filename onto the palette pane means import *
    elm.parent().droppable ({
        //accept: '.file .ui-draggable',
        drop: function(ev,ui) { 
            var droppedObject = jQuery(ui.draggable).clone()            
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
        elm.empty()
        elm.html(html)
        
        // make everything draggable
        jQuery('.objtype').draggable({ helper: 'clone', appendTo: 'body' })
                
        // collapse all and add click functionality
        jQuery('.library-list').hide()
        jQuery('.library-header').click(function () {
            $(this).next().toggle("normal")
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
        elm.empty()
        elm.html("<div>Updating...</div>")
        elm.effect('highlight',{color:'#ffd'},1000)
        model.getTypes(updatePalette)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)
    
    /***********************************************************************
     *  privileged (can access privates, accessible to public and outside) 
     ***********************************************************************/
 
}