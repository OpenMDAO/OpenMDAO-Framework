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
     
    var that = this,
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
    function updatePalette(types) {
        // remember what is expanded
        var expanded = jQuery('.library-list:visible')
    
        // build the new html
        var html="<div id='library'>"
        types.children().each(function() {
            html+= packageHTML($(this))
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
    function packageHTML(pkg) {
        var libname = pkg.attr("name")
        var html="<div class='library-header' title='"+libname+"'>"
        html+="<h3>"+libname+"</h3>"
        html+="</div>"
        html+="<ul class='library-list' title='"+libname+"'>"
        if (pkg.children().length === 0)
            html+="<div class='objtype' title='"+libname+" library is empty'> <i>None</i> </div>"
        else {
            pkg.children("Package").each(function() {
                html+=packageHTML($(this))
            })
            pkg.children("Type").each(function() {
                var name = $(this).attr("name");
                var path = $(this).attr("path");
                html+="<div class='objtype' path='"+path+"' title='"+name+"'>"+name+"</div>"
            })
        }
        html+="</ul>"
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