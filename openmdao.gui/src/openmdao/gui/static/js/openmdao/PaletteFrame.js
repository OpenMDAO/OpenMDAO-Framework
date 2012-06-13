
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PaletteFrame = function(id,model) {
    openmdao.PaletteFrame.prototype.init.call(this,id,'Libraries',[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        palette = jQuery('#'+id),
        libs = jQuery('<div>').appendTo(palette);

    // dropping a filename onto the palette pane means import *
    /*
    libs.droppable ({
        accept: '.file',
        drop: function(ev,ui) {
            debug.info('PaletteFrame drop: ',ev,ui);
            var droppedObject = jQuery(ui.draggable).clone();
            debug.info('PaletteFrame drop: ',droppedObject);
            var path = droppedObject.attr("path");
            debug.info('PaletteFrame drop: '+path);
            if (/.py$/.test(path)) {
                model.importFile(path);
            }
            else {
                alert("Not a python file:\n"+path);
            }
        }
    });
    */
    /** rebuild the Palette from a JSON library list of tuples of the form (libname, meta_dict) */
    function updatePalette(packages) {
        // remember what is expanded
        //var expanded = jQuery('.library-list:visible');

        // build the new html
        var html="<div id='library'>";
        html+= '<table cellpadding="0" cellspacing="0" border="0" id="objtypetable">';
        // headers: Class, Module Path, Version, Interfaces
        html += '<thead><tr><th></th><th></th><th></th><th></th></tr></thead><tbody>';
        jQuery.each(packages, function(name,item) {
            html+= packageHTML(name, item);
        });
        html+="</tbody></table></div>";

        // replace old html
        libs.html(html);
        
        var dtable = palette.find('#objtypetable').dataTable({
            'bPaginate': false,
            'bjQueryUI': true,
            'sScrollY': '500px',
            'bScrollCollapse': true,
            'aoColumnDefs': [
                 { 'bVisible': false, 'aTargets': [1,2,3] },
             ],
        });
        
        // make everything draggable
        jQuery('.objtype').draggable({ helper: 'clone', appendTo: 'body' });
        jQuery('.objtype').addClass('jstree-draggable'); // allow drop on jstree
    }

    /** build HTML string for a package */
    function packageHTML(name,item) {
        debug.info('item')
        debug.info(item)
        var html = "<tr><td class='objtype' modpath="+item.modpath+">"+name+"</td><td>"+
                   item.modpath+"</td><td>"+item.version+"</td><td>"+item.ifaces+"</td></tr>";
        return html;
    }

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== 'types') {
            debug.warn('Invalid types data:',message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            libs.html("<div>Updating...</div>")
                .effect('highlight',{color:'#ffd'},1000);
            updatePalette(message[1][0]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the display, with data from the model */
    this.update = function() {
        libs.html("<div>Updating...</div>")
            .effect('highlight',{color:'#ffd'},1000);
        model.getTypes(updatePalette);
    };

    // ask model for an update whenever something changes
    model.addListener('types',handleMessage);
    
    this.update();

};

/** set prototype */
openmdao.PaletteFrame.prototype = new openmdao.BaseFrame();
openmdao.PaletteFrame.prototype.constructor = openmdao.PaletteFrame;

