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
openmdao.PropertiesEditor = function(id,model) {
    /***********************************************************************
     *  private (available only to privileged methods) 
     ***********************************************************************/
     
    var self = this,
        elm = jQuery("#"+id),
        pathname = '',
        filterChars = '_' // filter vars with names that start with these chars
    
    debug.info('PropertiesEditor parent:')
    debug.info(elm.parent())
    debug.info('PropertiesEditor elm:')
    debug.info(elm)
    
    /**  build a jqGrid on the table */
    elm.jqGrid({ 
        defaults : {
            loadtext:       "Loading...",
            autowidth:      true,
            height:         '100%',
            shrinktofit:    true,
        },
        datatype: "json",
        colNames: ['Property','Value'],
        colModel: [ {name:'property', index:'property'},
                    {name:'value',    index:'value', sortable:false} ],
        caption: "Properties" 
    });
    //jQuery("#list2").jqGrid('navGrid','#pager2',{edit:false,add:false,del:false})

    /** make the parent element (tabbed pane) a drop target for obj objects * /
    elm.parent().droppable ({
        accept: '.obj',
        drop: function(ev,ui) { 
            var droppedObject = jQuery(ui.draggable).clone();
            self.editObject(droppedObject.attr("path"));
        }
    });
    /**/
  
    /** load the table with the properties of the given object
     * (in the case of a non-object, load properties of the parent object) */
    function loadTable(obj) {
        debug.info("PropertiesEditor.loadTable: (pathname="+self.pathname+")")
        debug.log(obj)
        debug.log(elm)
        debug.log(this.elm)
        debug.log(self.elm)
        if (obj == null || typeof obj !== 'object') {
            lastdot = self.pathname.lastIndexOf('.')
            if (lastdot>0)
                self.editObject(self.pathname.substring(0,lastdot))
            else
                self.editObject('')
        }
        else {
            elm.empty()
            
            // no caption for a pop-up, it's in the title bar
            elm.hasClass('ui-dialog-content') ? elm.setCaption('') : elm.setCaption(self.pathname)
            
            // if obj has a py/state, then those are the properties we want
            if (typeof obj['py/state'] !== "undefined")
                obj = obj['py/state']
            
            // get the list of properties and their values
            var data = [],
                types = ['number', 'string', 'boolean']
            
            jQuery.each(obj, function(property,value) {
                if (filterChars.indexOf(property[0])<0 && property.indexOf('py/')<0) {
                    propertyType = typeof value
                    if (types.indexOf(propertyType)>=0)
                        data.push( { 'property' : property, 'value' : value })
                    else
                        debug.warn("Not an editable property: "+property)
                }
            })
            elm.addRowData('property', data)
        }
        debug.info('PropertiesEditor parent:')
        debug.info(elm.parent())
        debug.info('PropertiesEditor elm:')
        debug.info(elm)
        return this
    }
    
    /** if there is an object loaded, update it from the model */
    function update() {
        debug.info('PropertiesEditor.update() pathname='+pathname)
        if (self.pathname && self.pathname.length>0)
            self.editObject(self.pathname)
    }
    
    /** ask model for an update whenever something changes */
    model.addListener(update)
    
    /** get outer box of jqGrid */
    function getOuter() {
        debug.info("PropertiesEditor.getOuter: (pathname="+self.pathname+")")
        debug.log(elm)
        debug.log(self.elm)
        // jqGrid buries the table 4 layers deep <gbox <gview <bdiv <div <table>>>>>
        debug.info(elm.parent().parent().parent().parent())
        return elm.parent().parent().parent().parent()  // FUGLY
    }
    
    /***********************************************************************
     *  privileged (can access privates, accessible to public and outside) 
     ***********************************************************************/
    
    /** get the specified object from model, load properties into table */
    this.editObject = function(path) {
        debug.info("PropertiesEditor.editObject: "+path+" (pathname="+self.pathname+")")
        if (self.pathname !== path)
            self.pathname = path
        model.getComponent(path, loadTable,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error editing object: "+jqXHR.statusText)
            }
        )
        return this
    }

    /** return the new outer element (after building the jqGrid) */
    this.getOuterElement = function() {
        return getOuter()
    }
}