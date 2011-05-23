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
        requiredFieldValidator = function(value) {
            if (value == null || value == undefined || !value.length)
                return {valid:false, msg:"This is a required field"};
            else
                return {valid:true, msg:null};
        }
        columns = [
            {id:"name",  name:"Name",  field:"name"},
            {id:"value", name:"Value", field:"value", editor:TextCellEditor},
        ],
        options = {
            editable: true,
            asyncEditorLoading: false,
            editOnDoubleClick: true,
            multiSelect: false,
            forceFitColumns: false,            
            autoHeight: true,
            autoEdit: false,
            //enableAddRow: true,
        },
        grid = new Slick.Grid("#"+id, [], columns, options)
        
        grid.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+self.pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        })
        
        // grid.onAddNewRow.subscribe(function(e, args) {
            // var item = args.item,
                // column = args.column;
            // debug.info("Added item:")
            // debug.info(item)
            // debug.info(column)
            // grid.invalidateRow(data.length);
            // data.push(item);
            // grid.updateRowCount();
            // grid.render();
        // });
        
        function requiredFieldValidator(value) {
            if (value == null || value == undefined || !value.length)
                return {valid:false, msg:"This is a required field"};
            else
                return {valid:true, msg:null};
        }
    
    /** make the parent element (tabbed pane) a drop target for obj objects * /
    elm.parent().droppable ({
        accept: '.obj',
        drop: function(ev,ui) { 
            var droppedObject = jQuery(ui.draggable).clone();
            self.editObject(droppedObject.attr("path"));
        }
    });
    /**/
  
    /** load the table with the given properties */
    function loadTable(properties) {
        data = properties
        grid.setData(properties)
        grid.updateRowCount()      
        grid.render()
    }
    
    /** if there is an object loaded, update it from the model */
    function update() {
        if (self.pathname && self.pathname.length>0)
            self.editObject(self.pathname)
    }
    
    /** ask model for an update whenever something changes */
    model.addListener(update)
    
    /***********************************************************************
     *  privileged (can access privates, accessible to public and outside) 
     ***********************************************************************/
    
    /** get the specified object from model, load properties into table */
    this.editObject = function(path) {
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

}