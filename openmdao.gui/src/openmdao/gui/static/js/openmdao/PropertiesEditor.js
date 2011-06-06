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
        nameHeader = jQuery("<h3>"),
        inputsHeader = jQuery("<h3>Inputs</h3>"),
        outputsHeader = jQuery("<h3>Outputs</h3>"),
        inputsDiv = jQuery("<div id='inputs'>"),
        outputsDiv = jQuery("<div id='outputs'>"),
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
        inputs_options = {
            editable: true,
            asyncEditorLoading: false,
            editOnDoubleClick: true,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
            //enableAddRow: true,
        },
        outputs_options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        }
        
    elm.html("")
    elm.append(nameHeader);
    elm.append('<p>')
    elm.append(inputsHeader)
    elm.append(inputsDiv)
    elm.append('<p>')
    elm.append(outputsHeader)
    elm.append(outputsDiv)

    var inputs = new Slick.Grid(inputsDiv, [], columns, inputs_options)       
    inputs.onCellChange.subscribe(function(e,args) {
        // TODO: better way to do this (e.g. model.setProperty(path,name,value)
        cmd = 'top.'+self.pathname+'.'+args.item.name+'='+args.item.value
        model.issueCommand(cmd)
    })
    
    var outputs = new Slick.Grid(outputsDiv, [], columns, outputs_options)       
        
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
    function loadTables(properties) {
        nameHeader.html(self.pathname)
        inputs.setData(properties['inputs'])
        inputs.updateRowCount()      
        inputs.render()
        outputs.setData(properties['outputs'])
        outputs.updateRowCount()      
        outputs.render()
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
        model.getComponent(path, loadTables,
            function(jqXHR, textStatus, errorThrown) {
                self.pathname = ''
                alert("Error editing object: "+jqXHR.statusText)
            }
        )
        return this
    }

}