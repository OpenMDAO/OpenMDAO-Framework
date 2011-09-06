
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ConstraintsPane = function(elm,model,pathname,name,editable) {
    var constraints,
        constraintsDiv = jQuery("<div id='"+name+"_constraints' >"),
        columns = [
            {id:"expr",  name:"Expression",  field:"expr", editor:TextCellEditor},
            {id:"scaler", name:"Scaler", field:"scaler", editor:TextCellEditor},
            {id:"adder", name:"Adder", field:"adder", editor:TextCellEditor},
            {id:"name",  name:"Name",  field:"name", editor:TextCellEditor},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        };
        
    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    elm.append(constraintsDiv);
    constraints = new Slick.Grid(constraintsDiv, [], columns, options)
    if (editable) {
        constraints.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** load the table with the given properties */
    this.loadTable = function(properties) {
        if (properties) {
            constraints.setData(properties)
        }
        else {
            constraints.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        constraints.updateRowCount()
        constraints.render()
    }    
}