
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ParametersPane = function(elm,model,pathname,name,editable) {
    var parms,
        parmsDiv = jQuery("<div id='"+name+"_parms' >"),
        columns = [
            {id:"target",  name:"Target",  field:"target", editor:TextCellEditor},
            {id:"low", name:"Low", field:"low", editor:TextCellEditor},
            {id:"high", name:"High", field:"high", editor:TextCellEditor},
            {id:"scaler", name:"Scaler", field:"scaler", editor:TextCellEditor},
            {id:"adder", name:"Adder", field:"adder", editor:TextCellEditor},
            {id:"fd_step", name:"fd_step", field:"fd_step", editor:TextCellEditor},
            {id:"scope", name:"Scope", field:"scope", editor:TextCellEditor},
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

    elm.append(parmsDiv);
    parms = new Slick.Grid(parmsDiv, [], columns, options)
    if (editable) {
        parms.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** load the table with the given properties */
    this.loadTable = function(properties) {
        if (properties) {
            parms.setData(properties)
        }
        else {
            parms.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        parms.updateRowCount()
        parms.render()
    }    
}