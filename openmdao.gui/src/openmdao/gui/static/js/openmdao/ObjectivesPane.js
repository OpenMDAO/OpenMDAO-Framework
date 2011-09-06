
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.ObjectivesPane = function(elm,model,pathname,name,editable) {
    var objectives,
        objectivesDiv = jQuery("<div id='"+name+"_objectives' >"),
        columns = [
            {id:"expr",  name:"Expression",  field:"expr", editor:TextCellEditor},
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

    elm.append(objectivesDiv).width('100%');
    objectives = new Slick.Grid(objectivesDiv, [], columns, options)
    if (editable) {
        objectives.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** load the table with the given properties */
    this.loadTable = function(properties) {
        if (properties) {
            objectives.setData(properties)
        }
        else {
            objectives.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        objectives.updateRowCount()
        objectives.render()
    }    
}