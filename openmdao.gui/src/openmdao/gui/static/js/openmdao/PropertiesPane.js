
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.PropertiesPane = function(elm,model,pathname,name,editable) {
    var props,
        propsDiv = jQuery("<div id='"+name+"_props' >"),
        columns = [
            {id:"name",  name:"Name",  field:"name",  width:80 },
            {id:"value", name:"Value", field:"value", width:80, editor:TextCellEditor},
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

    elm.append(propsDiv);
    props = new Slick.Grid(propsDiv, [], columns, options)
    if (editable) {
        props.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            props.setData(properties)
        }
        else {
            props.setData([])
            alert('Error getting properties for '+pathname+' ('+name+')')
            debug.info(properties)
        }
        props.updateRowCount()
        props.render()
    }    
}