
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

openmdao.PropertiesPane = function(elm,model,pathname,name,editable,meta) {
    var self = this,
        props,
        propsDiv = jQuery("<div id='"+name+"_props' style='overflow:none;'>"),
        columns = [
            {id:"name",  name:"Name",  field:"name",  width:80 },
            {id:"value", name:"Value", field:"value", width:80, editor:TextCellEditor},
            //{id:"valid", name:"Valid", field:"valid", width:60},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            autoEdit: false,
        };
        
    self.pathname = pathname;
        
    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }
    
    if (meta) {
        columns = [
            {id:"name",  name:"Name",  field:"name",  width:100 },
            {id:"type",  name:"Type",  field:"type", width:60},
            {id:"value", name:"Value", field:"value", width:100, editor:TextCellEditor},
            {id:"units", name:"Units", field:"units", width:60},
            {id:"valid", name:"Valid", field:"valid", width:60},
            {id:"desc",  name:"Description", field:"desc", width:120},
        ];
    }

    elm.append(propsDiv);
    props = new Slick.Grid(propsDiv, [], columns, options)
    if (editable) {
        props.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            cmd = 'top.'+self.pathname+'.'+args.item.name+'='+args.item.value
            model.issueCommand(cmd)
        });
   }
    
    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            // Sort by name
            properties.sort(function(a, b){
                var nameA=a.name.toLowerCase(), 
                    nameB=b.name.toLowerCase();
                if (nameA < nameB) //sort string ascending
                    return -1;
                if (nameA > nameB)
                    return 1;
                return 0; //default return value (no sorting)
            });
            props.setData(properties);
        }
        else {
            props.setData([]);
            alert('Error getting properties for '+self.pathname+' ('+name+')');
            debug.info(self.pathname,properties);
        }
        props.updateRowCount();
        props.render();
    }
}