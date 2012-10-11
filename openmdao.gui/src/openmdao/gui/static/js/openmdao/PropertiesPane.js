
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PropertiesPane = function(elm,model,pathname,name,editable,meta) {
    var self = this,
        props,
        propsDiv = jQuery("<div id='"+name+"_props' class='slickgrid' style='overflow:none;'>"),
        columns = [
            {id:"name",  name:"Name",  field:"name",  width:80 },
            {id:"value", name:"Value", field:"value", width:80, editor:openmdao.ValueEditor},
            //{id:"valid", name:"Valid", field:"valid", width:60},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            enableTextSelectionOnCells: true
        },
        _collapsed = {};

    self.pathname = pathname;
    if (editable) {
        options.editable = true;
        options.autoEdit = true;
    }

    if (meta) {
        columns = [
            {id:"name",      name:"Name",        field:"name",      width:100,  formatter:VarTableFormatter },
            {id:"type",      name:"Type",        field:"type",      width:60 },
            {id:"value",     name:"Value",       field:"value",     width:100 , editor:openmdao.ValueEditor },
            {id:"units",     name:"Units",       field:"units",     width:60  },
            {id:"valid",     name:"Valid",       field:"valid",     width:60 },
            {id:"desc",      name:"Description", field:"desc",      width:120 },
            {id:"connected", name:"Connected To",   field:"connected", width:100 },
            {id:"implicit", name:"Implicitly Connected To",   field:"implicit", width:100 },
        ];
    }

    function VarTableFormatter(row,cell,value,columnDef,dataContext) {
        var spacer = "<span style='display:inline-block;height:1px;width:" + (15 * dataContext["indent"]) + "px'></span>";
        var idx = dataView.getIdxById(dataContext.id);
        var nextline = dataView.getItemByIdx(idx+1)
        if (nextline && nextline.indent > dataContext.indent) {
            if (dataContext._collapsed) {
                return spacer + " <span class='toggle expand'></span>&nbsp;" + value;
            } else {
                return spacer + " <span class='toggle collapse'></span>&nbsp;" + value;
            }
        } else {
            return spacer + "<span class='toggle'></span>" + value;
        }
    }
    
    elm.append(propsDiv);
    var dataView = new Slick.Data.DataView({ inlineFilters: true });
    props = new Slick.Grid(propsDiv, dataView, columns, options);

    props.onBeforeEditCell.subscribe(function(row,cell) {
        if (props.getDataItem(cell.row).connected.length > 0) {
            return false;
        }

        else {
            return true;
        }
    });

    props.onClick.subscribe(function (e) {
        var cell = props.getCellFromEvent(e);
        if (cell.cell==0) {
            if (true) {
                var item = dataView.getItem(cell.row);
                if (item) {
                    //if (!item._collapsed) {
                    //    item._collapsed = true;
                    //} else {
                    //    item._collapsed = false;
                    //}
                    if (!_collapsed[item.id]) {
                        _collapsed[item.id] = true;
                    } else {
                        _collapsed[item.id] = false;
                    }
    
                    dataView.updateItem(item.id, item);
                }
                e.stopImmediatePropagation();
            }
        }
    });

    if (editable) {
        props.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. model.setProperty(path,name,value)
            model.setVariableValue(self.pathname + '.' + args.item.name, 
                                   args.item.value, args.item.type );
        });
    }

    /* Function that returns false for collapsed rows, and true for the rest.
    Used by Slickgrid */
    this.filter = function myFilter(item) {
        if (item.parent != null) {
            var idx = dataView.getIdxById(item.parent);
            var parent = dataView.getItemByIdx(idx)
    
            while (parent) {
                //if (parent._collapsed) {
                //    return false;
                //}
                if (_collapsed[parent]) {
                    return false;
                }
                var idx = dataView.getIdxById(parent);
                var parent = dataView.getItemByIdx(idx)
            }
        }
        return true;
    }

    /** load the table with the given properties */
    this.loadData = function(properties) {
        //variable to track cells that need to be highlighted
        var editableCells = {};

        if (properties) {
            // Sort by name
            properties.sort(function(a, b) {
                var nameA=a.id.toLowerCase(),
                    nameB=b.id.toLowerCase();
                if (nameA < nameB) { //sort string ascending
                    return -1;
                }
                if (nameA > nameB) {
                    return 1;
                }
                return 0; //default return value (no sorting)
            });


            jQuery.each(properties, function(index, value) {
                if (value.hasOwnProperty("connected")) {
                    var nameStyle = '',
                        valueStyle = '';
                    if (options.editable && (value.connected.length === 0) && (value.ttype != 'slot')) {
                        valueStyle += " cell-editable";
                    }
                    if (value.hasOwnProperty("implicit") && (value.implicit.length > 0)) {
                        //need a css class for highlighting implicitly connected inputs
                        if (name === "Inputs") {
                            nameStyle += " parameter";
                            valueStyle += " parameter";
                        }
                        else if (name === "Outputs") {
                            nameStyle += " objective";
                            valueStyle += " objective";
                        }
                    }
                    var css = {};
                    if (nameStyle !== '') {
                        css['name'] = nameStyle;
                    }
                    if (valueStyle !== '') {
                        css['value'] = valueStyle;
                    }
                    if (css !== {}) {
                        editableCells[index] = css;
                    }
                }
            });

            //props.setData(properties);
              dataView.beginUpdate();
              dataView.setItems(properties);
              dataView.setFilter(this.filter);
              dataView.endUpdate();

        }
        else {
            props.setData([]);
            alert('Error getting properties for '+self.pathname+' ('+name+')');
            debug.info(self.pathname,properties);
        }
        props.setCellCssStyles("highlight", editableCells);
        props.updateRowCount();
        props.render();
    };
};
