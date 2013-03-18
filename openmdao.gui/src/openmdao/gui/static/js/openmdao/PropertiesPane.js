
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PropertiesPane = function(elm,model,pathname,name,editable,meta) {
    var self = this,
        props,
        dataView,
        meta = meta,
        searchString = "",
        inlineFilter = undefined,
        propsDiv = jQuery("<div id='"+name+"_props' class='slickgrid' style='overflow:none;'>"),
        columns = [
            {id:"name",  name:"Name",  field:"name",  width:80,  formatter:VarTableFormatter  },
            {id:"value", name:"Value", field:"value", width:80, editor:openmdao.ValueEditor},
            //{id:"valid", name:"Valid", field:"valid", width:60},
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: true,
            enableTextSelectionOnCells: true
        },
        _collapsed = {},
        _filter = {},
        editableInTable = {};
    
    self.pathname = pathname;
    if (editable) {
        options.editable = true;
        options.autoEdit = true;
    }
    
    if (meta) {
        options.autoHeight = false;
        elm.append(jQuery("<div id='inlineFilter' class='filterpanel' style='float:right;padding:10px;'>Filter <input type='text' id='" + name + "_variableFilter' style='width:100px;'></div>"));
        propsDiv=jQuery("<div id='"+name+"_props' class='slickgrid' style='overflow:none; height:360px; width:620px;'>");
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

    elm.append(propsDiv);
    SetupTable();

    function SetupTable() {
        dataView = new Slick.Data.DataView({ inlineFilters: false });
        props = new Slick.Grid(propsDiv, dataView, columns, options);
        
        if(meta){
            jQuery("#" + name + "_variableFilter").keyup(function (e) {
                Slick.GlobalEditorLock.cancelCurrentEdit();

                searchString = this.value.toLowerCase();
                items = dataView.getItems();
                _filter = {};
                debug.info(items);
                for (i=items.length - 1; i>=0; i--){
                    name = (items[i].name) ? items[i].name.toLowerCase() : ""
                    units = (items[i].units) ? items[i].units.toLowerCase() : ""
                    description = (items[i].desc) ? items[i].desc.toLowerCase() : ""

                    if( searchString === ""){
                        _filter[items[i].id] = true;
                    }

                    else if( _filter[items[i].id] === true){
                        if( items[i].parent !== null ){
                            _filter[items[i].parent] = true;
                            _collapsed[items[i].parent] = false;
                        }
                    }
                        
                    else if(name.indexOf(searchString) !== -1 || units.indexOf(searchString) !== -1 || description.indexOf(searchString) !== -1) {
                        _filter[items[i].id] = true;
                        _collapsed[items[i].id] = false;
                        if(items[i].parent !== null){
                            _filter[items[i].parent] = true;
                            _collapsed[items[i].parent] = false;
                        }
                    }
                    
                    else {
                        _filter[items[i].id] = false;
                        _collapsed[items[i].id] = true;
                    }

                }
                dataView.refresh();
                highlightCells();
                jQuery(this).trigger('dialogresizestop')
            });
        }

        props.onBeforeEditCell.subscribe(function(row,cell) {
            if (props.getDataItem(cell.row).connected.length > 0) {
                return false;
            }
            
            else if (props.getDataItem(cell.row).ttype == 'slot') {
                return false;
            }
    
            else {
                return true;
            }
        });
    
        props.onClick.subscribe(function (e) {
            var cell = props.getCellFromEvent(e);
            if (cell.cell==0) {
                var item = dataView.getItem(cell.row);
                if (item.hasOwnProperty("vt")) {
                    if (!_collapsed[item.id]) {
                        _collapsed[item.id] = true;
                    } else {
                        _collapsed[item.id] = false;
                    }
                    // dataView needs to know to update.
                    dataView.updateItem(item.id, item);
                    highlightCells();
                    jQuery("#" + name + "_variableFilter").trigger('dialogresizestop')
                }
                e.stopImmediatePropagation();
            }
        });
    
        props.onCellChange.subscribe(function (e, args) {
            dataView.updateItem(args.item.id, args.item);
        });
      
        // wire up model events to drive the grid
        dataView.onRowCountChanged.subscribe(function (e, args) {
            props.resizeCanvas();
        });
        
        dataView.onRowsChanged.subscribe(function (e, args) {
            props.invalidateRows(args.rows);
            props.resizeCanvas();
        });

        if (editable) {
            props.onCellChange.subscribe(function(e,args) {
                // TODO: better way to do this (e.g. model.setProperty(path,name,value)
                model.setVariableValue(self.pathname + '.' + args.item.name, 
                                       args.item.value, args.item.type );
                // Need to clear mouse selection so that slickgrid doesn't launch
                // the editor for the next variable down (a la Excel)
                e.stopImmediatePropagation();
            });
        }
    }

        
    function VarTableFormatter(row,cell,value,columnDef,dataContext) {
        var spacer = "<span style='display:inline-block;height:1px;width:" + (15 * dataContext["indent"]) + "px'></span>";
        var idx = dataView.getIdxById(dataContext.id);
        var nextline = dataView.getItemByIdx(idx+1)
        if (nextline && nextline.indent > dataContext.indent) {
            if (_collapsed[dataContext.id]) {
                return spacer + " <span class='toggle expand'></span>&nbsp;" + value;
            } else {
                return spacer + " <span class='toggle collapse'></span>&nbsp;" + value;
            }
        } else {
            return spacer + "<span class='toggle'></span>" + value;
        }
    }
    
    function matchesFilter(item){
        return (item.name.indexOf(searchString) !== -1) ||
            (item.units.indexOf(searchString) !==-1 )||
            (item.description.indexOf(searchString)) !==-1
    }

    function expansionFilter(item, args){
        var idx, parent;
        if (item.parent != null) {
            idx = dataView.getIdxById(item.parent);
            parent = dataView.getItemByIdx(idx)
            while (parent) {
                if (_collapsed[parent.id]) {
                    return false;
                }
                idx = dataView.getIdxById(parent.parent);
                parent = dataView.getItemByIdx(idx);
            }
        }

        return true;

    }

    function textboxFilter(item, args){
        return _filter[item.id];
    }

    /* Function that returns false for collapsed rows, and true for the rest.
    Used by Slickgrid */
    this.filter = function myFilter(item, args) {
        return expansionFilter(item, args) && textboxFilter(item, args);
        //return true;
    }
    
    function updateFilter(args){
        dataView.setFilterArgs(args);
        dataView.refresh();
    }
    
    /* Sets the CSS style for cells based on connection status, while
    taking collapse/expand state into account. */
    function highlightCells() {
        var editableCells = {},
            idx = 0,
            properties = dataView.getItems();
        
        jQuery.each(properties, function(index, value) {
            if (self.filter(value)) {
                editableCells[idx] = editableInTable[value.id];
                idx += 1;
            }
        })
        props.setCellCssStyles("highlight", editableCells);
    }

    propsDiv.bind('resizeCanvas', function() {
        props.resizeCanvas();
    });

    /** load the table with the given properties */
    this.loadData = function(properties) {
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
            
                editableInTable[value.id] = {};
                _filter[value.id] = true;
                if (value.hasOwnProperty("parent")) {
                    if ( !_collapsed.hasOwnProperty(value.id) ) {
                        _collapsed[value.id] = true; 
                        _collapsed[value.parent] = true;
                    }
                }
                
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
                        editableInTable[value.id] = css;
                    }
                }
                
            });

            dataView.beginUpdate();
            dataView.setItems(properties);
            dataView.setFilterArgs({filter:expansionFilter});
            dataView.setFilter(this.filter);
            dataView.endUpdate();
            props.invalidate()

        }
        else {
            props.setData([]);
            alert('Error getting properties for '+self.pathname+' ('+name+')');
            debug.info(self.pathname,properties);
        }
        highlightCells();
        props.resizeCanvas();
    };
};
