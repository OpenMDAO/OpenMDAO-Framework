
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PropertiesPane = function(elm, project, pathname, name, editable, meta) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var self = this,
        props,
        dataView,
        searchString = "",
        current_item = {},
        inlineFilter,
        propsDiv = jQuery("<div id='"+name+"_props' class='slickgrid' style='overflow:none;'>"),
        columns = [
            { id:"name",  name:"Name",  field:"name",  width:80, formatter:VarTableFormatter },
            { id:"value", name:"Value", field:"value", width:80, editor:openmdao.ValueEditor, formatter:VarValueFormatter }
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

        elm.append(jQuery("<div id='inlineFilter' style='float:right;padding:10px;'>Filter <input type='text' id='"+name+"_variableFilter' style='width:100px;'><button id='"+name+"_clear'>X</button></div>"));
        propsDiv=jQuery("<div id='"+name+"_props' class='slickgrid' style='overflow:none; height:360px; width:620px;'>");
        columns = [
            { id:"info",  name:"",            field:"info",   width:30,  formatter:InfoFormatter },
            { id:"name",  name:"Name",        field:"name",   width:100, formatter:VarTableFormatter, sortable:true },
            { id:"type",  name:"Type",        field:"type",   width:30 },
            { id:"value", name:"Value",       field:"value",  width:100, editor:openmdao.ValueEditor, formatter:VarValueFormatter },
            { id:"hi",    name:"High",        field:"high",   width:30 },
            { id:"low",   name:"Low",         field:"low",    width:30 },
            { id:"units", name:"Units",       field:"units",  width:60 },
            { id:"desc",  name:"Description", field:"desc",   width:300 }
        ];

        var compName = pathname.replace(".", "-", "g");

        if (!(compName in openmdao.preferences.PropertiesPane)) {
            openmdao.preferences.PropertiesPane[compName] = {};
        }

        if (!(name.toLowerCase() in openmdao.preferences.PropertiesPane[compName])) {
            openmdao.preferences.PropertiesPane[compName][name.toLowerCase()] = {};
        }

        if (!("columns" in openmdao.preferences.PropertiesPane[compName][name.toLowerCase()])) {
            openmdao.preferences.PropertiesPane[compName][name.toLowerCase()].columns = {
                info  : true,
                name  : true,
                type  : false,
                value : true,
                hi    : false,
                low   : false,
                units : true,
                desc  : true
            };
        }
    }

    elm.append(propsDiv);
    SetupTable();

    //function to convert array to object
    function oc(a) {
        var o = {};
        for (var i=0; i<a.length; i++) {
            o[a[i]] = '';
        }
        return o;
    }

    function getExcludes() {
        return [
            "info", "name", "value", "units", "desc",
            "indent", "implicit_partial_indices", "partially_connected_indices",
            "id", "vt", "parent", "high", "low"
        ];
    }

    function excludeField(field, excludes) {
        return (field in oc(excludes));
    }

    var valueToString = function(key, value) {

       function numberToString(number) {
           if (typeof(number) === "number") {
               if (number > 1.0e+21) {
                    return number.toExponential(5);
               }
               return number.toFixed(5);
           }
           return number;
       }

       function highLowToString(highLow) {
           var high = highLow.high,
               low = highLow.low;
           return numberToString(high) + " / " + numberToString(low);
       }

       function connectedToString(connections) {
           var connected = connections.connected !== undefined ? connections.connected : connections;
           var partially_connected = connections.hasOwnProperty("partially_connected") ? connections.partially_connected : "";

           return connected + partially_connected;
       }

       var formatters = {
            "high" : numberToString,
            "low" : numberToString,
            "high-low" : highLowToString,
            "connected" : connectedToString
       };

       function hasFormatter(key) {
           return key in formatters;
       }

       function format(key, value) {
           var formatter = hasFormatter(key) ? formatters[key] : function(value) {
                return value;
           };

           return formatter(value);
       }

       return format(key, value);
    };

    var fieldNameToString = function(fieldName) {

       function highLowToString() {
           return "High/Low";
       }

       var formatters = {
           "high-low" : highLowToString
       };

       function hasFormatter(key) {
           return key in formatters;
       }

       function format(key) {
           var formatter = hasFormatter(key) ? formatters[key] : function() {
                return key;
           };

           return formatter();
       }

       return format(fieldName);
    };

    var weightedSort = function(a, b) {

        var weights = {
            "type"      : 4,
            "high-low"  : 3,
            "valid"     : 2,
            "connected" : 1,
            "implicit"  : 0
        };

        var getWeight = function(str) {
            return (str in weights) ? weights[str] : -1;
        };

        return getWeight(b) - getWeight(a);
    };

    var ItemFormatter = function() {

        this.cloneItem = function(item) {
            var newItem = {};
            for(var field in item) {
                newItem[field] = item[field];
            }

            return newItem;
        };

        this.groupFields = function(fields, groupName, item) {
            var field,
                newItem = this.cloneItem(item),
                group = {};

            for (i=0; i<fields.length; i++) {
                field = fields[i];
                if (!(field in item)) {
                    return newItem;
                }

                group[field] = item[field];
            }

            newItem[groupName] = group;

            return newItem;
        };

        this.orderFields = function(item, comparator) {
            var orderedFields = [];
            for (var field in item) {
                orderedFields.push(field);
            }
            orderedFields.sort(comparator);
            return orderedFields;
        };

        this.removeFields = function(item, excludes) {
           var newItem = this.cloneItem(item);
           for (var field in newItem) {
                if (excludeField(field, excludes)) {
                    delete newItem[field];
                }
           }

           return newItem;
        };
    };

    function getToolTip(item) {
        var str = "",
            fields,
            field;

        var formattedField = "",
            formattedValue = "";

        var itemFormatter,
            newItem;

        itemFormatter = new ItemFormatter();
        newItem = itemFormatter.cloneItem(item);

        newItem = itemFormatter.groupFields(["high", "low"], "high-low", item);

        newItem = itemFormatter.removeFields(newItem, getExcludes());

        fields = itemFormatter.orderFields(newItem, weightedSort);

        for (var i=0; i<fields.length; i++) {
            field = fields[i];

            formattedField = fieldNameToString(field);
            formattedValue = valueToString(field, newItem[field]);

            str = str + "<p>" + formattedField + " : " + formattedValue + "</p>";
        }

        if (str !== "") {
            return "<div id='tooltip'>" + str + "</div>";
        }

        return "";
    }

    function SetupTable() {
        dataView = new Slick.Data.DataView({ inlineFilters: false });
        props = new Slick.Grid(propsDiv, dataView, columns, options);
        if (meta) {
            var columnpicker = new Slick.Controls.ColumnPicker(columns, props, options);

            var visibility = openmdao.preferences.PropertiesPane[compName][name.toLowerCase()].columns;
            var visibleColumns = [];

            for (var i=0; i<columns.length; i++) {
                if (visibility[columns[i].id]) {
                   visibleColumns.push(columns[i]);
                }
            }

            props.setColumns(visibleColumns);

            // Sorting for the first column
            props.onSort.subscribe(function(e, args) {
                if (args.sortAsc) {
                    dataView.sort(sortAscending, true);
                } else {
                    dataView.sort(sortDescending, true);
                }
                highlightCells();
                props.invalidate();
                props.render();
            });

            props.onBeforeDestroy.subscribe(function(e, args) {
                var visibility = openmdao.preferences.PropertiesPane[compName][name.toLowerCase()].columns;
                var visibleColumns = args.grid.getColumns();
                var visibleColumnIds = {};

                for (var i=0; i<visibleColumns.length; i++) {
                    visibleColumnIds[visibleColumns[i].id] = undefined;
                }

                for (var columnId in visibility) {
                    if (columnId in visibleColumnIds) {
                        visibility[columnId] = true;
                    }
                    else {
                        visibility[columnId] = false;
                    }
                }
                columnpicker.destroy();
            });

            var variableFilter = elm.find('#'+name+'_variableFilter');
            variableFilter.keyup(function(e) {
                Slick.GlobalEditorLock.cancelCurrentEdit();
                self.refreshFilter(variableFilter.val());
            });

            var clrButton = elm.find('#'+name+'_clear');
            clrButton.click(function() {
                variableFilter.val('');
                self.refreshFilter('');
            });

            //TODO: On hover of variable icon should bring up tool tip with information for that row
        }

        props.onMouseEnter.subscribe(function(e, args) {
            var cell = args.grid.getCellFromEvent(e);
            if (cell.cell === 0) {
                elm.find(".variableInfo").tooltip({
                    content : function() {
                        var item = dataView.getItem(cell.row);
                        return getToolTip(item);
                    },
                    items : ".variableInfo",
                    hide : false,
                    show : false,
                    position : {
                        of : "#ObjectFrame_" + self.pathname.replace(/\./g,'-') + "_" + name,
                        my : "right top",
                        at : "left-20 top"
                    }
                });
            }

        });

        props.onBeforeEditCell.subscribe(function(row, cell) {
            var item = props.getDataItem(cell.row);
            if ((item.connection_types & 1) === 1) {
                return false;
            }
            else if (item.ttype == 'vartree') {
                return false;
            }
            else {
                return true;
            }
        });

        props.onClick.subscribe(function(e) {
            var cell = props.getCellFromEvent(e);
            var name_col_index = (meta) ? 1 : 0;
            if (cell.cell === name_col_index) {
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
                }
                e.stopImmediatePropagation();
            }

            if (dataView.getItem(cell.row).value === "Geometry") {
                var p = self.pathname + '.' + dataView.getItem(cell.row).id;
                openmdao.project.viewGeometry(p);
            }
        });

        props.onCellChange.subscribe(function(e, args) {
            dataView.updateItem(args.item.id, args.item);
        });

        // wire up project events to drive the grid
        dataView.onRowCountChanged.subscribe(function(e, args) {
            props.updateRowCount();
            props.render();
        });

        dataView.onRowsChanged.subscribe(function(e, args) {
            props.invalidateRows(args.rows);
            props.render();
        });

        if (editable) {
            props.onCellChange.subscribe(function(e, args) {
                // Need to clear mouse selection so that slickgrid doesn't launch
                // the editor for the next variable down (a la Excel)
                e.stopImmediatePropagation();

                var subpath = args.item.id;
                if (subpath.charAt(0) === '~') {
                    // Drop prefix seen on framework vars.
                    subpath = subpath.substr(1);
                }
                project.setVariableValue(self.pathname + '.' + subpath,
                                         args.item.value, args.item.type);
            });
        }
    }

    function InfoFormatter(row, cell, value, columnDef, dataContext) {
        return "<span class='ui-icon ui-icon-info variableInfo' title='' style='display:inline-block;'></span>";
    }

    function VarTableFormatter(row, cell, value, columnDef, dataContext) {
        var spacer ="<span style='display:inline-block;height:1px;width:" + (15 * dataContext.indent) + "px;'></span>";
        var idx = dataView.getIdxById(dataContext.id);
        var nextline = dataView.getItemByIdx(idx+1);
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

    function VarValueFormatter(row, cell, value, columnDef, dataContext) {
        if (dataContext.value === "Geometry") {
            return '<button class="ui-button ui-widget ui-state-default ui-corner-all ui-button-text-only" ' +
                   'role="button" aria-disabled="false">View Geom</button>' ;
        }
        return value ;
    }

    function getParentId(item){
        parentId = item.parent;
        if(item.id.indexOf('~') === 0){
            parentId = '~' + parentId;
        }

        return parentId;
    }

    function expansionFilter(item, args) {
        var idx, parent;
        if ((item.parent !== undefined) && (item.parent !== null)) {
            idx = dataView.getIdxById(getParentId(item));
            parent = dataView.getItemByIdx(idx);
            while (parent) {
                if (_collapsed[parent.id]) {
                    return false;
                }
                idx = dataView.getIdxById(getParentId(parent));
                parent = dataView.getItemByIdx(idx);
            }
        }

        return true;
    }

    function textboxFilter(item, args) {
        return _filter[item.id];
    }

    /* Function that returns false for collapsed rows, and true for the rest.
    Used by Slickgrid */
    this.filter = function myFilter(item, args) {
        return expansionFilter(item, args) && textboxFilter(item, args);
        //return true;
    };

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
        });
        props.setCellCssStyles("highlight", editableCells);
    }

    function sortAscending(rowa, rowb) {
        return sortView(rowa, rowb, 1);
    }
    function sortDescending(rowa, rowb) {
        return sortView(rowa, rowb, -1);
    }
    function sortView(rowa, rowb, asc) {
        var a = rowa.id,
            b = rowb.id,
            na = a.split('.'),
            nb = b.split('.'),
            min_dots = Math.min(na.length, nb.length);

        for (var idx=0; idx<min_dots; idx++) {
            var cmp = openmdao.Util.alphanumeric_compare(na[idx], nb[idx]);
            if (cmp > 0) {
                return asc;
            } else if (cmp < 0) {
                return -asc;
            }
        }
        return a.length - b.length;
    }

    propsDiv.on('resizeCanvas', function(e) {
        props.resizeCanvas();
    });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.refreshFilter = function(searchString) {
        searchString = searchString.toLowerCase();
        var items = dataView.getItems();
        _filter = {};
        for (var i=items.length - 1; i>=0; i--) {
            var name = (items[i].name) ? items[i].name.toLowerCase() : "";
            var units = (items[i].units) ? items[i].units.toLowerCase() : "";
            var description = (items[i].desc) ? items[i].desc.toLowerCase() : "";

            if (searchString === "") {
                _filter[items[i].id] = true;
            }

            else if (_filter[items[i].id] === true) {
                if (items[i].parent !== null) {
                    _filter[items[i].parent] = true;
                    _collapsed[items[i].parent] = false;
                }
            }

            else if (name.indexOf(searchString) !== -1 ||
                     units.indexOf(searchString) !== -1 ||
                     description.indexOf(searchString) !== -1) {
                _filter[items[i].id] = true;
                _collapsed[items[i].id] = false;
                if (items[i].parent !== null) {
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
        jQuery(this).trigger('dialogresizestop');
    };

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            jQuery.each(properties, function(index, value) {

                editableInTable[value.id] = {};
                _filter[value.id] = true;
                if (value.hasOwnProperty("parent")) {
                    if (!_collapsed.hasOwnProperty(value.id)) {
                        _collapsed[value.id] = true;
                        _collapsed[getParentId(value)] = true;
                    }
                }

                // set styles for connected, framework and editable values
                var nameStyle = '',
                    valueStyle = '';

                if (value.hasOwnProperty("connected")) {
                    if (options.editable && ((value.connection_types & 1) !== 1)
                        && (value.ttype != 'vartree')) {
                        valueStyle += " cell-editable";
                    }
                    if (value.hasOwnProperty("implicit") &&
                        (((value.connection_types & 4) === 4) ||
                         ((value.connection_types & 8) === 8))) {
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
                }
                else if (options.editable) {
                    valueStyle += " cell-editable";
                }

                if (value.hasOwnProperty("framework_var")) {
                    nameStyle += " framework_var";
                }

                var css = {};
                if (nameStyle !== '') {
                    css.name = nameStyle;
                }
                if (valueStyle !== '') {
                    css.value = valueStyle;
                }
                if (css !== {}) {
                    editableInTable[value.id] = css;
                }

                value.info = "";
            });

            dataView.beginUpdate();
            dataView.setItems(properties);
            dataView.setFilter(this.filter);
            dataView.endUpdate();
            dataView.sort(sortAscending, true);
            props.invalidate();

            var searchString = elm.find('#'+name+'_variableFilter').val();
            if (searchString) {
                self.refreshFilter(searchString);
            }
        }
        else {
            props.setData([]);
            alert('Error getting properties for '+self.pathname+' ('+name+')');
        }
        highlightCells();
        props.resizeCanvas();
    };

    this.destructor = function() {
        props.destroy();
    };
};
