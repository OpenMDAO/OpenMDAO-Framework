// Custom openMDAO slickgrid cell editors
(function($) {

    var customEditors = {
    
    //------------------------------------
    // Custom Editor for Dictionaries
    //------------------------------------
    DictEditor : function(args) {
        var this_editor = this;
        var input = [];
        var keys = [];
        var length = 0;
        var buttons = [];
        var loaded = false;
        var val_types = args.item.value_type;
        var key_types = args.item.key_type;
        var $add_button = $("<button>+</button>").button();
        var var_name = args.item['name'];
        if (typeof args.item.value == "string") {
            var values = JSON.parse(args.item.value);}
        else {var values = args.item.value;}
        var defaults = values;
        var grid = args.grid;
        var $container = $("<div id = '"+var_name+"-editor'/>");
        var $editor_dialog = $("<div id = '"+var_name+"-dialog'/>").dialog({
            width: "auto",
            modal: "true",
            title: "Editing dictionary: '"+var_name+"'",
                buttons: [{ 
                    text:"Submit changes",
                    id: "dict-edit-"+var_name+"-submit",
                    click: function() {
                        grid.getEditorLock().commitCurrentEdit();
                        $( this ).dialog( "close" );
                    }}, {
                    text:"Cancel",
                    id: "dict-edit-"+var_name+"-cancel",
                    click: function() {
                        grid.getEditorLock().cancelCurrentEdit();
                        $( this ).dialog( "destroy" );
                    }
                }]
                    });
            $editor_dialog.live('keyup', function(e){
                if (e.keyCode == 13) {
                    $(':button:contains("Submit changes")').click();
                }
            });

            $container.appendTo($editor_dialog);
            $("<br>").appendTo($editor_dialog);
            var $new_key = $("<INPUT type=text class='add-editor-text' size = 10/>");
            var $new_input = $("<INPUT type=text class='add-editor-editor-text' size = 10/>");


            $add_button.click( function (e,d) {
                this_editor.addKey($new_key.val());
                keys[length - 1].val($new_key.val());
                input[length - 1].val($new_input.val());
            });	

            this.init = function() {
                $("<text>Keys : Values<br></text>").prependTo($editor_dialog);
                for (var key in values) {
                    this.addKey(key);}

                $("<text>New element (Key, Value):</text><br>").appendTo($editor_dialog);
                $new_key.appendTo($editor_dialog);
                $new_input.appendTo($editor_dialog);
                $add_button.appendTo($editor_dialog);
            };


            this.addKey = function(key) {

            if (key != "py/object") {
                keys.push($("<INPUT type=text disabled = 'disabled' class='editor-text' size = 10/>").appendTo($container));
                input.push($("<INPUT type=text class='editor-text' size = 10/>").appendTo($container));
                $remove_button = $("<button class = 'remove-dict-edit' id = '"+key+"'>-</button><br>").button()
                buttons.push($remove_button.appendTo($container));

                $remove_button.click( function (e) {
                    del_key = e.currentTarget.id;
                    for (var i = 0; i< length; i++) {
                        if (del_key == keys[i].val()) {
                            input[i].remove();
                            keys[i].remove();
                            buttons[i].remove();

                            input.splice(i,1);
                            keys.splice(i,1);
                            buttons.splice(i,1);

                            break;
                        }
                    }
                    length = length -1;
                });			
                length++;
            }
        }

        this.destroy = function() {
            for (var i = 0; i< length; i++) {
                input[i].remove();
                keys[i].remove();
                buttons[i].remove();
            }
            $editor_dialog.dialog('destroy');
        };

        this.focus = function() {
            input.focus();
        };


        this.loadValue = function(item) {
            if (!loaded) {
                var i = 0;
                for (var key in item.value) {
                    if (key != "py/object") {
                        keys[i].val(key);
                        input[i].val(values[key]);
                        i++;
                    }
                } 
                loaded = true;
            }

        };

        this.serializeValue = function() {
            new_d = {};
            for (var i = 0; i < length; i++) {
                thisval = input[i].val();
                keyval = keys[i].val();
                if (key_types == "Float" || key_types == "Int") {keyval = parseFloat(keyval);}
                if (val_types == "Float" || val_types == "Int") {thisval = parseFloat(thisval);}
                new_d[keyval] = thisval;
            }
            return JSON.stringify(new_d);
        };
    
        this.applyValue = function(item,state) {
            item[args.column.field] = state;
        };

        this.isValueChanged = function() {
            return true;
        };
    
        this.validate = function() {
            return {
                valid: true,
                msg: null
            };
        };

        this.init();
    },

    //------------------------------------
    // Custom Editor for Enums
    //------------------------------------
    EnumEditor : function(args) {
        var $select;
        var grid = args.grid;
        var defaultValue;
        vals = args.item.values['py/tuple'];
        var var_name = args.item['name'];
        if (vals) {
            var values = vals;
        }
        else {var values = args.item.values;}
        var this_item = args.item;
        var value_types = args.item.value_types;
        var select_id = "editor-enum-"+var_name;
        this.init = function() {
            $select = $("<SELECT tabIndex='0' id='editor-enum-"+var_name+"'/>");
            for (var i = 0; i < values.length; i++) {$("<OPTION value='"+values[i]+"'>"+values[i]+"</OPTION>").appendTo($select);} 
            $select.appendTo(args.container);
            $select.focus();
            $select.change(function (e) {grid.getEditorLock().commitCurrentEdit();})
            
        };

        this.destroy = function() {
            $select.remove();
        };
    
        this.focus = function() {
            $select.focus();
        };
    
        this.loadValue = function(item) {
            defaultValue = item[args.column.field];
            $select.val(defaultValue);
            $select.select();
        };
    
        this.serializeValue = function() {
            return $select.val()
        };
    
        this.applyValue = function(item,state) {
            if (jQuery("#editor-enum-mode").length > 0) {
                current = jQuery("#editor-enum-mode")[0].selectedIndex;
                }
            else {current = 0;}
            current_type = value_types[current];
            if (current_type == "str") {
                item[args.column.field] = "'" + String(state) + "'";
            }
            else {
                item[args.column.field] = state;
            }
        };
    
        this.isValueChanged = function() {
            return ($select.val() != defaultValue);
        };
    
        this.validate = function() {
            return {
                valid: true,
                msg: null
            };
        };
    
        this.init();
        //jQuery("#"+select_id).change(function () {state = this.serializeValue(); this.applyValue(this_item, state);})
    },

    //------------------------------------
    // Custom Editor for Booleans
    //------------------------------------
    BoolEditor : function(args) {
        var $select;
        var var_name = args.item['name'];
        var defaultValue;
        var grid = args.grid;

        this.init = function() {
            $select = $("<SELECT tabIndex='0' class='editor-yesno' id = 'bool-editor-"+var_name+"'><OPTION value='True'>True</OPTION><OPTION value='False'>False</OPTION></SELECT>");
            $select.appendTo(args.container);
            $select.focus();
            $select.change(function (e) {grid.getEditorLock().commitCurrentEdit();})
        };
    
        this.destroy = function() {
            $select.remove();
        };
    
        this.focus = function() {
            $select.focus();
        };
    
        this.loadValue = function(item) {
            defaultValue = item[args.column.field];
            $select.val(defaultValue);
            $select.select();
        };
    
        this.serializeValue = function() {
            if ($select.val() == "True") {
            return "True";
            }
            else {return "False";}
        };
    
        this.applyValue = function(item,state) {
            item[args.column.field] = state;
        };
    
        this.isValueChanged = function() {
            return ($select.val() != defaultValue);
        };
    
        this.validate = function() {
            return {
                valid: true,
                msg: null
            };
        };
    
        this.init();
    },

    //------------------------------------
    // Custom Editor for Arrays
    //------------------------------------
    ArrayEditor : function(args) {
        var var_name = args.item['name'];
        var grid = args.grid;
        var input = [];	    
        var $container = $("<div id = '"+var_name+"-editor' style='padding: 0px 0px 2px;'/>");
        var win = "<div id = 'array-editor-dialog-"+var_name+"'/>";
        var lbracketSVG = '<svg height="15" width="8">'
                        + '<text x="4" y="13" font-size="10" style="fill:gray">[</text>'
                        + '</svg>',
            rbracketSVG = '<svg height="15" width="8">'
                        + '<text x="4" y="13" font-size="10" style="fill:gray">]</text>'
                        + '</svg>',
            spacerSVG   = '<svg height="15" width="8"></svg>'
                        
        var $editor_dialog = $(win).dialog({
            width: "auto",
            modal: "true",
            title: "Editing array: '"+var_name+"'",
            buttons: [{
                text:"Submit changes",
                id: "array-edit-"+var_name+"-submit",
                click: function() {
                    grid.getEditorLock().commitCurrentEdit();
                    $( this ).dialog( "close" );
                    win.remove();
            }},{
                text:"Cancel",
                id: "array-edit-"+var_name+"-cancel",
                click: function() {
                    grid.getEditorLock().cancelCurrentEdit();
                    $( this ).dialog( "close" );
                    win.remove();
                }
            }]
        });
        
        $editor_dialog.live('keyup', function(e){
            if (e.keyCode == 13) {
                $(':button:contains("Submit changes")').click();
            }
        });

        $container.appendTo($editor_dialog);
        var length;
        var default_length;
        var dim = args.item['dim'].split(',');
        for (var i=0; i<dim.length; i++) {
            dim[i] = parseFloat(dim[i]);
        }
        var fixed = args.item['fixed'];
        
        // Button to extend array        
        var $add_button = $("<button id = 'array-edit-add-"+var_name+"'>+</button>").button();
        $add_button.click( function () {
            input.push($("<INPUT type=text class='editor-text' value = '0.' size = 6/>"));
            length = input.length;
            $container.empty();
                for (var i = 0; i< length; i++) {
                    jQuery(input[i]).appendTo($container);
            }
            dim[0] += 1;
        });
        
        // Button to contract array
        var $subtract_button = $("<button id = 'array-edit-add-"+var_name+"'>-</button>").button();
        $subtract_button.click( function () {
            if (length > 1) {
                input = input.slice(0,length-1);
                length = input.length;
                $container.empty();
                for (var i = 0; i< length; i++) {
                    jQuery(input[i]).appendTo($container);
                }
            }
        });

        this.init = function() {
            parsed = this.splitData(args.item['value']);
            length = parsed.length;
        
            ndim = dim.length;
            jQuery(lbracketSVG).appendTo($container);
            if (ndim == 2) {
                jQuery(lbracketSVG).appendTo($container);
            }
            
            var partially_connected = args.item["partially_connected_indices"] !== undefined ? this.splitData(args.item['partially_connected_indices']) : [];
            var implicit_partial_indices = args.item["implicit_partial_indices"] !== undefined ? this.splitData(args.item['implicit_partial_indices']) : [];

            default_length = length;
            for (var i = 0; i< length; i++) {
                var array_value = jQuery("<INPUT type=text class='editor-text' size = 6/>")
                array_value.appendTo($container);
                if(jQuery.inArray("" + i, partially_connected) > -1){
                    array_value.prop("disabled", true);
                }

                else if(jQuery.inArray("" + i, implicit_partial_indices) > -1){
                    array_value.prop("disabled", true);
                }

                input.push(array_value);
                
                // New row gets a new line.
                if (ndim>1 && dim[0] > 1 && (i+1)%dim[1] == 0) { 
                    if (i<length-1) {
                        jQuery(rbracketSVG).appendTo($container);
                        jQuery('<br />').appendTo($container);
                        jQuery(spacerSVG).appendTo($container);
                        jQuery(lbracketSVG).appendTo($container);
                    }
                }
            }

            jQuery(rbracketSVG).appendTo($container);
            if (ndim == 2) {
                jQuery(rbracketSVG).appendTo($container);
            }
            jQuery('<br />').appendTo($container);

            // For now, you can only extend/contract a 1D array.
            if (!fixed && ndim == 1) {
                $add_button.appendTo($editor_dialog);
                $subtract_button.appendTo($editor_dialog);
            }
        };

        this.splitData = function (input_data) {
            // Split out all numerical values from the array.
            var step1 = input_data.replace(/\[/g, '').replace(/\]/g, '');
            return step1.trim().split(/[ ,]+/); 
        }

        this.destroy = function() {
            for (var i = 0; i< length; i++) {
                input[i].remove();
            }
            $editor_dialog.dialog('destroy');
            $add_button.remove();
            $subtract_button.remove();
        };
    
        this.focus = function() {
            input.focus();
        };
    
        this.getValue = function() {
            return input.val();
        };

        this.setValue = function(val) {
            values = this.splitData(args.item['value']);
            for (var i = 0; i< length; i++) {
                input[i].val(values[i]);
            }
        };
    
        this.loadValue = function(item) {
            defaultValue = item[args.column.field] || "";
            values = this.splitData(defaultValue);
            for (var i = 0; i< length; i++) {
                input[i].val(values[i]);
            }
            input[0].select();
        };

        this.serializeValue = function() {
            // Turns the values in the boxes into a valid python array.
            
            if (input.length == 0) {
                return '[]';
            }                
            
            state = [];
            for (var i = 0; i< length; i++) {
                state.push(input[i].val());
            }
            
            state[0] = "["+state[0];
            state[length-1] = state[length-1] + "]";
            
            ndim = dim.length;

            new_state = [];
            row = [];
            for (var i = 0; i < state.length; i++) {
                row.push(state[i]);
                if (ndim>1 && (i+1)%dim[1] == 0) { 
                    row[0] = "["+row[0];
                    row[row.length-1] = row[row.length-1] + "]";
                    row = row.join(", ");
                    new_state.push(row);
                    row = [];
                }
            }
            if (ndim==1) {
                row = row.join(", ");
                new_state.push(row);
            }
            new_state = new_state.join(", ");
            return new_state;
        }

        this.applyValue = function(item,state) {
            item[args.column.field] = state;
        };
    
        this.isValueChanged = function() {
            values =   this.splitData(defaultValue);
            for (var i = 0; i< length; i++) {
                if (input[i].val() != values[i] || length != default_length) {return true;}
            }
            return false;
        };

        this.validate = function() {
            if (args.column.validator) {
                var validationResults = args.column.validator(input.val());
                if (!validationResults.valid)
                    return validationResults;
            }
            return {
                valid: true,
                msg: null
            };
        };

        this.init();
    }
};

$.extend(window, customEditors);
})(jQuery);
