// Custom openMDAO slickgrid cell editors 
(function($) {

    var customEditors = {

        DictEditor : function(args) {
	    var this_editor = this;
	    var input = [];
	    var keys = [];
	    var length = 0;
	    var buttons = [];
	    var loaded = false;
	    var val_types = args.item.value_type;
	    var key_types = args.item.key_type;
	    console.log(args.item);
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
				    //val_types.splice(i,1);
				    //key_types.splice(i,1);
				    
				    //delete values[del_key];
				    
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
			//val_types.push(typeof values[key]);
			//key_types.push(typeof key);
			input[i].val(values[key]);
			i++;
			}
                    } loaded = true;}
		    
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
		console.log(state);
                item[args.column.field] = state;
            };
           
            this.isValueChanged = function() {
                //return ($select.val() != defaultValue);
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

        EnumEditor : function(args) {
            var $select;
            var defaultValue;
	    vals = args.item.values['py/tuple'];
	    var var_name = args.item['name'];
	    if (vals) {
		var values = vals;
	    }
	    else {var values = args.item.values;}
            var scope = this;

            this.init = function() {
                $select = $("<SELECT tabIndex='0' id='editor-enum-"+var_name+"'/>");
		for (var i = 0; i < values.length; i++) {$("<OPTION value='"+values[i]+"'>"+values[i]+"</OPTION>").appendTo($select);} 
                $select.appendTo(args.container);
                $select.focus();
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

        BoolEditor : function(args) {
            var $select;
	    var var_name = args.item['name'];
            var defaultValue;
            var scope = this;

            this.init = function() {
                $select = $("<SELECT tabIndex='0' class='editor-yesno' id = 'bool-editor-"+var_name+"'><OPTION value='True'>True</OPTION><OPTION value='False'>False</OPTION></SELECT>");
                $select.appendTo(args.container);
                $select.focus();
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



        ArrayEditor : function(args) {
	    var var_name = args.item['name'];
	    var grid = args.grid;
	    var var_item = args.item;
	    var var_editor = this;
	    var input = [];	    
	    var $container = $("<div />");
            var $editor_dialog = $("<div id = 'array-editor-dialog-"+var_name+"'/>").dialog({
				width: "auto",
				title: "Editing array: '"+var_name+"'",
            			buttons: [{ text:"Submit changes",
					    id: "array-edit-"+var_name+"-submit",
					    click: function() {
						    grid.getEditorLock().commitCurrentEdit();
						    $( this ).dialog( "close" );
					    }},{
					    text:"Cancel",
					    id: "array-edit-"+var_name+"-cancel",
					    click: function() {
						    grid.getEditorLock().cancelCurrentEdit();
						    $( this ).dialog( "close" );
					    }}
					]
						});
		$editor_dialog.live('keyup', function(e){
		  if (e.keyCode == 13) {
		    $(':button:contains("Submit changes")').click();
		  }
		});

	    $container.appendTo($editor_dialog);
            var scope = this;
            var length;
	    var dim;
	    var default_length;
	    var $add_button = $("<button id = 'array-edit-add-"+var_name+"'>+</button>").button();
	    var $subtract_button = $("<button id = 'array-edit-add-"+var_name+"'>-</button>").button();
	    $add_button.click( function () {
			input.push($("<INPUT type=text class='editor-text' value = '0.' size = 6/>"));
			length = input.length;
			$container.empty();
			 for (var i = 0; i< length; i++) {
			  jQuery(input[i]).appendTo($container);
			}
					});
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
		
		dim = [this.getDim(args.item['value'])];
		dim.push(length / dim[0]);
		
		default_length = length;
                for (var i = 0; i< length; i++) {
                    input.push($("<INPUT type=text class='editor-text' size = 6/>").appendTo($container));
                    if (i > 0 && dim[0] > 1 && (i+1)%dim[1] == 0) { $('<br>').appendTo($container);}
                    }
		    
		    if (dim[0] == 1) {
		   $add_button.appendTo($editor_dialog);
		   $subtract_button.appendTo($editor_dialog);
		   }

            };
	    
	    this.getDim = function(input_data) {
		step1 = input_data.split("[").length;
		if (step1 > 2) {return step1 - 2;}
		else {return 1;}
		}
	    
	    this.splitData = function (input_data) {
		var substr = '';
		var parsed = [];
		step1 = input_data.split('[').join('').split(']').join('').split('  ').join(' ').split(',').join(' ');
		for (var i = 0; i < step1.length; i++) {
		    if (step1[i] == " ") {
			    if (substr.length > 0) {
				parsed.push(substr);
				substr = '';
			    }
			}
		    else {
			substr = substr + step1[i];
			}
		}
		if (substr.length > 0) {parsed.push(substr);}
		return parsed
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
                values =   this.splitData(args.item['value']);
                for (var i = 0; i< length; i++) {
                    input[i].val(values[i]);
                    
                    }
            };

            this.loadValue = function(item) {
                defaultValue = item[args.column.field] || "";
                values =   this.splitData(defaultValue);
                for (var i = 0; i< length; i++) {
                    input[i].val(values[i]);
                    
                    }
                input[0].select();
            };


            this.serializeValue = function() {
                state = [];
                for (var i = 0; i< length; i++) {
                    state.push(input[i].val());
                    
                    }
                state[0] = "["+state[0];
                state[length-1] = state[length-1] + "]";
		
		if (dim[0] > 1) {
		    new_state = [];
		    row = [];
		    for (var i = 0; i < state.length; i++) {
			row.push(state[i]);
			 if (i > 0 && (i+1)%dim[1] == 0) { 
			    row[0] = "["+row[0];
			    row[row.length-1] = row[row.length-1] + "]";
			    row = row.join(", ");
			    new_state.push(row);
			     row = [];
				    }
		    
                    }
		    new_state = new_state.join(", ");
		    return new_state;
			    }				
		else {
		    return state.join(",  ");
		    }
            };

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
