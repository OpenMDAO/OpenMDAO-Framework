// Custom openMDAO slickgrid cell editors 
(function($) {

    var customEditors = {

        BoolEditor : function(args) {
            var $select;
            var defaultValue;
            var scope = this;

            this.init = function() {
                $select = $("<SELECT tabIndex='0' class='editor-yesno'><OPTION value='True'>True</OPTION><OPTION value='False'>False</OPTION></SELECT>");
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
            var $editor_dialog = $("<div />").dialog({
				width: "auto",
				title: "Editing array: '"+var_name+"'",
            			buttons: {
					    "Submit changes": function() {
						    grid.getEditorLock().commitCurrentEdit();
						    $( this ).dialog( "close" );
					    },
					    "Cancel": function() {
						    grid.getEditorLock().cancelCurrentEdit();
						    $( this ).dialog( "close" );
					    }
					}
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
	    var $add_button = $("<button>+</button>").button();
	    var $subtract_button = $("<button>-</button>").button();
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
		    console.log(new_state);
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
        },


    }

    $.extend(window, customEditors);

})(jQuery);
