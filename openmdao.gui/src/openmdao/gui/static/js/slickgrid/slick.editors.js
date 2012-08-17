/* THESE FORMATTERS & EDITORS ARE JUST SAMPLES! */

(function($) {

    var SlickEditor = {

        SelectorCellFormatter : function(row, cell, value, columnDef, dataContext) {
            return (!dataContext ? "" : row);
        },

        PercentCompleteCellFormatter : function(row, cell, value, columnDef, dataContext) {
            if (value == null || value === "")
                return "-";
            else if (value < 50)
                return "<span style='color:red;font-weight:bold;'>" + value + "%</span>";
            else
                return "<span style='color:green'>" + value + "%</span>";
        },

        GraphicalPercentCompleteCellFormatter : function(row, cell, value, columnDef, dataContext) {
            if (value == null || value === "")
                return "";

            var color;

            if (value < 30)
                color = "red";
            else if (value < 70)
                color = "silver";
            else
                color = "green";

            return "<span class='percent-complete-bar' style='background:" + color + ";width:" + value + "%'></span>";
        },

        YesNoCellFormatter : function(row, cell, value, columnDef, dataContext) {
            return value ? "Yes" : "No";
        },

        BoolCellFormatter : function(row, cell, value, columnDef, dataContext) {
            return value ? "<img src='../images/tick.png'>" : "";
        },

        TaskNameFormatter : function(row, cell, value, columnDef, dataContext) {
            // todo:  html encode
            var spacer = "<span style='display:inline-block;height:1px;width:" + (2 + 15 * dataContext["indent"]) + "px'></span>";
            return spacer + " <img src='../images/expand.gif'>&nbsp;" + value;
        },

        ResourcesFormatter : function(row, cell, value, columnDef, dataContext) {
            var resources = dataContext["resources"];

            if (!resources || resources.length == 0)
                return "";

            if (columnDef.width < 50)
                return (resources.length > 1 ? "<center><img src='../images/user_identity_plus.gif' " : "<center><img src='../images/user_identity.gif' ") +
                        " title='" + resources.join(", ") + "'></center>";
            else
                return resources.join(", ");
        },

        StarFormatter : function(row, cell, value, columnDef, dataContext) {
            return (value) ? "<img src='../images/bullet_star.png' align='absmiddle'>" : "";
        },


        TextCellEditor : function(args) {
            var $input;
            var defaultValue;
            var scope = this;

            this.init = function() {
                $input = $("<INPUT type=text class='editor-text' />")
                    .appendTo(args.container)
                    .bind("keydown.nav", function(e) {
                        if (e.keyCode === $.ui.keyCode.LEFT || e.keyCode === $.ui.keyCode.RIGHT) {
                            e.stopImmediatePropagation();
                        }
                    })
                    .focus()
                    .select();
            };

            this.destroy = function() {
                $input.remove();
            };

            this.focus = function() {
                $input.focus();
            };

            this.getValue = function() {
                return $input.val();
            };

            this.setValue = function(val) {
                $input.val(val);
            };

            this.loadValue = function(item) {
                defaultValue = item[args.column.field] || "";
                $input.val(defaultValue);
                $input[0].defaultValue = defaultValue;
                $input.select();
            };

            this.serializeValue = function() {
                return $input.val();
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return (!($input.val() == "" && defaultValue == null)) && ($input.val() != defaultValue);
            };

            this.validate = function() {
                if (args.column.validator) {
                    var validationResults = args.column.validator($input.val());
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

        IntegerCellEditor : function(args) {
            var $input;
            var defaultValue;
            var scope = this;

            this.init = function() {
                $input = $("<INPUT type=text class='editor-text' />");

                $input.bind("keydown.nav", function(e) {
                    if (e.keyCode === $.ui.keyCode.LEFT || e.keyCode === $.ui.keyCode.RIGHT) {
                        e.stopImmediatePropagation();
                    }
                });

                $input.appendTo(args.container);
                $input.focus().select();
            };

            this.destroy = function() {
                $input.remove();
            };

            this.focus = function() {
                $input.focus();
            };

            this.loadValue = function(item) {
                defaultValue = item[args.column.field];
                $input.val(defaultValue);
                $input[0].defaultValue = defaultValue;
                $input.select();
            };

            this.serializeValue = function() {
                return parseInt($input.val(),10) || 0;
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return (!($input.val() == "" && defaultValue == null)) && ($input.val() != defaultValue);
            };

            this.validate = function() {
                if (isNaN($input.val()))
                    return {
                        valid: false,
                        msg: "Please enter a valid integer"
                    };

                return {
                    valid: true,
                    msg: null
                };
            };

            this.init();
        },

        DateCellEditor : function(args) {
            var $input;
            var defaultValue;
            var scope = this;
            var calendarOpen = false;

            this.init = function() {
                $input = $("<INPUT type=text class='editor-text' />");
                $input.appendTo(args.container);
                $input.focus().select();
                $input.datepicker({
                    showOn: "button",
                    buttonImageOnly: true,
                    buttonImage: "../images/calendar.gif",
                    beforeShow: function() { calendarOpen = true },
                    onClose: function() { calendarOpen = false }
                });
                $input.width($input.width() - 18);
            };

            this.destroy = function() {
                $.datepicker.dpDiv.stop(true,true);
                $input.datepicker("hide");
                $input.datepicker("destroy");
                $input.remove();
            };

            this.show = function() {
                if (calendarOpen) {
                    $.datepicker.dpDiv.stop(true,true).show();
                }
            };

            this.hide = function() {
                if (calendarOpen) {
                    $.datepicker.dpDiv.stop(true,true).hide();
                }
            };

            this.position = function(position) {
                if (!calendarOpen) return;
                $.datepicker.dpDiv
                    .css("top", position.top + 30)
                    .css("left", position.left);
            };

            this.focus = function() {
                $input.focus();
            };

            this.loadValue = function(item) {
                defaultValue = item[args.column.field];
                $input.val(defaultValue);
                $input[0].defaultValue = defaultValue;
                $input.select();
            };

            this.serializeValue = function() {
                return $input.val();
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return (!($input.val() == "" && defaultValue == null)) && ($input.val() != defaultValue);
            };

            this.validate = function() {
                return {
                    valid: true,
                    msg: null
                };
            };

            this.init();
        },

        YesNoSelectCellEditor : function(args) {
            var $select;
            var defaultValue;
            var scope = this;

            this.init = function() {
                $select = $("<SELECT tabIndex='0' class='editor-yesno'><OPTION value='yes'>Yes</OPTION><OPTION value='no'>No</OPTION></SELECT>");
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
                $select.val((defaultValue = item[args.column.field]) ? "yes" : "no");
                $select.select();
            };

            this.serializeValue = function() {
                return ($select.val() == "yes");
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

        YesNoCheckboxCellEditor : function(args) {
            var $select;
            var defaultValue;
            var scope = this;

            this.init = function() {
                $select = $("<INPUT type=checkbox value='true' class='editor-checkbox' hideFocus>");
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
                if (defaultValue)
                    $select.attr("checked", "checked");
                else
                    $select.removeAttr("checked");
            };

            this.serializeValue = function() {
                return $select.attr("checked");
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return ($select.attr("checked") != defaultValue);
            };

            this.validate = function() {
                return {
                    valid: true,
                    msg: null
                };
            };

            this.init();
        },

        PercentCompleteCellEditor : function(args) {
            var $input, $picker;
            var defaultValue;
            var scope = this;

            this.init = function() {
                $input = $("<INPUT type=text class='editor-percentcomplete' />");
                $input.width($(args.container).innerWidth() - 25);
                $input.appendTo(args.container);

                $picker = $("<div class='editor-percentcomplete-picker' />").appendTo(args.container);
                $picker.append("<div class='editor-percentcomplete-helper'><div class='editor-percentcomplete-wrapper'><div class='editor-percentcomplete-slider' /><div class='editor-percentcomplete-buttons' /></div></div>");

                $picker.find(".editor-percentcomplete-buttons").append("<button val=0>Not started</button><br/><button val=50>In Progress</button><br/><button val=100>Complete</button>");

                $input.focus().select();

                $picker.find(".editor-percentcomplete-slider").slider({
                    orientation: "vertical",
                    range: "min",
                    value: defaultValue,
                    slide: function(event, ui) {
                        $input.val(ui.value)
                    }
                });

                $picker.find(".editor-percentcomplete-buttons button").bind("click", function(e) {
                    $input.val($(this).attr("val"));
                    $picker.find(".editor-percentcomplete-slider").slider("value", $(this).attr("val"));
                })
            };

            this.destroy = function() {
                $input.remove();
                $picker.remove();
            };

            this.focus = function() {
                $input.focus();
            };

            this.loadValue = function(item) {
                $input.val(defaultValue = item[args.column.field]);
                $input.select();
            };

            this.serializeValue = function() {
                return parseInt($input.val(),10) || 0;
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return (!($input.val() == "" && defaultValue == null)) && ((parseInt($input.val(),10) || 0) != defaultValue);
            };

            this.validate = function() {
                if (isNaN(parseInt($input.val(),10)))
                    return {
                        valid: false,
                        msg: "Please enter a valid positive number"
                    };

                return {
                    valid: true,
                    msg: null
                };
            };

            this.init();
        },

        StarCellEditor : function(args) {
            var $input;
            var defaultValue;
            var scope = this;

            function toggle(e) {
                if (e.type == "keydown" && e.which != 32) return;

                if ($input.css("opacity") == "1")
                    $input.css("opacity", 0.5);
                else
                    $input.css("opacity", 1);

                e.preventDefault();
                e.stopPropagation();
                return false;
            }

            this.init = function() {
                $input = $("<IMG src='../images/bullet_star.png' align=absmiddle tabIndex=0 title='Click or press Space to toggle' />")
                    .bind("click keydown", toggle)
                    .appendTo(args.container)
                    .focus();
            };

            this.destroy = function() {
                $input.unbind("click keydown", toggle);
                $input.remove();
            };

            this.focus = function() {
                $input.focus();
            };

            this.loadValue = function(item) {
                defaultValue = item[args.column.field];
                $input.css("opacity", defaultValue ? 1 : 0.2);
            };

            this.serializeValue = function() {
                return ($input.css("opacity") == "1");
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return defaultValue != ($input.css("opacity") == "1");
            };

            this.validate = function() {
                return {
                    valid: true,
                    msg: null
                };
            };

            this.init();
        },

        /*
         * An example of a "detached" editor.
         * The UI is added onto document BODY and .position(), .show() and .hide() are implemented.
         * KeyDown events are also handled to provide handling for Tab, Shift-Tab, Esc and Ctrl-Enter.
         */
        LongTextCellEditor : function (args) {
            var $input, $wrapper;
            var defaultValue;
            var scope = this;

            this.init = function() {
                var $container = $("body");

                $wrapper = $("<DIV style='z-index:10000;position:absolute;background:white;padding:5px;border:3px solid gray; -moz-border-radius:10px; border-radius:10px;'/>")
                    .appendTo($container);

                $input = $("<TEXTAREA hidefocus rows=5 style='backround:white;width:250px;height:80px;border:0;outline:0'>")
                    .appendTo($wrapper);

                $("<DIV style='text-align:right'><BUTTON>Save</BUTTON><BUTTON>Cancel</BUTTON></DIV>")
                    .appendTo($wrapper);

                $wrapper.find("button:first").bind("click", this.save);
                $wrapper.find("button:last").bind("click", this.cancel);
                $input.bind("keydown", this.handleKeyDown);

                scope.position(args.position);
                $input.focus().select();
            };

            this.handleKeyDown = function(e) {
                if (e.which == $.ui.keyCode.ENTER && e.ctrlKey) {
                    scope.save();
                }
                else if (e.which == $.ui.keyCode.ESCAPE) {
                    e.preventDefault();
                    scope.cancel();
                }
                else if (e.which == $.ui.keyCode.TAB && e.shiftKey) {
                    e.preventDefault();
                    grid.navigatePrev();
                }
                else if (e.which == $.ui.keyCode.TAB) {
                    e.preventDefault();
                    grid.navigateNext();
                }
            };

            this.save = function() {
                args.commitChanges();
            };

            this.cancel = function() {
                $input.val(defaultValue);
                args.cancelChanges();
            };

            this.hide = function() {
                $wrapper.hide();
            };

            this.show = function() {
                $wrapper.show();
            };

            this.position = function(position) {
                $wrapper
                    .css("top", position.top - 5)
                    .css("left", position.left - 5)
            };

            this.destroy = function() {
                $wrapper.remove();
            };

            this.focus = function() {
                $input.focus();
            };

            this.loadValue = function(item) {
                $input.val(defaultValue = item[args.column.field]);
                $input.select();
            };

            this.serializeValue = function() {
                return $input.val();
            };

            this.applyValue = function(item,state) {
                item[args.column.field] = state;
            };

            this.isValueChanged = function() {
                return (!($input.val() == "" && defaultValue == null)) && ($input.val() != defaultValue);
            };

            this.validate = function() {
                return {
                    valid: true,
                    msg: null
                };
            };

            this.init();
        },
        
        /*
         * This the constructor for a default custom datatype editor.
         * All custom datatype editors should inherit CellEditor.
         */    
        CellEditor : function(args){
            this.init(args)
        },

        /*
        * This is the constructor for the editor to be used
        * when defining a slickgrid. VariableEditor uses the
        * delegation pattern by resolving which datatype editor
        * to use based off of the 'type' attribute, then calls
        * the appropriate method of the datatype editor.
        *
        * For more information on the delegation patttern,
        * visit http://en.wikipedia.org/wiki/Delegation_pattern
        */
        VariableEditor : function(args){
            this.init(args)
        }
    };

    //CellEditor's init method varies slightly from the default editors provided
    //by slickgrid. It saves the args object as a data member to be available
    //for reference by other CellEditor methods.
    SlickEditor.CellEditor.prototype.init = function(args){ this.args = args }

    /*
     * The following functions are required functions for any custom
     * slick grid editor:
     *      destroy
     *      focus
     *      isValueChanged
     *      serializeValue
     *      loadValue
     *      applyValue
     *      validate
     */
    SlickEditor.CellEditor.prototype.destroy = function(){}
    SlickEditor.CellEditor.prototype.focus = function(){}
    SlickEditor.CellEditor.prototype.isValueChanged = function(){ return false; }
    SlickEditor.CellEditor.prototype.serializeValue = function(){ return ""; }
    SlickEditor.CellEditor.prototype.loadValue = function(item){}
    SlickEditor.CellEditor.prototype.applyValue = function(item, state){}
    SlickEditor.CellEditor.prototype.validate = function(){ return { valid: false, msg: "This field is required" } }

    /*
     * The following functions are optional functions for 
     * custom slickgrid editors:
     *
     *      hide
     *      show
     *      position 
     */
    SlickEditor.CellEditor.prototype.hide = function(){}
    SlickEditor.CellEditor.prototype.show = function(){}
    SlickEditor.CellEditor.prototype.position = function(cellbox){}
  
    /*
     * The next three lines cause VariableEditor
     * to inherit CellEditor
     */ 
    SlickEditor.VariableEditor.prototype = new SlickEditor.CellEditor()
    SlickEditor.VariableEditor.constructor = SlickEditor.VariableEditor
    SlickEditor.VariableEditor.superClass = SlickEditor.CellEditor.prototype
   

    /*
     * Editors is a public static data member of VariableEditor.
     * It is an object that maps a data type to a CellEditor 
     * and is used by VariableEditor to delegate function
     * calls.
     */
    SlickEditor.VariableEditor.editors = {'str' : SlickEditor.TextCellEditor }  
    
    /*
     * The following function declaractions override CellEditor's
     * functions. 
     */

    /*
     * The init function calls CellEditor's init method to initialize
     * args. This makes the parameter accessible to the rest of 
     * overriden functions. The correct data type editor is also
     * set. If the editor is not in editors, VariableEditor defaults
     * to using TextCellEditor.
     */
    SlickEditor.VariableEditor.prototype.init = function(args){

        SlickEditor.VariableEditor.superClass.init.call(this, args)
        var editors = SlickEditor.VariableEditor.editors
        this.editor = (args.item.type in editors) 
            ? new editors[args.item.type](args) 
            : new SlickEditor.TextCellEditor(args)
    }  
    
    /*
     * If the function is required for a custom slickgrid editor,
     * VariableEditor delegates the work to editor. For functions
     * that are expected to return a value, VariableEditor returns 
     * the value returned by editor.
     */
    SlickEditor.VariableEditor.prototype.destroy = function(){ 
        this.editor.destroy() 
    }

    SlickEditor.VariableEditor.prototype.focus = function(){ 
        this.editor.focus()
    }

    SlickEditor.VariableEditor.prototype.isValueChanged = function(){
        return this.editor.isValueChanged()
    }

    SlickEditor.VariableEditor.prototype.serializeValue = function(){
        return this.editor.serializeValue()
    }

    SlickEditor.VariableEditor.prototype.loadValue = function(item){
        this.editor.loadValue(item)
    }

    SlickEditor.VariableEditor.prototype.applyValue = function(item, state){
        this.editor.applyValue(item, state)
    }

    SlickEditor.VariableEditor.prototype.validate = function(){
        return this.editor.validate()
    }

    /*
     * If the function is optional, the delegated call is 
     * wrapped in a try/catch block. If the execution is 
     * not successful, VariableEditor delegates the work
     * to it's parent, CellEditor. 
     */

    SlickEditor.VariableEditor.prototype.hide = function(){
        try{
            this.editor.hide()
        }
        catch{
            //TODO: Should report something to the user maybe
            //Default to using hide method of CellEditor
            SlickEditor.VariableEditor.superClass.hide.call(this)
        }

    }

    SlickEditor.VariableEditor.prototype.show = function(){
        try{
            this.editor.show()
        }
        catch{
            //TODO: Should report something to the user maybe
            //Default to using show method of CellEditor
            SlickEditor.VariableEditor.superClass.show.call(this)
        }
    }

    SlickEditor.VariableEditor.prototype.position = function(cellbox){
        try{
            this.editor.position(cellbox)
        }
        catch{
            //TODO: Should report something to the user maybe
            //Default to using position method of CellEditor
            SlickEditor.VariableEditor.superClass.position.call(this, cellbox)
        }
    }

    $.extend(window, SlickEditor);

})(jQuery);
