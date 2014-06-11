
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConstraintsPane = function(elm, project, pathname, name, editable) {

    /***********************************************************************
     *  private
     ***********************************************************************/

    var constraints,
        constraintsDiv = jQuery("<div id='"+name+"_constraints' class='slickgrid' style='overflow:none; height:320px; width:620px'>"),
        addButton = jQuery("<button>Add Constraint</button>").button(),
        clrButton = jQuery("<button>Clear Constraints</button>").button(),
        columns = [
            {id:"del",    name:"",            field:"del",     width:25, formatter:buttonFormatter},
            {id:"expr",   name:"Expression",  field:"expr",    width:148},
            {id:"name",   name:"Name",        field:"name",    width:50}
        ],
        options = {
            asyncEditorLoading: false,
            multiSelect: false,
            autoHeight: false,
            autoEdit: false
        };

    function buttonFormatter(row,cell,value,columnDef,dataContext) {
        button = '<div class="ui-icon-trash"></div>';
        return button;
    }

    elm.append(constraintsDiv);

    var tabdiv = jQuery('<div class="post_slick" style="height:40px;">'),
        table = jQuery('<table width="100%">'),
        row = jQuery('<tr>').append(jQuery('<td style="text-align:left">').append(addButton))
                            .append(jQuery('<td style="text-align:right">').append(clrButton));
    table.append(row);
    tabdiv.append(table);
    elm.append(tabdiv);

    if (editable) {
        options.editable = true;
        options.editOnDoubleClick = true;
    }

    constraints = new Slick.Grid(constraintsDiv, [], columns, options);
    if (editable) {
        constraints.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. project.setProperty(path,name,value)
            var cmd = pathname+'.'+args.item.name+' = '+args.item.value;
            project.issueCommand(cmd);
        });
    }

    constraints.onClick.subscribe(function (e) {
        var cell = constraints.getCellFromEvent(e);
        if (cell.cell === 0) {
            var delname = constraints.getData()[cell.row].name;
            var cmd = pathname+'.remove_constraint("'+delname+'")';
            project.issueCommand(cmd);
        }
    });

    constraintsDiv.bind('resizeCanvas', function() {
        constraints.resizeCanvas();
    });

    /** add a new constraint */
    function addConstraint(expr, name) {
        var cmd = pathname+".add_constraint('"+expr+"'";
        if (name) {
            cmd = cmd + ", name='"+name+"'";
        }
        cmd = cmd + ")";
        project.issueCommand(cmd);
    }

    /** prompt for new constraint */
    function promptForConstraint(callback) {
        // Build dialog markup
        var win = jQuery('<div id="constraint-dialog"></div>'),
            expr   = jQuery('<input id="constraint-expr" type="text" style="width:100%"></input>'),
            name   = jQuery('<input id="constraint-name" type="text" style="width:75%"></input>');

        win.append(jQuery('<div>Expression: </div>').append(expr));

        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Name: </div>').append(name)));
        table.append(row);
        win.append(table);


        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Constraint',
            'buttons': [
                {
                    text: 'Ok',
                    id: 'constraint-ok',
                    click: function() {
                        jQuery(this).dialog('close');
                        callback(expr.val(), name.val());
                        // remove from DOM
                        win.remove();
                    }
                },
                {
                    text: 'Cancel',
                    id: 'constraint-cancel',
                    click: function() {
                        jQuery(this).dialog('close');
                        // remove from DOM
                        win.remove();
                    }
                }
            ]
        });
    }

    /** clear all constraints */
    function clearConstraints() {
        var cmd = pathname+".clear_constraints()";
        project.issueCommand(cmd);
    }

    addButton.click(function() { promptForConstraint(addConstraint); });
    clrButton.click(function() { clearConstraints(); });


    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            constraints.setData(properties);
        }
        else {
            constraints.setData([]);
            alert('Error getting properties for '+pathname+' ('+name+')');
        }
        constraints.resizeCanvas();
    };
};
