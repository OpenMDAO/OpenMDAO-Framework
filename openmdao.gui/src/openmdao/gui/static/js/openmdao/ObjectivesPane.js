
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ObjectivesPane = function(elm, project, pathname, name, editable) {
    var objectives,
        objectivesDiv = jQuery("<div id='"+name+"_objectives' class='slickgrid' style='overflow:none; height:320px; width:620px'>"),
        addButton = jQuery("<button>Add Objective</button>").button(),
        clrButton = jQuery("<button>Clear Objectives</button>").button(),
        columns = [
            {id:"del",   name:"",            field:"del",   width:25, formatter:buttonFormatter},
            {id:"expr",  name:"Expression",  field:"expr",  width:180},
            {id:"name",  name:"Name",        field:"name",  width:50}
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
    elm.append(objectivesDiv).width('100%');

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

    objectives = new Slick.Grid(objectivesDiv, [], columns, options);
    if (editable) {
        objectives.onCellChange.subscribe(function(e,args) {
            // TODO: better way to do this (e.g. project.setProperty(path,name,value)
            cmd = pathname+'.'+args.item.name+' = '+args.item.value;
            project.issueCommand(cmd);
        });
    }
    objectives.onClick.subscribe(function (e) {
        var cell = objectives.getCellFromEvent(e);
        if (cell.cell === 0) {
            var delname = objectives.getData()[cell.row].name;
            var cmd = pathname+'.remove_objective("'+delname+'")';
            project.issueCommand(cmd);
        }
    });

    /** add a new objective */
    function addObjective(expr,name) {
        var cmd = pathname+".add_objective('"+expr+"'";
        if (name) {
            cmd = cmd + ", name='"+name+"'";
        }
        cmd = cmd + ")";
        project.issueCommand(cmd);
    }

    objectivesDiv.bind('resizeCanvas', function() {
        objectives.resizeCanvas();
    });

    /** prompt for new objective */
    function promptForObjective(callback) {
        // Build dialog markup
        var win = jQuery('<div id="objective-dialog"></div>'),
            expr   = jQuery('<input id="objective-expr" type="text" style="width:100%"></input>'),
            name   = jQuery('<input id="objective-name" type="text" style="width:75%"></input>');

        win.append(jQuery('<div>Expression: </div>').append(expr));

        var table = jQuery('<table>');
        row = jQuery('<tr>').append(jQuery('<td>').append(jQuery('<div>Name: </div>').append(name)));
        table.append(row);
        win.append(table);


        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'title': 'New Objective',
            'buttons': [
                {
                    text: 'Ok',
                    id: 'objective-ok',
                    click: function() {
                        jQuery(this).dialog('close');
                        callback(expr.val(),name.val());
                        // remove from DOM
                        win.remove();
                    }
                },
                {
                    text: 'Cancel',
                    id: 'objective-cancel',
                    click: function() {
                        jQuery(this).dialog('close');
                        // remove from DOM
                        win.remove();
                    }
                }
            ]
        });
    }

    /** clear all objectives */
    function clearObjectives() {
        var cmd = pathname+".clear_objectives()";
        project.issueCommand(cmd);
    }

    addButton.click(function() { promptForObjective(addObjective); });
    clrButton.click(function() { clearObjectives(); });

    /** load the table with the given properties */
    this.loadData = function(properties) {
        if (properties) {
            objectives.setData(properties);
        }
        else {
            objectives.setData([]);
            alert('Error getting properties for '+pathname+' ('+name+')');
        }
        objectives.updateRowCount();
        objectives.render();
    };
};

