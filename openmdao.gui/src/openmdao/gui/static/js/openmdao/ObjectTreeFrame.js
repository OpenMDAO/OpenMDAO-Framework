/**
 *  ObjectTreeFrame: a frame for displaying a hiearchy of objects
 *
 *  .
 *
 *  Arguments:
 *      id:          id of the element on which t build the frame
 *      project:     object that provides access to the openmdao project
 *      select_fn:   function to be called when an object is selected
 *      dblclick_fn: function to be called when an object is double-clicked
 *      workflow_fn: function to be called to view the workflow of an object
 *      dataflow_fn: function to be called to view the dataflow of an object
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ObjectTreeFrame = function(id, project, select_fn, dblclick_fn, workflow_fn, dataflow_fn) {
    openmdao.ObjectTreeFrame.prototype.init.call(this,id,'Objects');

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        selectorHTML = '<select id="'+id+'_select" style="display:block;background:gray;width:100%">'
            + '<option value="Components">Components</option>'
            + '<option value="Workflow">Workflow</option>'
            + '</select>',
        selector = jQuery(selectorHTML)
            .combobox()
            .appendTo(self.elm),
        treeElem = jQuery('<div>')
            .appendTo('<div style="height:100%">')
            .appendTo(self.elm),
        tree;

    // set tree view based on selected value
    selector.change(function(e) {
        debug.info('ObjectTreeFrame change:', selector.val(), this.val, this);
        if (tree) {
            tree.destructor();
        }
        if (selector.val() === 'Components') {
            tree = openmdao.ComponentTreePane(treeElem, project, select_fn, dblclick_fn, workflow_fn, dataflow_fn);
        }
        else if (selector.val() === 'Workflow') {
            tree = openmdao.WorkflowTreePane(treeElem, project, select_fn, dblclick_fn, workflow_fn, dataflow_fn);
        }
        else {
            debug.error('ObjectTreeFrame: Invalid object tree type selected');
        }
    });

    // load initial object tree
    project.project_ready.always(function() {
        selector.css({'display': 'block'}); // not sure why it's hidden to start
        selector.val('Workflow');
        tree = openmdao.WorkflowTreePane(treeElem, project, select_fn, dblclick_fn, workflow_fn, dataflow_fn);
    });

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.destructor = function() {
        tree.destructor();
    };

};

/** set prototype */
openmdao.ObjectTreeFrame.prototype = new openmdao.BaseFrame();
openmdao.ObjectTreeFrame.prototype.constructor = openmdao.ObjectTreeFrame;
