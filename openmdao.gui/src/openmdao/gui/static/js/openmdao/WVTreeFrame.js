/*
 * simple key-stroke & mouse driven UI for WV
 *
 * Notes: wv.sceneUpd should be set to 1 if the scene should rerendered
 *        wv.sgUpdate will be set to 1 if the sceneGraph has changed
 *                    should be set back to 0 after UI responds
 */

var openmdao = (typeof openmdao === "undefined" || !openmdao) ? {} : openmdao ;

openmdao.WVTreeFrame = function(id, wv) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var _self = this,
        _elem = jQuery('#'+id),
        _edgesData,
        _edgesTree,
        _facesData,
        _facesTree,
        _setCheckboxes = false;

    /** determine if an attribute is set in a grpim attributes value */
    function isAttributeSet(attrs, attr) {
        return (attrs & attr) !== 0;
    }

    /** set the enabled/disabled state of a gprim attribute */
    function setAttribute(gprim_name, attr, enabled) {
        var gprim = wv.sceneGraph[gprim_name];

        if (enabled) {
            gprim.attrs |= attr;
        }
        else {
            gprim.attrs &= ~attr;
        }

        wv.sceneUpd = 1;
    }

    /** (re)create the trees for Edges and Faces from the scenegraph.
        the attributes for each graphic primitive in the tree are three
        GPtype-specific checkboxes for toggling visualization options
        for that primitive.
     */
    function makeTrees() {
        var gprim, gprim_name,
            template = '<input type="checkbox" name="NAME" value="VALUE">',
            openNodes = [];

        // save noms of currently open nodes
        _elem.find('li.jstree-open').each(function () {
            openNodes.push(this.getAttribute('nom'));
        });

        // initialize edges data with root node
        _edgesData = [{
            data: 'Edges',
            attr: {
                nom: 'Edges',
                viz: template
                        .replace('NAME', 'Edges')
                        .replace('VALUE', wv.plotAttrs.ON),
                grd: template
                        .replace('NAME', 'Edges')
                        .replace('VALUE', wv.plotAttrs.POINTS),
                ori: template
                        .replace('NAME', 'Edges')
                        .replace('VALUE', wv.plotAttrs.ORIENTATION)
            },
            state: openNodes.indexOf('Edges') >= 0 ? 'open' : 'closed',
            children: []
        }];

        // initialize faces data with root node
        _facesData = [{
            data: 'Faces',
            attr: {
                nom: 'Faces',
                viz: template
                        .replace('NAME', 'Faces')
                        .replace('VALUE', wv.plotAttrs.ON),
                grd: template
                        .replace('NAME', 'Faces')
                        .replace('VALUE', wv.plotAttrs.LINES),
                trn: template
                        .replace('NAME', 'Faces')
                        .replace('VALUE', wv.plotAttrs.TRANSPARENT)
            },
            state: openNodes.indexOf('Faces') >= 0 ? 'open' : 'closed',
            children: []
        }]

        // get data for edge and face gprims in sceneGraph
        for (gprim_name in wv.sceneGraph) {
            gprim = wv.sceneGraph[gprim_name];
            if (gprim.GPtype === 1) {
                _edgesData[0].children.push({
                    data: gprim_name,
                    attr: {
                        nom: gprim_name,
                        viz: template
                                .replace('NAME',  gprim_name)
                                .replace('VALUE', wv.plotAttrs.ON),
                        grd: template
                                .replace('NAME',  gprim_name)
                                .replace('VALUE', wv.plotAttrs.POINTS),
                        ori: template
                                .replace('NAME',  gprim_name)
                                .replace('VALUE', wv.plotAttrs.ORIENTATION)
                    },
                    state: openNodes.indexOf(gprim_name) >= 0 ? 'open' : undefined,
                    children: []
                })
            }
            else if (gprim.GPtype === 2) {
                _facesData[0].children.push({
                    data: gprim_name,
                    attr: {
                        nom: gprim_name,
                        viz: template
                                .replace('NAME',  gprim_name)
                                .replace('VALUE', wv.plotAttrs.ON),
                        grd: template
                                .replace('NAME',  gprim_name)
                                .replace('VALUE', wv.plotAttrs.LINES),
                        trn: template
                                .replace('NAME',  gprim_name)
                                .replace('VALUE', wv.plotAttrs.TRANSPARENT)
                    },
                    state: openNodes.indexOf(gprim_name) >= 0 ? 'open' : undefined,
                    children: []
                })
            }
            else {
                console.log('ERROR: unknown GPtype for', gprim_name, '=', gprim.GPtype, gprim);
            }
        }

        // remove any existing trees (with their event handlers, etc.)
        _elem.empty();

        // when both trees are ready, set the checkboxes
        var edges_tree_ready = jQuery.Deferred(),
            faces_tree_ready = jQuery.Deferred();

        jQuery.when(edges_tree_ready, faces_tree_ready)
              .done(setCheckboxes);

        //create new tree elements
        _edgesTree = jQuery('<div id="'+id+'_edges_tree">')
            .appendTo(_elem)
            .on('click', clickHandler);

        _facesTree = jQuery('<div id="'+id+'_faces_tree">')
            .appendTo(_elem)
            .on('click', clickHandler);

        // (re)create tree for the edge primitives
        _edgesTree.jstree({
            plugins: ["themes", "json_data", "grid"],
            themes : { "theme" : "openmdao" },
            json_data: {data: _edgesData},
            grid: {
                columns: [
                    {width: 160, header: "Edges", title:"_DATA_"},
                    {cellClass: "col1", value: "viz", width: 30, header: "Viz", title: "Visible"},
                    {cellClass: "col2", value: "grd", width: 30, header: "Grd", title: "Grid"},
                    {cellClass: "col3", value: "ori", width: 30, header: "Ori", title: "Orientation"}
                ],
                resizable:true
            }
        })
        .bind("loaded.jstree", function(event, data) {
            edges_tree_ready.resolve();
        });

        // (re)create tree for the face primitives
        _facesTree.jstree({
            plugins: ["themes", "json_data", "grid"],
            themes : { "theme" : "openmdao" },
            json_data: {data: _facesData},
            grid: {
                columns: [
                    {width: 160, header: "Faces", title:"_DATA_"},
                    {cellClass: "col1", value: "viz", width: 30, header: "Viz", title: "Visible"},
                    {cellClass: "col2", value: "grd", width: 30, header: "Grd", title: "Grid"},
                    {cellClass: "col3", value: "trn", width: 30, header: "Trn", title: "Transparent"}
                ],
                resizable:true
            }
        })
        .bind("loaded.jstree", function(event, data) {
            faces_tree_ready.resolve();
        });
    }

    /** set checkboxes to match current state of gprims
        set the root node checkboxes to checked if all children are checked
    */
    function setCheckboxes() {
        var edges_cnt = _edgesData[0].children.length,
            faces_cnt = _facesData[0].children.length,
            edges_viz_cnt = 0,
            edges_grd_cnt = 0,
            edges_ori_cnt = 0,
            faces_viz_cnt = 0,
            faces_grd_cnt = 0,
            faces_trn_cnt = 0,
            is_viz, is_grd, is_ori, is_trn, checkbox,
            points_disabled = 0,
            orientation_disabled = 0,
            selector = 'input[type=checkbox][name*="NAME"][value="VALUE"]';

        for (gprim_name in wv.sceneGraph) {
            gprim = wv.sceneGraph[gprim_name];
            if (gprim.GPtype === 1) {
                is_viz = isAttributeSet(gprim.attrs, wv.plotAttrs.ON);
                is_grd = isAttributeSet(gprim.attrs, wv.plotAttrs.POINTS);
                is_ori = isAttributeSet(gprim.attrs, wv.plotAttrs.ORIENTATION);

                if (is_viz) edges_viz_cnt += 1;
                if (is_grd) edges_grd_cnt += 1;
                if (is_ori) edges_ori_cnt += 1;

                checkbox = _edgesTree.find(selector.replace('NAME', gprim_name).replace('VALUE', wv.plotAttrs.ON));
                checkbox.prop('checked', is_viz);

                checkbox = _edgesTree.find(selector.replace('NAME', gprim_name).replace('VALUE', wv.plotAttrs.POINTS));
                // we can't show points if gprim points are not defined (just checking the first point)
                if (gprim.nStrip > 0 && gprim.points !== undefined && gprim.points[0] !== undefined) {
                    checkbox.prop('checked', is_grd);
                    checkbox.prop('disabled', false);
                }
                else {
                    checkbox.prop('checked', false);
                    checkbox.prop('disabled', true);
                    points_disabled += 1;
                }

                checkbox = _edgesTree.find(selector.replace('NAME', gprim_name).replace('VALUE', wv.plotAttrs.ORIENTATION));
                // we can't show orientation if gprim has no triangles
                if (gprim.triangles !== undefined) {
                    checkbox.prop('checked', is_ori);
                    checkbox.prop('disabled', false);
                }
                else {
                    checkbox.prop('checked', false);
                    checkbox.prop('disabled', true);
                    orientation_disabled += 1;
                }
            }
            else if (gprim.GPtype === 2) {
                is_viz = isAttributeSet(gprim.attrs, wv.plotAttrs.ON);
                is_grd = isAttributeSet(gprim.attrs, wv.plotAttrs.LINES);
                is_trn = isAttributeSet(gprim.attrs, wv.plotAttrs.TRANSPARENT);

                if (is_viz) faces_viz_cnt += 1;
                if (is_grd) faces_grd_cnt += 1;
                if (is_trn) faces_trn_cnt += 1;

                checkbox = _elem.find(selector.replace('NAME', gprim_name).replace('VALUE', wv.plotAttrs.ON));
                checkbox.prop('checked', is_viz);

                checkbox = _elem.find(selector.replace('NAME', gprim_name).replace('VALUE', wv.plotAttrs.LINES));
                checkbox.prop('checked', is_grd);

                checkbox = _elem.find(selector.replace('NAME', gprim_name).replace('VALUE', wv.plotAttrs.TRANSPARENT));
                checkbox.prop('checked', is_trn);
            }
        }

        // set checkboxes for edges root node
        is_viz = (edges_cnt > 0 && edges_viz_cnt === edges_cnt) ? true : false;
        is_grd = (edges_cnt > 0 && edges_grd_cnt === edges_cnt) ? true : false;
        is_ori = (edges_cnt > 0 && edges_ori_cnt === edges_cnt) ? true : false;

        checkbox = _elem.find(selector.replace('NAME', 'Edges').replace('VALUE', wv.plotAttrs.ON));
        checkbox.prop('checked', is_viz);

        checkbox = _elem.find(selector.replace('NAME', 'Edges').replace('VALUE', wv.plotAttrs.POINTS));
        checkbox.prop('checked', is_grd);
        if (points_disabled === edges_cnt) {
            checkbox.prop('disabled', true)
        }

        checkbox = _elem.find(selector.replace('NAME', 'Edges').replace('VALUE', wv.plotAttrs.ORIENTATION));
        checkbox.prop('checked', is_ori);
        if (orientation_disabled === edges_cnt) {
            checkbox.prop('disabled', true)
        }

        // set checkboxes for faces root node
        is_viz = (faces_cnt > 0 && faces_viz_cnt === faces_cnt) ? true : false;
        is_grd = (faces_cnt > 0 && faces_grd_cnt === faces_cnt) ? true : false;
        is_trn = (faces_cnt > 0 && faces_trn_cnt === faces_cnt) ? true : false;

        checkbox = _elem.find(selector.replace('NAME', 'Faces').replace('VALUE', wv.plotAttrs.ON));
        checkbox.prop('checked', is_viz);

        checkbox = _elem.find(selector.replace('NAME', 'Faces').replace('VALUE', wv.plotAttrs.LINES));
        checkbox.prop('checked', is_grd);

        checkbox = _elem.find(selector.replace('NAME', 'Faces').replace('VALUE', wv.plotAttrs.TRANSPARENT));
        checkbox.prop('checked', is_trn);
    }

    /** set the appropriate attribute(s) when a checkbox is clicked */
    function clickHandler(e) {
        if (jQuery(e.target).is('input:checkbox')) {
            if (e.target.name === 'Faces') {
                for (var i in _facesData[0].children) {
                    setAttribute(_facesData[0].children[i].data,
                        Number(e.target.value), e.target.checked);
                }
            }
            else if (e.target.name === 'Edges') {
                for (var i in _edgesData[0].children) {
                    setAttribute(_edgesData[0].children[i].data,
                        Number(e.target.value), e.target.checked);
                }
            }
            else {
                setAttribute(e.target.name, Number(e.target.value), e.target.checked);
            }
            // need to update the state of parent/child checkboxes
            _setCheckboxes = true;
        }
        else if (jQuery(e.target).is('ins.jstree-icon')) {
            // seems to be a bug, expanding tree clears the root node checkboxes
            // this fix makes collapsing the tree unnecessarily slow, though
           _setCheckboxes = true;
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** Initialize the trees for Edges and Faces in the sceneGraph */
    this.wvInitUI = function() {
        if (wv.sceneGraph === undefined) {
            alert("There is no sceneGraph!");
            return;
        }
        makeTrees();
    }

    /** if the scene graph has been updated, recreate the trees
        if checkboxes need updated, update the checkboxes
     */
    this.wvUpdateUI = function() {
        if (wv.sceneGraph === undefined) {
            alert("There is no sceneGraph!");
            return;
        }

        if (wv.sgUpdate === 1) {
            // scene graph has changed, update trees
            makeTrees();
        }
        else if (_setCheckboxes) {
            // update checkboxes
            _setCheckboxes = false;
            setCheckboxes();
        }
    }

}