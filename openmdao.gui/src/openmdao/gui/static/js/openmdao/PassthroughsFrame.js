
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.PassthroughsFrame = function(model,pathname,src_comp,dst_comp) {
    var id = ('PassthroughsFrame-'+pathname).replace(/\./g,'-');
    openmdao.PassthroughsFrame.prototype.init.call(this, id,
        'Passthroughs: '+openmdao.Util.getName(pathname));

    /***********************************************************************
     *  private
     ***********************************************************************/
    console.log(model);
    // model.issueCommand(cmd);
    // initialize private variables
    var self = this,
        // component selectors
        componentsHTML = '<div style="width:100%;background:grey"><table>'
                       +        '<tr><td>Variable Name:</td>'
                       +            '<td>Passthrough:</td>'
                       +        '</tr>'
                       +        '<tr><td>name</td><td>check</td></tr>'
                       + '</table></div>',
        componentsDiv = jQuery(componentsHTML)
            .appendTo(self.elm);

}
/** set prototype */
openmdao.PassthroughsFrame.prototype = new openmdao.BaseFrame();
openmdao.PassthroughsFrame.prototype.constructor = openmdao.PassthroughsFrame;
