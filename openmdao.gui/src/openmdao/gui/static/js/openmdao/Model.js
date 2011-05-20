/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.Model=function() {

    /***********************************************************************
     *  private (available only to privileged methods) 
     ***********************************************************************/
     
    var self = this,
        callbacks = [],
        modes = ['design', 'analysis'],
        mode = modes[0]
        
    /***********************************************************************
     *  privileged (can access privates, accessible to public and outside) 
     ***********************************************************************/
    
    /** add a listener, i.e. a function that will be called when something changes */
    this.addListener = function(callback) {
        callbacks.push(callback)
    }

    /** notify all listeners that something has changed (by calling all callbacks) */
    this.updateListeners = function() {
        debug.info('Model: updating listeners:')
        for ( var i = 0; i < callbacks.length; i++ )
            if (typeof callbacks[i] == 'function')
                callbacks[i]()
            else
                debug.error('Model: listener did not provide a valid callback function!')
    }

    /** get current mode (design/analysis) */
    this.getMode = function() {
        return mode;
    }
    
    /** set mode as 'design' */
    this.setModeDesign = function() {
        self.mode = 'design'
        self.updateListeners()
    }

    /** set mode as 'analysis' */
    this.setModeAnalysis = function() {
        self.mode = 'analysis'
        self.updateListeners()
    }

    /** get the list of object types that are available for creation */
    this.getTypes = function(callback, errorHandler) {
        if (typeof callback != 'function')
            return;

        jQuery.ajax({
            type: 'GET',
            url:  'types',
            dataType: 'json',
            success: callback,
            error: errorHandler
        })
    }

    /** get a new (empty) model */
    this.newModel = function(typepath,name,x,y) {
        jQuery.ajax({
            type: 'POST',
            url:  'model',
            success: self.updateListeners
        })
    }

    /** save the current project */
    this.saveProject = function() {
        jQuery.ajax({
            type: 'POST',
            url:  'project',
            success: self.updateListeners
        })
    }
    
    /** get a JSON representation of the model */
    this.getJSON = function(callback, errorHandler) {
        if (typeof callback != 'function')
            return
            
        jQuery.ajax({
            type: 'GET',
            url:  'model',
            dataType: 'json',
            success: callback,
            error: errorHandler
        })
    }

    /** get a JSON representation the specified object in the model */
    this.getObject = function(pathname, callback, errorHandler) {
        if (typeof callback != 'function')
            return

        var obj = self.getJSON()

        if (pathname.length >0) {
            var tokens = pathname.split('.'),
                len=tokens.length
            for (i=0;i<len;i++) {
                if (typeof obj[tokens[i]] !== "undefined")
                    obj = obj[tokens[i]]
                else    // may be under py/state
                    obj = obj["py/state"][tokens[i]]
            }
        }
        
        callback(obj)
    }
    
    /** get  hierarchical list of components*/
    this.getComponents = function(callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'components',
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            })
        }
    }
    
    /** get  attributes of a component*/
    this.getComponent = function(name,callback,errorHandler) {
        if (typeof callback != 'function')
            return
        else {
            jQuery.ajax({
                type: 'GET',
                url:  'component/'+name,
                dataType: 'json',
                data: {},
                success: callback,
                error: errorHandler
            })
        }
    }
    
    /** add an object of the specified type & name to the model (at x,y) */
    this.addComponent = function(typepath,name,x,y) {
        if (typeof(x) !== 'number')  x = 1
        if (typeof(y) !== 'number')  y = 1
        
        jQuery.ajax({
            type: 'POST',
            url:  'component/'+name,
            data: {'type': typepath, 'x': x, 'y': y },
            success: self.updateListeners
        })
    }

    /** issue the specified command against the model */
    this.issueCommand = function(cmd, callback, errorHandler) {
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'command',
            data: { 'command': cmd },
            success: function(txt) { 
                        if (typeof callback == 'function') {
                            callback(txt)
                        };
                        self.updateListeners()
                     },
            error: errorHandler
        })
    }

    /** get any queued output from the model */
    this.getOutput = function(callback, errorHandler) {
        jQuery.ajax({
            url: 'output',
            success: function(text) { 
                        if (typeof callback == 'function') {
                            callback(text)
                        };
                     },
            error: errorHandler
        })
    }

    /** set the working directory of the model */
    this.setWD = function(folder) {
        jQuery.ajax({
            type: 'POST',
            url:  'cwd',
            data: { 'folder': folder },
            success: self.updateListeners
        })
    }

    /** get the working directory of the model */
    this.getWD = function() {
        jQuery.ajax({
            type: 'GET',
            url:  'cwd',
            success: function(folder) { return folder }
        })
    }

    /** get a recursize file listing of the model working directory (as JSON) */
    this.getFiles = function(callback, errorHandler) {
        if (typeof callback != 'function')
            return

        jQuery.ajax({
            type: 'GET',
            url:  'files',
            dataType: 'json',
            data: {},
            success: callback,
            error: errorHandler
        })
    }

    /** get the contents of the specified file */
    this.getFile = function(filepath, callback, errorHandler) {
        if (typeof callback != 'function')
            return;

        jQuery.ajax({
            type: 'GET',
            url:  'file'+filepath.replace(/\\/g,'/'),
            dataType: 'text',
            success: callback,
            error: errorHandler
        })
    }

    /** set the contents of the specified file */
    this.setFile = function(filepath, contents, errorHandler) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+filepath.replace(/\\/g,'/'),
            data: { 'contents': contents},
            success: self.updateListeners,
            error: errorHandler
        })
    }

    /** create a new folder in the model working directory with the specified path */
    this.createFolder = function(folderpath) {
        jQuery.ajax({
            type: 'POST',
            url:  'file/'+folderpath.replace(/\\/g,'/'),
            data: { 'isFolder': true},
            success: self.updateListeners
        })
    }

    /** create a new file in the model working directory with the specified path  */
    this.newFile = function(folderpath) {
        openmdao.Util.promptForName(function(name) {
            if (folderpath)
                name = folderpath+'/'+name
            var contents = '"""\n   '+name+'\n"""\n\n'
            self.setFile(name,contents)
        })
    }

    /** prompt for name & create a new folder */
    this.newFolder = function(folderpath) {
        debug.info("model.newFolder folderpath="+folderpath)
        openmdao.Util.promptForName(function(name) {
            if (folderpath)
                name = folderpath+'/'+name
            self.createFolder(name,self.updateListeners)
        })
    }

    /** upload a file to the model working directory */
    this.uploadFile = function() {
        // TODO: make this an AJAX call so we can updateListeners afterwards
        openmdao.Util.popupWindow('/upload','Add File',150,400);
    }

    /** delete the file in the model working directory with the specified path */
    this.removeFile = function(filepath) {
        jQuery.ajax({
            type: 'DELETE',
            url:  'file'+filepath.replace(/\\/g,'/'),
            data: { 'file': filepath },
            success: self.updateListeners
        })
    }
    
    /** import the contents of the specified file into the model */
    this.importFile = function(filepath) {
        // change path to package notation and import
        var path = filepath.replace(/.py/g,'').
                            replace(/\\/g,'.').
                            replace(/\//g,'.')
        self.issueCommand("from "+path+" import *")
        self.updateListeners
    }

    /** execute the specified file */
    this.runModel = function() {
        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { },
            success: self.updateListeners
        })
    }
    
    /** execute the specified file */
    this.execFile = function(filepath) {
        // convert to relative path with forward slashes
        var path = filepath.replace(/\\/g,'/')
        if (path[0] == '/')
            path = path.substring(1,path.length)

        // make the call
        jQuery.ajax({
            type: 'POST',
            url:  'exec',
            data: { 'filename': path },
            success: self.updateListeners
        })
    }

    /** exit the model */
    this.exit = function() {
        jQuery.ajax({
            type: 'POST',
            url: 'exit',
        })
    }
    
}



