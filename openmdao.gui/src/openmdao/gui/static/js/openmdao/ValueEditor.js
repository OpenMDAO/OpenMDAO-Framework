
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ;

/*
* This is the constructor for the editor to be used
* when defining a slickgrid. ValueEditor uses the
* delegation pattern by resolving which datatype editor
* to use based off of the 'type' attribute, then calls
* the appropriate method of the datatype editor.
*
* For more information on the delegation patttern,
* visit http://en.wikipedia.org/wiki/Delegation_pattern
*/
openmdao.ValueEditor = (function(){

    var editors = {}
    var unregistered = {}

    function constructorFn(args){
        this.init(args)
    }

    /*
     * Make the constructor of ValueEditor inherit CellEditor
     */
    openmdao.Util.inherit(constructorFn, openmdao.CellEditor)

    /*
    * The init function calls CellEditor's init method to initialize
    * args. This makes the parameter accessible to the rest of 
    * overriden functions. The correct data type editor is also
    * set. If the editor is not in editors, ValueEditor defaults
    * to using TextCellEditor.
    */
    constructorFn.prototype.init = function(args){
        var dataType = args.item.type
        if(constructorFn.isRegistered(dataType)){
            this.superClass.init.call(this, args)
            this.editor = new editors[dataType](this.args) 
        }

        else{
            unregistered[dataType] = true
        }
    }

    /*
    * Editors is a private static property of ValueEditor.
    * It is an object that maps a data type to a CellEditor 
    * and is used by ValueEditor to delegate function
    * calls. You must register your data type editor 
    * using the addEditor method.
    *
    * name: String representation of name of editor
    * editor: the editor to register
    *
    */
    constructorFn.registerEditor = function(name, constructor){
        editors[name] = constructor
    }

    constructorFn.isRegistered = function(name){
        return (name in editors)
    }

    return constructorFn

})();

openmdao.ValueEditor.prototype.destroy = function(){
    if(this.editor)
    {
        this.editor.destroy() 
    }
}

openmdao.ValueEditor.prototype.focus = function(){
    this.editor.focus()
}

openmdao.ValueEditor.prototype.isValueChanged = function(){ 
    return this.editor.isValueChanged(); 
}

openmdao.ValueEditor.prototype.serializeValue = function(){ 
    return this.editor.serializeValue(); 
}

openmdao.ValueEditor.prototype.loadValue = function(item){
    if(!this.editor){
        this.destroy()
    }

    this.editor.loadValue(item)
}

openmdao.ValueEditor.prototype.applyValue = function(item, state){
    this.editor.applyValue(item, state)
}

openmdao.ValueEditor.prototype.validate = function(){ 
    return this.editor.validate()
}

/*
* If the function is optional, the delegated call is 
* wrapped in a try/catch block. If the execution is 
* not successful, ValueEditor delegates the work
* to it's parent, CellEditor. 
*/
openmdao.ValueEditor.prototype.hide = function(){
    try{
        this.editor.hide()
    }
    catch(err){
        //TODO: Should report something to the user maybe
        //Default to using hide method of CellEditor
        this.superClass.hide.call(this)
    }
}

openmdao.ValueEditor.prototype.show = function(){
    try{
        this.editor.show()
    }
    catch(err){
        //TODO: Should report something to the user maybe
        //Default to using show method of CellEditor
        this.superClass.show.call(this)
    }
}

openmdao.ValueEditor.prototype.position = function(cellbox){
    try{
        this.editor.position(cellbox)
    }
    catch(err){
        //TODO: Should report something to the user maybe
        //Default to using position method of CellEditor
        this.superClass.position.call(this, cellbox)
    }
}
