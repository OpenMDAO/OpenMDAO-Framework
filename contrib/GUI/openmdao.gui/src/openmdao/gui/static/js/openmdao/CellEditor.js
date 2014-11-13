
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ;

/*
* If you intend to create a custom slickgrid editor to be 
* used for providing an interface for editing a specific data type,
* please be sure to read the comments from here to the end of
* the file.
*
* The comments will hopefully explain how to properly set up your
* editor, as well as explain some the tools used in the process.
*/

/*
* This the constructor for a default custom datatype editor.
* All custom datatype editors should inherit CellEditor.
*/    
openmdao.CellEditor = function(args){
        this.init(args)
}

/* CellEditor's init method varies slightly from the default editors provided
* by slickgrid. It saves the args object as a property to be available
* for reference by other CellEditor functions. Thus, args should be available
* to any privileged or public function.
*/
openmdao.CellEditor.prototype.init = function(args){
    this.args = args
}

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
openmdao.CellEditor.prototype.destroy = function(){

}

openmdao.CellEditor.prototype.focus = function(){

}

openmdao.CellEditor.prototype.isValueChanged = function(){ 
    return false; 
}

openmdao.CellEditor.prototype.serializeValue = function(){ 
    return ""; 
}

openmdao.CellEditor.prototype.loadValue = function(item){

}

openmdao.CellEditor.prototype.applyValue = function(item, state){

}

openmdao.CellEditor.prototype.validate = function(){ 
    return { valid: false, msg: "This field is required" } 
}

/*
* The following functions are optional functions for 
* custom slickgrid editors:
*
*      hide
*      show
*      position 
*/
openmdao.CellEditor.prototype.hide = function(){
}

openmdao.CellEditor.prototype.show = function(){
}

openmdao.CellEditor.prototype.position = function(cellbox){

}
