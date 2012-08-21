
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.EnumEditor = function(args){
    this.init(args)
}

openmdao.Util.inherit(openmdao.EnumEditor, openmdao.CellEditor)

openmdao.EnumEditor.prototype.init = function(args){
    $select;
    value = args.item.value
    debug.info(value)

    var buildSelect = function(enumValues){
        var options
        for(i=0; i<enumValues.length; i++)
        {
            options = options + buildOption(enumValues[i])
        }
        
        return "<select>" + options + "</select>"
    }

    var buildOption = function(option){
        return "<option>" + option + "</option>"
    }
    this.superClass.init.call(this,args)
}


openmdao.EnumEditor.prototype.destroy = function(){

}


openmdao.EnumEditor.prototype.focus = function(){

}


openmdao.EnumEditor.prototype.isValueChanged = function(){
    return false
}
openmdao.EnumEditor.prototype.serializeValue = function(){
    return ""
}

openmdao.EnumEditor.prototype.loadValue = function(item){

}

openmdao.EnumEditor.prototype.applyValue = function(item,state){

}

openmdao.EnumEditor.prototype.validate = function(){
    return { valid: false, msg: "This field is required" } 
}

openmdao.EnumEditor.prototype.hide = function(){

}

openmdao.EnumEditor.prototype.show = function(){

}

openmdao.EnumEditor.prototype.position = function(cellbox){

}
