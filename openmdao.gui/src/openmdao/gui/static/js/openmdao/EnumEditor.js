
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.EnumEditor = function(args){
    this.init(args)
}

openmdao.Util.inherit(openmdao.EnumEditor, openmdao.CellEditor)

openmdao.EnumEditor.prototype.init = function(args){
    $select;
    value = args.item.value
    debug.info(value)

    var buildSelect = function(values){
        return "<select>" + buildOptions(values) + "</select>"
    }

    var buildOption = function(value){
        return "<option>" + option + "</option>"
    }

    var buildOptions = function(values){
        var options = ""
        for(i=0; i<options.length; i++)
        {
            options = options + buildOption(enumValues[i])
        }
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
