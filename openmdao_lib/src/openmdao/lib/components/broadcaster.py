from openmdao.main.datatypes.api import List, Str, Float, Dict

from openmdao.main.api import Component


class Broadcaster(Component):
    """Takes inputs and passes them directly to outputs
    to be broadcast out to other components."""

    names = List(Str, iotype="in", desc="Names of the variables you want to broadcast from this component.")
    types = Dict({'default': Float}, iotype="in", desc="Name/type pairs describing the variable types of each broadcast variable; "
                 "'default' name is used if no other type is set explicitly.")

    def __init__(self, names, types=None):
        """names: ListSrt, list of the variable names you would like the broadcaster to create for you. All inputs will be named with an '_in' added. Outputs will follow the name given.
        types: Dict, dictionary of the name/type pairs describing which types you would like to broadcast. If given, the name 'default' indicates the default variable type to use."""

        super(Broadcaster, self).__init__()
        self._vars = []
        if types is not None:
            self.types = types
        self.names = names

    def _types_changed(self, old, new):
        if self.names:
            self._names_changed(self.names, self.names)

    #code to create inputs and outputs when names is changed
    def _names_changed(self, old, new):
        for in_var, out_var in self._vars:
            if self.parent:
                self.parent.disconnect('.'.join([self.name, in_var]))
                self.parent.disconnect('.'.join([self.name, out_var]))
            self.remove_trait(in_var)
            self.remove_trait(out_var)
        self._vars = []
        for name in new:
            if name in self.types:
                traits = self.types[name]
            elif 'default' in self.types:
                traits = self.types['default']
            else:
                self.raise_exception('No type was provided for "%s" and no "default" type was provided. '
                'Specify at least one of these.' % name, ValueError)

            in_var = "%s_in" % name
            out_var = name
            self.add_trait(in_var, Float(iotype="in", low=-9e99, high=9e99))
            self.add_trait(out_var, Float(iotype="out"))

            self._vars.append((in_var, out_var))

    def execute(self, *args, **kwargs):
        for in_var, out_var in self._vars:
            setattr(self, out_var, getattr(self, in_var))
