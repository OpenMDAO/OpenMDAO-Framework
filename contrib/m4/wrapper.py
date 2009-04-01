"""
Wrap OpenMDAO component as M4 Model.
"""

class M4Model(object):
    """ Wrap OpenMDAO component as M4 Model. """

    def __init__(self, component, inputs, outputs):
        self.component = component
        self.input_vars = inputs
        self.output_vars = outputs

    def __call__(self, input_vec, i_th_func):
        return self.RunModel(input_vec, i_th_func)

    def RunModel(self, input_vec, i_th_func):
        """ Run component with given inputs, returning i_th output. """
        for i, value in enumerate(input_vec):
            self.component.set(self.input_vars[i], value)
        self.component.run()
        return self.component.get(self.output_vars[i_th_func])

