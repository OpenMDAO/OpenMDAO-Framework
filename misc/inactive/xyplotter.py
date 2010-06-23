#public symbols
__all__ = ['XYplotter']

import sys
import os

import numpy as np

from enthought.traits.api import on_trait_change
from openmdao.main.api import Component, ExprEvaluator
from openmdao.lib.api import Str

if 'matplotlib' not in sys.modules:
    import matplotlib
    if sys.platform == 'darwin':
        matplotlib.use('MacOSX')
    else:
        matplotlib.use('TkAgg')
      
import matplotlib.pyplot as plt

plt.ion()  # interactive mode

_colors = set([
    'b',
    'g',
    'r',
    'c',
    'm',
    'y',
    'k',
    'w'
    ])

_styles = set([
    '-',
    '--',
    '-.',
    ':',
    '.',
    ',',
    'o',
    'v',
    '^',
    '<',
    '>',
    '1',
    '2',
    '3',
    '4',
    's',
    'p',
    '*',
    'h',
    'H',
    '+',
    'x',
    'D',
    'd',
    '|',
    '_'
    ])

class XYplotter(Component):
    """Displays simple XY plots. It collects X and Y values while
    running as part of a workflow, but does not display anything until its
    display() function is called."""

    title = Str(iotype='in', desc='the title of the plot')
    
    def __init__(self, doc=None):
        super(XYplotter, self).__init__(doc)
        self._fig = plt.figure()
        self._ax = self._fig.add_subplot(111)
        self.lines = []

    def add_line(self, x=None, y=None, line_type=None, label=None):
        if y is None:
            self.raise_exception('You must supply a Y axis expression')
        if x:
            x = ExprEvaluator(x, self.parent)
        else:
            x = None
        y = ExprEvaluator(y, self.parent)
        self.lines.append([x, y, [], [], line_type, label])
        
    def clear(self, data_only=False):
        if data_only:
            for line in self.lines:
                line[2] = []
                line[3] = []
        else:
            self.lines = []
    
    def _pre_execute (self):
        """For now, just force an execution on every run()."""
        self._call_execute = True
        super(XYplotter, self)._pre_execute()
    
    def execute(self):
        try:
            for line in self.lines:
                if line[0]:   # if x is defined
                    line[2].append(line[0].evaluate())
                line[3].append(line[1].evaluate())
        except Exception as err:
            self.raise_exception(str(err), type(err))

    def display(self):
        for line in self.lines:
            args = []
            kwargs = {}
            if line[0]:
                args.append(line[2])
            args.append(line[3])
            if line[4]:
                args.append(line[4])
            if line[5]:
                kwargs['label'] = '%s' % line[5]
            else:
                kwargs['label'] = '%s' % line[1]
            plt.plot(*args, **kwargs)
        plt.legend()
        plt.title(self.title)
        plt.show()

    #@on_trait_change('x') 
    #def _expr_changed(self, obj, name, old, new):
        #expr = getattr(obj, name)
        #try:
            ## force checking for existence of vars referenced in expression
            #expr.refs_valid()  
        #except (AttributeError, RuntimeError), err:
            #msg = "invalid value '%s' in Expression '%s': %s"
            #self.raise_exception( msg % (str(expr), name, err), TraitError)
        
    #@on_trait_change('ylist') 
    #def _exprlist_changed(self, obj, name, old, new):
        #self._ys = []
        #exprevals = getattr(obj, name)
        #for i, expr in enumerate(exprevals):
            #try:
                ## force checking for existence of vars referenced in expression
                #expr.refs_valid()  
            #except (AttributeError, RuntimeError), err:
                #msg = "invalid value '%s' for Expression '%s[%d]': %s"
                #self.raise_exception( msg % \
                    #(str(expr), name, i, err), TraitError)

        