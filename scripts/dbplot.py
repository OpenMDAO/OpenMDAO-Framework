import sys
import os

import numpy as np
from optparse import OptionParser

from enthought.traits.api import on_trait_change
from openmdao.main.api import Component, ExprEvaluator
from openmdao.lib.api import Str
from openmdao.util.plot import get_lines

if 'matplotlib' not in sys.modules:
    import matplotlib
    if sys.platform == 'darwin':
        matplotlib.use('MacOSX')
    else:
        matplotlib.use('TkAgg')
      
import matplotlib.pyplot as plt

#plt.ion()  # interactive mode

_colors = [
    'b',
    'g',
    'r',
    'c',
    'm',
    'y',
    'k',
    'w'
    ]

_styles = [
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
    ]


def displayXY(lines, title='', grid=False, xlabel='', ylabel=''):

    fig = plt.figure()
    fig.add_subplot(111)
    
    for line in lines:
        args = []
        kwargs = {}
        args.append(line[0])
        args.append(line[1])
        #if line[5]:
            #kwargs['label'] = '%s' % line[5]
        #else:
            #kwargs['label'] = '%s' % line[0]
        plt.plot(*args, **kwargs)
    if grid:
        plt.grid(True)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.legend()
    plt.title(title)
    plt.show()

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-x", "", action="store", type="string", dest="xnames",
                      help="names of x variables")
    parser.add_option("-y", "", action="store", type="string", dest="ynames",
                      help="names of y variables")
    parser.add_option("-d", "--dbfile", action="store", type="string", dest="dbname",
                      help="database filename")
    parser.add_option("-t", "--title", action="store", type="string", dest="title",
                      help="plot title",)
    parser.add_option("", "--xlabel", action="store", type="string", dest="xlabel",
                      help="x axis label")
    parser.add_option("", "--ylabel", action="store", type="string", dest="ylabel",
                      help="y axis label")
    parser.add_option("-g", "--grid", action="store_true", dest="grid",
                      help="makes grid visible")
    parser.add_option("", "--cases", action="store", type="string", dest="case_sql",
                      help="sql syntax to select certain cases")
    parser.add_option("", "--vars", action="store", type="string", dest="var_sql",
                      help="sql syntax to select certain vars")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    if len(args) > 0 or not options.ynames or not options.dbname:
        parser.print_help()
        sys.exit(-1)
    
    xs = options.xnames.split(',')
    ys = options.ynames.split(',')
    
    if len(xs) > 1 and len(xs) != len(ys):
        print "Number of x variables doesn't match number of y variables."
        sys.exit(-1)
    
    displayXY(get_lines(options.dbname, xs, ys, options.case_sql, options.var_sql), 
              title=options.title, grid=options.grid, xlabel=options.xlabel,
              ylabel=options.ylabel)
    
