import sys
import os

import numpy as np
from optparse import OptionParser

from enthought.traits.api import on_trait_change
from openmdao.main.api import Component, ExprEvaluator
from openmdao.lib.api import Str
from openmdao.util.plot import cmdlineXYplot


if __name__ == '__main__':
    cmdlineXYplot()
    #parser = OptionParser()
    #parser.add_option("-x", "", action="store", type="string", dest="xnames",
                      #help="names of x variables")
    #parser.add_option("-y", "", action="store", type="string", dest="ynames",
                      #help="names of y variables")
    #parser.add_option("-d", "--dbfile", action="store", type="string", dest="dbname",
                      #help="database filename")
    #parser.add_option("-t", "--title", action="store", type="string", dest="title",
                      #help="plot title",)
    #parser.add_option("", "--xlabel", action="store", type="string", dest="xlabel",
                      #help="x axis label")
    #parser.add_option("", "--ylabel", action="store", type="string", dest="ylabel",
                      #help="y axis label")
    #parser.add_option("-g", "--grid", action="store_true", dest="grid",
                      #help="makes grid visible")
    #parser.add_option("", "--cases", action="store", type="string", dest="case_sql",
                      #help="sql syntax to select certain cases")
    #parser.add_option("", "--vars", action="store", type="string", dest="var_sql",
                      #help="sql syntax to select certain vars")

    #(options, args) = parser.parse_args(sys.argv[1:])
    
    #if len(args) > 0 or not options.ynames or not options.dbname:
        #parser.print_help()
        #sys.exit(-1)
    
    #xs = options.xnames.split(',')
    #ys = options.ynames.split(',')
    
    #if len(xs) > 1 and len(xs) != len(ys):
        #print "Number of x variables doesn't match number of y variables."
        #sys.exit(-1)
    
    #displayXY(options.dbname, xs, ys, options.case_sql, options.var_sql, 
              #title=options.title, grid=options.grid, xlabel=options.xlabel,
              #ylabel=options.ylabel)
    
