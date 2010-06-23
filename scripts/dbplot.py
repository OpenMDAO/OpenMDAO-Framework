import sys
import os
import sqlite3

import numpy as np
from optparse import OptionParser

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

lines = []
typeidx = 0


def get_data(dbname, varnames, case_selector=None, var_selector=None):
    connection = sqlite3.connect(dbname)
    
    sql = ["SELECT * FROM cases"]
    if case_selector:
        sql.append("WHERE %s" % case_selector)
        
    casecur = connection.cursor()
    casecur.execute(' '.join(sql))
    
    sql = ["SELECT name, value, entry from casevars WHERE case_id=%s"]
    for i,name in enumerate(varnames):
        if i==0:
            sql.append("AND (")
        else:
            sql.append("OR")
        sql.append("name='%s'" % name)
    if i>0: sql.append(")")
    
    if var_selector:
        sql.append("AND %s" % var_selector)
    combined = ' '.join(sql)
    
    varcur = connection.cursor()
    
    vardict = {}
    for case_id,name,msg,retries,model_id,timeEnter in casecur:
        varcur.execute(combined % case_id)
        for vname, value, entry in varcur:
            if entry:
                vname = "vname%s" % entry
            dval = vardict.setdefault(vname, [])
            dval.append(value)
            print vname,'=',value
            
    return vardict


def displayXY(title=''):

    fig = plt.figure()
    fig.add_subplot(111)
    
    for line in lines:
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
    plt.title(title)
    plt.show()


def add_line(x, xvals, y, yvals, line_type=None, label=None):
    global lines, typeidx
    if line_type is None:
        line_type = '%s%s' % (_colors[typeidx],_styles[typeidx])
        typeidx += 1
    lines.append((x, y, xvals, yvals, line_type, label))
        
if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-x", "", action="store", type="string", dest="xnames",
                      help="names of x variables")
    parser.add_option("-y", "", action="store", type="string", dest="ynames",
                      help="names of y variables")
    parser.add_option("-d", "--dbfile", action="store", type="string", dest="dbname",
                      help="database filename")
    parser.add_option("-t", "--title", action="store", type="string", dest="title",
                      help="plot title")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    if len(args) > 0 or not options.xnames or not options.ynames or not options.dbname:
        parser.print_help()
        sys.exit(-1)
    
    xs = options.xnames.split(',')
    ys = options.ynames.split(',')
    
    if len(xs) > 1 and len(xs) != len(ys):
        print "Number of x variables doesn't match number of y variables."
        sys.exit(-1)
    
    vdict = get_data(options.dbname, xs+ys)
    
    for i,yname in enumerate(ys):
        if len(xs) == 1:
            xname = xs[0]
        else:
            xname = xs[i]
        add_line(xname, yname, vdict[xname], vdict[yname])
        
    displayXY(title=options.title)
    
    #import pprint
    #pprint.pprint(vdict)
