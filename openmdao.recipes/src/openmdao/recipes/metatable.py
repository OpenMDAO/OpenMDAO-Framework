
import os
import sys
import logging
import StringIO
import rfc822
import fnmatch
import pprint

import zc.buildout

from pkg_resources import WorkingSet, Environment, Requirement

from openmdao.recipes.utils import find_all_deps

class MetadataTable(object):
    """A buildout recipe that creates a file with a restructured text table of
    metadata collected from all dependent distribs based on the 'eggs' 
    parameter.  
    
    The following options are supported:

    **meta_names**
        name of metadata to pull from distributions

    **headers**
        text for the top of table columns (must be same size as meta_names)

    *data_templates* 
        basic string templates (allowing for % substitution)
        for each column (must be empty or be the same size as meta_names)

    **outfile**
        name of file to contain the generated rst table

    *max_col_width*
        maximum allowed column width (data will be truncated if it doesn't fit)
    
    *excludes*
        patterns to exclude distributions from the table. These patterns allow 
        simple * and ? wildcarding. 
    """

    def __init__(self, buildout, name, options):
        self.name = name
        self.logger = logging.getLogger(name)
        self.partsdir = buildout['buildout']['parts-directory']
        meta_names = options.get('meta_names', '')
        self.meta_names = [x.strip() for x in meta_names.split()]
        headers = options.get('headers', '')
        self.headers = [x.strip() for x in headers.split('\n')]
        data_templates = options.get('data_templates', '')
        self.data_templates = [x.strip() for x in data_templates.split('\n') if x.strip()]
        if len(self.data_templates) == 0:
            self.data_templates = ['%s' for x in self.meta_names]
        self.outfile = options.get('outfile', name+'_table.rst')
        self.max_col_width = int(options.get('max_col_width', 80))
        self.col_spacer = ' '
        excludes = options.get('excludes', '')
        self.excludes = [x.strip() for x in excludes.split('\n') if x.strip()]

        dev_egg_dir = buildout['buildout']['develop-eggs-directory']
        dev_eggs = fnmatch.filter(os.listdir(dev_egg_dir),'*.egg-link')
        # grab the first line of each dev egg link file
        self.dev_eggs = [open(os.path.join(dev_egg_dir,f),'r').readlines()[0].strip() 
                            for f in dev_eggs]
                            
        # build up a list of all egg dependencies resulting from our 'eggs' parameter
        env = Environment(self.dev_eggs+[buildout['buildout']['eggs-directory']])
        reqs = [Requirement.parse(x.strip()) for x in options['eggs'].split()]
        self.depdists = find_all_deps(reqs, env)
            
        self.logger.debug("dependency list is: %s" % 
                             [str(dist) for dist in self.depdists])

    def _get_metadata(self, names):
        meta = {}
        for dist in self.depdists:
            mvalues = {}
            instr = StringIO.StringIO(dist.get_metadata('PKG-INFO'))
            message = rfc822.Message(instr)
            for name in names:
                try:
                    mvalue[name] = dist.get_metadata('EGG-INFO/%s.txt' % name)
                except:
                    mvalues[name] = message.get(name, 'UNKNOWN')
            meta[dist.project_name] = mvalues
        return meta
    
    def _write_file(self, fname):
        outfile = open(fname, 'wb')
        
        metadict = self._get_metadata(self.meta_names)
        to_remove = set()
        for pattern in self.excludes:
            to_remove.update(fnmatch.filter(metadict.keys(), pattern))
        for rem in to_remove:
            del metadict[rem]
        for meta in metadict.values():
            for i,name in enumerate(self.meta_names):
                meta[name] = self.data_templates[i] % str(meta[name])
        # figure out sizes of table columns
        colwidths = [len(s)+1 for s in self.headers]
        for i,name in enumerate(self.meta_names):
            sz = max([len(m[name]) for m in metadict.values()])+1
            sz = min(sz, self.max_col_width)
            colwidths[i] = max(colwidths[i], sz)        
        
        try:
            # write header
            outfile.write(self._get_border_line(colwidths, char='='))
            for i,header in enumerate(self.headers):
                outfile.write(header+' '*(colwidths[i]-len(header)))
                outfile.write(self.col_spacer)
            outfile.write('\n')
            outfile.write(self._get_border_line(colwidths, char='='))
            
            # write table data
            tups = [(k,v) for k,v in metadict.items()]
            tups = sorted(tups, lambda x,y: cmp(x[0].lower(), y[0].lower()))
            for j,tup in enumerate(tups):
                for i,name in enumerate(self.meta_names):
                    outfile.write(self._get_table_cell(tup[1][name], colwidths[i]))
                    outfile.write(self.col_spacer)
                outfile.write('\n')
                if j<len(tups)-1:
                    outfile.write(self._get_border_line(colwidths, char='-'))
                
            # bottom border
            outfile.write(self._get_border_line(colwidths, char='='))
            outfile.write('\n')
        except Exception, err:
            self.logger.error(str(err))
            raise zc.buildout.UserError('write to %s failed' % fname)
        finally:
            outfile.close()
    
    def _get_border_line(self, colwidths, char):
        parts = []
        for i in range(len(self.meta_names)):
            parts.append(char*colwidths[i])
            parts.append(self.col_spacer)
        parts.append('\n')
        return ''.join(parts)
    
    def _get_table_cell(self, data, colwidth):
        return data+' '*(colwidth-len(data))
    
    def install(self): 
        if self.headers and len(self.headers) != len(self.meta_names):
            raise zc.buildout.UserError("number of headers doesn't match number of metadata items")
        if not os.path.isdir(os.path.join(self.partsdir, self.name)):
            os.makedirs(os.path.join(self.partsdir, self.name))
            
        if os.path.isabs(self.outfile):
            fname = self.outfile
        else:
            fname = os.path.join(self.partsdir, self.name, self.outfile)
        self._write_file(fname)
        
        return [fname]

     
    update = install

