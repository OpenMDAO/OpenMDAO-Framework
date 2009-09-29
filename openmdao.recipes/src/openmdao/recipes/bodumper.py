
import os
import sys
import logging
# for some reason, pprint doesn't format buildout dicts properly. I guess they
# must not be real dicts...
#import pprint

import zc.buildout

class BuildoutDumper(object):
    """A buildout recipe that dumps keys and values of buildout parts dictionaries
    to a file.
    
    The following options are supported:

    **outfile**
        name of file to contain the output
    """

    def __init__(self, buildout, name, options):
        self.name = name
        self.buildout = buildout
        self.logger = logging.getLogger(name)
        self.partsdir = buildout['buildout']['parts-directory']
        self.outfile = options.get('outfile', name+'_bodump.txt')
        self.parts = options.get('parts', None)
        if self.parts is None:
            self.parts = ['buildout']+ [x.strip() for x in buildout['buildout']['parts'].split('\n')
                                              if x.strip()]
        else:
            self.parts = [x.strip() for x in self.parts.split('\n') if x.strip()]

    def _write_file(self, fname):
        outfile = open(fname, 'w')
        try:
            for part in self.parts:
                outfile.write('    part %s:\n' % part)
                for k,v in self.buildout[part].items():
                    v = v.strip()
                    if '\n' in v:
                        v = [x.strip() for x in v.split('\n') if x.strip()]
                    outfile.write('        %s: %s\n' % (k,v))
                outfile.write('\n')
        except Exception, err:
            self.logger.error(str(err))
            raise zc.buildout.UserError('write to %s failed' % fname)
        finally:
            outfile.close()
        
    def install(self): 
        if not os.path.isdir(os.path.join(self.partsdir, self.name)):
            os.makedirs(os.path.join(self.partsdir, self.name))
            
        if os.path.isabs(self.outfile):
            fname = self.outfile
        else:
            fname = os.path.join(self.partsdir, self.name, self.outfile)
        self._write_file(fname)
        
        return [fname]

     
    update = install

