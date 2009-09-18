
import os
import sys
import logging
import StringIO
import rfc822
import fnmatch
import pprint

import zc.buildout

from pkg_resources import WorkingSet, Environment, Requirement


class MetadataCollector(object):
    """A buildout recipe that creates a file with metadata collected from all 
    dependent distribs based on the 'eggs' parameter.
    """

    def __init__(self, buildout, name, options):
        self.name = name
        self.logger = logging.getLogger(name)
        self.partsdir = buildout['buildout']['parts-directory']
        meta_names = options.get('meta_names', '')
        self.meta_names = [x.strip() for x in meta_names.split()]

        dev_egg_dir = buildout['buildout']['develop-eggs-directory']
        dev_eggs = fnmatch.filter(os.listdir(dev_egg_dir),'*.egg-link')
        # grab the first line of each dev egg link file
        self.dev_eggs = [open(os.path.join(dev_egg_dir,f),'r').readlines()[0].strip() 
                            for f in dev_eggs]
                            
        # build up a list of all egg dependencies resulting from our 'eggs' parameter
        self.eggs = set()
        env = Environment(self.dev_eggs+[buildout['buildout']['eggs-directory']])
        for egg in [x.strip() for x in options['eggs'].split()]:
            self._add_deps(self.eggs, env, WorkingSet(), Requirement.parse(egg))
            
        self.logger.debug("dependency set is: %s" % 
                             [str(dist) for dist in self.eggs])


    def _add_deps(self, deps, env, ws, req):
        """Add a dependency for the given requirement and anything the resulting
        distrib depends on.
        """
        dist = env.best_match(req, ws)
        if dist is None:
            self.logger.error('No distrib found for %s' % req)
        else:
            deps.add(dist)
            reqs = dist.requires()
            for r in reqs:
                self._add_deps(deps, env, ws, r)

    def _get_metadata(self, names):
        meta = {}
        for dist in self.eggs:
            mvalues = {}
            instr = StringIO.StringIO(dist.get_metadata('PKG-INFO'))
            message = rfc822.Message(instr)
            for name in names:
                try:
                    mvalue[name] = dist.get_metadata('EGG-INFO/%s.txt' % name)
                except:
                    mvalues[name] = message.get(name, 'UNKNOWN')
            meta[str(dist.as_requirement())] = mvalues
        return meta
    
    def _write_file(self, fname):
        outfile = open(fname, 'wb')
        metadict = {}
        try:
            pprint.pprint(self._get_metadata(self.meta_names), outfile)
        except Exception, err:
            self.logger.error(str(err))
            raise zc.buildout.UserError('write of metadata file failed')
        finally:
            outfile.close()
    
    def install(self):        
        if not os.path.isdir(os.path.join(self.partsdir,'metadata')):
            os.makedirs(os.path.join(self.partsdir,'metadata'))
            
        fname = os.path.join(self.partsdir, 'metadata','metadata.txt')
        self._write_file(fname)
        
        return [fname]

     
    update = install

