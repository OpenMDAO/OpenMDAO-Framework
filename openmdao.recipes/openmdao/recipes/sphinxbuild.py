
import os
import os.path
import sys
import stat
from subprocess import check_call

script_template = """#!%(python)s

from subprocess import Popen

# run this without blocking
Popen(["%(browser)s", r"%(index)s"])

"""
    

class SphinxBuild(object):
    """Build Sphinx documentation and create a script to bring up the
    documentation in a browser. This is specific to the OpenMDAO Sphinx docs and
    is not a  general purpose Sphinx building recipe.
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        self.branchdir = os.path.split(buildout['buildout']['directory'])[0]
        self.executable = buildout['buildout']['executable']
        
        self.docdir = options.get('doc_dir') or 'docs'
        self.builddir = options.get('build_dir') or '_build' 
        self.browser = options.get('browser') or 'firefox'
        
        #TODO: add format option so that buildout user could specify building
        #      of html, latex, or both
              

               
    def install(self):
        
        startdir = os.getcwd()
        if not os.path.isdir(self.docdir):
            self.docdir = os.path.join(self.branchdir, self.docdir)
        if not os.path.isdir(self.docdir):
            raise RuntimeError('doc directory '+self.docdir+' not found')
        os.chdir(self.docdir)
        
        # build the docs using Sphinx (just run the Makefile)
        try:
            check_call(['make','html'])
        finally:
            os.chdir(startdir)
        
        # create a bin/docs script
        scriptname = os.path.join(self.buildout['buildout']['directory'],
                                  'bin','docs')
        script = open(scriptname, 'w')
        
        idxpath = os.path.join(self.branchdir, self.docdir, self.builddir,
                               'html','index.html')
        script.write(script_template % dict(python=self.executable,
                                            browser=self.browser,
                                            index=idxpath))
        script.close()
        os.chmod(scriptname, 
                 stat.S_IREAD|stat.S_IWRITE|stat.S_IEXEC|
                 os.stat(scriptname).st_mode)
        
        return [scriptname]
        
    update = install

