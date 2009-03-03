
import os
import os.path
import sys
import stat
from subprocess import check_call

script_template = """#!%(python)s

import webbrowser

wb = webbrowser.get("%(browser)s")
wb.open(r"%(index)s")

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
        self.interpreter = os.path.join(buildout['buildout']['bin-directory'], 'python')
        self.executable = buildout['buildout']['executable']
        
        self.docdir = options.get('doc_dir') or 'docs'
        self.builddir = options.get('build_dir') or '_build' 
        self.builder = options.get('build_script') or os.path.join(self.branchdir,
                                                                   'docs',
                                                                   'python-scripts',
                                                                   'sphinx-build')
        
               
    def install(self):
        
        startdir = os.getcwd()
        if not os.path.isdir(self.docdir):
            self.docdir = os.path.join(self.branchdir, self.docdir)
        if not os.path.isdir(self.docdir):
            raise RuntimeError('doc directory '+self.docdir+' not found')
        os.chdir(self.docdir)
        
        # make necessary directories if they aren't already there
        if not os.path.isdir(os.path.join(self.builddir,'html')):
            os.makedirs(os.path.join(self.builddir,'html'))
        if not os.path.isdir(os.path.join(self.builddir,'doctrees')):
            os.makedirs(os.path.join(self.builddir,'doctrees'))
        if not os.path.isdir('generated_images'):
            os.makedirs('generated_images')
            
        # build the docs using Sphinx
        try:
            sys.path[0:0] = [os.path.abspath('python-scripts')]
#            execfile(os.path.join('python-scripts','rebuild.py'))
            check_call([self.interpreter, self.builder, '-b', 'html', 
                        '-d', os.path.join(self.builddir,'doctrees'), 
                        '.', os.path.join(self.builddir,'html')])
        finally:
            os.chdir(startdir)
        
        # create a bin/docs script
        if sys.platform == 'win32':
            scriptname = os.path.join(self.buildout['buildout']['directory'],
                                     'bin','docs.py')
            bat = open(os.path.join(self.buildout['buildout']['directory'],
                                    'bin','docs.bat'), 'w')
            bat.write("@echo off\npython %s"%(scriptname,))
            bat.close()
            browser = self.options.get('browser') or 'windows-default'
        else:
            scriptname = os.path.join(self.buildout['buildout']['directory'],
                                     'bin','docs')
            browser = self.options.get('browser') or 'firefox'
        script = open(scriptname, 'w')
        
        idxpath = os.path.join(self.branchdir, self.docdir, self.builddir,
                               'html','index.html')
        script.write(script_template % dict(python=self.executable,
                                            browser=browser,
                                            index=idxpath))
        script.close()
        os.chmod(scriptname, 
                 stat.S_IREAD|stat.S_IWRITE|stat.S_IEXEC|
                 os.stat(scriptname).st_mode)
        
        return [scriptname]
        
    update = install

