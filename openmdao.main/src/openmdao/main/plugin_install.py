from subprocess import call
from urllib2 import urlopen
from json import loads


def plugin_install():
    """Routine to go and grab all of the publicly available
      plugins from the OpenMDAO-Plugins organization, install each
      plugin to the current virtualenv, and then run the tests on
      that plugin before installing the next. 
    """
    plugin_url = 'https://api.github.com/orgs/OpenMDAO-Plugins/repos?type=public'
       
    github_plugins = []       
    plugin_page = urlopen(plugin_url)
    for line in plugin_page.fp:
        text = loads(line)
        for item in sorted(text):
            github_plugins.append(item['name'])

    for plugin in github_plugins:
        try:
            print "Installing plugin: ", plugin
            if not call(['plugin', 'install', '--github', plugin]):
                raise RuntimeError("Installation of plugin %s failed." % plugin)
            else:
                print "Installation of ", plugin, " succeeded."
            
            print "Testing plugin: ", plugin    
            if not call(['openmdao', 'test', plugin]):
                raise RuntimeError("Testung of plugin %s failed." % plugin)
            else:
                print "Testing of plugin ", plugin, "succeeded."
                
        finally:
            print "Next plugin..."
            

if __name__ == '__main__':
    plugin_install()