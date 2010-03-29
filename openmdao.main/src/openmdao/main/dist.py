"""
distutils/setuptools related stuff
"""

import os
import pprint

def assert_dict_or_none(dist, attr, value):
    """Verify that value is a dict or None"""
    if not isinstance(value, dict) and value is not None:
        raise DistutilsSetupError(
            "%r must be a dict or None (got %r)" % (attr, value)
            )
    
    
def write_openmdao_meta(cmd, basename, filename):
    """Write *openmdao_metadata* setup arg to a file.
    Currently it's just pretty-printed, but this may change...
    """
    argname = os.path.splitext(basename)[0]
    value = getattr(cmd.distribution, argname, None)
    if value is not None:
        out = pprint.pformat(value)+'\n'
    cmd.write_or_delete_file(argname, filename, out)


