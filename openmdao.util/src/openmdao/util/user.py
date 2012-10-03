
import os
import sys

def get_username():
    """ Return username for current user. """
    if sys.platform == 'win32':
        return os.environ['USERNAME']
    else:
        import pwd
        return pwd.getpwuid(os.getuid()).pw_name

