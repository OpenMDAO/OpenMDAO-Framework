
import sys
import os
import atexit
from optparse import OptionParser

from openmdao.devtools.utils import fabric_cleanup

def _push_dist(dist, destination='~', py='python2.7'):
    """Take a directory containing release files (openmdao package
    distributions, install scripts, etc., and place the files in the
    proper locations on the server.

    dist: str
        pathname of distribution

    destination: str
        the location where the release files are to be placed on the server

    py: str
        python version to use to build index files
    """
    # push new distribs to the server
    put(dist, '%s/dists/%s' % (destination, dist))
    run('chmod 644 %s/dists/%s' % (destination, dist))

    # update the index.html for the dists directory on the server
    run('cd %s/dists && %s mkegglistindex.py' % (destination, py))

def main():
    from fabric.api import run, put, settings

    atexit.register(fabric_cleanup, True)
    parser = OptionParser(usage="%prog DIST1 DIST2 ... DIST_N")
    (options, args) = parser.parse_args(sys.argv[1:])

    with settings(host_string='openmdao@web39.webfaction.com'):
        for dist in args:
            if not os.path.isfile(dist):
                print "distribution file %s not found" % dist
                continue

            _push_dist(dist)

if __name__ == '__main__':
    main()
