#!/usr/bin/env python

import sys
import logging

from pkg_resources import Requirement
from setuptools.package_index import PackageIndex

_logger = logging.getLogger()

_pypi = 'http://pypi.python.org/simple'

def _enable_console(level):
    """ Configure logging to receive log messages at the console. """
    global _logger
    # define a Handler which writes messages to sys.stderr
    CONSOLE = logging.StreamHandler()
    CONSOLE.setLevel(level)
    CONSOLE.setFormatter(logging.Formatter('%(message)s'))
    _logger.addHandler(CONSOLE)


def grab_distrib(req, index=None, dest='.', search_pypi=True):
    """\
Downloads a distribution from the given package index(s) based on the
given requirement string(s). Downloaded distributions are placed in the
specified destination or the current directory if no destination is
specified.  If a distribution cannot be found in the given index(s), the
Python Package Index will be searched as a last resort unless 
search_pypi is False.  This does NOT install the distribution.
    """
    
    # allow multiple package indexes to be specified
    if index is None:
        index = []
    elif isinstance(index, basestring):
        index = [index]
    # else just assume it's some iterator of indexes
        
    # add PyPI as last place to search if it wasn't already specified
    if search_pypi and _pypi not in index and (_pypi+'/') not in index:
        index.append(_pypi)
    
    # allow specification of single or multiple requirements    
    if isinstance(req, basestring):
        reqs = [Requirement.parse(req)]
    elif isinstance(req, Requirement):
        reqs = [req]
    else:
        reqs = []
        for rr in req:
            if isinstance(rr, basestring):
                reqs.append(Requirement.parse(rr))
            elif isinstance(rr, Requirement):
                reqs.append(rr)
            else:
                raise TypeError("supplied requirement arg must be a string"+
                                " or a Requirement, but given type is %s" %
                                type(rr))
        
    index_list = [PackageIndex(idx,search_path=[]) for idx in index]
    
    for req in reqs:
        fetched = None
        for idx in index_list:
            _logger.info('Looking for %s at package index %s' % (req, idx.index_url))
            fetched = idx.download(req, dest)
            if fetched:
                _logger.info('    %s successfully downloaded' % fetched)
                break
        else:
            _logger.error("couldn't find distrib for %s" % req)
        
    return fetched
    

if __name__ == '__main__':
    from optparse import OptionParser
    
    usage = "%prog [options] req(s)"
    parser = OptionParser(usage=usage, description=grab_distrib.__doc__)
    parser.add_option("-i", "--index", action="append", type="string", dest="index",
                      help="package index url (separate -i for each one)", default=[])
    parser.add_option("-d", "--dest", action="store", type="string", dest="dest",
                      help="destination directory", default='.')
    parser.add_option("-q", "--quiet", action="store_true", dest="quiet",
                      help="no output")
    parser.add_option("--nopypi", action="store_true", dest="nopypi",
                      help="do not search PyPI")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    if len(args) < 1:
        parser.print_help()
        sys.exit(1)
        
    if options.quiet:
        loglevel = logging.CRITICAL
    else:
        loglevel = logging.INFO
    
    _logger.setLevel(loglevel)
    _enable_console(loglevel)
        
    grab_distrib(req=args, index=options.index, dest=options.dest, 
                 search_pypi=not options.nopypi)
    
    
grab_distrib.__doc__ += """
Requirements may be supplied as strings or as Requirement objects.
"""
