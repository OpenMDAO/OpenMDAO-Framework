"""OpenMDAO Command Line Interface stuff"""

import os
import sys
import webbrowser
from ConfigParser import SafeConfigParser
from argparse import ArgumentParser
from subprocess import call, check_call

from openmdao.main.plugin import plugin_docs, plugin_build_docs
from openmdao.test.testing import read_config, filter_config, run_openmdao_suite

def list_testhosts(options):
    hostlist, config = read_config(options)
    for host in filter_config(hostlist, config, options):
        plat = config.get(host, 'platform')
        py = config.get(host, 'py')
        print host.ljust(30), plat.ljust(10), py

def test_openmdao(options, args=None):
    run_openmdao_suite(sys.argv[1:])
    
def _get_openmdao_parser():
    """Sets up the plugin arg parser and all of its subcommand parsers."""
    
    top_parser = ArgumentParser()
    subparsers = top_parser.add_subparsers(title='subcommands')
    
    parser = subparsers.add_parser('list_testhosts', 
                                   description="build sphinx doc files")
    parser.add_argument("-c", "--config", action='store', dest='cfg', metavar='CONFIG',
                        default='~/.openmdao/testhosts.cfg',
                        help="Path of config file where info for remote testing/building hosts is located")
    parser.add_argument("--filter", action='append', dest='filters', 
                        default=[],
                        help="boolean expression to filter hosts")
    parser.add_argument("--host", action='append', dest='hosts', metavar='HOST',
                        default=[],
                        help="Select host from config file to run on. "
                             "To run on multiple hosts, use multiple --host args")
    parser.set_defaults(func=list_testhosts)

    parser = subparsers.add_parser('docs', 
                                   description="display docs for a plugin")
    parser.add_argument('plugin_dist_name', nargs='?',
                        help='name of plugin distribution or class')
    parser.add_argument("-b", "--browser", action="store", type=str, 
                        dest='browser', choices=webbrowser._browsers.keys(),
                        help="browser name")
    parser.set_defaults(func=plugin_docs)
    
    parser = subparsers.add_parser('test', 
                                   description="run the OpenMDAO test suite")
    parser.set_defaults(func=test_openmdao)
        
    try:
        from openmdao.devtools.build_docs import build_docs, test_docs
        from openmdao.devtools.push_docs import push_docs
        
        parser = subparsers.add_parser('build_docs', 
                                       description="build OpenMDAO docs")
        parser.add_argument("-v", "--version", action="store", type=str, 
                            dest="version",
                            help="the OpenMDAO version")
        parser.set_defaults(func=build_docs)
        
        parser = subparsers.add_parser('test_docs', 
                                       description="test the OpenMDAO docs")
        parser.set_defaults(func=test_docs)
        
        parser = subparsers.add_parser('push_docs', 
                                       description="push OpenMDAO dev docs to a server")
        parser.add_argument('host', help='host to push docs to')
        parser.add_argument("-d", "--destination", action="store", type=str, 
                            dest="docdir", default='downloads',
                            help="directory where dev_docs directory will be placed")
        parser.add_argument("-n", "--nodocbuild", action="store_true", 
                            dest="nodocbuild",
                            help="used for testing. The docs will not be rebuilt if they already exist")
        parser.set_defaults(func=push_docs)
    except ImportError:
        pass
    
    return top_parser


def openmdao():
    parser = _get_openmdao_parser()
    options, args = parser.parse_known_args()
    options.func(options, args)
    
if __name__ == '__main__':
    openmdao()
    
