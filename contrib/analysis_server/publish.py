"""
Support for remote publishing of components.
All inputs, outputs, and no-argument methods will be accessible.
"""

from __future__ import absolute_import

import optparse
import os.path
import sys

from openmdao.main.api import set_as_top

from analysis_server import client, server


def publish_class(path, version, comment, filename, classname,
                  host='localhost', port=server.DEFAULT_PORT):
    """
    Publish egg on server at `host`:`port` under `path` and `version` with
    `comment` given `filename` and `classname`.
    """
    dirname = os.path.dirname(filename)
    if not os.path.isabs(dirname):
        cwd = os.getcwd()
        if dirname:
            dirname = os.path.join(cwd, dirname)
        else:
            dirname = cwd
    if not dirname in sys.path:  # Ensure importable.
        sys.path.insert(0, dirname)
    modname = os.path.basename(filename)[:-3]  # Drop '.py'
    try:
        __import__(modname)
    except ImportError as exc:
        raise RuntimeError("Can't import %r: %r" % (modname, exc))

    module = sys.modules[modname]
    try:
        cls = getattr(module, classname)
    except AttributeError as exc:
        raise RuntimeError("Can't get class %r in %r: %r"
                           % (classname, modname, exc))
    try:
        obj = cls()
    except Exception as exc:
        raise RuntimeError("Can't instantiate %s.%s: %r" 
                           % (modname, classname, exc))
    set_as_top(obj)
    publish_object(path, version, comment, obj, host, port)


def publish_object(path, version, comment, obj,
                   host='localhost', port=server.DEFAULT_PORT):
    """
    Publish egg on server at `host`:`port` under `path` and `version` with
    `comment` given component `obj`.
    """
    category, slash, name = path.rpartition('/')
    egg_info = obj.save_to_egg(name, version)
    eggfile = egg_info[0]
    try:
        publish_egg(path, version, comment, eggfile, host, port)
    finally:
        os.remove(eggfile)


def publish_egg(path, version, comment, eggfile,
                host='localhost', port=server.DEFAULT_PORT):
    """
    Publish egg on server at `host`:`port` under `path` and `version` with
    `comment` given `eggfile`.
    """
    try:
        _client = client.Client(host, port)
    except Exception as exc:
        raise RuntimeError("Can't connect to %s:%d: %s" % (host, port, exc))

    _client.publish_egg(path, version, comment, eggfile)


def main():  # pragma no cover
    """
    Publish a component.

    Usage: python publish.py --path=path --version=ver [--comment=text]{--egg=filename | --file=filename [--classname=classname]}[--host=address][--port=number][--list]

    --path: string
        Path to publish under.

    --version: string
        Version to publish under.

    --comment: string
        Version comment.

    --egg: string
        Egg file containing component to publish.

    --file: string
        Python file containing component to publish.

    --classname: string
        Name of component class.

    --host: string
        IPv4 address or hostname. Default is 'localhost'.

    --port: int
        Server port (default 1835).

    --list:
        List existing versions for "path", do not publish.
    """
    parser = optparse.OptionParser()
    parser.add_option('--path', action='store', type='string',
                      help='path to publish under')
    parser.add_option('--version', action='store', type='string',
                      help='version to publish under')
    parser.add_option('--comment', action='store', type='string',
                      help='version description')
    parser.add_option('--egg', action='store', type='string',
                      help='egg file containing component to publish')
    parser.add_option('--file', action='store', type='string',
                      help='python file containing component to publish')
    parser.add_option('--classname', action='store', type='string',
                      help='name of component class')
    parser.add_option('--host', action='store', type='string',
                      default='localhost', help='host to connect to')
    parser.add_option('--port', action='store', type='int',
                      default=server.DEFAULT_PORT, help='port to connect to')
    parser.add_option('--list', action='store_true',
                      help='list existing versions for "path", do not publish')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    if not options.path:
        print '--path is required'
        sys.exit(1)

    if options.list:
        try:
            _client = client.Client(options.host, options.port)
        except Exception as exc:
            print "Can't connect to %s:%d: %s" \
                  % (options.host, options.port, exc)
            sys.exit(1)
        for version in _client.versions(options.path):
            print version
        sys.exit(0)

    if not options.version:
        print '--version is required'
        sys.exit(1)

    if options.egg:
        if not os.path.exists(options.egg):
            print 'Egg file %r does not exist' % options.egg
            sys.exit(1)
        try:
            publish_egg(options.path, options.version, options.comment,
                        options.egg, options.host, options.port)
        except Exception as exc:
            print exc
            sys.exit(1)
    else:
        if not options.file:
            print '--file is required if --egg is not used'
            sys.exit(1)

        if not os.path.exists(options.file):
            print 'Python file %r does not exist' % options.file
            sys.exit(1)

        if not options.classname:
            print '--classname is required'
            sys.exit(1)

        try:
            publish_class(options.path, options.version, options.comment,
                          options.file, options.classname,
                          options.host, options.port)
        except Exception as exc:
            print exc
            sys.exit(1)


if __name__ == '__main__':  # pragma no cover
    main()

