import optparse
import os.path
import sys

from openmdao.main.api import set_as_top

import client
import server


def publish_model(path, version, filename, classname,
                  host='localhost', port=server.DEFAULT_PORT):
    """
    Publish egg under `path` and `version` given `filename` and `classname`.
    """
    cwd = os.getcwd()
    dirname = os.path.dirname(filename)
    if dirname:
        dirname = os.path.join(cwd, dirname)
    else:
        dirname = cwd
    if not dirname in sys.path:
        sys.path.insert(0, dirname)
    modname = os.path.basename(filename)[:-3]  # drop '.py'
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
    publish_object(path, version, obj, host, port)


def publish_object(path, version, obj,
                   host='localhost', port=server.DEFAULT_PORT):
    """ Publish egg under `path` and `version` given `obj`. """
    category, slash, name = path.rpartition('/')
    egg_info = obj.save_to_egg(name, version)
    eggfile = egg_info[0]
    try:
        publish_egg(path, version, eggfile, host,  port)
    finally:
        os.remove(eggfile)


def publish_egg(path, version, eggfile,
                host='localhost', port=server.DEFAULT_PORT):
    """ Publish egg under `path` and `version` given `eggfile`. """
    try:
        _client = client.Client(host, port)
    except Exception as exc:
        raise RuntimeError("Can't connect to %s:%d: %s" % (host, port, exc))

    _client.publish_egg(path, version, eggfile)


def main():  # pragma no cover
    """
    Starts up an interactive session, useful for testing.

    Usage: python publish.py --path=path --version=ver {--egg=filename | --file=filename [--classname=classname]}[--host=address][--port=number]

    --path: string
        Path to publish under

    --version: string
        Version to publish under

    --egg: string
        Egg file containing model to publish

    --file: string
        Python file containing model to publish.

    --classname: string
        Name of model class

    --host: string
        IPv4 address or hostname. Default is 'localhost'.

    --port: int
        Server port (default 1835).
    """
    parser = optparse.OptionParser()
    parser.add_option('--path', action='store', type='string',
                      help='path to publish under')
    parser.add_option('--version', action='store', type='string',
                      help='version to publish under')
    parser.add_option('--egg', action='store', type='string',
                      help='egg file to publish')
    parser.add_option('--file', action='store', type='string',
                      help='python file containing model to publish')
    parser.add_option('--classname', action='store', type='string',
                      help='name of model class')
    parser.add_option('--host', action='store', type='string',
                      default='localhost', help='host to connect to')
    parser.add_option('--port', action='store', type='int',
                      default=server.DEFAULT_PORT, help='port to connect to')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    if not options.path:
        print '--path is required'
        sys.exit(1)

    if not options.version:
        print '--version is required'
        sys.exit(1)

    if options.egg:
        if not os.path.exists(options.egg):
            print 'Egg file %r does not exist' % options.egg
            sys.exit(1)
        try:
            publish_egg(options.path, options.version, options.egg,
                        options.host, options.port)
        except Exception as exc:
            print exc
            sys.exit(1)
    else:
        if not options.file:
            print '--file is required is --egg is not used'
            sys.exit(1)

        if not os.path.exists(options.file):
            print 'Python file %r does not exist' % options.file
            sys.exit(1)

        if not options.classname:
            print '--classname is required'
            sys.exit(1)

        try:
            publish_model(options.path, options.version, options.file,
                          options.classname, options.host, options.port)
        except Exception as exc:
            print exc
            sys.exit(1)


if __name__ == '__main__':  # pragma no cover
    main()

