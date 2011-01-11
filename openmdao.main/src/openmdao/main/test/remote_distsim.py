"""
Run a distributed simulation between a remote host and this one.
Similar to code in test_distsim.py, but doesn't assume localhost.
"""

import atexit
import logging
import optparse
import os.path
import shutil

from openmdao.main.api import set_as_top
from openmdao.main.mp_util import read_server_config
from openmdao.main.objserverfactory import connect, start_server
from openmdao.main.rbac import get_credentials
from openmdao.main.test.test_distsim import Model


def main():
    """
    Usage:

    python remote_distsim.py [--server][--client][--tunnel] [config_filename]

    --server:
        Run server.

    --client:
        Run client.

    --tunnel:
        Connect via ssh tunnel.

    config_filename:
        Path to server config file, default ``server.cfg``.
        Only used on client side.
    """
    parser = optparse.OptionParser()
    parser.add_option('--server', action='store_true', default=False,
                      help='Run server')
    parser.add_option('--client', action='store_true', default=False,
                      help='Run client')
    parser.add_option('--tunnel', action='store_true', default=False,
                      help='Connect via ssh tunnel')

    options, arguments = parser.parse_args()
    if arguments:
        config_filename = arguments[0]
    else:
        config_filename = 'server.cfg'

    # Default is to run 'self-contained'.
    if not options.server and not options.client:
        options.server = True
        options.client = True

    if options.server:
        # Start factory in subdirectory.
        print 'Starting factory server...'
        os.environ['OPENMDAO_KEEPDIRS'] = '1'
        server_dir = 'Factory'
        if os.path.exists(server_dir):
            shutil.rmtree(server_dir)
        os.mkdir(server_dir)
        os.chdir(server_dir)
        try:
            server, config_filename = start_factory(options.tunnel)
        finally:
            os.chdir('..')
        config_filename = os.path.join(server_dir, config_filename)
        if not options.client:
            print '        configuration file:', config_filename
        atexit.register(cleanup, server_dir)

    if options.client:
        # Run client.
        client(config_filename, not options.server)
        if options.server:
            stop_factory(server, config_filename)
    else:
        # Wait until killed.
        try:
            server.wait()
        except KeyboardInterrupt:
            pass


def cleanup(server_dir):
    """ Clean up server tree. """
    keepdirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
    if not keepdirs and os.path.exists(server_dir):
        shutil.rmtree(server_dir)


def start_factory(tunnel):
    """ Start factory server.  Returns (server, config_filename). """
    address = 'localhost' if tunnel else None

    credentials = get_credentials()
    allowed_users = {credentials.user: credentials.public_key}

    allowed_types = ['openmdao.main.test.test_distsim.Box']

    server = start_server(address=address,
                          allowed_users=allowed_users,
                          allowed_types=allowed_types,
                          allow_tunneling=tunnel)

    address, port, pubkey = read_server_config('server.cfg')
    print 'Factory pid:', server.pid
    print '        address:', address
    print '        port:', port

    return (server, 'server.cfg')


def connect_to_factory(config_filename, verbose=False):
    """ Connect to factory indicated by `config_filename`. """
    address, port, pubkey = read_server_config(config_filename)
    if verbose:
        print 'Connecting to %s:%d' % (address, port)
    return connect(address, port, pubkey=pubkey)


def stop_factory(server, config_filename):
    """ Shutdown factory and server. """
    print 'Stopping factory server.'
    factory = connect_to_factory(config_filename)
    factory.cleanup()
    server.terminate(timeout=10)


def client(config_filename, verbose=False):
    """ Connect to factory, create remote component for model, and run. """
    factory = connect_to_factory(config_filename, verbose)

    # Create remote component.
    # Explicitly create component server first so we can get output later.
    box_server = factory.create('')
    box = factory.create('openmdao.main.test.test_distsim.Box',
                         server=box_server)

    # Create local simulation using remote component.
    model = set_as_top(Model(box))
    model.log_level = logging.DEBUG

    # Run simulation.
    model.run()

    print '\nRemote component server output:'
    with box_server.open('stdout', 'r') as inp:
        for line in inp.readlines():
            print line.rstrip()
    print


if __name__ == '__main__':
    main()

