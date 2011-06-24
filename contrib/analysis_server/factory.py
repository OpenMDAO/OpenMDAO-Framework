import optparse
import sys

from openmdao.main.factory import Factory

import client
import proxy
import server


class ASFactory(Factory):
    """
    Factory for components running under an AnalysisServer.

    host: string
        Host name or IP address of the AnalysisServer to connect to.

    port: int
        Port number of the AnalysisServer to connect to.
    """

    def __init__(self, host, port=server.DEFAULT_PORT):
        super(ASFactory, self).__init__()
        self._host = host
        self._port = port
        self._client = client.Client(host, port)
        self._types = None

    def create(self, typname, version=None, server=None,
               res_desc=None, **ctor_args):
        """
        Create a `typname` object.

        typname: string
            Type of object to create.

        version: string or None
            Version of `typname` to create.

        server:
            Not used.

        res_desc: dict or None
            Not used.

        ctor_args: dict
            Other constructor arguments.  Not used.
        """
        if self._types is None:
            self.get_available_types()

        for typ, ver in self._types:
            if typ == typname:
                if version is None or ver == version:
                    return proxy.ComponentProxy(typname, self._host, self._port)
        return None

    def get_available_types(self, groups=None):
        """
        Returns a set of tuples of the form ``(typname, version)``,
        one for each available component type.

        groups: list[string]
            OpenMDAO entry point groups.
            Only 'openmdao.component' is suppoerted.
        """

        if groups is not None and 'openmdao.component' not in groups:
            return []

        if self._types is None:
            self._types = []
            for comp in self._client.list_components():
                info = self._client.describe(comp)
                self._types.append((comp, info['Version']))
        return self._types


def main():  # pragma no cover
    """
    Used for testing. Connects to server, lists available types, creates
    a proxy component for the first type listed, and runs that component.

    Usage: python factory.py [--host=address][--port=number]

    --host: string
        IPv4 address or hostname. Default is 'localhost'.

    --port: int
        Server port (default 1835).
    """
    parser = optparse.OptionParser()
    parser.add_option('--host', action='store', type='string',
                      default='localhost', help='host to connect to')
    parser.add_option('--port', action='store', type='int',
                      default=server.DEFAULT_PORT, help='port to connect to')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    factory = ASFactory(options.host, options.port)
    types = factory.get_available_types()
    print 'Available types:'
    for typ, ver in types:
        print '   ', typ, ver

    comp = factory.create(types[0][0])
    comp.run()


if __name__ == '__main__':  # pragma no cover
    main()

