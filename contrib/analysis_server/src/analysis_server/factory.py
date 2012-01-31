from openmdao.main.factory import Factory

from analysis_server import client, proxy, server


class ASFactory(Factory):
    """
    Factory for components running under an AnalysisServer.
    An instance would typically be passed to
    :meth:`openmdao.main.factorymanager.register_class_factory`.

    host: string
        Host name or IP address of the AnalysisServer to connect to.

    port: int
        Port number of the AnalysisServer to connect to.
    """

    def __init__(self, host='localhost', port=server.DEFAULT_PORT):
        super(ASFactory, self).__init__()
        self._host = host
        self._port = port
        self._client = client.Client(host, port)

    def shutdown(self):
        """ Shutdown factory. """
        if self._client is not None:
            self._client.quit()
            self._client = None

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
        for typ, ver in self.get_available_types():
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
            Only 'openmdao.component' is supported.
        """

        if groups is not None and 'openmdao.component' not in groups:
            return []

        types = []
        self._list('', types)
        return types


    def _list(self, category, types):
        """ List components in `category` and sub-categories. """
        if category:
            category += '/'

        for comp in self._client.list_components(category):
            comp = '%s%s' % (category, comp)
            try:
                versions = self._client.versions(comp)
            except RuntimeError:
                types.append((comp, ''))
            else:
                for version in versions:
                    types.append((comp, version))

        for sub in self._client.list_categories(category):
            sub = '%s%s' % (category, sub)
            self._list(sub, types)

