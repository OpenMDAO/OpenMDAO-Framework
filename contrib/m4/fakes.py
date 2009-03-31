"""
Fake implementations for development until the real thing is available.
"""

__all__ = ('FakeSocket', 'FakeROSE')
__version__ = '0.0'

from openmdao.main import Driver, Container, Case
from openmdao.main.component import RUN_OK, RUN_FAILED, RUN_STOPPED
from openmdao.main.interfaces import IContainer, ICaseIterator

class FakeSocket(Container):
    """
    Just a stand-in until we have a real socket definition.
    Don't assume this API is valid for the real code.
    """

    def __init__(self, name='Socket', parent=None,
                 plugin=None, interface=None, required=True):
        super(FakeSocket, self).__init__(name, parent)
        self.interface = interface
        self.required = required
        self.plugin = plugin

    def _get_plugin(self):
        return self._plugin

    def _set_plugin(self, plugin):
        if self.interface is not None and plugin is not None:
            if not self.interface.providedBy(plugin):
                self.raise_exception('plugin does not support required interface',
                                     TypeError)
        self._plugin = plugin
        if plugin is not None and IContainer.providedBy(plugin):
            plugin.parent = self

    plugin = property(_get_plugin, _set_plugin)


class FakeROSE(Driver):
    """
    Just a stand-in until we have a concurrent-capable driver.
    The intent is that a set of cases to be run is provided by
    an ICaseIterator, and the system does its best to run those cases.
    Other than that, don't assume this API is valid for the real code.
    """

    def __init__(self, name='FakeROSE', parent=None):
        super(FakeROSE, self).__init__(name, parent)
        FakeSocket('iterator', self, None, ICaseIterator, True)
        FakeSocket('outerator', self, None, None, True)

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

    def execute(self):
        """ Run each case in iterator record results in outerator. """
        if self.iterator.plugin is None:
            self.error('No iterator plugin')
            return RUN_FAILED

        if self.outerator.plugin is None:
            self.error('No outerator plugin')
            return RUN_FAILED

        for case in self.iterator.plugin:
            if self._stop:
                return RUN_STOPPED

            for name, index, value in case.inputs:
                try:
                    self.parent.set(name, value, index)
                except Exception, exc:
                    msg = "Exception setting '%s': %s" % (name, str(exc))
                    self.error(msg)
                    self.outerator.plugin.append(Case(case.inputs, None,
                                                 RUN_FAILED, msg))
                    break
            else:
                status = self.parent.workflow.run()
                if status == RUN_OK:
                    results = []
                    for name, index, value in case.outputs:
                        try:
                            value = self.parent.get(name, index)
                            results.append((name, index, value))
                        except Exception, exc:
                            msg = "Exception getting '%s': %s" % (name, str(exc))
                            self.error(msg)
                            self.outerator.plugin.append(Case(case.inputs, None,
                                                         RUN_FAILED, msg))
                            break
                    else:
                        self.outerator.plugin.append(Case(case.inputs, results,
                                                     RUN_OK, ''))
                else:
                    self.outerator.plugin.append(Case(case.inputs, None,
                                                 status, ''))
        return RUN_OK

