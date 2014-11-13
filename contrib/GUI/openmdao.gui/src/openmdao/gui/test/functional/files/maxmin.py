"""
Trivial model for testing max/min button.
"""

from openmdao.main.api import Assembly
from openmdao.lib.components.api import ExternalCode


class Sub(Assembly):

    def configure(self):
        extcode = self.add('extcode', ExternalCode())
        extcode.command = ['date']
        self.driver.workflow.add('extcode')


class MaxMin(Assembly):

    def configure(self):
        self.add('sub', Sub())
        self.driver.workflow.add('sub')


if __name__ == '__main__':
    maxmin = MaxMin()
    maxmin.run()

