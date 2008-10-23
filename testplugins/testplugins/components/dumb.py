

from openmdao.main.component import Component

class DumbComponent(Component):
    def __init__(self, name):
        Component.__init__(self, name)
        self.fnum = 3.14
        self.inum = 2
        self.svar = 'abcdefg'
        self.version = '0.1'

    def execute(self):
        self.fnum += 2.0
        self.inum -= 3
        self.svar = self.svar[::-1]


