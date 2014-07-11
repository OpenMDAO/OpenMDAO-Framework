from openmdao.main.api import Assembly, Component
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import Uniform

from openmdao.examples.simple.paraboloid import Paraboloid

from openmdao.lib.casehandlers.api import JSONCaseRecorder

class Analysis(Assembly):

    def configure(self):
        self.add('paraboloid', Paraboloid())

        self.add('driver', DOEdriver())
        self.driver.DOEgenerator = Uniform(1000)

        self.driver.add_parameter('paraboloid.x', low=-50, high=50)
        self.driver.add_parameter('paraboloid.y', low=-50, high=50)

        self.driver.add_response('paraboloid.f_xy')

        self.recorders = [JSONCaseRecorder(out='doe.json')]

if __name__ == "__main__":

    #-----------------------------
    # Run analysis
    #-----------------------------
    import os
    if os.path.exists('doe.json'):
        os.remove('doe.json')

    from openmdao.lib.casehandlers.api import CaseDataset

    analysis = Analysis()

    analysis.run()

    #----------------------------------------------------
    # Print out history of our objective for inspection
    #----------------------------------------------------
    case_dataset = CaseDataset('doe.json', 'json')
    data = case_dataset.data.by_variable().fetch()

    for case in data:
        print case