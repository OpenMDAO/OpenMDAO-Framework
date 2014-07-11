import time

from openmdao.main.api import Assembly, Component
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial, Uniform
from openmdao.examples.simple.paraboloid import Paraboloid

from openmdao.lib.casehandlers.api import JSONCaseRecorder, BSONCaseRecorder


class Analysis(Assembly):

    def configure(self):

        self.add('paraboloid', Paraboloid())

        self.add('driver', DOEdriver())
        #There are a number of different kinds of DOE available in openmdao.lib.doegenerators
        #self.driver.DOEgenerator = FullFactorial(10) #Full Factorial DOE with 10 levels for each variable
        self.driver.DOEgenerator = Uniform(1000) 

        #DOEdriver will automatically record the values of any parameters for each case
        self.driver.add_parameter('paraboloid.x', low=-50, high=50)
        self.driver.add_parameter('paraboloid.y', low=-50, high=50)
        #tell the DOEdriver to also record any other variables you want to know for each case
        self.driver.add_response('paraboloid.f_xy')

        self.recorders = [JSONCaseRecorder('doe.json'), BSONCaseRecorder('doe.bson')]


if __name__ == "__main__":

    import time

    analysis = Analysis()

    tt = time.time()
    analysis.run()

    

    print "Elapsed time: ", time.time()-tt, "seconds"

    print analysis.driver.case_inputs.paraboloid.x[:5]


    