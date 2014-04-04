from openmdao.main.api import Component, Assembly
from openmdao.main.datatypes.api import Array, Float
from openmdao.lib.drivers.api import CaseIteratorDriver


class SimpleComp(Component):

    in1 = Float(6, iotype='in', low=-1e10, high=1e10)
    in2 = Float(7, iotype='in', low=-1e10, high=1e10)

    out1 = Float(iotype='out')
    out2 = Float(iotype='out')

    def execute(self):
        print self.get_pathname(), 'execute', self.in1, self.in2
        self.out1 = self.in1 + self.in2
        self.out2 = self.in1 * self.in2


class Aggregator(Component):

    in1 = Array(iotype='in')
    in2 = Array(iotype='in')
    in3 = Array(iotype='in')

    out1 = Array(iotype='out')

    def execute(self):
        print self.get_pathname(), 'execute', self.in1, self.in2, self.in3
        self.out1 = self.in1 + self.in2 + self.in3


class SampleAssembly(Assembly): 

    def configure(self): 

        self.add('a', SimpleComp())
        self.add('b', SimpleComp())
        self.add('c', SimpleComp())
        self.add('d', Aggregator())

        self.connect('a.out1', 'b.in1')
        self.connect('b.out1', 'c.in1')

        self.add('cid_driver', CaseIteratorDriver())

        #note, using "new" parameter interface that does not require low/high
        self.cid_driver.add_parameter('b.in2')
        self.cid_driver.add_parameter('c.in2')

        #tells the driver which values from the MP runs are of interest,
        # allows us to create extra variables only for the needed values
        #output arrays created on the driver on the fly, with no size
        # determined until runtime
        #variable names have no special meaning, just chosen for clarity here
        self.cid_driver.add_response('b.out1')
        self.cid_driver.add_response('b.out2')
        self.cid_driver.add_response('c.out1')

        #vtree inputs created on the fly based on parameters given
        #number of multi-point executions given at runtime based on length of
        # inputs, all inputs must be same length
        self.cid_driver.case_inputs.b.in2 = [1, 2, 3, 4, 5, 6]
        self.cid_driver.case_inputs.c.in2 = [0, 1, 0, 1, 0, 1]

        #d is a component that does mp_aggregation 
        #NOTE: d is expecting arrays of equal length
        from openmdao.main.container import dump
        dump(self.cid_driver, recurse=True)
        self.connect('cid_driver.case_outputs.b.out1', 'd.in1')
        self.connect('cid_driver.case_outputs.b.out2', 'd.in2')
        self.connect('cid_driver.case_outputs.c.out1', 'd.in3')

        #Options: 
        #  1) d could  be a very simple component that requires all the array
        #     data to be pulled onto one processor
        #  2) d could be a more advanced component that works with distributed
        #     vectors to do aggregation operations (like sum or norm)
        #Possibly could make a setting or two different MPIMultiPoint drivers
        # to control this behavior. 
        #One would require a standard array output. The other a distributed
        # vector. I'm not sure what the right answer is... 

        self.driver.workflow.add(['a', 'cid_driver', 'd'])

        self.cid_driver.workflow.add(['b', 'c'])


if __name__ == '__main__':
    SampleAssembly().run()

