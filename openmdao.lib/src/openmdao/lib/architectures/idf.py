from openmdao.main.api import Architecture
from openmdao.lib.drivers.api import SLSQPdriver#, COBYLAdriver as SLSQPdriver

class IDF(Architecture):

    """ Architecture that uses a single top level optimizer,
    enforcing consistency with equality constraints."""

    def __init__(self, *args, **kwargs):
        super(IDF, self).__init__(*args, **kwargs)

        # the following variables determine the behavior of check_config
        self.param_types = ['continuous']
        self.constraint_types = ['ineq']
        self.num_allowed_objectives = 1
        self.has_coupling_vars = True
        self.has_global_des_vars = False

    def configure(self):
        """Setup an IDF architecture inside this assembly.
        """
        #create the top level optimizer
        self.parent.add("driver", SLSQPdriver())
        self.parent.driver.iprint = 0
        self.parent.recorders = self.data_recorders
        global_dvs = []
        local_dvs = []

        for k, v in self.parent.get_global_des_vars():
            global_dvs.append(v)
            # and add the broadcast parameters to the driver
            self.parent.driver.add_parameter(v, name=k)
            v.initialize(self.parent, self.parent)

        for k, v in self.parent.get_local_des_vars():
            local_dvs.append(v)
            #add the local design variables to the driver
            self.parent.driver.add_parameter(v, name=k)
            v.initialize(self.parent, self.parent)

        #TODO: possibly add method for passing constraint directly?
        #add the constraints to the driver
        for const in self.parent.list_constraints():
            self.parent.driver.add_constraint(const)

        #set the objective
        objective = self.parent.get_objectives().items()[0]
        self.parent.driver.add_objective(objective[1].text, name=objective[0])

        #add the coupling vars parameters/constraints to the solver
        for key, couple in self.parent.list_coupling_vars().iteritems():
            self.parent.driver.add_parameter(couple.indep.target, low=-9.e99,
                                             high=9.e99, name=key)
            self.parent.driver.add_constraint("(%s-%s) <= .001"
                                              % (couple.indep.target,
                                                 couple.dep.target))
            self.parent.driver.add_constraint("(%s-%s) <= .001"
                                              % (couple.dep.target,
                                                 couple.indep.target))

