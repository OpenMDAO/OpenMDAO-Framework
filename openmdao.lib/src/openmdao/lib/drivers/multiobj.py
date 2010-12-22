from random import Random
from ecspy import ec
from ecspy import emo
from ecspy import variators, terminators, observers, archivers, replacers

from openmdao.lib.datatypes.api import Python, Enum, Float, Int, Bool, Instance, Event

from openmdao.main.case import Case
from openmdao.main.api import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjectives
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.lib.caserecorders.api import DBCaseRecorder,DumpCaseRecorder,ListCaseRecorder
from openmdao.lib.caseiterators.api import DBCaseIterator,ListCaseIterator
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import ICaseIterator, ICaseRecorder

@add_delegate(HasParameters, HasObjectives)
class MultiObj(CaseIterDriverBase):
    """Driver for NSGA-II algorithm"""
    
    opt_type = Enum("minimize", values=["minimize", "maximize"],
                    iotype="in",
                    desc='Sets the optimization to either minimize or maximize '
                         'the objective functions.')
                         
    _opt_mapping = {"minimize":False,
                                    "maximize":True}
     
    generations = Int(50, iotype="in",
                      desc="The maximum number of generations the algorithm "
                           "will evolve to before stopping.")
                           
    population_size = Int(100, iotype="in",
                          desc = "The size of the population in each "
                                 "generation.")
    
    crossover_rate = Float(1.0, iotype="in", low=0.0,
                           high=1.0, desc="The crossover rate used when two "
                          "parent genomes reproduce to form a child genome.")
                          
    mutation_rate = Float(0.1, iotype="in", low=0.0, 
                          high=1.0, desc="The mutation rate applied to "
                                         "population members.")

    crossover_type = Enum('blend',values=['n_point','uniform','blend','differential','simulated_binary','laplace'],
                                        iotype='in',desc="crossover method used in the evolutaionary algorithm")
    
    _crossover_mapping = {'n_point':variators.n_point_crossover,
                                              'uniform':variators.uniform_crossover,
                                              'blend':variators.blend_crossover,
                                              'differential':variators.differential_crossover,
                                              'simulated_binary':variators.simulated_binary_crossover}
    
    mutation_type = Enum('gaussian',values=['gaussian','bit_flip','nonuniform','mptm'],
                                        iotype='in',desc='mutation method used in evolutionary algorithm')
    
    _mutation_mapping = {'gaussian':variators.gaussian_mutation,
                                            'bit_flip':variators.bit_flip_mutation}
    
    case_set = Instance(ICaseIterator,iotype='out')
    
    def __init__(self,doc=None):
        super(MultiObj, self).__init__(doc)
        self.bounder = None
        self.cases = []

    def _make_bounder(self):
        lows = []
        highs = []
        for param in self.get_parameters().values():
            lows.append(param.low)
            highs.append(param.high)
        return ec.Bounder(lows,highs)
        
    def _make_generator(self,random,args):
        lows = []
        highs = []
        for param in self.get_parameters().values():
            lows.append(param.low)
            highs.append(param.high)
        return [random.uniform(low,high) for low,high in zip(lows,highs)]
        
    def _make_evaluator(self,candidates,args):
        fitness = []
        for c in candidates:
            fs = self._run_model(c)
            fitness.append(emo.Pareto(fs))
        return fitness
    
    def _run_model(self,chromosome):
        self.set_events()
        self.set_parameters([val for val in chromosome])
        self.run_iteration()
        
        objs = self.eval_objectives()
        outs = self.get_objectives().values()
        ins =  self.get_parameters().values()

        outputs = []
        inputs = []

        for parameter,value in zip(outs,objs):
            outputs.append((parameter,None,value))
        
        for parameter,value in zip(ins,chromosome):
            inputs.append(('meta_'+str(parameter.expreval),None,value))

        case = Case(inputs=inputs,outputs=outputs)
        #if case not in self.case_set.get_iterator().get_iter():
        if case not in self.cases:
            self.cases.append(case)
            #self.case_set.record(case)
        return objs
        
    def execute(self):
        self.cases = []
        self.bounder = self._make_bounder()
        self.ea = emo.NSGA2(Random())
        self.ea.variator = [self._crossover_mapping[self.crossover_type],
                                    self._mutation_mapping[self.mutation_type]]
        self.ea.terminator = terminators.generation_termination
        self.final_pop = self.ea.evolve(generator = self._make_generator,
                                                  evaluator = self._make_evaluator,
                                                  pop_size = self.population_size,
                                                  maximize = self._opt_mapping[self.opt_type],
                                                  bounder = self.bounder,
                                                  max_generations = self.generations,
                                                  mutation_rate = self.mutation_rate)
        self.case_set = ListCaseIterator(self.cases)