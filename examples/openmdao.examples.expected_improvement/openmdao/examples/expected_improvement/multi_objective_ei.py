'''Multi Ojective EI Example'''

import os
from tempfile import mkdtemp
import os.path
import shutil

from numpy import sin, cos, pi

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.hasstopcond import HasStopConditions

from openmdao.lib.components.api import MetaModel, ParetoFilter, \
     MultiObjExpectedImprovement
from openmdao.lib.drivers.adaptivesampledriver import AdaptiveSampleDriver
from openmdao.lib.drivers.api import Genetic, FixedPointIterator
from openmdao.lib.casehandlers.api import DBCaseIterator
from openmdao.lib.casehandlers.api import DBCaseRecorder, DumpCaseRecorder

from openmdao.lib.surrogatemodels.api import KrigingSurrogate

from openmdao.lib.doegenerators.api import OptLatinHypercube, FullFactorial

from openmdao.examples.expected_improvement.spiral_component import SpiralComponent


class MyDriver(Driver):
    """Custom driver to retrain the MetaModel each iteration. Also records each
    retrain case"""

    def __init__(self):
        super(MyDriver,self).__init__()

        self.ins = ['spiral_meta_model.x','spiral_meta_model.y']
        self.outs = ['spiral_meta_model.f1_xy','spiral_meta_model.f2_xy']

    def execute(self):
        self.set_events()
        self.run_iteration()

        inputs = [(name, self.parent.get(name)) for name in self.ins]
        outputs = [(name, self.parent.get(name)) for name in self.outs]
        case = Case(inputs = inputs,
                    outputs = outputs)

        for recorder in self.recorders:
            recorder.record(case)

class Analysis(Assembly):
    '''Top level assembly for the Multi Ojective EI Example.'''

    def __init__(self):
        super(Analysis, self).__init__()

        self._tdir = mkdtemp()

    def configure(self):

        driver = self.add('driver', FixedPointIterator())
        adapt = self.add('adapt', AdaptiveSampleDriver())
        MOEI_opt = self.add('MOEI_opt', Genetic())

        self.add('spiral', SpiralComponent())

        kwargs = {'params':("x", "y"),
                  'responses':('f1_xy', 'f2_xy')}

        meta = self.add('meta', MetaModel(**kwargs))
        meta.default_surrogate = KrigingSurrogate()
        pareto = self.add('pareto', ParetoFilter(**kwargs))

        MOEI = self.add('MOEI', MultiObjExpectedImprovement())

        #initial training DOE
        adapt.DOEgenerator = OptLatinHypercube(num_samples=25)
        adapt.add_parameter('spiral.x')
        adapt.add_parameter('spiral.y')
        adapt.add_response('spiral.f1_xy')
        adapt.add_response('spiral.f2_xy')

        #pass training data from sampler to metamodel and pareto filter
        self.connect('adapt.all_case_inputs.spiral.x', ['meta.params.x',
                                                        'pareto.params.x'])
        self.connect('adapt.all_case_inputs.spiral.y', ['meta.params.y',
                                                        'pareto.params.y'])
        self.connect('adapt.all_case_outputs.spiral.f1_xy', ['meta.responses.f1_xy',
                                                            'pareto.responses.f1_xy'])
        self.connect('adapt.all_case_outputs.spiral.f2_xy', ['meta.responses.f2_xy',
                                                            'pareto.responses.f2_xy'])

        # MOEI optimization to find next point
        MOEI_opt.opt_type = "maximize"
        MOEI_opt.population_size = 100
        MOEI_opt.generations = 10
        #MOEI_opt.selection_method = "tournament"
        MOEI_opt.add_parameter("meta.x", low=0.75, high=5.*pi)
        MOEI_opt.add_parameter("meta.y", low=0.75, high=5.*pi)
        MOEI_opt.add_objective("MOEI.PI")

        #Iterative sampling process
        driver.add_parameter('adapt.adaptive_inputs.spiral.x[0]',
                             low=-1e99, high=1e99)
        driver.add_parameter('adapt.adaptive_inputs.spiral.y[0]',
                             low=-1e99, high=1e99)
        driver.add_constraint('adapt.adaptive_inputs.spiral.x[0] = meta.x')
        driver.add_constraint('adapt.adaptive_inputs.spiral.y[0] = meta.y')
        driver.max_iterations = 30

        #Iteration Heirarchy
        driver.workflow.add(['adapt', 'pareto', 'MOEI_opt'])
        adapt.workflow.add(['spiral'])
        MOEI_opt.workflow.add(['meta', 'MOEI'])

        #FPI now support stop conditions
        driver.add_stop_condition('MOEI.EI <= .0001')


        ## Old

        ##Components
        #self.add("spiral_meta_model",MetaModel())
        #self.spiral_meta_model.default_surrogate = KrigingSurrogate()
        #self.spiral_meta_model.model = SpiralComponent()
        #self.spiral_meta_model.recorder = DBCaseRecorder(':memory:')
        #self.spiral_meta_model.force_execute = True
        #self.add("MOEI",ConnectableMultiObjExpectedImprovement())
        #self.MOEI.criteria = ['spiral_meta_model.f1_xy','spiral_meta_model.f2_xy']

        #self.add("filter",ConnectableParetoFilter())
        #self.filter.criteria = ['spiral_meta_model.f1_xy','spiral_meta_model.f2_xy']
        #self.filter.case_sets = [self.spiral_meta_model.recorder.get_iterator()]
        #self.filter.force_execute = True

        ##Driver Configuration
        #self.add("DOE_trainer",DOEdriver())
        #self.DOE_trainer.sequential = True
        #self.DOE_trainer.DOEgenerator = OptLatinHypercube(num_samples=25)
        #self.DOE_trainer.add_parameter("spiral_meta_model.x")
        #self.DOE_trainer.add_parameter("spiral_meta_model.y")
        #self.DOE_trainer.add_event("spiral_meta_model.train_next")
        #self.DOE_trainer.case_outputs = ['spiral_meta_model.f1_xy',
                                         #'spiral_meta_model.f2_xy']
        #self.DOE_trainer.recorders = [DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))]

        #self.add("MOEI_opt",Genetic())
        #self.MOEI_opt.opt_type = "maximize"
        #self.MOEI_opt.population_size = 100
        #self.MOEI_opt.generations = 10
        ##self.MOEI_opt.selection_method = "tournament"
        #self.MOEI_opt.add_parameter("spiral_meta_model.x")
        #self.MOEI_opt.add_parameter("spiral_meta_model.y")
        #self.MOEI_opt.add_objective("MOEI.PI")

        #self.add("retrain",MyDriver())
        #self.retrain.add_event("spiral_meta_model.train_next")
        #self.retrain.recorders = [DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))]

        #self.add("iter",IterateUntil())
        #self.iter.iterations = 30
        #self.iter.add_stop_condition('MOEI.PI <= .0001')


        ##Iteration Heirarchy
        #self.driver.workflow.add(['DOE_trainer', 'iter'])

        #self.DOE_trainer.workflow.add('spiral_meta_model')

        #self.iter.workflow = SequentialWorkflow()
        #self.iter.workflow.add(['filter', 'MOEI_opt', 'retrain'])

        #self.MOEI_opt.workflow.add(['spiral_meta_model', 'MOEI'])
        #self.retrain.workflow.add('spiral_meta_model')

        ##Data Connections
        #self.connect("filter.pareto_set","MOEI.best_cases")
        #self.connect("spiral_meta_model.f1_xy","MOEI.predicted_values[0]")
        #self.connect("spiral_meta_model.f2_xy","MOEI.predicted_values[1]")

    def cleanup(self):
        """cleans up any files left in the temp directory from execution"""
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.lib.casehandlers.api import case_db_to_dict

    seed = None
    backend = None
    figname = None
    for arg in sys.argv[1:]:
        if arg.startswith('--seed='):
            import random
            seed = int(arg.split('=')[1])
            random.seed(seed)
        if arg.startswith('--backend='):
            backend = arg.split('=')[1]
        if arg.startswith('--figname='):
            figname = arg.split('=')[1]
    import matplotlib
    if backend is not None:
        matplotlib.use(backend)
    elif sys.platform == 'win32':
        matplotlib.use('WxAgg')
    from matplotlib import pyplot as plt, cm
    from matplotlib.pylab import get_cmap
    from mpl_toolkits.mplot3d import Axes3D
    from numpy import meshgrid,array, pi,arange,cos


    #create the analysis
    analysis = Analysis()

    #run the analysis
    analysis.run()


    #plot the samples points, along with the data from the function
    def f1(x,y):
        return cos(x)/x+sin(y)/y

    def f2(x,y):
        return sin(x)/x+cos(y)/y

    X_range = arange(0.75,5.*pi,0.5)
    Y_range = arange(0.75,5.*pi,0.5)

    X , Y = meshgrid(X_range,Y_range)
    Z1,Z2 = f1(X,Y),f2(X,Y)

    plt.figure()
    plt.subplot(121)
    plt.contour(X,Y,Z1,50)
    plt.axis([0.75,5*pi,0.75,5*pi])

    plt.subplot(122)
    plt.contour(X,Y,Z2,50)
    cb = plt.colorbar(shrink=.6)
    plt.axis([0.75,5*pi,0.75,5*pi])

    plt.figure()

    Z1_pred = []
    Z2_pred = []

    for x_row,y_row in zip(X,Y):
        row1 = []
        row2 = []
        for x,y in zip(x_row,y_row):
            analysis.spiral_meta_model.x = x
            analysis.spiral_meta_model.y = y
            analysis.spiral_meta_model.execute()
            row1.append(analysis.spiral_meta_model.f1_xy.mu)
            row2.append(analysis.spiral_meta_model.f2_xy.mu)
        Z1_pred.append(row1)
        Z2_pred.append(row2)
    Z1_pred = array(Z1_pred)
    Z2_pred = array(Z2_pred)

    #plot the initial training data
    data_train = case_db_to_dict(os.path.join(analysis._tdir,'trainer.db'),
                                     ['spiral_meta_model.x',
                                      'spiral_meta_model.y',
                                      'spiral_meta_model.f1_xy',
                                      'spiral_meta_model.f2_xy'])

    plt.scatter(data_train['spiral_meta_model.x'],
                data_train['spiral_meta_model.y'],s=30,c='#572E07',zorder=10)

    data_EI = case_db_to_dict(os.path.join(analysis._tdir,'retrain.db'),
                                     ['spiral_meta_model.y',
                                      'spiral_meta_model.x',
                                      'spiral_meta_model.f1_xy',
                                      'spiral_meta_model.f2_xy'])

    count = len(data_EI['spiral_meta_model.x'])
    colors = arange(0,count)/float(count)
    color_map = get_cmap('spring')

    f1_train = [case.mu for case in data_train['spiral_meta_model.f1_xy']]
    f2_train = [case.mu for case in data_train['spiral_meta_model.f2_xy']]
    f1_iter = [case.mu for case in data_EI['spiral_meta_model.f1_xy']]
    f2_iter = [case.mu for case in data_EI['spiral_meta_model.f2_xy']]

    plt.subplot(121)
    plt.contour(X,Y,Z1_pred,50)
    plt.scatter(data_train['spiral_meta_model.x'],
                data_train['spiral_meta_model.y'],s=30,c='#572E07',zorder=10)
    plt.scatter(data_EI['spiral_meta_model.x'],data_EI['spiral_meta_model.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)
    plt.axis([0.75,5*pi,0.75,5*pi])

    plt.subplot(122)
    plt.contour(X,Y,Z2_pred,50)
    cb = plt.colorbar(shrink=.6)
    plt.scatter(data_train['spiral_meta_model.x'],
                data_train['spiral_meta_model.y'],s=30,c='#572E07',zorder=10)
    plt.scatter(data_EI['spiral_meta_model.x'],data_EI['spiral_meta_model.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)

    plt.axis([0.75,5*pi,0.75,5*pi])

    plt.figure()
    plt.scatter(Z1,Z2)
    plt.scatter(f1_train,f2_train,s=30,c='#572E07',zorder=10)
    plt.scatter(f1_iter,f2_iter,s=30,c=colors,zorder=11,cmap=color_map)

    plt.show()
    analysis.cleanup()
