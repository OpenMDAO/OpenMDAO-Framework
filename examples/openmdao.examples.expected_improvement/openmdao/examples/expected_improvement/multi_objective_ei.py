'''Multi Ojective EI Example'''

from tempfile import mkdtemp
import shutil

from numpy import sin, cos, pi

from openmdao.main.api import Assembly
from openmdao.lib.components.api import MetaModel, ParetoFilter, \
     MultiObjExpectedImprovement
from openmdao.lib.doegenerators.api import OptLatinHypercube
from openmdao.lib.drivers.adaptivesampledriver import AdaptiveSampleDriver
from openmdao.lib.drivers.api import Genetic, FixedPointIterator
from openmdao.lib.surrogatemodels.api import KrigingSurrogate

from openmdao.examples.expected_improvement.spiral_component import SpiralComponent


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
        self.add('pareto', ParetoFilter(**kwargs))

        self.add('MOEI', MultiObjExpectedImprovement())

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

        #connect meta and pareto to ei
        self.connect('[meta.f1_xy, meta.f2_xy]', 'MOEI.current')
        self.connect('pareto.pareto_outputs', 'MOEI.target')

        # MOEI optimization to find next point
        MOEI_opt.opt_type = "maximize"
        MOEI_opt.population_size = 100
        MOEI_opt.generations = 10
        #MOEI_opt.selection_method = "tournament"
        MOEI_opt.add_parameter("meta.x", low=0.75, high=5.*pi)
        MOEI_opt.add_parameter("meta.y", low=0.75, high=5.*pi)
        MOEI_opt.add_objective("MOEI.PI")

        #Iterative sampling process
        driver.add_parameter('adapt.adaptive_inputs.spiral.x[0]')
        driver.add_parameter('adapt.adaptive_inputs.spiral.y[0]')
        driver.add_constraint('adapt.adaptive_inputs.spiral.x[0] = meta.x')
        driver.add_constraint('adapt.adaptive_inputs.spiral.y[0] = meta.y')
        driver.max_iterations = 30

        #Iteration Heirarchy
        driver.workflow.add(['adapt', 'pareto', 'MOEI_opt'])
        adapt.workflow.add(['spiral'])
        MOEI_opt.workflow.add(['meta', 'MOEI'])

        #FPI now support stop conditions
        driver.add_stop_condition('MOEI.PI <= .0001')


    def cleanup(self):
        """cleans up any files left in the temp directory from execution"""
        shutil.rmtree(self._tdir, ignore_errors=True)


if __name__ == "__main__": #pragma: no cover

    import sys

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

    from matplotlib import pyplot as plt
    from matplotlib.pylab import get_cmap
    from numpy import meshgrid, array, arange


    #create the analysis
    analysis = Analysis()

    #run the analysis
    analysis.run()


    #plot the samples points, along with the data from the function
    def f1(x, y):
        return cos(x)/x+sin(y)/y

    def f2(x, y):
        return sin(x)/x+cos(y)/y

    X_range = arange(0.75, 5.*pi, 0.5)
    Y_range = arange(0.75, 5.*pi, 0.5)

    X,   Y = meshgrid(X_range, Y_range)
    Z1, Z2 = f1(X, Y), f2(X, Y)

    plt.figure()
    plt.subplot(121)
    plt.contour(X, Y, Z1, 50)
    plt.axis([0.75, 5*pi, 0.75, 5*pi])

    plt.subplot(122)
    plt.contour(X, Y, Z2, 50)
    cb = plt.colorbar(shrink=.6)
    plt.axis([0.75, 5*pi, 0.75, 5*pi])

    plt.figure()

    Z1_pred = []
    Z2_pred = []

    for x_row, y_row in zip(X, Y):
        row1 = []
        row2 = []
        for x, y in zip(x_row, y_row):
            analysis.meta.x = x
            analysis.meta.y = y
            analysis.meta.execute()
            row1.append(analysis.meta.f1_xy.mu)
            row2.append(analysis.meta.f2_xy.mu)
        Z1_pred.append(row1)
        Z2_pred.append(row2)
    Z1_pred = array(Z1_pred)
    Z2_pred = array(Z2_pred)

    #plot the initial training data
    data_train = {}
    data_train['meta.y'] = analysis.adapt.DOE_inputs.spiral.y
    data_train['meta.x'] = analysis.adapt.DOE_inputs.spiral.x
    data_train['meta.f1_xy'] = analysis.adapt.DOE_outputs.spiral.f1_xy
    data_train['meta.f2_xy'] = analysis.adapt.DOE_outputs.spiral.f2_xy

    plt.scatter(data_train['meta.x'],
                data_train['meta.y'], s=30, c='#572E07', zorder=10)

    n_train = len(data_train['meta.y'])
    data_EI = {}
    data_EI['meta.y'] = analysis.adapt.all_case_inputs.spiral.y[n_train:]
    data_EI['meta.x'] = analysis.adapt.all_case_inputs.spiral.x[n_train:]
    data_EI['meta.f1_xy'] = analysis.adapt.all_case_outputs.spiral.f1_xy[n_train:]
    data_EI['meta.f2_xy'] = analysis.adapt.all_case_outputs.spiral.f2_xy[n_train:]

    count = len(data_EI['meta.x'])
    colors = arange(0, count)/float(count)
    color_map = get_cmap('spring')

    f1_train = [case for case in data_train['meta.f1_xy']]
    f2_train = [case for case in data_train['meta.f2_xy']]
    f1_iter = [case for case in data_EI['meta.f1_xy']]
    f2_iter = [case for case in data_EI['meta.f2_xy']]

    plt.subplot(121)
    plt.contour(X, Y, Z1_pred, 50)
    plt.scatter(data_train['meta.x'],
                data_train['meta.y'], s=30, c='#572E07', zorder=10)
    plt.scatter(data_EI['meta.x'], data_EI['meta.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)
    plt.axis([0.75, 5*pi, 0.75, 5*pi])

    plt.subplot(122)
    plt.contour(X, Y, Z2_pred, 50)
    cb = plt.colorbar(shrink=.6)
    plt.scatter(data_train['meta.x'],
                data_train['meta.y'], s=30, c='#572E07', zorder=10)
    plt.scatter(data_EI['meta.x'], data_EI['meta.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)

    plt.axis([0.75, 5*pi, 0.75, 5*pi])

    plt.figure()
    plt.scatter(Z1, Z2)
    plt.scatter(f1_train, f2_train, s=30, c='#572E07', zorder=10)
    plt.scatter(f1_iter, f2_iter, s=30, c=colors, zorder=11, cmap=color_map)

    plt.show()
    analysis.cleanup()
