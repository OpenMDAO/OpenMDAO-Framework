'''Single Ojective EI Example'''

from tempfile import mkdtemp
import os.path
import shutil

from openmdao.lib.components.api import MetaModel, ExpectedImprovement, \
                                        ParetoFilter
from openmdao.lib.doegenerators.api import OptLatinHypercube
from openmdao.lib.drivers.adaptivesampledriver import AdaptiveSampleDriver
from openmdao.lib.drivers.api import Genetic, FixedPointIterator
from openmdao.lib.optproblems.branin import BraninComponent
from openmdao.lib.surrogatemodels.api import KrigingSurrogate
from openmdao.main.api import Assembly, Component
from openmdao.lib.datatypes.api import Float

class BraninConstraintComponent(BraninComponent):
    """ Constraint x* """
    g_xy = Float(iotype='out')
    def execute(self):
        super(BraninConstraintComponent, self).execute()
        self.g_xy = -((self.x +5.) * self.y - 45.)



class Analysis(Assembly):
    '''Top level assembly for the Single Ojective EI Example.'''

    def __init__(self):
        super(Analysis, self).__init__()

        self._tdir = mkdtemp()

    def configure(self):

        driver = self.add('driver', FixedPointIterator())
        adapt = self.add('adapt', AdaptiveSampleDriver())
        ei_opt = self.add('ei_opt', Genetic())

        branin = self.add('branin', BraninConstraintComponent())

        kwargs = {'params':("x", "y"),
                  'responses':('f_xy', 'g_xy' )}
        meta = self.add('meta', MetaModel(**kwargs))
        meta.default_surrogate = KrigingSurrogate()

        kwargs = {'params':("x", "y"),
                  'responses':('f_xy', ),
                  'constraints':('g_xy', )}
        pareto = self.add('pareto', ParetoFilter(**kwargs))

        ei_obj = self.add('ei_obj', ExpectedImprovement())
        ei_cons = self.add('ei_cons', ExpectedImprovement())

        #initial training DOE
        adapt.DOEgenerator = OptLatinHypercube(num_samples=15)
        #adapt.DOEgenerator = Uniform(100)
        adapt.add_parameter('branin.x', low=-5, high=10)
        adapt.add_parameter('branin.y', low=0, high=15)
        adapt.add_response('branin.f_xy')
        adapt.add_response('branin.g_xy')

        #pass training data from sampler to metamodel and pareto filter
        self.connect('adapt.all_case_inputs.branin.x',
                     ['meta.params.x', 'pareto.params.x'])
        self.connect('adapt.all_case_inputs.branin.y',
                     ['meta.params.y', 'pareto.params.y'])
        self.connect('adapt.all_case_outputs.branin.f_xy',
                     ['meta.responses.f_xy', 'pareto.responses.f_xy'])

        self.connect('adapt.all_case_outputs.branin.g_xy',
                     ['meta.responses.g_xy', 'pareto.constraints.g_xy'])

        #connect meta and pareto to ei
        self.connect('meta.f_xy', 'ei_obj.current') #this passes a normal distribution variable
        self.connect('meta.g_xy', 'ei_cons.current') #this passes a normal distribution variable
        self.connect('pareto.pareto_outputs[0, 0]', 'ei_obj.target') #for single objective, frontier is just a scalar value that is the minimum of the set
        ei_cons.target = 0.0


        #EI optimization to find next point
        ei_opt.opt_type = "maximize"
        ei_opt.population_size = 100
        ei_opt.generations = 10
        ei_opt.add_parameter('meta.x', low=-5, high=10)
        ei_opt.add_parameter('meta.y', low=0, high=15)
        ei_opt.add_objective('ei_obj.PI * ei_cons.PI') #could use ei.EI too

        #Iterative sampling process
        # TODO: Note, soon low and high will not be needed.
        driver.add_parameter('adapt.adaptive_inputs.branin.x[0]',
                             low=-1e99, high=1e99)
        driver.add_parameter('adapt.adaptive_inputs.branin.y[0]',
                             low=-1e99, high=1e99)
        driver.add_constraint('adapt.adaptive_inputs.branin.x[0] = meta.x')
        driver.add_constraint('adapt.adaptive_inputs.branin.y[0] = meta.y')
        driver.max_iterations = 30

        #Iteration Heirarchy
        driver.workflow.add(['adapt', 'pareto', 'ei_opt'])
        adapt.workflow.add(['branin'])
        ei_opt.workflow.add(['meta', 'ei_obj', 'ei_cons'])

        #FPI now support stop conditions
        driver.add_stop_condition('ei_obj.EI * ei_cons.PI <= .0001')

    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)


if __name__ == "__main__": #pragma: no cover

    import sys

    import matplotlib
    from matplotlib import pyplot as plt
    from matplotlib.pylab import get_cmap
    from numpy import meshgrid, array, pi,arange,cos

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

    if backend is not None:
        matplotlib.use(backend)

    elif sys.platform == 'win32':
        matplotlib.use('WxAgg')

    analysis = Analysis()
    analysis.run()

    points = [(-pi, 12.275, .39789),
              (pi, 2.275, .39789),
              (9.42478, 2.745, .39789)]

    for x, y, z in points:
        print "x: ", x, "; y: ", y
        analysis.meta.x = x
        analysis.meta.y = y
        analysis.meta.execute()
        print "f_xy: ",analysis.meta.f_xy, " % error: ", \
              (analysis.meta.f_xy.mu - z)/z*100

    #Generate the Contour plot to show the function
    def branin(x,y):
        return (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+\
               10.*(1.-1./(8.*pi))*cos(x)+10.


    X_range = arange(-5, 10.2, .25)
    Y_range = arange(0, 15.2, .25)

    X,Y = meshgrid(X_range, Y_range)
    Z = branin(X, Y)

    plt.contour(X, Y, Z, arange(1,200,2), zorder=1)

    cb = plt.colorbar(shrink=.45)

    #plot constraint bounday
    def branin_cons(x):
        return 45./(x+5.)

    x_cons = arange(-2, 10.2, .25)
    y_cons = branin_cons(x_cons)
    plt.plot(x_cons, y_cons, 'k')

    #plot the initial training data
    data_train = {}
    data_train['meta.y'] = analysis.adapt.DOE_inputs.branin.y
    data_train['meta.x'] = analysis.adapt.DOE_inputs.branin.x
    data_train['meta.f_xy'] = analysis.adapt.DOE_outputs.branin.f_xy

    plt.scatter(data_train['meta.x'],
                data_train['meta.y'], s=30, c='#572E07', zorder=10)


    n_train = len(data_train['meta.y'])
    data_EI = {}
    data_EI['meta.y'] = analysis.adapt.all_case_inputs.branin.y[n_train:]
    data_EI['meta.x'] = analysis.adapt.all_case_inputs.branin.x[n_train:]
    data_EI['meta.f_xy'] = analysis.adapt.all_case_outputs.branin.f_xy[n_train:]

    count = len(data_EI['meta.x'])
    colors = arange(0,count)/float(count)

    color_map = get_cmap('spring')

    print "# adaptive samples:", len(data_EI['meta.x'])

    points = [(-pi,12.275,.39789), (pi,2.275,.39789), (9.42478,2.745,.39789)]
    distance = [10000., 10000., 10000.]
    closest_points = [(), (), ()]
    for x,y,z in zip(data_EI['meta.x'], data_EI['meta.y'], data_EI['meta.f_xy']):
        for i,p in enumerate(points):
            d = ((p[0]-x)**2 + (p[1]-y)**2)**.5
            if d < distance[i]:
                distance[i] = d
                closest_points[i] = (x, y, z)

    print "closest solutions: ", closest_points

    plt.scatter(data_EI['meta.x'], data_EI['meta.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)

    plt.axis([-5, 10, 0, 15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Function Contours and EI Sample Points")
    plt.text(10.9,11, "Branin\nFunction\nValue")

    if figname is not None:
        matplotlib.pylab.savefig(figname)

    plt.figure()
    Z2 = []

    for x_row,y_row in zip(X, Y):
        row = []
        for x,y in zip(x_row, y_row):
            analysis.meta.x = x
            analysis.meta.y = y
            analysis.meta.execute()
            row.append(analysis.meta.f_xy.mu)
        Z2.append(row)
    Z2 = array(Z2)

    plt.contour(X, Y, Z2, arange(1, 200, 2), zorder=1)
    cb = plt.colorbar(shrink=.45)
    plt.axis([-5, 10, 0, 15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Meta Model Contours")
    plt.text(10.9,11, "Meta Model\nFunction\nValue")

    plt.show()

    analysis.cleanup()

