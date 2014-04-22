import os.path

from openmdao.main.problem_formulation import ArchitectureAssembly

from openmdao.lib.architectures.api import EGO

from openmdao.lib.optproblems.branin import BraninComponent


class Analysis(ArchitectureAssembly):
    def __init__(self):
        super(Analysis, self).__init__()

        self.add('branin', BraninComponent())

        #Problem Formulation

        #Local Des Vars
        self.add_parameter('branin.x',low=-5.,high=10.)
        self.add_parameter('branin.y',low=0.,high=15.)

        #No Global Des Vars or Coupling Vars

        #Objective (single only)
        self.add_objective('branin.f_xy')
        #No constraints for this problem




if __name__ == "__main__":
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
    from numpy import meshgrid,array, pi,arange,cos

    analysis = Analysis()
    analysis.architecture = EGO()
    analysis.architecture.initial_DOE_size = 20
    analysis.architecture.sample_iterations = 20
    analysis.architecture.EI_PI = "PI"
    analysis.architecture.min_ei_pi = .01
    analysis.run()


    #A whole bunch of stuff to make some cool plots!
    points = [(-pi,12.275,.39789),(pi,2.275,.39789),(9.42478,2.745,.39789)]
    for x,y,z in points:
        print "x: ", x, "; y: ", y
        analysis.branin.x = x
        analysis.branin.y = y
        analysis.branin.execute()
        print "f_xy: ",analysis.branin.f_xy, " % error: ", \
              (analysis.branin.f_xy.mu - z)/z*100

    #Generate the Contour plot to show the function
    def branin(x,y):
        return (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+\
               10.*(1.-1./(8.*pi))*cos(x)+10.

    X_range = arange(-5,10.2,.25)
    Y_range = arange(0,15.2,.25)

    X,Y = meshgrid(X_range,Y_range)
    Z = branin(X,Y)

    plt.contour(X,Y,Z,arange(1,200,2),zorder=1)

    cb = plt.colorbar(shrink=.45)
    #plot the initial training data
    data_train = case_db_to_dict(os.path.join(analysis.architecture._tdir,'trainer.db'),
                                     ['branin.y',
                                      'branin.x',])

    plt.scatter(data_train['branin.x'],
                data_train['branin.y'],s=30,c='#572E07',zorder=10)

    data_EI = {}
    data_EI['branin.x'] = data_train['branin.x'][analysis.architecture.initial_DOE_size:]
    data_EI['branin.y'] = data_train['branin.y'][analysis.architecture.initial_DOE_size:]


    count = len(data_EI['branin.x'])
    print "%d adaptively sampled points"%count
    colors = arange(0,count)/float(count)

    color_map = get_cmap('spring')


    plt.scatter(data_EI['branin.x'],data_EI['branin.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)

    plt.axis([-5,10,0,15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Function Contours and EI Sample Points")
    plt.text(10.9,11,"Branin\nFunction\nValue")

    if figname is not None:
        matplotlib.pylab.savefig(figname)

    plt.figure()
    Z2 = []

    for x_row,y_row in zip(X,Y):
        row = []
        for x,y in zip(x_row,y_row):
            analysis.branin.x = x
            analysis.branin.y = y
            analysis.branin.execute()
            row.append(analysis.branin.f_xy.mu)
        Z2.append(row)
    Z2 = array(Z2)
    plt.contour(X,Y,Z2,arange(1,200,2),zorder=1)
    cb = plt.colorbar(shrink=.45)
    plt.axis([-5,10,0,15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Meta Model Contours")
    plt.text(10.9,11,"Meta Model\nFunction\nValue")

    plt.show()

    analysis.architecture.cleanup()
