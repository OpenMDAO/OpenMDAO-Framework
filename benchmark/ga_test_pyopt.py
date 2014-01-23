import pyOpt 
import time

count = 0

def rosenbrock(x):
    """2-dimensional Rosenbrock Function"""
    global count
    f = (1-x[0])**2+100*(x[1]-x[0]**2)**2
    g = [0.0]*1
    g[0] = x[0] + x[1] - 2.0
    fail=0  #Flag for function evaluation failure
    count += 1
    return f,g,fail

def main():
    """NSGA2 Optimization of 2D Rosenbrock Function"""
    #Problem setup
    opt_prob = pyOpt.Optimization('2D Rosenbrock',rosenbrock)
    opt_prob.addObj('f')
    opt_prob.addVar('x1','c',lower=0.0,upper=5.0,value=4.0)
    opt_prob.addVar('x2','c',lower=-5.0, upper=5.0,value=4.0)
    opt_prob.addCon('g1','i')

    print opt_prob

    nsga2 = pyOpt.NSGA2()
    nsga2.setOption('maxGen',250)
    nsga2.setOption('pMut_real',0.4)
    #nsga2.setOption('PrintOut',2) #Control output files

    import time
    tt = time.time()
    #Run the problem
    nsga2(opt_prob)
    print "Elapsed time: ", time.time() - tt, "seconds"
    print "Function Evaluations: ", count

    #Print the solution
    print opt_prob.solution(0)

if __name__=='__main__':
    main()