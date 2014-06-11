from ordereddict import OrderedDict

from petsc4py import PETSc

class ParamVec(OrderedDict):
    """ An ordereddict of varnames to views within the parameter 
    array of a nonlinear system.
    """

    def __init__(self, comp, array):
        self.array = array
        i1, i2 = 0, 0
        for key in comp.variables:
            self[key] = OrderedDict()

        for subsystem in comp.localSubsystems:
            for key in subsystem.variables:
                args = subsystem.variables[key]['args']
                for key2 in args:
                    if key2 not in subsystem.variables \
                            and key2 in comp.variables:
                        i2 += args[key2].shape[0]
                        self[key][key2] = self.array[i1:i2]
                        i1 += args[key2].shape[0]

        if comp.localSubsystems == []:
            n,c = comp.name, comp.copy
            args = comp.variables[n,c]['args'] 
            if (n,c) in args:
                i2 += args[n,c].shape[0]
                self[n,c][n,c] = self.array[i1:i2]
                i1 += args[n,c].shape[0]
                
        self.petsc_vec = PETSc.Vec().createWithArray(self.array, 
                                                     comm=comp.comm)        
    
    def __call__(self, inp1=[], inp2=[]):
        system = self.system
        if inp2 == []:
            return self[system.name, system.copy][system._ID(inp1)]
        else:
            return self[system._ID(inp1)][system._ID(inp2)]

class UnknownsVec(OrderedDict):
    """ An ordereddict of varnames to views within the unknown 
    array of a nonlinear system.
    """

    def __init__(self, comp):
        super(UnknownsVec, self).__init__()
        i1, i2 = 0, 0
        for i, key in enumerate(comp.variables):
            size = comp.varSizes[comp.rank, i]
            i2 += size
            self[key] = self.array[i1:i2]
            i1 += size

        return PETSc.Vec().createWithArray(self.array, 
                                           comm=comp.comm)

    def __call__(self, var=[]):
        system = self.system
        if var == []:
            return self[system.name, system.copy]
        else:
            return self[system._ID(var)]

