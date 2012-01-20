"""
    ``doedriver.py`` -- Driver that executes a Design of Experiments.
    
"""

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import ListStr, Slot,Float

from openmdao.main.case import Case
from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.drivers.caseiterdriver import CaseIterDriverBase
from openmdao.util.decorators import add_delegate
from openmdao.main.hasparameters import HasParameters
from numpy import exp,var

@add_delegate(HasParameters)

class DOEdriver(CaseIterDriverBase):
    """ Driver for Design of Experiments """
    
    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, iotype='in', required=True,
                          desc='Iterator supplying normalized DOE values.')
    
    case_outputs = ListStr([], iotype='in', 
                           desc='A list of outputs to be saved with each case.')
    
    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        params = self.get_parameters().values()
        self.DOEgenerator.num_parameters = len(params)
        
        for row in self.DOEgenerator:
            vals = [p.low+(p.high-p.low)*val for p,val in zip(params,row)]
            case = self.set_parameters(vals, Case(parent_uuid=self._case_id))
            # now add events
            for varname in self.get_events(): 
                case.add_input(varname,True)
            case.add_outputs(self.case_outputs)    
            
            yield case
            
def f1(k):
    return 1/(1+k)
def f2(k):
    return k/(1+k)
def b(k,beta=0.1):
    return beta*exp(-10/(k*beta)) 
    
@add_delegate(HasParameters)            
class NeiborhoodDOEdriver(CaseIterDriverBase):
    """ Driver for Design of Experiments within a specified neighborhood around a point"""
    
    # pylint: disable-msg=E1101
    DOEgenerator = Slot(IDOEgenerator, iotype='in', required=True,
                          desc='Iterator supplying normalized DOE values.')
    
    case_outputs = ListStr([], iotype='in',desc='A list of outputs to be saved with each case.')
    
    alpha=Float(1., iotype='in',desc='Multiplicative factor for neighborhood DOE Driver')
    
    beta=Float(1., iotype='in',desc='Another factor for neighborhood DOE Driver')

    def get_case_iterator(self):
        """Returns a new iterator over the Case set."""
        return self._get_cases()
        
    def _get_cases(self):
        params = self.get_parameters().values()
        self.DOEgenerator.num_parameters = len(params)
        
        M=[]
        P=[]
        
        for p in params:
            temp= p.evaluate()
            P.append(temp)
            M.append((temp-p.low)/(p.high-p.low))
        
        for row in list(self.DOEgenerator)+[tuple(M)]:
            vals=[]
            for p,val,curval in zip(params,row,P):
                #create multiplicative factor
                k=min([curval-p.low,p.high-curval])
                
                
                
                newlow_a=curval-self.alpha*abs(curval-p.low)**(self.beta)
                newhigh_a=curval+self.alpha*abs(p.high-curval)**(self.beta)
                newval_a = newlow_a+(newhigh_a-newlow_a)*val
                
                
                
                '''
                k_left=curval-p.low
                k_right=p.high-curval
                k_prime=abs(curval)*self.beta+0.1
                if k_left==0:
                    klist=[k_right,k_prime]
                    k_min=min(klist)
                    newval_b=curval+k_min*val
                    
                elif k_right==0:
                    klist=[k_left,k_prime]
                    k_min=min(klist)
                    newval_b=curval-k_min*(1-val)
                    
                else:
                    klist=[k_left,k_right,k_prime]
                    k_min=min(klist)
                    
                    if val<0.5:
                        newval_b=curval-k_min+2*val*k_min
                    else:
                        newval_b=curval+k_min-2*(val-0.5)*k_min
                '''
                
                #print curval,newval_b,val
                newval=newval_a
                
                #mult=self.alpha*f1(k)+b(k,beta=self.beta)*f2(k)

                #new value
                #newval=curval*(1+(val-0.5)*mult)
                
                #newval=(p.low+(p.high-p.low)*val-curval)*self.alpha+curval
                
                
				#respect boundaries
                #print '%s: %f,%f'%(p.name,curval,newval),
                
				#respect boundaries
                if newval<p.low:
                    newval=p.low
                elif newval>p.high:
                    newval=p.high
                
                vals.append(newval)
                
            case = self.set_parameters(vals, Case(parent_uuid=self._case_id))
            #print 
            # now add events
            for varname in self.get_events(): 
                case.add_input(varname,True)
            case.add_outputs(self.case_outputs)              
            yield case            
