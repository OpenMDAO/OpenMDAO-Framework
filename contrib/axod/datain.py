"""
    datain.py     -  for component.
    allows to get input values from other components
              modifies input file with new values
              
    input values:  input_filename = input filename
    use as:  Axod_Compn(input_filename='   ')
                


"""

import os.path
import shutil
import sys

if '.' not in sys.path:
    sys.path.append('.')

# pylint: disable-msg=E0611,F0401
from numpy import float32, zeros, ndarray


from openmdao.main.api import Component, Container
from openmdao.main.datatypes.api import Float, Array, Str, Int

__all__ = ('AxodCompn',)

_ZEROS48 = zeros((48,),float32)
_ZEROS5  = zeros((5,),float32)

class Datain(Container):


#    def __init__(self,name):
#        Container.__init__(self, name)        
#    """ namelist 'DATAIN'  for AXOD. """


    ttin = Float(iotype='in', units='degR',
                 desc='Inlet total temperature (radially constant).')

    ptin = Float(iotype='in', units='psi',
                 desc='Inlet total pressure (radially constant).')

    pfind = Float(iotype='in',
                  desc='Turbine total pressure ratio to be searched for')

    rpm = Float(iotype='in', units=('1/min'), desc='Stage rotative speed.')

    #seta = Float(iotype='in', desc='Stator efficiency, decimal.')
    seta   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #scf = Float(iotype='in', desc='Stator flow coefficient, decimal.')
    scf    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #reta = Float(iotype='in', desc='Rotor efficiency, decimal.')
    reta   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rcf = Float(iotype='in', desc='Rotor flow coefficient, decimal.')
    rcf    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')
 
    #ttinh = Float(iotype='in',  units= 'degR',  \
    #               desc='Inlet total temperature radial distribution')
    ttinh    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')
                  
    #ptinh = Float(iotype='in',  units= 'psi',
    #               desc='Inlet total pressure distribution')
    ptinh    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #alf0h = Float(iotype='in',  units= 'deg',
    #               desc='Inlet flow angle radial distribution')
    alf0h    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    wair =  Float(iotype='in', desc='Inlet water/air ratio')

    fair =  Float(iotype='in', desc='Inlet fuel/air ratio')

    ptps =  Float(iotype='in', 
          desc='Starting value of first-stator meanline Inlet total to exit-static pressure ratio')
 
    delc =  Float(iotype='in', desc='ptps increment to initial blade-row choke')

    dell =  Float(iotype='in', desc='ptps increment from initial to last blade-row choke')

    dela =  Float(iotype='in', desc='ptps increment from last blade-row choke to exit-annulus choke')

    stg  =  Float(iotype='in', desc='Number of stages, max 8')

    sect =  Float(iotype='in', desc='Number of radial sectors, max 6')

    expn =  Float(iotype='in', desc='Negative-incidence exponent, default=4.0')

    rg   =  Float(iotype='in', units='(ft*lbf)/(lbm*degR)',desc='Gas constant')

    paf  =  Float(iotype='in',desc='Profile (temp & press) averaging switch for next stage inlet')

    sli  =  Float(iotype='in',desc='Stage loss-value for srec, seta, scf, rrec, reta, rcf, rtf')

    aacs =  Float(iotype='in',desc='Turbine-exit mach number for termination of speed line')

    vctd =  Float(iotype='in',desc='Output Switch (1.0-overall performance plus all variable values)')

    #pcnh =  Float(iotype='in',desc='Sector height distribution, fraction of annulus height')
    pcnh     = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    wg   =  Float(iotype='in',units='lb/s',desc='Mass flow rate')

    epr  =  Float(iotype='in',desc='switch for high pressure-ratio correction to blade row efficiency')

    wtol =  Float(iotype='in',desc='Tolerance for mass-flow rate convergence, default=1.0e-5')

    rhotol = Float(iotype='in',desc='Tolerance for density convergence, default=1.0e-4')

    prtol = Float(iotype='in',desc='Tolerance for presuure ratio convergenc, default=1.0e-8')

    trloop = Float(iotype='in',desc='Debug output switch for iteration control variables')

    pfind  = Float(iotype='in',desc='Selected value of turbine total pressure to be searched for')

    dhfind  = Float(iotype='in',desc='Selected value of turbine specific work to be searched for')

    iar    = Int(iotype='in',desc='Switch for axial chord length, default=0')

    icyl   = Int(iotype='in',desc='Switch for blading angle definition,default=0')

    icf    = Int(iotype='in',desc='Switch for flow coefficient variation, default=0')

    endplt = Float(iotype='in',desc='Switch for writing a map file in NEPP format,default=0')

    endjob = Float(iotype='in',desc='Switch for last case, default=0')

    stage  = Float(iotype='in',desc='Stage Number')

    #gamg   = Float(iotype='in',desc='Specific heat ratio')
    gamg     = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #dr     = Float(iotype='in',units='inch',desc='Hub diameter')
    dr     = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #dt     = Float(iotype='in',units='inch',desc='Tip diameter')
    dt     = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rwg    = Float(iotype='in',desc='Ratio of Station mass-flow rate to turbine inlet mass-flow rate')
    rwg    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #twg    = Float(iotype='in',units='degR',desc='Temperature of the Cooolant specified by rwg')
    twg    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #pwg    = Float(iotype='in',units='psi',desc='Pressure of the Cooolant specified by pwg')
    pwg    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #sdia   = Float(iotype='in',units='deg',desc='Stator vane inlet angle')
    sdia   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #sdea   = Float(iotype='in',units='deg',desc='Stator vane exit angle')
    sdea   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #spa   = Float(iotype='in',units='inch**2',desc='Stator throat area per unit height')
    spa    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    sesth = Float(iotype='in',desc='Ratio of blade height at stator exit to blade height at stator throat')

    #rdia   = Float(iotype='in',units='deg',desc='Rotor blade inlet angle')
    rdia   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rdea   = Float(iotype='in',units='deg',desc='Rotor blade exit angle')
    rdea   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rpa   = Float(iotype='in',units='inch**2',desc='Rotor throat area per unit height')
    rpa    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    rerth = Float(iotype='in',desc='Ratio of blade height at rotor exit to blade height at rotor throat')

    #srec  = Float(iotype='in',desc='Stator inlet recovery efficiency, decimal')
    srec   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rrec  = Float(iotype='in',desc='Rotor inlet recovery efficiency, decimal')
    rrec   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rtf   = Float(iotype='in',desc='Rotor test factor, decimal')
    rtf    = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rvu1  = Float(iotype='in',units='inch*ft/s',desc='Design Stator-exit angular momentum')
    rvu1   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    #rvu2  = Float(iotype='in',units='inch*ft/s',desc='Design rotor-exit angular momentum')
    rvu2   = Array(_ZEROS5,dtype=float32,shape=(5,),iotype='in')

    endstg  = Float(iotype='in',desc='Switch for last stage, 1.0=last stage')

    expp    = Float(iotype='in',desc='positive-incidence exponent def=3.0  ')
    def __init__(self):
        super(Datain, self).__init__()        
        self._inputs = ['ttin', 'ptin', 'pfind', 'rpm', 'seta', 'scf', 'reta', 'rcf', \
                        'ttinh','ptinh','alf0h','wair','fair','ptps','delc','dell','dela',    \
                        'stg','sect','expn','expp','rg','paf','sli','aacs','vctd','epr', \
                        'wtol','rhotol','prtol','trloop','pfind','dhfind','iar','icyl', \
                        'icf','endplt','endjob','pwg','spa','sesth','trdiag',\
                        'rpa','rerth','rvu1','rvu2','endstg','stage','rdia',  \
                        'pcnh','gamg','sdia','srec','rtf','rrec','rwg','twg','sdea','rdea',
                        'wg','dr','dt']

        self.ind = 0
        self._nml_modified = []
        self.on_trait_change(self._trait_change, '+iotype')

    def _trait_change(self, obj, name, old, new):
        """
        Track *changes* so we don't output more than intended.
        This will *not* get called when the value is set but not changed.
        """
        if name not in self._nml_modified:
            self._nml_modified.append(name)

    

    def read(self,stream):
        #  reads the namelist datain..................
        #print self._inputs
#        for name in self._inputs:
        self._nml_modified = []
        line = stream.readline().strip()
        while line:
            #print  'line=',line
            if '/'  in line: return True
            if '&END'  in line: return True
            if '$END' in line: return True
            #print  'line=',line,'  before = in line'
            fields = line.split('=')
            name0 = fields[0].strip().lower()
            for name in self._inputs:
                if name == name0 :
                    if name not in self._nml_modified:
                        self._nml_modified.append(name)
                    value0  = fields[-1]
                    #print "'%s' found in '%s'" % (name0, value0)
                    val_field = value0.split(',')
                    #print  'fields in val_field =', len(val_field)
                    if len(val_field) > 2:
                        i = 1
                        while i < len(val_field):
                            #print 'name0=',name0,' val_field[',i-1,']=',val_field[i-1]
                            if ('*' in val_field[i-1]):
                                fld = val_field[i-1].split('*')
                                ndi = int(fld[0])
                                #print ' ndi =',ndi
                                id = 1
                                while id < ndi:
                                    val = float(fld[1])
                                    #print 'self.set', name, val, i-1
                                    self.set(name,val,[i-1])
                                    #print  ' val = ',val,'  in * part'  
                                    value = float(val)
                                    #print  ' value[',i,'] = ',value,'  in * part'  
                                    id  = id + 1
                                    i = i + 1
                            else:
                                #print ' after else *******  1'
                                val = float(val_field[i-1])
                                #print 'self.set', name, val, i-1
                            self.set(name,val,[i-1])
                            #name0[i] = name
                            value = float(val)
                            #print  ' value[',i-1,'] = ',value,'  in else part'  
                            #print  'name =',name,' value=',value
                            i = i + 1
                            #print '  i =',i,'  end of if ...'

                    else:
                        #print ' after else *******  2'
                        #val = value0.replace(',','')
                        val = val_field[0]
                        #print ' name=',name,' val =',val,'  after   array........'
                        if ('*' in val):
                            fld = val.split('*')
                            nfld = int(fld[0])
                            val1 = float(fld[-1])
                            value = float(val1)
                            #print  'name=',name,'  value= ',value,'  for * variable'
                            ii = 0
                            while ii < nfld:
                                self.set(name,value,[ii])
                                ii = ii + 1
                        else:
                            #print ' name=',name,' val =',val,'  befoe attrval........'
                            attrval = getattr(self,name)
                            if isinstance(attrval,int):
                                #print 'name =',name,' val =',val
                                value = int(val)
                                setattr(self, name, value)
                            else:
                                value = float(val)
                                setattr(self, name, value)
                                # parse the value and then setattr(self, name, value)
                                #print ' reading a new line******************'
            line = stream.readline().strip()
#        if ('/' in line) or ('&END' in line)  or ('$END'in line) : return
        return False


    def write(self,stream):
        #  reads the namelist datain..................
        #print  'in  write  .... datain ....'
        stream.write(' &DATAIN\n')
        for name in self._nml_modified:
            #print  'name =', name
            value = getattr(self, name)
            if isinstance(value, ndarray):
                stream.write('    %s = ' % name.upper())
                for val in value:
                    stream.write('%s,' % val)
                stream.write('\n')
            else:
                stream.write('    %s = %s,\n' % (name.upper(), value))
        stream.write('    /\n')
        return

