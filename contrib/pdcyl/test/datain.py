"""
    datain.py     -  for component.
    allows to get input values from other components
              modifies input file with new values
              
    input values:  input_filename = input filename
    use as:  PdcylComp(input_filename='   ')
                


"""

import os.path
import shutil
import sys

if '.' not in sys.path:
    sys.path.append('.')

# pylint: disable-msg=E0611,F0401
from numpy import float32, zeros, ndarray


from openmdao.main.api import Component, set_as_top, Container
from openmdao.lib.api import Float, Array, Str, Int

__all__ = ('PdcylComp',)

#_ZEROS48 = zeros((48,),float32)
_ZEROS12  = zeros((12,),int)

class Datain(Container):


#    def __init__(self,name):
#        Container.__init__(self, name)        
#    """  'DATAIN'  for PDCYL.in  """


    icalc   = Int(iotype='in',desc='print switch')
#Wing geometry...
    wsweep  = Float(iotype='in', units='deg',
                 desc='Wing sweep referenced to the leading edge')

    war     = Float(iotype='in',desc='Wing Aspect Ratio')

    wtaper  = Float(iotype='in', desc=' Wing taper ratio')

    wtcroot = Float(iotype='in',desc=' Wing thickness-to-cord at root')

    wtctip  = Float(iotype='in',desc=' Wing thickness-to-cord at tip') 

    warea   =  Float(iotype='in', units='ft**2', desc ='Wing planform area')

#Material properties

    ps     = Float(iotype='in',desc='Plasticity factor') 
    tmgw   = Float(iotype='in',units='inch',desc='Min. gage thickness for the wing')  
    effw   = Float(iotype='in',desc='Buckling efficiency of the web')
    effc   = Float(iotype='in',desc='Buckling efficiency of the covers')
    esw    = Float(iotype='in',units='psi',desc='Young<92>s Modulus for wing material')
    fcsw   = Float(iotype='in',units='psi',desc='Ult. compressive strength of wing')
    dsw    = Float(iotype='in',units='lb/inch**3',desc=' Density of the wing material')
    kdew   = Float(iotype='in',desc=' Knock-down factor for Young''s Modulus')
    kdfw   = Float(iotype='in',desc='Knock-down factor for Ultimate strength')

#Geometric parameters

    istama   = Int(iotype='in',desc=' 1 - Position of wing is unknown; 2 - position is known')
    cs1      = Float(iotype='in',desc='Position of structural wing box from leading edge as percent of root chord')
    cs2      = Float(iotype='in',desc=' Position of structural wing box from trailing edge as percent of root chord')
    uwwg     = Float(iotype='in',units='lb/ft**2',desc=' Wing Weight / Wing Area of baseline aircraft  ')
    xwloc1   = Float(iotype='in',desc=' Location of wing as a percentage of body length')
    #xloc1   = Float(iotype='in',desc=' Location of wing as a percentage of body length')
#   note:  input file has xloc1 and not xwloc1

#Structural Concept
    
    claqr   = Float(iotype='in',desc='Ratio of body lift to wing lift')
    ifuel   = Int(iotype='in',desc=' 1 - No fuel is stored in the wing; 2 - Fuel is stored in the wing')
    cwman   = Float(iotype='in',desc='Design maneuver load factor')
    cf      = Float(iotype='in',desc='Shanley''s const. for frame bending')

#Tails

    itail   = Int(iotype='in',desc=' 1 - Control surfaces mounted on tail; 2 - Control surfaces mounted on wing')
    uwt     = Float(iotype='in',units='lb/ft**2',desc='(Htail Weight + Vtail Weight) / Htail Area of baseline aircraft')
    clrt    = Float(iotype='in', desc=' Location of tail as a percentage of body length')
    harea   = Float(iotype='in', units='ft**2',desc=' Location of tail as a percentage of body length')

#Fuelage geometry
    frn     = Float(iotype='in',desc='Fineness ratio of the nose section      (length/diameter)')
    frab    = Float(iotype='in',desc='Fineness ratio of the after-body section   (length/diameter)')
    bodl    = Float(iotype='in',units='ft',desc='Length of the fuselage  ')
    bdmax   = Float(iotype='in',units='ft',desc='Maximum diameter of fuselage')
    vbod    = Float(iotype='in',units='ft**3',desc='Fuselage total volume ')
    volnose = Float(iotype='in',units='ft**3',desc='Nose Volume')                
    voltail = Float(iotype='in',units='ft**3',desc='Tail volume ')
#      Structural Concept
    ckf     = Float(iotype='in',desc='Frame stiffness coefficient')
    ec      = Float(iotype='in',desc='Power in approximation equation for buckling stability')
    kgc     = Float(iotype='in',desc='Buckling coefficient for component general buckling of stiffener web panel')
    kgw     = Float(iotype='in',desc='Buckling coefficient for component local buckling of web panel')
#     KCONT(12)   ! Structural Geometry Concept Top/Bottom
#     KCONB(12)   ! 2 - Simply stiffened shell, frames, sized for minimum weight in buckling
#                 ! 3 - Z-stiffened shell, frames, best buckling
#                 ! 4 - Z-stiffened shell, frames, buckling-minimum gage compromise
#                 ! 5 - Z-stiffened shell, frames, buckling-pressure compromise
#                 ! 6 - Truss-core sandwich, frames, best buckling
#                 ! 8 - Truss-core sandwich, no frames, best buckling
#                 ! 9 - Truss-core sandwich, no frames, buckling-min. gage-pressure compromise

#   kcont   = Array(_ZEROS12,dtype=int,shape=(12,),iotype='in')
    kcont   = Int(iotype='in',desc=' input as kcont(1)')
    kconb   = Int(iotype='in',desc=' input as kconb(1)')

#   kconb   = Array(_ZEROS12,dtype=int,shape=(12,),iotype='in')

#Material properties
#   ftst    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   ftsb    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   fcst    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   fcsb    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   est     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   esb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   eft     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   efb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dst     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dsb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dft     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   dfb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   tmgt    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
#   tmgb    = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
    ftst    = Float(iotype='in',desc='input as ftst(1) ')
    ftsb    = Float(iotype='in',desc='input as ftsb(1) ') 
    fcst    = Float(iotype='in',desc='input as fcst(1) ') 
    fcsb    = Float(iotype='in',desc='input as fcsb(1) ') 
    est     = Float(iotype='in',desc='input as est(1) ')
    esb     = Float(iotype='in',desc='input as esb(1) ') 
    eft     = Float(iotype='in',desc='input as eft(1) ')
    efb     = Float(iotype='in',desc='input as efb(1) ') 
    dst     = Float(iotype='in',desc='input as dst(1) ') 
    dsb     = Float(iotype='in',desc='input as dsb(1) ') 
    dft     = Float(iotype='in',desc='input as dft(1) ') 
    dfb     = Float(iotype='in',desc='input as dfb(1) ') 
    tmgt    = Float(iotype='in',desc='input as tmgt(1) ') 
    tmgb    = Float(iotype='in',desc='input as tmgb(1) ')
    kde     = Float(iotype='in',desc='Knock-down factor for modulus')
    kdf     = Float(iotype='in',desc='Knock-down factor for strength')
#Geometric parameters
    clbr1   = Float(iotype='in',desc='Fuselage break point as a fraction of total fuselage length')
    icyl    = Int(iotype='in',desc=' 1 - modeled with a mid-body cylinder, 0 - use two power-law bodies back to back')

#Engines
    neng    = Int(iotype='in',desc=' Total number of engines')
    nengwing= Int(iotype='in',desc=' Number of engines on wing')
    wfp     = Float(iotype='in',desc='(Engine Weight * NENG) / WGTO')
    clrw1   = Float(iotype='in',desc=' Location of first engine pair.  Input 0 for centerline engine.') 
    clrw2   = Float(iotype='in',desc=' Location of second engine pair.  measured from body centerline') 
    clrw3   = Float(iotype='in',desc=' Location of third engine pair.  measured from body centerline') 

#Loads
    deslf   = Float(iotype='in',desc='Design load factor')
    ultlf   = Float(iotype='in',desc='Ultimate load factor (usually 1.5*DESLF)')
#   axac    = Float(iotype='in',units='g',desc='Axial acceleration')
    axac    = Float(iotype='in',desc='Axial acceleration')
    cman    = Float(iotype='in',desc=' Weight fraction at maneuver')
    iload   = Int(iotype='in',desc='1 - Analyze maneuver only,2 - Analyze maneuver and landing only')
    #pgt     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
    #pgb     = Array(_ZEROS12,dtype=float,shape=(12,),iotype='in')
    pgt     = Float(iotype='in',desc=' input as pgt(1)') 
    pgb     = Float(iotype='in',desc=' input as pgb(1)')
    wfbump  = Float(iotype='in',desc=' Weight fraction at bump')
    wfland  = Float(iotype='in',desc=' Weight fraction at landing')
    
#Landing Gear
    vsink   = Float(iotype='in',units='ft/s',desc='Design sink velocity at landing ')
    stroke  = Float(iotype='in',units='ft',desc=' Stroke of landing gear  ')
    clrg1   = Float(iotype='in',desc='Length fraction of nose landing gear measured as a fraction of total fuselage length') 
    clrg2   = Float(iotype='in',desc='Length fraction of main landing gear measured as a fraction of total fuselage length') 
    wfgr1   = Float(iotype='in',desc='Weight fraction of nose landing gear')
    wfgr2   = Float(iotype='in',desc='Weight fraction of main landing gear')
    igear   = Int(iotype='in',desc='1 - Main landing gear located on fuselage,2 - Main landing gear located on wing')
    gfrl    = Float(iotype='in',desc='Ratio of force taken by nose landing gear to force taken by main gear at landing')
    clrgw1  = Float(iotype='in',desc='Position of wing gear as a fraction of structural semispan')
    clrgw2  = Float(iotype='in',desc='Position of second pair wing gear as a fraction of structural semispan')

#Weights
    wgto    = Float(iotype='in',units='lb',desc=' Gross takeoff weight')
    wtff    = Float(iotype='in',desc='Weight fraction of fuel')
    cbum    = Float(iotype='in',desc='Weight fraction at bump')
    clan    = Float(iotype='in',desc='Weight fraction at landing')

#Factors
    #ischrenk= Float(iotype='in',desc='1 - use Schrenk load distribution on wing,Else - use trapezoidal distribution')
    ischrenk= Int(iotype='in',desc='1 - use Schrenk load distribution on wing,Else - use trapezoidal distribution')
    icomnd  = Int(iotype='in',desc='1 - print gross shell dimensions envelope,2 - print detailed shell geometry')
    #icomnd  = Float(iotype='in',desc='1 - print gross shell dimensions envelope,2 - print detailed shell geometry')
    wgno    = Float(iotype='in',desc='Nonoptimal factor for wing (including the secondary structure)')
    slfmb   = Float(iotype='in',desc='Static load factor for bumps')
    wmis    = Float(iotype='in',desc='Volume component of secondary structure')
    wsur    = Float(iotype='in',desc='Surface area component of secondary structure')
    wcw     = Float(iotype='in',desc='Factor in weight equation for nonoptimal weights')
    wca     = Float(iotype='in',desc='Factor in weight equation for nonoptimal weights')
    nwing   = Int(iotype='in',desc='Number of wing segments for analysis')
    emptyline = Str('                                                           ')
    onespace =' '

    def __init__(self):
        super(Datain, self).__init__()        
        self._inputs = ['icalc', 'wsweep', 'war', 'wtaper', 'wtcroot', 'wtctip', 'warea', \
                        'ps', 'tmgw', 'effw', 'effc', 'esw', 'fcsw', 'dsw', 'kdew', 'kdfw',   \
                        'istama', 'cs1', 'cs2', 'uwwg', 'xwloc1', 'claqr', 'ifuel', 'cwman', 'cf', \
                        'itail', 'uwt', 'clrt', 'harea', 'frn','frab', 'bodl', 'bdmax', 'vbod', \
                        'volnose', 'voltail', 'ckf', 'ec', 'kgc', 'kgw', 'kcont', 'kconb', 'ftst', 'ftsb',\
                        'fcst', 'fcsb', 'est', 'esb', 'eft', 'efb', 'dst', 'dsb', 'dft', 'dfb', \
                        'tmgt' ,'tmgb', 'kde', 'kdf', 'clbr1', 'icyl' , 'neng', 'nengwing', 'wfp', \
                        'clrw1','clrw2', 'clrw3', 'deslf', 'ultlf', 'axac', 'cman', 'iload', 'pgt', 'pgb', \
                        'cbum', 'clan', \
                        'wfbump', 'wfland', 'vsink', 'stroke', 'clrg1', 'clrg2', 'wfgr1', 'wfgr2', \
                        'igear', 'gfrl', 'clrgw1', 'clrgw2', 'wgto', 'wtff',  'ischrenk', 'icomnd', \
                        'wgno', 'slfmb', 'dlfmb', 'wmis', 'wsur', 'wcw', 'wca', 'nwing', ] 

        #self.iindex = [5, 26, 34, 41, 59, 60, 86, 91, 92, 105, 121, 137, 139, 147 ]
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
        #print 'Entering in read  (datain)************'
        self._nml_modified = []
        line1 = stream.readline()
        i = 1
        while  line1:
            line = line1.strip()
            #print ' in datain  in while loop    i=',i,'  line=',line
            if len(line) == 0:
                while not line:
                    line = stream.readline().strip()
                    i = i + 1
                               
            if ':' in line: 
                line1 = stream.readline()
                i = i + 1
                continue 
            if i == 3: 
                line1 = stream.readline()
                i = i + 1
                continue 
            fields = line.split()
            #print  'fields = ',fields[0],'***',fields[1],'  i=',i 
            value0 = fields[0].strip()
            name0 =  fields[1]
            #print  'name0 =',name0
            if '=' in fields[1]:  
                fld = fields[1].split('=')
                name0 = fld[0]
            else:
                name0 = fields[1]
                     
            #print 'i =',i,'value0',value0,' name0=',name0,'  i=',i
            name = name0
            for name in self._inputs:
                #print  'name =',name,'     in self._inputs....i=',i
                if name == name0:
                    if name not in self._nml_modified:
                        self._nml_modified.append(name)
                    value0 = fields[0]
                    attrval = getattr(self,name)
                    #print  'attrval =',attrval
                    if isinstance(attrval,int):
                        #print 'name =',name,' val =',value0
                        value = int(value0)
                        setattr(self, name, value)
                        #print  'name =',name,'  value =',value,' after setattr'
                    else:
                        value = float(value0)
                        setattr(self, name, value)
                        #print  'name =',name,'  value =',value,' after setattr'
            #print  'name =',name,'  value =',value,' after  setattr'
            line1 = stream.readline()
            i = i + 1
            #print ' in datain  in while loop end    i=',i,'  line=1',line1
            if i > 147: return 0


    def write(self, inp, stream):
        #  writes the input file with updated data values
        #print  'in  write  .... datain ....'
        i = 1
        while i < 150:
            line1 =  inp.readline()
            #print   'i =',i,  line1,  '  in write...'
            line = line1.strip()
            if  len(line)  == 0: 
                stream.write(line1)
                #print   'i =',i,  line1,  '  if 1   blank line  ...'
                i = i + 1
                continue
            if  ':' in line1: 
                stream.write(line1)
                #print   'i =',i,  line1,  '  if 2  has =   ...'
                i = i + 1
                continue
            if i == 3: 
                stream.write(line1)
                #print   'i =',i,  line1,  '  if 3     ...'
                i = i + 1
                continue 
            #line = line1.strip()
            fields = line.split()
            #print 'len(fields) =',len(fields),'  i=',i
            name0 =  fields[1].strip()
            #print  'name0 =',name0,'  in write...'
            if '=' in fields[1]:  
                fld = fields[1].split('=')
                name0 = fld[0]
            else:
                name0 = fields[1].strip()
                #for name in self._nml_modified:
            #print  'name =', name0
            for name in  self._nml_modified:
                if name == name0:
                   value = getattr(self, name)
                   #print '   name =',name,'  value =',value, '  i=',i
                   stream.write('%s        %s \n' % (value , name.lower()))
            i = i + 1
        #stream.write('    /\n')
        return 0

            
