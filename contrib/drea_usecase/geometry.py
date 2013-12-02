from openmdao.main.api import VariableTree
from openmdao.main.datatypes.api import Float

class Geometry(VariableTree):
    """Container of variables defining the mixer-ejector geometry"""

    length = Float(10.0, units='ft', desc='Ejector width')
    width = Float(6.0, units='ft', desc='Ejector width')
    Apri = Float(6.00, units='ft**2', desc='Primary nozzle exit area')
    Asec = Float(8.40, units='ft**2', desc='Secondary nozzle exit area')
    Aexit = Float(13.68, units='ft**2', desc='Ejector exit area')
    AsAp = Float(1.4,desc='Area ratio (secondary area/primary area)')
    AR = Float(1.25, desc='Aspect ratio of the ejector at the inlet')
    AeAt = Float(0.95, desc='Area ratio (exit area/total inlet area)')
    ChuteAngles = Float(-10.0, units='deg', desc='Chute divergence angles. Outer tangent line relative to ejector flow surface at nozzle downstream of mixer exit')
    Num_Lobes = Float(18.0, desc='Number of spokes or lobes of the mixer')
    LhMh = Float(0.90, desc='Lobe height to mixing section height (from centerline) ratio (chute penetration)')
    LhWave = Float(2.88, desc='Lobe height to wavelength ratio')
    
    def calc_geom(self,length,Apri,AsAp,AR,AeAt,LhMh,LhWave):
        self.length = length
        self.Apri = Apri
        self.AsAp = AsAp
        self.AR = AR
        self.AeAT = AeAt
        self.LhMh = LhMh
        self.LhWave = LhWave
        
        self.Asec = AsAp*Apri
        self.Aexit = AeAt*(Apri+self.Asec)
        self.width = (AR*(Apri+self.Asec))**0.5
        self.Num_Lobes = 4*self.width**2*LhWave/((Apri+self.Asec)*LhMh)
