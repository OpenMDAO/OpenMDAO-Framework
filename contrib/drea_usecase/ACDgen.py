"""
    ACDgen.py - Creates ACD Table inputs for ANOPP for the 3 certification points
"""

from openmdao.main.api import Component, Slot
from numpy import around
from openmdao.main.interfaces import ICaseIterator

class ACDgen(Component):
    
    case_data = Slot(ICaseIterator)
    
    def execute(self):

        pts = {1:"FO",2:"App",3:"SL"}
        
        for k in [1,2,3]:
            thetas = []
            freq = []
            phis = []
            Machs = []
            PCs = []
            SPLs = []
            NPHI = 0
            NMACH = 0
            NPOW = 0
            
            # Create list of cases for specific observer point
            for case in self.case_data:
                if case['point'] == k:
                    phis.append(case['hsrnoise.phi'])
                    Machs.append(case['dreaprep.Mach'])
                    thetas.append(case['hsrnoise.thetas'])
                    freq.append(case['hsrnoise.Freq'])
                    PCs.append(case['dreaprep.PC'])
                    SPLs.append(case['hsrnoise.SPL_corr'])
                    if phis.index(case['hsrnoise.phi']) == len(phis)-1:
                        NPHI += 1
                    if Machs.index(case['dreaprep.Mach']) == len(Machs)-1:
                        NMACH += 1
                    if PCs.index(case['dreaprep.PC']) == len(PCs)-1:
                        NPOW += 1
            
            # Create input file
            out_file = open(''.join(['ACD_',pts[k],'.input']), 'w')
            self.print_heading(out_file, SPLs[0].shape[0], SPLs[0].shape[1], NPHI, NPOW, NMACH)
            for i in range(len(SPLs)):
                #Check SPL table dimensions for consistency before writing
                if SPLs[0].shape != SPLs[i].shape:
                    self.raise_exception('Inconsistant SPL Table Size',ValueError)                
                self.print_table(out_file, phis[i], Machs[i], PCs[i], thetas[i], freq[i], SPLs[i])
            out_file.close()            
            
    def print_heading(self, outfile, NFREQ, NTHETA, NPHI, NPOW, NMACH):
        """Writes the first four lines of the ACD file."""
        heading_lines = [' UPDATE NEWU=ACD SOURCE=* $\n',' -ADDR OLDM=* NEWM=SPL FORMAT=0 $\n','            1.0               $ Source radius, RS (ft)\n']
        outfile.writelines(heading_lines)
        params = [str(NFREQ),str(NTHETA),str(NPHI),str(NPOW),str(NMACH)]
        outfile.writelines(['            ',", ".join(params),'   $ NFREQ, NTHETA, NPHI, NPOW, NMACH\n'])
        
    def print_table(self, outfile, phi, Mach, PC, thetas, freq, SPL):
        """Writes the SPL table specific information of the ACD file."""
        values = [str(phi),str(Mach),str(PC/100)]
        SPL = around(SPL,decimals=1)
        outfile.writelines(['            ',", ".join(values),'       $ Azimuthal angle, Mach number, Power setting\n'])
        outfile.writelines(['            ','  '.join(map(str, thetas[0:9])),'\n           ',' '.join(map(str, thetas[9:17])),'       $ Yaw angles\n'])
        for i in range(SPL.shape[0]):
            outfile.writelines([map(str,freq[i,:])[0],' ',' '.join(map(str, SPL[i,:10])),'\n',' '.join(map(str, SPL[i,10:])),' $\n'])
