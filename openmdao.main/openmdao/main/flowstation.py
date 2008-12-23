"""
NPSS style FlowStation class. (not fully implemented)
"""

# pylint: disable-msg=C0103

class FlowStation(object):
    """An NPSS style FlowStation class to manage thermodynamic properties
    of a point in a fluid flow.
    """
    def __init__(self):
        self.Aphy = 0.      # physical area
        self.AphyDes = 0.   # design area
        self.Cd = 0.        # discharge coefficient
        self.Cps = 0.       # const pressure spec heat based on static cond
        self.Cpt = 0.       # const pressure spec heat based on total cond
        self.Cvs = 0.       # const volume spec heat based on static cond
        self.Cvt = 0.       # const volume spec heat based on total cond
        self.FAR = 0.       # fuel to air ratio
        self.S = 0.         # entropy
        self.gams = 0.      # gamma based on static cond
        self.gamt = 0.      # gamma based on total cond
        self.hs = 0.        # static specific enthalpy
        self.ht = 0.        # total specific enthalpy
        self.imp = 0.       # impulse function
        self.ks = 0.        # thermal conductivity based on static cond
        self.kt = 0.        # thermal conductivity based on total cond
        self.MN = 0.        # Mach number
        self.MNdes = 0.     # design Mach number
        self.mus = 0.       # viscosity based on static cond
        self.mut = 0.       # viscosity based on total cond
        self.Ps = 0.        # static pressure
        self.Pt = 0.        # total pressure
        self.Prs = 0.       # Prandtl number
        self.PsSat = 0.     # saturation pressure based on static cond
        self.PtSat = 0.     # saturation pressure based on total cond
        self.rhos = 0.      # density based on static cond
        self.rhot = 0.      # density based on total cond
        self.Rs = 0.        # gas constant based on static cond
        self.Rt = 0.        # gas constant based on total cond
        self.swirl = 0.     # swirl angle
        self.Ts = 0.        # static temperature
        self.Tt = 0.        # total temperature
        self.TsSat = 0.     # saturation temperature based on static cond
        self.TtSat = 0.     # saturation temperature based on total cond
        self.us = 0.        # internal energy based on static cond
        self.ut = 0.        # internal energy based on total cond
        self.V = 0.         # flow velocity
        self.W = 0.         # total weight flow
        self.Wh = 0.        # water flow
        self.Wf = 0.        # fuel flow
        self.Wa = 0.        # air flow
        self.WAR = 0.       # water to air ratio
        self.Wc = 0.        # corrected weight flow
        self.Wp = 0.        # referenced weight flow
        self.Zs = 0.        # compressibility based on static cond
        self.Zt = 0.        # compressibility based on total cond

