
from openmdao.main.container import Container

class FlowStation(object):
  def __init__(self):
    self.Aphy = 0.      # physical area
    self.AphyDes = 0.   # design area
    self.Cd = 0.        # discharge coefficient
    self.Cps = 0.       # constant pressure specific heat based on static conditions
    self.Cpt = 0.       # constant pressure specific heat based on total conditions
    self.Cvs = 0.       # constant volume specific heat based on static conditions
    self.Cvt = 0.       # constant volume specific heat based on total conditions
    self.FAR = 0.       # fuel to air ratio
    self.S = 0.         # entropy
    self.gams = 0.      # gamma based on static conditions
    self.gamt = 0.      # gamma based on total conditions
    self.hs = 0.        # static specific enthalpy
    self.ht = 0.        # total specific enthalpy
    self.imp = 0.       # impulse function
    self.ks = 0.        # thermal conductivity based on static conditions
    self.kt = 0.        # thermal conductivity based on total conditions
    self.MN = 0.        # Mach number
    self.MNdes = 0.     # design Mach number
    self.mus = 0.       # viscosity based on static conditions
    self.mut = 0.       # viscosity based on total conditions
    self.Ps = 0.        # static pressure
    self.Pt = 0.        # total pressure
    self.Prs = 0.       # Prandtl number
    self.PsSat = 0.     # saturation pressure based on static conditions
    self.PtSat = 0.     # saturation pressure based on total conditions
    self.rhos = 0.      # density based on static conditions
    self.rhot = 0.      # density based on total conditions
    self.Rs = 0.        # gas constant based on static conditions
    self.Rt = 0.        # gas constant based on total conditions
    self.swirl = 0.     # swirl angle
    self.Ts = 0.        # static temperature
    self.Tt = 0.        # total temperature
    self.TsSat = 0.     # saturation temperature based on static conditions
    self.TtSat = 0.     # saturation temperature based on total conditions
    self.us = 0.        # internal energy based on static conditions
    self.ut = 0.        # internal energy based on total conditions
    self.V = 0.         # flow velocity
    self.W = 0.         # total weight flow
    self.Wh = 0.        # water flow
    self.Wf = 0.        # fuel flow
    self.Wa = 0.        # air flow
    self.WAR = 0.       # water to air ratio
    self.Wc = 0.        # corrected weight flow
    self.Wp = 0.        # referenced weight flow
    self.Zs = 0.        # compressibility based on static conditions
    self.Zt = 0.        # compressibility based on total conditions

