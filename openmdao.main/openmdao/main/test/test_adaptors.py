# pylint: disable-msg=C0111,C0103,E0611,W0232,W0105

from zope.schema import TextLine, Float
"""
from zope.interface import implements, Interface
from zope.component import getGlobalSiteManager, adapts

from openmdao.main.float import Float

class Int_Float_Adapter(object):

    implements(IFloat)
    adapts(IInt)
    
gsm = getGlobalSiteManager()
gsm.registerAdaptor(FrontDeskNG, (IGuest,), IDesk, 'ng')


verifyClass(IFloat, Float)  # returns True or 
                       #raises zope.interface.exceptions.BrokenImplementation


IRecipe[key]  # access via __getitem__ allowed on interfaces

Can you have a multiadapter that adapts two objects with the same interface?

"""


from zope.interface import Interface
from zope.interface import implements
from zope.component import adapts

class IAdapteeOne(Interface):
    f = Float(
        title = u"f",
        description = u"a float",
        required = True
        )
        
    s = TextLine(
        title = u"s",
        required = True,
        )
        

class IAdapteeTwo(Interface):
    pass

class IFunctionality(Interface):
    pass

class MyFunctionality(object):
    implements(IFunctionality)
    adapts(IAdapteeOne, IAdapteeOne)

    def __init__(self, xone, xtwo):
        self.one = xone
        self.two = xtwo
        
    def xfer(self):
        self.two.f = self.one.f
        self.two.s = self.one.s

from zope.component import getGlobalSiteManager
gsm = getGlobalSiteManager()

gsm.registerAdapter(MyFunctionality)

class One(object):
    implements(IAdapteeOne)
    
    f = 1.2
    s = 'foobar'
    
    def dump(self):
        print 'f = ',self.f
        print 's = ',self.s

class Two(object):
    implements(IAdapteeTwo)

one = One()
otherone = One()
two = Two()

from zope.component import getMultiAdapter


ma = getMultiAdapter((one,otherone), IFunctionality)
print ma

one.dump()
one.f = 99.7
one.dump()
otherone.dump()
ma.xfer()
otherone.dump()


