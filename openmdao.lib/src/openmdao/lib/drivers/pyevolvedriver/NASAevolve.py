import random

def G1DListCrossOverRealHypersphere(genome, **args):
    """ A genome reproduction algorithm, developed by Tristan Hearn at 
    the NASA Glenn Research Center, which uses a hypersphere defined
    by a pair of parents to find the space of possible children. 
    children are then picked at random from that space. """

    
    gMom = args['mom']
    gDad = args['dad']
    
    sister = gMom.clone()
    brother = gDad.clone()
    
    bounds = (genome.getParam("rangemin",0),genome.getParam("rangemax",100))
    dim = len(genome)
    numparents = 2.0
        
    # find the center of mass (average value) between the two parents for each dimension
    cmass = [(gm+gd)/2.0 for gm,gd in zip(gMom,gDad)]
    
    radius = max(sum([(cm-gM)**2 for cm,gM in zip(cmass,gMom)]),
                 sum([(cm-gD)**2 for cm,gD in zip(cmass,gDad)])
             )**.5
    
    #generate a random unit vectors in the hyperspace
    seed_sister = [random.uniform(-1,1) for i in range(0,dim)]
    magnitude = sum([x**2 for x in seed_sister])**.5
    while magnitude > 1: #checksum to enforce a circular distribution of random numbers
        seed_sister = [random.uniform(-1,1) for i in range(0,dim)]
        magnitude = sum([x**2 for x in seed_sister])**.5        
    
    seed_brother = [random.uniform(-1,1) for i in range(0,dim)]
    magnitude = sum([x**2 for x in seed_brother])**.5
    while magnitude > 1: #checksum to enforce a circular distribution of random numbers
        seed_brother = [random.uniform(-1,1) for i in range(0,dim)]
        magnitude = sum([x**2 for x in seed_brother])**.5    
    
    #create a children
    sister.resetStats()
    brother.resetStats()
    
    sister.genomeList = [cm+radius*sd for cm,sd in zip(cmass,seed_sister)]
    brother.genomeList = [cm+radius*sd for cm,sd in zip(cmass,seed_brother)]
    
    if type(gMom.genomeList[0]) == int: #preserve the integer type of the genome if necessary
        sister.genomeList = [int(round(x)) for x in sister.genomeList]   
        brother.genomeList = [int(round(x)) for x in brother.genomeList]  
    
    return (sister,brother)