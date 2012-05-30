1) Sub-Assembly Modeling with DREA/HSR Noise Model
   - Import DREA Wrapper, Variable Tree Definitions, 
     HSR_Noise wrapper, ACDGen into project
   - Create new file, define DREAprep class
   - Create a new sub-assembly inside top
       - add DREA, DREAprep, HSRnoise instances into sub-assembly
       - add all three instances to 'driver' workflow
       - add CaseIterDriver as 'driver' for sub-assembly
         - Note: Workflow should persist after the replace of the driver
       - create data connections between components   
       
   - Add case CSV file to project
       - configure sub-assebly caseiterdriver to use csv file
   - Promote the "evaluated" output from sub-assembly case-iterdriver to assembly border
   - Connect 'evaluated' assenbly output to ACDGen
   - Add sub-assembly and ACDGen instance to top.driver.workflow
 
 
1.1) Surrogate Modeling with DREA/HSR Noise Model
   - Import DREA Wrapper, Variable Tree Definitions, 
     HSR_Noise wrapper, ACDGen, DreaPrep into project
   - Add instance of CaseIterDriver called 'analysis' into top
   - add instance of DOEdriver called 'RSEtrainer' into top
   - add instance of metamodel called metaDREA into top
       - add ResponseSurface into surrogate slot
       - add instance of DREA into model slot
   - add metaDrea to 'RSEtrainer' workflow, set train_next event for metaDrea     
   - add metaDrea, DREAprep, HSRnoise into 'analysis' workflow
   - add 'RSEtrainer' to top.driver.workflow   
   - add 'analysis' to top.driver workflow
   - add ACDgen to top.driver.workflow
   - create data connections between components   
       
   - Add case CSV file to project
       - configure 'analysis' caseiterdriver to use csv file
   - Connect 'evaluated' assenbly output to ACDGen
   

2) Construct the LDI injector Model with RAM
   - open new blank model
   - import wrappers for CUBIT, NCC, TecPlot into project 
   - create instances of all components in top assembly
   - add DOEdriver to assembly as 'driver'
   - specify the RAM for CUBIT and NCC instances as NAS or HX 
   - select a DOEgenerator from the library and add it to the DOEdriver
   - configure any options specific to the DOEgenerator
   - establish all relevant data connections between components
   - select a case recorder from library and add it to DOEdriver
   - Run model 
      - DOE csv file is recorded, and can be viewed by user 
   - User reconfigured DOE driver to run of the DOE csv record
   - User re-runs only case X from the re-run file

3) Using Automatic Architecture on a Problem
