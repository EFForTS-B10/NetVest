# NetVest
Coupling framework NetVest repository
## Technical Software Documentation
### Folder structure
input: input folder of EFForTS-ABM
ncinv: input and output folder for InVEST Habitat Quality
output: output folder of EFForTS-ABM
par_ABM: Parameter files of EFForTS-ABM
scr_ABM: NetLogo Source (nls) files of EFForTS-ABM
test_integration: Software Testing scripts

### NetLogo Source (nls) Files of NetVest

•	ecol_biodiv_ncinv.nls
•	ecol_biodiv_ncinv_unit_test.nls
•	ecol_biodiv_ncinv_integration_test.nls


### Functionality

•	Application of NetVest to couple programming language and environment NetLogo [B1] with NATCAP InVEST® models [B2] for simulation of complex economic-ecological trade-offs
•	InVEST maps are generated based on agent-related variables in NetLogo 
•	LULC types can be mapped between NetLogo and InVEST®
•	InVEST® ecosystem-service score is returned as NetLogo agent property for each time step
•	For testing purposes unit test, integration test and system test are attached. The system test as test scenario allows to simulate the impact of rubber and oil palm plantations on habitat quality in a test model landscape within EFForTS-ABM [B3]

### Installation 
It is highly recommended to install and execute the models on a Linux System running the Ubuntu 20.04 version. 

•	Install Ubuntu 20.04 see [B4]
•	Install InVEST® Python API see [B5]
•	Install NetLogo version 6.2.1 see [B1]
•	Install EFForTS-ABM version 1.1 see [B6]

Installation and execution on other system setups might be possible but is not thoroughly tested.
### Configuration
EFForTS-ABM uses several NetLogo extensions that may not be bundled within NetLogo version 6.2.1.
Extensions that have been used for this model are: gis, matrix, nw, ls, csv and py.
The extension versions we used for this model are distributed within this model folder and should be found by NetLogo 6.2.1 automatically when the model is loaded.
However, in order to use these extensions in other models, the extension folders can also be copied to the NetLogo extensions folder which can be found here:
"..path.to.your.NetLogo.6.2.1.installation\app\extensions"

Adapt workdir_ncinv within ecol_biodiv_ncinv_unit_test.nls.  
### Test Scenario Runs

•	Run the unit tests via the NetLogo GUI Interface tab of EFForTS-ABM "Unit test Habitat Quality". Depending on which unit test to be executed enter "noimpact", "localimpact" or "globalimpact" into the input box "ncinv_test" on the Interface tab.
•	Run the integration tests via the NetLogo GUI Interface tab of EFForTS-ABM "Integration test Habitat Quality".
•	Run the system test scenario via the NetLogo GUI Interface tabs of EFForTS-ABM „setup“ and „go“. 
•	Find the resulting InVEST® Habitat Quality maps in the specified output folder.

### Model Scenario Run
•	Identify your list of relevant LULC types and list of impacts and relate them to the corresponding variables in EFForTS-ABM
•	Create the input files for InVEST® (sensitivitytable.txt, impacttable.txt) 
•	Implement mapping for LULC types in ecol_biodiv_ncinv.nls procedures biodiv-ncinv-translate-lulc and biodiv-ncinv-aggregate-habitatquality-landuse
•	Implement definition for impacts in ecol_biodiv_ncinv.nls procedures biodiv-ncinv-write-maps, biodiv-ncinv-write-impact-map and biodiv-ncinv-convert-maps-tif
•	Adapt the software testing to another use case.
