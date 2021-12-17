Integrationtest

Aim 1:   Successful execution of the unit test and correct file in output folder
         Verification: Console output of unit test execution: [Success] [Success] 
         Verification: Console output of function fileexist(): quality_c_experiment.asc in
                       output folder: [Habitat quality map exists] 

Aim 2:   LULC map 
Aim 2.1: Correct translation of LULC values: comparison of land-use map within EFForTS-ABM before          translation with land-use map withing EFForTS-ABM after translation of patch variable
         Verification: Console output of function validation_translation(): [TRUE] + Plot
Aim 2.2: Correct generation of LULC map: comparison of land-use map withing EFForTS-ABM after              translation of patch variable with generated lulc map for InVEST.
         Verification: Console output of function validation_translation(): [TRUE] + Plot

Aim 3:   Impact map
Aim 3.1: Correct translation of impact values: comparison of land-use map only for within                  EFForTS-ABM before translation with land-use map withing EFForTS-ABM after translation of          patch variable for oilpalm and rubber, respectivly.
         Verification: Console output of function validation_translation(): [TRUE] + Plot
Aim 3.2: Correct generation of impact map: Comparison of land-use map withing EFForTS-ABM after            translation of with generated impact map for InVEST for oilpalm and rubber, respectivly.
         Verification: Console output of function validation_translation(): [TRUE] + Plot
         
Aim 4:   Habitat quality map
         Storing values of habitat quality map of InVEST within EFForTS-ABM: Comparison of habitat          quality map with map within EFForTS-ABM  with values of p_habitat_quality
         Verification: Console output of function validation_translation(): [TRUE] + Plot

Inputfolder:
impacttable.txt:      with two impacts (oilpalm and rubber plantations) with maximal
                      distance of 0.05 (which means that impact only has local effect) and
                      with linear distance-decay-function

sensitivitytable.txt: with 3 possible LULC classifications (oilpalm,
                      rubber, forest). Cells with oilpalm, rubber or forest are considered
                      as suitable habitat and are all equally sensitive to the impacts.