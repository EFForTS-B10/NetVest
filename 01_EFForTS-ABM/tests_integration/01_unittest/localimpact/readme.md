Unit test: localimpact - Presence of 1 Impact

Aim 1: Successful execution of the unit test
Verification: Console output of unit test execution: [Success] [Success] 
Verification: console output of function fileexist(): quality_c_localimpact.asc in output folder: [Habitat quality map
               exists] ; Screenshot

Aim 2: Validation of correct transformation of asc to tif and of tif to asc.
Verification: Console output of function validation_transformation() : [Correct
              transformation] ; Comparing raster plots

Aim 3: Validating habitat quality values calculated by InVEST. Expected that all parcels comprise highest habitat quality score (1). Except of parcel comprising the impact (= local impact).
Verification: Console output of function validation_maps() : [Validating expected result]
              Comparing raster plots 

   

Input folder:

impacttable.txt:      with two impacts (oilpalm and rubber plantations) with maximal
                      distance of 0.05 (which means that impact only has local effect) and
                      with linear distance-decay-function

sensitivitytable.txt: with 5 possible LULC classifications (unused, village, oilpalm,
                      rubber, forest). Cells with oilpalm, rubber or forest are considered                       as habitat and are all equally sensitive to the impacts.

lulc.asc:             land-use and land-cover map with only forest cells (integer = 4)

oilpalm_c.asc:        presence of one impact oilpalm (integer = 1) in one quadrant of the                       map

rubber_c.asc:         no presence of impact rubber (integer = 0)

