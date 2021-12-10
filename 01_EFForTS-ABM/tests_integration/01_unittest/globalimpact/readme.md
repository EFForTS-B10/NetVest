Unit test: globalimpact - 1 impact influencing whole landscape

Aim 1: Successful execution of the unit test
Verification: Console output of unit test execution: [Success] [Success] 
Verification: Console output of function fileexist(): quality_c_globalimpact.asc in
              output folder: [Habitat quality map exists] ; Screenshot
               
Aim 2: Validating habitat degradation values calculated by InVEST. Expected that there is a linear decrease of degradation scores with increasing distance from location of impact with the highest degradation score on the cell comprising the impact.
Verification: Regression models for every direction (north, east, south, west) with relationship of degradation score and distance from impact.


Input folder:

impacttable.txt:      with two impacts (oilpalm and rubber plantations) with maximal
                      distance of 3.68 (which means that impact influences every cell on                        the landscape) and with linear distance-decay-function

sensitivitytable.txt: with 5 possible LULC classifications (unused, village, oilpalm,
                      rubber, forest). Cells with oilpalm, rubber or forest are considered                       as habitat and are all equally sensitive to the impacts.

lulc.asc:             land-use and land-cover map with only forest cells (integer = 4)

oilpalm_c.asc:        presence of one impact oilpalm (integer = 1) in the middle of the                         map

rubber_c.asc:         no presence of impact rubber (integer = 0)

