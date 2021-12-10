Unittest: noimpact - no impact presence

Aim 1: Successful execution of the unit test
Verification: Console output of unit test execution: [Success] [Success] 
Verification: Console output of function fileexist(): quality_c_noimpact.asc in
              output folder: [Habitat quality map exists] ; Screenshot

Aim 2: Validating habitat quality values calculated by InVEST. Expected that all cells of the resulting habitat-quality map comprise highest possible habitat-quality score (1) when no impact is present.
Verification: Console output of function validationmaps(): [Expected result] + Figure of
              expected map and resulting map 


Inputfolder:

impacttable.txt:      with two impacts (oilpalm and rubber plantations) with maximal
                      distance of 0.05 (which means that impact only has local effect) and
                      with linear distance-decay-function

sensitivitytable.txt: with 5 possible LULC classifications (unused, village, oilpalm,
                      rubber, forest). Cells with oilpalm, rubber or forest are considered
                      as suitable habitat and are all equally sensitive to the impacts.

lulc.asc:             land-use and land-cover map with only forest cells (integer = 4)

oilpalm_c.asc:        no presence of impact oilpalm (integer = 0)

rubber_c.asc:         no presence of impact rubber (integer = 0)