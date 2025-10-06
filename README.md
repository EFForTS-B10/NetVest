# NetVest
This repository contains the release of EFForTS-ABM v1.1. This model version 1.1 extends the version 1.0 published with Dislich et al. 2018.

The extension includes the release of the coupling framework NetVest. NetVest couples dynamic simulation models with static models. With this release we demonstrate the transferability and implementation of NetVest to couple the EFForTS-ABM land-use change model (Dislich et al., 2018) with the Habitat Quality model (Nelson et al., 2012) from the InVEST® suite (Kareiva et al., 2011). This coupling enables dynamic and simultaneous execution of EFForTS-ABM and InVEST® Habitat Quality, allows to explore the impacts of smallholder-driven land-use changes over time and space on economic functions and biodiversity in Indonesia and enhances our understanding of the underlying mechanisms.

For further information see Henzler et al. unpublished.

# Technical Software Documentation
## Folder structure

• input: input folder of EFForTS-ABM

• ncinv: input and output folder for InVEST Habitat Quality

• output: output folder of EFForTS-ABM

• par_ABM: Parameter files of EFForTS-ABM

• scr_ABM: NetLogo Source (nls) files of EFForTS-ABM

• test_integration: Software Testing scripts

## NetLogo Source (nls) Files of NetVest

• ecol_biodiv_ncinv.nls

• ecol_biodiv_ncinv_unit_test.nls

• ecol_biodiv_ncinv_integration_test.nls

## Functionality

•Implementation of NetVest to couple the spatio-temporal land-use change model EFForTS-ABM (Dislich et al., 2018), built in NetLogo [https://www.uni-goettingen.de/en/software/593072.html] with the Habitat Quailty model (Nelson et al., 2012) from the InVEST® model suite [https://naturalcapitalproject.stanford.edu/software/invest], built in Python (https://www.python.org/downloads/) for simulation of complex economic-ecological trade-offs

• Input data for the Habitat Quality model is derived from EFForTS-ABM. Non-spatial input data is derived once at initialization from the input folder of EFFOrTS-ABM, whereas spatial input data (Land use and land cover (LULC) maps and impact maps) for the Habitat Quality model are generated dynamically based on agent-related variables in EFForTS-ABM

• LULC types can be mapped between EFForTS-ABM and InVEST® Habitat Quality

• InVEST® habitat quality score is returned as EFForTS-ABM agent property for each time step

• For testing purposes unit test, integration test and system test are attached. The system test as test scenario allows to simulate the impact of rubber and oil palm plantations on habitat quality in a test model landscape within EFForTS-ABM.
	
[![Figure2-v3.png](https://i.postimg.cc/HkQ4NMWm/Figure2-v45drawio.png)]([https://postimg.cc/9DQ7Rrqg])

## Installation

It is highly recommended to install and execute the models on a Linux System running the Ubuntu 20.04 version.

### Manual installation

• Install Ubuntu 20.04, see https://releases.ubuntu.com/20.04/

• Install InVEST® Python API, see https://www.python.org/downloads/

• Install NetLogo version 6.2.1, see https://ccl.northwestern.edu/netlogo/

• Install EFForTS-ABM version 1.1 (clone this repository)

Installation and execution on other system setups might be possible but is not thoroughly tested.

### Docker container

Instead of installing every component manually, a docker container containing every component can be used:

• docker pull ecomod/rstudio:NetVest

• adapt {HOME} for netlogopath in the R Skripts for exeution of NetVest to /opt/netlogo/6.2.1) 




## Configuration

• EFForTS-ABM uses several NetLogo extensions that may not be bundled within NetLogo version 6.2.1. Extensions that have been used for this model are: gis, matrix, nw, ls, csv and py. The extension versions we used for this model are distributed within this model folder and should be found by NetLogo 6.2.1 automatically when the model is loaded. However, in order to use these extensions in other models, the extension folders can also be copied to the NetLogo extensions folder which can be found here: "..path.to.your.NetLogo.6.2.1.installation\app\extensions"


## Test Scenario Runs

• Run the unit tests by clicking on "Unit test Habitat Quality" in the NetLogo GUI Interface tab of EFForTS-ABM. Depending on which unit test to be executed enter "noimpact", "localimpact" or "globalimpact" into the input box "ncinv_test" on the Interface tab.

• Run the integration test by clicking on "Integration test Habitat Quality" in the NetLogo GUI Interface tab of EFForTS-ABM .

[![Screenshot-Unittest.png](https://i.postimg.cc/L5gSZsPM/Screenshot-Unittest.png)](https://postimg.cc/QKDw2hS6)

• Run the system test scenario via the NetLogo GUI Interface tabs of EFForTS-ABM „setup“ and „go“.

[![Screenshot-modelrun.png](https://i.postimg.cc/3NLdBNSf/Screenshot-modelrun.png)](https://postimg.cc/m1HLZLG7)

• Find the resulting InVEST® Habitat Quality maps in the specified output folder:

"{HOME}/NetVest/ncinv/habitatquality/output" for tif-files

"{HOME}/NetVest/output" for asc-files

## Customized Model Scenario Run

• Identify your list of relevant LULC types and list of impacts and relate them to the corresponding variables in EFForTS-ABM.

• Create the input files for InVEST® Habitat Quality (sensitivitytable.txt, impacttable.txt)

• To implement the mapping of LULC types between EFForTS-ABM and InVEST® Habitat Quality in ecol_biodiv_ncinv.nls, adapt the agent property within EFForTS-ABM (p_landuse) as a basis for LULC classification for InVEST® Habitat Quality (p_landuse_ncinv):

```
;; Create new variable for lulc-classification used by InVEST Habitat Quality
to biodiv-ncinv-translate-lulc
 print["translating lulc values"]
  ask patches
  [
    set p_landuse_ncinv abs p_landuse  
  ]
end
```

• To implement the mapping of impacts, adapt "oilpalm" and "rubber" to your list of relevant impacts and adapt the agent property "p_landuse_ncinv" as basis for impact mapping in ecol_biodiv_ncinv.nls:

```
to biodiv-ncinv-write-maps
 print["start write maps to output folder"]
  biodiv-ncinv-write-lulc-map 
  biodiv-ncinv-write-impact-map "oilpalm" 
  biodiv-ncinv-write-impact-map "rubber" 
end
```

```
;; Generation of impact-maps
to biodiv-ncinv-write-impact-map [impact-name]
  ask patches
  [set p_impact_ncinv FALSE]
 print["writing impact map(s)"]
  ;; iterate throug all possible impacts
  show impact-name
  (ifelse
    impact-name = "oilpalm"
    [ask patches with [p_landuse_ncinv = 0] [set p_impact_ncinv TRUE] 
       print ["generating oilpalm impact map"]]
    impact-name = "rubber"
      [ask patches with [p_landuse_ncinv = 1 ] [set p_impact_ncinv TRUE] 
       print ["generating rubber impact map"]]
  )
  let new-raster gis:create-raster (max-pxcor + 1) (max-pycor + 1) gis:world-envelope
  let xcount 0
  let ycount max-pycor
  let ycount-raster 0
  while [ycount >= 0]
  [
    while[xcount <= max-pxcor]
    [
      ifelse ([p_impact_ncinv] of patch xcount ycount = TRUE)
          [gis:set-raster-value new-raster xcount ycount-raster 1]
          [gis:set-raster-value new-raster xcount ycount-raster 0]
      set xcount xcount + 1
    ]
    set xcount 0
    set ycount ycount - 1
    set ycount-raster ycount-raster + 1
  ]

  ;; store impact-map
  let filename (word workdir_ncinv "/output/" (word (impact-name)) "_c.asc")
  if (file-exists? filename) [file-delete filename]
  gis:store-dataset new-raster filename

  ask patches [set p_impact_ncinv FALSE]
end
```

```
to biodiv-ncinv-convert-maps-tif
 print ["converting maps"]
 print (word "workdir_ncinv: "workdir_ncinv)
  foreach ["lulc" "oilpalm_c" "rubber_c"]
[
    [x] ->
    biodiv-ncinv-convert-to-tif (x)
  ]
```

• To implement aggregation of habitat quality scores by land-use types, adapt forest, oilpalm and rubberto your list of relevant land-use types:

```
to biodiv-ncinv-aggregate-habitatquality-landuse
  print ["aggregating habitat quality scores for each landuse"]
  set forest_hq 0
  set forest_hq mean [p_hq_ncinv] of patches with [p_landuse = -100]
  print (word "forest-hq mean:" forest_hq)
  if any? patches with [p_landuse = 0]
  [
  set oilpalm_hq 0
  set oilpalm_hq mean [p_hq_ncinv] of patches with [p_landuse = 0]
  print (word "oilpalm-hq mean:" oilpalm_hq)
  ]
  if any? patches with [p_landuse = 1]
  [
  set rubber_hq 0
  set rubber_hq mean [p_hq_ncinv] of patches with [p_landuse = 1]
  print (word "rubber-hq mean:" rubber_hq)
  ]
end
```
• Adapt the software testing to another use case.
