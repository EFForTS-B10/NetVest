# EFForTS-ABM
EFForTS-ABM model repository

## Update new implemented parameters
If changes of parameters are made or new parameters are implemented into EFForTS-ABM, they also have to be changed within the Refforts Repository (https://github.com/efforts-b10/Refforts). This makes it possible to have access to the parameters when working with the Refforts package to get parameter settings automatically for analysis.


# EFForTS-LGraf

The current version of the Landscape Generator EFForTS-LGraf is nested within this repository.
It is integrated as a git submodule referring to [nldoc/EFForTS-LGraf](https://github.com/efforts-b10/EFForTS-LGraf).
The manual for EFForTS-LGraf can be accessed via [https://nldoc.github.io/EFForTS-LGraf.bookdown/](https://efforts-b10.github.io/EFForTS-LGraf.bookdown/).

## Update procedure
If changes are made within the EFForTS-LGraf repository we need to update the model in this repository as well.
To do this, delete the subfolder"EFForTS-LGraf" in the local working copy.
Then perform `git submodule update --init` in the current R session terminal to load the current version from the master branch.
