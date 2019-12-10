# EFForTS-ABM
EFForTS-ABM model repository


# EFForTS-LGraf

The current version of the Landscape Generator EFForTS-LGraf is nested within this repository.
It is integrated as a git submodule referring to [nldoc/EFForTS-LGraf](https://github.com/nldoc/EFForTS-LGraf).

## Update procedure
If changes are made within the EFForTS-LGraf repository we need to update the model in this repository as well.
To do this, delete the subfolder"EFForTS-LGraf" in the local working copy.
Then perform `git submodule --init` in the current R session terminal to load the current version from the master branch.
