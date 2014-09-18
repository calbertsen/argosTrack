# argosTrack

R package for fitting movement models to Argos data

## Installing the package

The package requires [`TMB`](http://www.tmb-project.org), `mvtnorm` to be installed.

To install the package from Github use

```
library(devtools)
install_github("calbertsen","argosTrack")
```

## To do:

- Implement summary methods
- Add data
- Add example
- Add better documentation
- Add a vignette
- Handle numeric date input in argosTrack
- Ensure bootstrap is called with a list of (lists of) arguments for fitting
