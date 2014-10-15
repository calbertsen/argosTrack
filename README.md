# argosTrack

R package for fitting movement models to Argos data

## Installing the package

The package requires [`TMB`](http://www.tmb-project.org) and `mvtnorm` to be installed.

To install the package from GitHub use

```
library(devtools)
install_github("argosTrack","calbertsen")
```

## To do:

- Implement summary methods
- Add example
- Add better documentation
- Add a vignette
- Add background to map
- Ensure bootstrap is called with a list of (lists of) arguments for fitting
- New line after progress bar in `bootstrap`
- `bootstrap` should be able to continue at the right number (if saved - how should number of simulations be handled?).
- Use `file.path` instead of `paste` in `bootstrap`
