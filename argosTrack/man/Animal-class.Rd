% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Animal.R
\docType{class}
\name{Animal-class}
\alias{Animal-class}
\title{A Reference Class for combining animal movement model parts.}
\description{
A Reference Class for combining animal movement model parts.
}
\section{Fields}{

\describe{
\item{\code{name}}{String identifying the animal}

\item{\code{observation}}{Observation reference class}

\item{\code{movement}}{Movement reference class}

\item{\code{measurement}}{Measurement reference class}

\item{\code{optim}}{List to keep results of optimization through \code{\link{fitTrack}}}
}}

\section{Methods}{

\describe{
\item{\code{addToLatPlot(obsarglist = list(pch = 16, col = "grey"), movarglist = list(),
  sdarglist = list(col = "grey", border = NA, lwd = 3, lty = 2), sd = TRUE)}}{Function to add estimated movement and observation latitudes to a plot. obsarglist is a list of base graphics arguments to be used for observation data, movarglist is a list of base graphics arguments to be used for movement estimates, and sdarglist is a list of base graphics arguments to be used for movement standard errors (if sd = TRUE).}

\item{\code{addToLonPlot(obsarglist = list(pch = 16, col = "grey"), movarglist = list(),
  sdarglist = list(col = "grey", border = NA, lwd = 3, lty = 2), sd = TRUE)}}{Function to add estimated movement and observation longitudes to a plot. obsarglist is a list of base graphics arguments to be used for observation data, movarglist is a list of base graphics arguments to be used for movement estimates, and sdarglist is a list of base graphics arguments to be used for movement standard errors (if sd = TRUE).}

\item{\code{addToMapPlot(obsarglist = list(type = "b", pch = 16, col = "grey"),
  movarglist = list())}}{Function to add estimated movement and observation coordinates to a plot. obsarglist is a list of base graphics arguments to be used for observation data, movarglist is a list of base graphics arguments to be used for movement estimates, and sdarglist is a list of base graphics arguments to be used for movement standard errors (if sd = TRUE).}

\item{\code{getRange(sd = FALSE, zoomToState = FALSE)}}{Returns the range of animal coordinates. If zoomToState = TRUE, only movement data is used. If sd = TRUE, the range includes standard errors of the movement estimates.}

\item{\code{getTMBdata()}}{Function to return a data list for TMB::MakeADFun}

\item{\code{getTMBmap(fixcorrection = FALSE, ...)}}{Function to return a map list for TMB::MakeADFun. If fixcorrection = TRUE, the ratio between measurement errors for argos data is fixed.}

\item{\code{getTMBparameters()}}{Function to return a parameter list for TMB::MakeADFun}

\item{\code{initialize(observation, movement, measurement, name = "", ...)}}{Method to initialize the class. 'observation' is an Observation reference class object, 'movement' is a Movement reference class object, 'measurement' is a Measuremenbt reference class object, and 'name' is an identifier string.}

\item{\code{simulate(newObject = FALSE)}}{Function to simulate movement and measurement data for the Animal. A list containing the movement (X) and measurement (Y) data is returned. If newObject = TRUE, an Animal object is added to the list.}

\item{\code{updateFromFit(opt)}}{Function to save the result from nlminb.}
}}

\note{
The reference class methods are not intended to be used by the user. Please use appropriate functions such as \code{\link{fitTrack}}, \code{\link{plot}}, \code{\link{plotLon}}, \code{\link{plotLat}}, \code{\link{plotMap}}, and \code{\link{simTrack}}.
}
\seealso{
\code{\link{Animal}} \code{\link{Movement-class}} \code{\link{Observation-class}} \code{\link{Measurement-class}}
}
\author{
Christoffer Moesgaard Albertsen
}
