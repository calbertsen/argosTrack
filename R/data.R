#' Track data from the adult ringed seal with tag id 106373.
#'
#' A dataset containing tracking data from the Argos system for an adult ringed seal (Pusa hispida) released 10-30-2011 near Sanikiluaq, Nunavut, Canada.
#'
#'
#' @format A data frame with 2698 rows and 5 variables:
#' \describe{
#'   \item{id}{Tag id}
#'   \item{date}{Date and time of the observation}
#'   \item{lc}{Argos location class of the observation}
#'   \item{lat}{Observed latitude (degrees north) from the Argos system}
#'   \item{lc}{Observed longitude (degrees east) from the Argos system}
#'   
#' }
#' @source Fisheries and Oceans Canada
"adult_ringed_seal"

#' Track data from the adult ringed seal with tag id 43847.
#'
#' @format A data frame with 3586 rows and 5 variables:
#' \describe{
#'   \item{id}{Tag id}
#'   \item{date}{Date and time of the observation}
#'   \item{lc}{Argos location class of the observation}
#'   \item{lat}{Observed latitude (degrees north) from the Argos system}
#'   \item{lc}{Observed longitude (degrees east) from the Argos system}
#'   
#' }
#' @source Fisheries and Oceans Canada
"subadult_ringed_seal"


#' World shorelines in high resolution
#'
#' @format A list of 145101 matrices each defining the longitude and latitude of a shoreline polygon. The polygons define the boundary between land and ocean, except Antarctica, and the boundary between Antarctica grounding-line and ocean. 
#' The longitudes are between -180 and 180 degrees East, while the latitudes are between -90 and 90 degrees North.
#' @source A Global Self-consistent, Hierarchical, High-resolution Geography Database \url{http://www.soest.hawaii.edu/pwessel/gshhg/index.html}
"worldShorelines"

#' #' Area of world shoreline polygons in high resolution
#'
#' @format A vector of length 145101 with areas of the World shoreline data polygons in the same order.
#' @source A Global Self-consistent, Hierarchical, High-resolution Geography Database \url{http://www.soest.hawaii.edu/pwessel/gshhg/index.html}
"worldShorelinesArea"
