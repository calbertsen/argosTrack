library(RCurl)
library(jsonlite)
library(argosTrack)

## Only publicly available data

url <- "https://www.movebank.org/movebank/service/public/json?entity_type=study"
jdat <- getURLContent(url)
dat <- fromJSON(jdat)

movebank2argosTrack <- function(individual){

    name <- individual$individual_local_identifier
    tagType <- individual$sensor_type_id
    nObs <- length(individual$locations)
    now <- Sys.time()
    Epoch <- now - as.numeric(now)
    dates <- as.POSIXct(unlist(lapply(individual$locations,function(x)x$timestamp/1000)),tz="UCT",origin="1970-01-01 00:00:00 UCT")
    lon <- unlist(lapply(individual$locations,function(x)x$location_long))
    lat <- unlist(lapply(individual$locations,function(x)x$location_lat))

##     external_id	id	is_location_sensor	name
## bird-ring	397	true	Bird Ring
## gps	653	true	GPS
## radio-transmitter	673	true	Radio Transmitter
## argos-doppler-shift	82798	true	Argos Doppler Shift
## natural-mark	2365682	true	Natural Mark
## acceleration	2365683	false	Acceleration
## solar-geolocator	3886361	true	Solar Geolocator
## accessory-measurements	7842954	false	Accessory Measurements
## solar-geolocator-raw	9301403	false	Solar Geolocator Raw
## barometer	77740391	false	Barometer
## magnetometer	77740402	false	Magnetometer

    
    if(tagType == "653"){ ## GPS
        lc <- rep("GPS",nObs)
    }else if(tagType == "82798"){ ## Argos
        lc <- unlist(lapply(individual$locations,function(x)x$argos_lc))
    }else if(tagType == "3886361"){
        lc <- rep("S",nObs)
    }else{
        stop("Tag type can not be used with argosTrack")
    }

    Observation(lat = lat,
                lon = lon,
                dates = dates,
                locationclass = lc)
}

## GPS data
i <- 1
study_url <- paste0("https://www.movebank.org/movebank/service/public/json?study_id=",
                    dat$id[i],
                    "&sensor_type=",
                    dat$sensor_type_ids[i])
sdat <- fromJSON(getURLContent(study_url),simplifyDataFrame=FALSE)


plot(obs <- movebank2argosTrack(sdat[[1]][[1]]))

anim <- Animal(observation = obs,
               movement = argosTrack:::IDCRW(unique(obs$dates)),
               measurement = Measurement(model="n"),
               name = sdat[[1]][[1]]$individual_local_identifier)


fitTrack(anim, fixdrift = TRUE, equaldecay = TRUE)

plot(anim)

anim




## Argos data
i <- 12
study_url <- paste0("https://www.movebank.org/movebank/service/public/json?study_id=",
                    dat$id[i],
                    "&sensor_type=",
                    dat$sensor_type_ids[i],
                    "&attributes=timestamp,location_long,location_lat,argos_lc")
sdat <- fromJSON(getURLContent(study_url),simplifyDataFrame=FALSE)


plot(obs <- movebank2argosTrack(sdat[[1]][[5]]))

anim <- Animal(observation = obs,
               movement = argosTrack:::IDCRW(unique(obs$dates)),
               measurement = Measurement(model="n"),
               name = sdat[[1]][[1]]$individual_local_identifier)


fitTrack(anim, fixdrift = TRUE, equaldecay = TRUE)

plot(anim)

anim




## Geolocation data
i <- 31
study_url <- paste0("https://www.movebank.org/movebank/service/public/json?study_id=",
                    dat$id[i],
                    "&sensor_type=",
                    dat$sensor_type_ids[i])
sdat <- fromJSON(getURLContent(study_url),simplifyDataFrame=FALSE)


plot(obs <- movebank2argosTrack(sdat[[1]][[1]]))

anim <- Animal(observation = obs,
               movement = argosTrack:::IDCRW(unique(obs$dates)),
               measurement = Measurement(model="n"),
               name = sdat[[1]][[1]]$individual_local_identifier)


fitTrack(anim, fixdrift = TRUE, equaldecay = TRUE)

plot(anim)

anim

par(mfrow=c(2,1))
plotLon(anim)
lines(anim$movement$dates,
      anim$movement$mu[2,] + 2  * exp(anim$measurement$splineXlogSd),
      lty = 2, lwd = 2, col = rgb(0,0,0,0.3))
lines(anim$movement$dates,
      anim$movement$mu[2,] - 2  * exp(anim$measurement$splineXlogSd),
      lty = 2, lwd = 2, col = rgb(0,0,0,0.3))
plotLat(anim)
sd <- anim$measurement$reportSpline[anim$observation$dayOfYear,1]
lines(anim$movement$dates,
      anim$movement$mu[1,] + 2  * sd,
      lty = 2, lwd = 2, col = rgb(0,0,0,0.3))
lines(anim$movement$dates,
      anim$movement$mu[1,] - 2  * sd,
      lty = 2, lwd = 2, col = rgb(0,0,0,0.3))


