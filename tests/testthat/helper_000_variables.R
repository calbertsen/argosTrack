
## Create data frame of movement models to test
allMoveNames <- names(getClass("Movement")@subclasses)
exportedMoveNames <- allMoveNames[allMoveNames %in% ls("package:argosTrack")]

readyMoveNames <- c("IDCRW")

for(mn in readyMoveNames)
    eval(parse(text=sprintf("%s <- argosTrack:::%s",mn,mn)))

moveNamesUse <- c(exportedMoveNames,readyMoveNames)

regulars <- c("DCRW","DSBHN","DSBW")
