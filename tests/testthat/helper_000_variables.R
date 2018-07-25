
## Create data frame of movement models to test
allMoveNames <- names(getClass("Movement")@subclasses)

excludeMoveNames <- c("CSB","MPCTCRW","OUL","OUV")

moveNamesUse <- readyMoveNames <- setdiff(allMoveNames, excludeMoveNames)

regulars <- c("DCRW","DSBHN","DSBW")


