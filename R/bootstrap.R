
#' @export
bootstrap <- function(object,...) UseMethod("bootstrap")
#' @export
bootstrap.argostrack <- function(object,args,n,folder=NULL){
    
    simFits <- list()
    mse <- array(dim=c(2,n,length(args)))
    convergence <- array(dim=c(2,n,length(args)))
    messages <- array(dim=c(2,n,length(args)))

    for(i in 1:n){
        obs <- simulate(object)
        if(!is.null(folder)){
            pth <- strsplit(folder,"/")[[1]]
            save(obs,
                 file=paste(c(pth,
                     paste0("simdata",i,".Rdata")),
                     collapse="/")
                 )
        }

        simFits[[i]]<-list()
        for(j in 1:length(args)){
            simFits[[i]][[j]] <- NA
            try({
                arguse <- args[[j]]
                arguse$lon <- obs$observation[2,]
                arguse$lat <- obs$observation[1,]
                arguse$dates <- obs$dates
                arguse$locationclass <- obs$locationclass
                fit <- do.call(argosTrack,arguse)
                mse[,i,j] <- c(mean((fit$positions[1,]-obs$position[1,])^2),
                               mean((fit$positions[2,]-obs$position[2,])^2)
                               )
                convergence[,i,j] <- fit$optimization$convergence
                messages[,i,j] <- fit$optimization$message
                simFits[[i]][[j]] <- fit
            },silent=TRUE)
        }
    }

    if(!is.null(folder)){
        pth <- strsplit(folder,"/")[[1]]
        save(mse,simFits,
             file=paste(c(pth,
                 paste0("bootstrap_result",".Rdata")),
                 collapse="/")
             )

    }
    dimnames(mse) <- list(c("latitude","longitude"),
                          NULL,
                          names(args)
                          )
    res <- list()
    class(res) <- "argostrack_bootstrap"
    res$mse <- mse
    res$convergence <- convergence
    res$messages <- messages
    return(res)
}
