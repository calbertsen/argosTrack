TestRC <- setRefClass("TestRC",
                    fields = list(call = "list",
                                  failed = "logical"),
                    methods = list(
                        add = function(call,failed){
                            .self$call <- c(.self$call,call)
                            .self$failed <- c(.self$failed,failed)                           
                        },
                        show = function() {
                            res <- data.frame("Result"=c("Tests run","Successful","Failed"),
                                             "Number"=c(length(call),sum(!failed),sum(failed)))
                            print(res, row.names = FALSE)
                            cat("\n\n")
                            if(sum(failed) > 0){
                                cat(rep("-",20),"\n\n")
                                cat("Failing tests:\n")
                                for(i in which(.self$failed))
                                    cat(as.character(as.expression(.self$call[[i]])),"\n")
                                cat("\n\n")
                                stop("There were failed tests", call. = FALSE)
                            }
                        }
                    )
                    )


check_env_var <- function(v){
    as.logical(Sys.getenv(v,"false"))
}

.do_check <- function(stmt, call){
    if(!stmt){
        Test$add(call,TRUE)
        return(invisible(FALSE))
    }
    Test$add(call,FALSE)
    return(invisible(TRUE))
}

gives_error <- function(expr){
    .do_check(inherits(tryCatch(expr, error = function(e) {v<-"Error";class(v)<-"Error";v}),"Error"), match.call())
}

gives_warning <- function(expr){
    .do_check(inherits(tryCatch(expr, warning = function(e) {v<-"Warning";class(v)<-"Warning";v}),"Warning"), match.call())
}
gives_message <- function(expr){
    .do_check(!inherits(tryCatch(expr, message = function(e) {v<-"Message";class(v)<-"Message";v}),"Message"), match.call())
}
gives_output <- function(expr){
    tmp <- capture.output(expr)
    .do_check(length(tmp) > 0, match.call())
}
gives_no_output <- function(expr){
    tmp <- capture.output(expr)
    .do_check(length(tmp) == 0, match.call())
}
is_true<- function(expr){
    .do_check(expr, match.call())
}
is_false<- function(expr){
    .do_check(!expr, match.call())
}

is_equal <- function(o1,o2, check.attributes = FALSE, ...){
    .do_check(isTRUE(all.equal(o1,o2, check.attributes = check.attributes, ...)), match.call())
}

is_not_equal <- function(o1,o2, check.attributes = FALSE, ...){
    .do_check(!isTRUE(all.equal(o1,o2, check.attributes = check.attributes, ...)), match.call())
}

is_identical <- function(o1,o2,...) {
    .do_check(identical(o1,o2, ...), match.call())
}

is_not_identical <- function(o1,o2,...) {
    .do_check(!identical(o1,o2, ...), match.call())
}

faster_than <- function(expr, target) {
    .do_check(system.time(expr)[3] < target, match.call())
}
