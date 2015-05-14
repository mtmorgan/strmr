## yield_status

.strm_yield_status <- function(status, condition=NULL) {
    value <- list(error=-1L, pending=1L, done=2L)
    stopifnot(status %in% names(value))
    class <- c(sprintf("strm_%s", status), "strm_yield_status", "strmr")
    structure(list(status=value[status], condition=condition), class=class)
}

print.strm_yield_status <- function(x, ...)
    cat(class(x)[1], "\n")
