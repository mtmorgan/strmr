## yield_status

.strm_yield_status <- function(status, condition=NULL) {
    value <- list(done=0L, error=-1L)
    stopifnot(status %in% names(value))
    class <- c(sprintf("strm_%s", status), "strm_yield_status", "strmr")
    structure(list(status=value[status], condition=condition), class=class)
}

print.strm_yield_status <- function(x, ...)
    cat("strm_yield_status:", class(x)[1], "\n")

## strm_yield_

strm_yield_ <- function(.data, ..., class=NULL)
    UseMethod("strm_yield_")

strm_yield_.default <-
    function(.data, size=NA, ..., class=NULL)
{
    force(.data)
    offset <- 0L
    length <- length(.data)
    yield <- function(sz) {
        if (is.na(size))
            size <- sz
        len <- min(length - offset, size)
        result <- if (len == 0L) {
            .strm_yield_status("done")
        } else .data[offset + seq_len(len)]
        offset <<- offset + len
        result
    }
    structure(list(dataclass=class(.data), length=length, yield=yield),
              class=c(class, "strm_yield_", "strm"))
}

.strm_yield_factory <- function(class)
    function(.data, ...)
       strm_yield_(.data, ..., class=class)

strm_yield_csv_ <- .strm_yield_factory("csv")

strm_yield_delim_ <- .strm_yield_factory("delim")

print.strm_yield_ <-
    function(x, ...)
{
    cat(class(x)[1], "on", x$dataclass, "of length", x$length, "\n")
}
    
