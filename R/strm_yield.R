## strm_yield_

strm_yield_ <- function(.data, ..., class=NULL)
    UseMethod("strm_yield_")

.strm_yield_data <- function(x)
    x$env[["data"]]

.strm_yield_last <- function(x)
    x$env[["last"]]

.strm_yield_class <- function(x)
    class(.strm_yield_data(x))

.strm_yield_length <- function(x)
    x$length

strm_yield_.default <-
    function(.data, size=NA, ..., class=NULL)
{
    env <- new.env(parent=emptyenv())
    env[["data"]] <- .data
    env[["last"]] <- .strm_yield_status("pending")
    offset <- 0L
    length <- length(.data)
    yield <- function(sz) {
        if (is.na(size))
            size <- sz
        len <- min(length - offset, size)
        result <- if (len == 0L) {
            .strm_yield_status("done")
        } else env[["data"]][offset + seq_len(len)]
        env[["last"]] <- result
        offset <<- offset + len
        result
    }
    structure(list(env=env, length=length, yield=yield),
              class=c(class, "strm_yield_", "strmr"))
}

.strm_yield_factory <- function(class)
    function(.data, ...)
       strm_yield_(.data, ..., class=class)

strm_yield_csv_ <- .strm_yield_factory("csv")

strm_yield_delim_ <- .strm_yield_factory("delim")

print.strm_yield_ <-
    function(x, ...)
{
    cat(class(x)[1], "on", .strm_yield_class(x), "of length",
        .strm_yield_length(x), "\n")
    if (!is(last <- .strm_yield_last(x), "strm_yield_status"))
        cat("last:\n")
    print(last)
}
