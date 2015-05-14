## strm_yield_

strm_yield_ <- function(.data, ..., class=NULL)
    UseMethod("strm_yield_")

.strm_yield_data <- function(x)
    x$env[["data"]]

.strm_yield_size <- function(x)
    x$yield.size

.strm_yield_offset <- function(x)
    x$offset

.strm_yield_length <- function(x)
    x$length

.strm_yield_value <- .strmr_value

.strm_yield_class <- function(x)
    class(.strm_yield_data(x))

strm_yield_.default <-
    function(.data, yield.size=NA, ..., class=NULL)
{
    env <- new.env(parent=emptyenv())
    env[["data"]] <- .data
    structure(list(env=env, yield.size=yield.size, length=length(.data),
                   offset=0L, strmr_value=.strm_yield_status("pending")),
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
    cat(class(x)[1], " on ", .strm_yield_class(x), " of length ",
        .strm_yield_length(x), "; yield size ", .strm_yield_size(x), "\n",
        sep="")
    cat("value:\n")
    print(.strmr_value(x))
}
