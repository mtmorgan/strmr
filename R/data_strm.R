## data_strm_

data_strm <- function(.data, ..., class=NULL)
    UseMethod("data_strm")

.data_strm_data <- function(x)
    x$env[["data"]]

.data_strm_size <- function(x) {
    ## FIXME -- determine from backend size
    yield.size <- x$yield.size
    if (is.na(yield.size))
        yield.size <- .data_strm_length(x)
    yield.size
}

.data_strm_offset <- function(x)
    x$offset

.data_strm_length <- function(x)
    x$length

.data_strm_value <- strmr_value

.data_strm_class <- function(x)
    class(.data_strm_data(x))

data_strm.default <-
    function(.data, yield.size=NA, ..., class=NULL)
{
    env <- new.env(parent=emptyenv())
    env[["data"]] <- .data
    structure(list(env=env, yield.size=yield.size, length=length(.data),
                   offset=0L, strmr_value=.strm_yield_status("pending")),
              class=c(class, "data_strm", "strmr"))
}

.data_strm_byrow <-
    function(.data, yield.size=NA, ..., class="data_strm_byrow")
{
    env <- new.env(parent=emptyenv())
    env[["data"]] <- .data
    structure(list(env=env, yield.size=yield.size, length=nrow(.data),
                   offset=0L, strmr_value=.strm_yield_status("pending")),
              class=c(class, "data_strm", "strmr"))
}

data_strm.data.frame <- .data_strm_byrow

## .data_strm_factory <- function(class)
##     function(.data, ...)
##        data_strm_(.data, ..., class=class)

## data_strm_csv_ <- .data_strm_factory("csv")

## data_strm_delim_ <- .data_strm_factory("delim")

print.data_strm <-
    function(x, ...)
{
    cat(class(x)[1], " on ", .data_strm_class(x), " of length ",
        .data_strm_length(x), "; yield size ", .data_strm_size(x), "\n",
        sep="")
    cat("value:\n")
    print(strmr_value(x))
}
