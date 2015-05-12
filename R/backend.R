## 

.backend <- new.env(parent=emptyenv())

.backend_set_current <- function(value) {
    .backend[["current"]] <- value
    .backend_get_current()
}

.backend_exists <- function()
    !is.null(.backend[["current"]])

.backend_get_current <- function() {
    b <- .backend[["current"]]
    if (is.null(b))
        b <- backend()
    b
}

## 

backend <-
    function(spec, type, ..., outfile="")
{
    if (missing(spec) && missing(type)) {
        if (.backend_exists())
            return(.backend_get_current())
    }
    if (missing(spec))
        spec <- as.integer(ceiling(0.75 * parallel::detectCores()))
    if (missing(type))
        type <- "socket"                  # FIXME -- platform-specific
    stopifnot(length(type) == 1L, type %in% c("socket"))
    cl <- switch(type, socket={
        invalid <- c("snowlib", "scriptdir")
        if (any(idx <- invalid %in% names(list(...))))
            stop("argument ", paste(sQuote(invalid[idx]), collapse=", "),
                 "not allowed for SOCK cluster")
        snowlib <- system.file(package="strmr", "node")
        scriptdir <- file.path(snowlib, "snow")
        makeSOCKcluster(spec, snowlib=snowlib, scriptdir=scriptdir, ...,
                        outfile=outfile)
    }, {
        stop("unknown backend type '", type, "'")
    })
    class(cl) <- c(sprintf("strm_%s", type), "strm_backend", class(cl))
    .backend_set_current(cl)
}

close.strm_backend <- function(con, ...) {
    stopCluster(con)
}

print.strm_backend <- function(x, ...) {
    hosts <- table(vapply(x, `[[`, character(1), "host"))
    cat(sprintf("%s backend of length %d", class(x)[[1]], length(x)),
        paste(sprintf("\n  %s (x%d)", names(hosts), as.vector(hosts)),
              collapse=""),
        "\n")
}
