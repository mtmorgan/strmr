## 

.backend <- new.env(parent=emptyenv())

.set_current_backend <- function(value) {
    .backend[["current"]] <- value
    .get_current()
}

.get_current_backend <- function() {
    b <- .backend[["current"]]
    if (is.null(b))
        b <- backend()
    b
}

## 

backend <-
    function(spec, type, ..., outfile="")
{
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
    class(cl) <- c(sprintf("strmr_%s", type), "strmr_backend", class(cl))
    .set_current_backend(cl)
}

close.strmr_backend <- function(con, ...) {
    stopCluster(con)
}

print.strmr_backend <- function(x, ...) {
    hosts <- table(vapply(x, `[[`, character(1), "host"))
    cat(sprintf("%s backend of length %d", class(x)[[1]], length(x)),
        paste(sprintf("\n  %s (x%d)", names(hosts), as.vector(hosts)),
              collapse=""),
        "\n")
}
