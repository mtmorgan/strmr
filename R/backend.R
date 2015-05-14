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

.backend_class <-
    function(x, type)
{
    c(sprintf("strm_%s", type), "strm_backend", class(cl))
}

backend <-
    function(spec, type, ...)
{
    if (missing(spec) && missing(type) && .backend_exists())
        return(.backend_get_current())
    ## FIXME: platform-specific default
    backend_socket(spec, ...)
}

close.strm_backend <- function(con, ...) {}

print.strm_backend <- function(x, ...)
    cat(class(x)[1], "backend\n")

## serial

backend_serial <-
    function(...)
{
    cl <- list()
    class(cl) <- .backend_class(cl, "serial")
    .backend_set_current(cl)
}

## socket

backend_socket <-
    function(spec, ...)
{
    if (missing(spec))
        spec <- as.integer(ceiling(0.75 * parallel::detectCores()))
    invalid <- c("snowlib", "scriptdir")
    if (any(idx <- invalid %in% names(list(...))))
        stop("argument ", paste(sQuote(invalid[idx]), collapse=", "),
             "not allowed for SOCK cluster")
    snowlib <- system.file(package="strmr", "node")
    scriptdir <- file.path(snowlib, "snow")
    cl <- makeSOCKcluster(spec, snowlib=snowlib, scriptdir=scriptdir, ...)
    class(cl) <- .backend_class(cl, "socket")
    .backend_set_current(cl)
}

close.strm_socket <- function(con, ...) {
    stopCluster(con)
}

print.strm_socket <- function(x, ...) {
    hosts <- table(vapply(x, `[[`, character(1), "host"))
    cat(sprintf("%s backend of length %d", class(x)[[1]], length(x)),
        paste(sprintf("\n  %s (x%d)", names(hosts), as.vector(hosts)),
              collapse=""),
        "\n")
}
