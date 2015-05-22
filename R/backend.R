## 

.backend <- new.env(parent=emptyenv())

.backend_set_current <- function(value, register) {
    if (register) {
        .backend[["current"]] <- value
        .backend_get_current()
    } else value
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
    c(sprintf("strm_%s", type), "strm_backend", class(x))
}

backend <-
    function(spec, type, ...)
{
    if (missing(spec) && missing(type) && .backend_exists())
        return(.backend_get_current())
    ## FIXME: platform-specific default
    switch(type,
           socket=backend_socket(spec, ...),
           serial=backend_serial(...),
           stop("unknown backend '", type, "'"))
}

backend_value <- strmr_value

close.strm_backend <- function(con, ...) {}

print.strm_backend <-
    function(x, ...)
{
    cat(class(x)[1], "backend\n")
    cat("backend_value:\n")
    print(backend_value(x))
}

## serial

backend_serial <-
    function(..., register=TRUE)
{
    cl <- list(strmr_value=.strm_yield_status("pending"))
    class(cl) <- .backend_class(cl, "serial")
    .backend_set_current(cl, register)
}

## socket

backend_socket <-
    function(spec, ..., register=TRUE)
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
    cl$strmr_value <- .strm_yield_status("pending")
    class(cl) <- .backend_class(cl, "socket")
    .backend_set_current(cl, register)
}

close.strm_socket <- function(con, ...) {
    nodes <- !nzchar(names(con))
    stopCluster(con[nodes])
}

print.strm_socket <- function(x, ...) {
    nodes <- !nzchar(names(x))
    hosts <- table(vapply(x[nodes], `[[`, character(1), "host"))
    cat(sprintf("%s backend of length %d", class(x)[[1]], sum(nodes)),
        paste(sprintf("\n  %s (x%d)", names(hosts), as.vector(hosts)),
              collapse=""),
        "\n")
}
