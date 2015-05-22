strm <-
    function(.data, expr, ..., backend=NULL, job.size=NA_integer_,
             class=NULL)
{
    stopifnot(is(.data, "data_strm"),
              is.null(backend) || is(backend, "strm_backend"),
              length(job.size) == 1L, is.integer(job.size))
    if (is.null(backend))
        backend <- .backend_get_current()

    structure(list(data=.data, backend=backend, job.size=job.size,
                   expr=substitute(expr), env=parent.frame()),
              class=c(class, "strm", "strmr"))
}

.strm_data <- function(x)
    x$data

.strm_backend <- function(x)
    x$backend

.strm_job_size <- function(x)
    x$job.size

strm_expr <- function(x)
    x$expr

.strm_env <- function(x)
    x$env

print.strm <-
    function(x, ...)
{
    cat("data_strm: ", class(.strm_data(x))[1], "\n",
        "backend: ", class(.strm_backend(x))[1], "\n",
        "job size: ", .strm_job_size(x), "\n",
        "strm_expr() prints expression under evaluation\n",
        sep="")
}
