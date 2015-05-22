yield1 <-
    function(x, ...)
{
    value <- UseMethod("yield1")
    stopifnot(identical(class(x), class(value)))
    value
}

yield1.data_strm <-
    function(x)
{
    offset <- .data_strm_offset(x)
    length <- min(.data_strm_length(x) - offset, .data_strm_size(x))
    x$strmr_value <- if (length == 0L) {
        .strm_yield_status("done")
    } else .data_strm_data(x)[offset + seq_len(length)]
    x$offset <- offset + length
    x
}

yield1.data_strm_byrow <-
    function(x)
{
    offset <- .data_strm_offset(x)
    length <- min(.data_strm_length(x) - offset, .data_strm_size(x))
    x$strmr_value <- if (length == 0L) {
        .strm_yield_status("done")
    } else .data_strm_data(x)[offset + seq_len(length),,drop=FALSE]
    x$offset <- offset + length
    x
}

## 

yield1.strm_serial <-
    function(x, expr, env, data, ...)
{
    x$strmr_value <- eval(expr, env)(data)
    x
}

## 

yield1.strm <-
    function(x)
{
    if (is(strmr_value(x), "strm_done"))
        return(x)

    data <- .strm_data(x)
    backend <- .strm_backend(x)
    expr <- strm_expr(x)
    env <- .strm_env(x)
    job.size <- .strm_job_size(x)
    while (is(strmr_value(backend), "strm_pending")) {
        ## prime back-end
        data <- yield1(data)
        if (is(strmr_value(data), "strm_done")) {
            strmr_value(backend) <- strmr_value(data)
        } else {
            backend <- yield1(backend, expr, env, strmr_value(data), job.size)
        }
    }

    x$strmr_value <- strmr_value(backend)
    if (!is(strmr_value(backend), "done"))
        strmr_value(backend) <- .strm_yield_status("pending")
    x$data <- data
    x$backend <- backend
    x
}
