yield1 <-
    function(x)
{
    stopifnot(is(x, "strm_yield_"))
    UseMethod("yield1")
}

yield1.strm_yield_ <-
    function(x)
{
    offset <- .strm_yield_offset(x)
    length <- min(.strm_yield_length(x) - offset, .strm_yield_size(x))
    x$value <- if (length == 0L) {
        .strm_yield_status("done")
    } else .strm_yield_data(x)[offset + seq_len(length)]
    x$offset <- offset + length
    x
}

