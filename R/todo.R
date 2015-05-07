## to-be-implemented functionality

parallel_lapply_ <-
    function(.data, expr, ..., backend, job.size=NA, by.dim=1L)
{
}

yield_ <-
    function(.data, size=NA)
{
}

parallel_ <-
    function(.data, expr, ..., backend, job.size=NA, by.dim=1)
{
}

reduce_ <-
    function(.data, FUN)
{
}

reduce_list_ <-
    function(.data)
{
    reduce_(.data, list)
}

reduce_c_ <-
    function(.data)
{
    reduce_(.data, c)
}

ireduce_ <-
    function(.data, FUN, in.order=FALSE)
{
}

complete_ <-
    function(.data)
{
}
