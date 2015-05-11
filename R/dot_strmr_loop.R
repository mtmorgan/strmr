.strmr_loop <- function(master) {
    ## from snow::slaveLoop
    requireNamespace("snow")
    repeat tryCatch({
        msg <- snow::recvData(master)
        cat(paste("Type:", msg$type, "\n"))
        if (msg$type == "DONE") {
            snow::closeNode(master)
            break
        }
        else if (msg$type == "EXEC") {
            success <- TRUE
            t1 <- proc.time()
            value <- tryCatch({
                snow::docall(msg$data$fun, msg$data$args)
            }, error = function(e) {
                success <<- FALSE
                class(e) <- c("remote", "strmr", class(e))
                e
            })
            t2 <- proc.time()
            value <- list(type = "VALUE", value = value, success = success, 
                time = t2 - t1, tag = msg$data$tag)
            snow::sendData(master, value)
        }
    }, interrupt = function(e) NULL)
}
