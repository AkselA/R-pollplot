#' @export

as.zoo.pollplot <- function(x, avg.dup=TRUE, ...) {
    if (avg.dup) {
        app <- apply(x, 1:2, mean, na.rm=TRUE)
        nna <- !is.na(app[,1])
        app <- app[nna,]
        colnames(app) <- colnames(x)
        timevec <- time(x)[nna]
    } else {
        d <- dim(x)
        app <- t(matrix(aperm(x, c(2, 1, 3)), nrow=d[2]))
        nna <- !is.na(app[,1])
        app <- app[nna,]
        colnames(app) <- colnames(x)
        timevec <- rep(time(x), d[3])[nna]
        
        if (length(dup <- duplicated(timevec)) > 0) {
            timedup <- timevec[dup]
            warning(paste("duplicates at", paste(timedup, collapse=", ")),
              call.=FALSE)
        }
    }
    suppressWarnings(zoo(app, order.by=timevec, ...))
}

# z <- as.zoo(tail(pp, 120), TRUE)
# plot(z)
# plot(rollmean(z, 3))

# sd(1:6)
# sd(c(1.5, 3.5, 5.5))