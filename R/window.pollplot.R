#' @export

time.pollplot <- function(x, ...) {
	attr(x, "date")
}

#' @export

start.pollplot <- function(x, ...) {
	attr(x, "date")[1]
}

#' @export

end.pollplot <- function(x, ...) {
	d <- time(x)
	d[length(d)]
}

#' @export

window.pollplot <- function(x, start=NULL, end=NULL, ...) {
	if (is.null(start)) start <- start(x)
	if (is.null(end)) end <- end(x)
	if (is.character(start) | is.numeric(start)) {
		start <- zoo::as.Date(start, ...)
	}
	if (is.character(end) | is.numeric(end)) {
		end <- zoo::as.Date(end, ...)
	}
	if (start < start(x)) {
		start <- start(x)
		warning("'start' is earlier than first sample")
	}
	if (end > end(x)) {
		end <- end(x)
		warning("'end' is later than last sample")
	}
    d <- time(x)
    s <- which(d == start)
    e <- which(d == end)
    
    x.w <- x[s:e, , ]
    attr(x.w, "date") <- d[s:e]
    attr(x.w, "info") <- attr(x, "info")
    class(x.w) <- class(x)
    x.w
}

#' @export

head.pollplot <- function(x, n=14L, ...) {
	d <- time(x)
	dlen <- length(d)
    n <- if (n < 0L) 
        max(dlen + n, 0L)
    else min(n, dlen)
    s <- seq_len(n)
    x.h <- x[s, , ]
    attr(x.h, "date") <- d[s]
    attr(x.h, "info") <- attr(x, "info")
    class(x.h) <- class(x)
    x.h
}

#' @export

tail.pollplot <- function(x, n=14L, ...) {
	d <- time(x)
	dlen <- length(d)
    n <- if (n < 0L) 
        max(dlen + n, 0L)
    else min(n, dlen)
    s <- seq.int(to=dlen, length.out=n)
    x.t <- x[s, , ]
    attr(x.t, "date") <- d[s]
    attr(x.t, "info") <- attr(x, "info")
    class(x.t) <- class(x)
    x.t
}

#' @export

`[.pollplot` <- function(x, i, j, k, drop=FALSE) {
	d <- dim(x)
    if (missing(i))
        i <- seq_len(d[1])
    if (missing(j))
        j <- seq_len(d[2])
    if (missing(k))
        k <- seq_len(d[3])
    xo <- unclass(x)[i, j, k, drop=drop]
    attr(xo, "date") <- attr(x, "date")[i]
    attr(xo, "info") <- attr(x, "info")[k,]
    class(xo) <- "pollplot"
    xo
}
