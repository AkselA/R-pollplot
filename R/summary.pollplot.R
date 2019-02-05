#' @export

print.pollplot <- function(x, ...) {
	d <- time(x)
	l <- apply(x, 3, 
	  function(y) {
		nna <- rowSums(!is.na(y)) > 0
		dtf <- data.frame(date=d[nna], y[nna, , drop=FALSE])
		rownames(dtf) <- NULL
		dtf
	  }
	)
	# x[, ,"id11"]
	# unclass(x)
	i <- attr(x, "info")
	n <- names(l)
	
	invisible(lapply(1:length(l), 
	  function(y) {
		cat("index: ", y, ", name: ", n[y],
		  ", institute: ", i[y, 2], ", client: ", i[y, 3], "\n", sep="")
	  	if (nrow(l[[y]]) == 0) {
	  		cat("empty\n")
	  	} else {
		    print(l[[y]])
		}
		cat("\n")
	  }
	))
}


#' @export

summary.pollplot <- function(object, round=1, ...) {
	d <- time(object)
	l <- apply(object, 3, 
	  function(y) {
		nna <- rowSums(!is.na(y)) > 0
		dtf <- data.frame(date=d[nna], y[nna, , drop=FALSE])
		rownames(dtf) <- NULL
		dtf
	  }
	)
    dtf <- do.call(rbind, l)
	summ <- sapply(dtf[,-1], 
	  function(y) {
	  	q <- quantile(y, c(1, 0.75, 0.5, 0.25, 0))
	  	c(q, mean=mean(y))
	  }
	)
	nr <- nrow(dtf)
	ra <- as.character(range(dtf[,1]))
	summ <- round(summ, round)
    cat("Summary on", nr, "sample points, between",
      ra[1], "and", ra[2], "\n\n")
    print(summ)
    invisible(summ)
}


#' @export

str.pollplot <- function(object, ...) {
	cat("'pollplot'")
	str(unclass(object), ...)
}
