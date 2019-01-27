#' Get polling results
#' 
#' Retrieve polling results from \code{pollofpolls.no}
#' 
#' @param serieid id of the series to be retrieved
#' @param min.length minimum length for included series
#' @param verbose print progress messages
#' @param list return output as a list
#' 
#' @details
#' Calls are made to \code{pollofpolls.no} to retrieve archived general election
#' polling results. 
#' 
#' @return
#' if \code{list=FALSE} an object of class \code{pollplot} is returned.
#' This is an array of NA-padded polling data. 
#' Dates are along the first dimension, parties (always 10) are
#' along the second dimension, and series are along the third dimension.
#' In addition to dimnames, a few other attributes are included. \code{date} 
#' is a \code{"Date"} vector corresponding to the first dimension of the array.
#' \code{info} is a \code{data.frame} of serieid, institute and client values
#' corresponding to the retrieved series.
#' 
#' @export
#' 
#' @examples
#' pp <- get_polls(5:8)
#' summary(pp)
#' 
#' # head, tail and window methods
#' # first year
#' head(pp, 365)
#' window(pp, start=start(pp), end=start(pp)+365)
#' 
#' # entire 2010
#' window(pp, start="2010-01-01", end="2010-12-31")
#' 
#' # last 30 days
#' tail(pp, 30)
#' window(pp, start=Sys.Date()-30)
#' 
#' # end is always Sys.Date() at the time of retrieval
#' end(pp)
#' 
#' # start is the date of the earliest sample. Depends on serieid
#' start(pp)
#' start(get_polls(10))
#' 
#' # basic plotting
#' pp.t <- window(pp, start=Sys.Date()-120)
#' pp.tf <- apply(pp.t, 1:2, mean, na.rm=TRUE)
#' nna <- which(!is.na(pp.tf[, 1]))
#' matplot(time(pp.t)[nna], pp.tf[nna, ], type="l", xaxt="n")
#' axis.Date(1, time(pp.t)[nna])
#' legend("top", colnames(pp.tf), ncol=5, bty="n", xpd=NA, inset=-0.15,
#'   cex=0.85, lty=1:5, col=1:6)
#' 
#' # more compact format. No methods for it...
#' pp.l <- get_polls(c(2, 7, 10), list=TRUE)
#' 
#' # but easily converted to a list of irregular-time zoo objects
#' library(zoo)
#' pp.z <- lapply(pp.l, function(x) read.zoo(x[,-(1:2)]))
#' 
#' library(xts)
#' pp.zc <- apply.daily(do.call(rbind.xts, pp.z), "mean")
#' 
#' plot(rollmean(pp.zc, 30))
#' 


get_polls <- function(serieid=c(1:8, 15, 25, 160, 626), min.length=8, 
  verbose=TRUE, list=FALSE) {	
	
	loc <- Sys.getlocale("LC_CTYPE")
	Sys.setlocale("LC_CTYPE", "C")
    
	urls <- paste0("http://www.pollofpolls.no/",
	  "lastned.csv?tabell=liste_gallupserie&serieid=",
	   serieid, "&limit=0")

	resp.all <- list()
	for (i in 1:length(urls)) {
		
		resp.r <- tryCatch(
    	  read.csv2(urls[i], skip=2, stringsAsFactors=FALSE),
    	  error=function(e) e, silent=TRUE)
	    
	    if (inherits(resp.r, "error")) {
	    	if (verbose) {
	    	    cat(paste0("Skipping series ", serieid[i], 
	    	      ". Missing data.", "\n"))
	    	}
	    	next
	    }

	    # drop row with election results
	    resp.r <- resp.r[!grepl("Valg", resp.r[,1]),]

	    if ((nr <- nrow(resp.r)) < min.length) {
	    	if (verbose) {
	    	    cat(paste0("Skipping series ", serieid[i], 
	    	      ". Length < min.length.", "\n"))
	    	}
	    	next
	    }
	    
	    if (verbose) {
	    	    cat(paste0("Retrieving series ", serieid[i], "\n"))
	    }
	    	    
	    # create institute, client and date columns
	    mat1 <- matrix(unlist(strsplit(resp.r[,1], " - ")), ncol=2, byrow=TRUE)
        inscli <- regmatches(mat1[,1], regexpr("/", mat1[,1]), invert=TRUE)
	    mat2 <- trimws(matrix(unlist(inscli), ncol=2, byrow=TRUE))

        ins <- mat2[,1]
        cli <- mat2[,2]

        ins1 <- names(which.max(table(ins[1:min.length])))        
        cli1 <- names(which.max(table(cli[1:min.length])))
        
	    dat <- as.Date(mat1[,2], format="%d/%m-%Y")
	    
	    # clean up response columns
	    res <- sub("\\s*\\([^\\)]+\\)", "", as.matrix(resp.r[,-1]))
	    res <- sub(",", ".", res)
	    storage.mode(res) <- "numeric"
	    
	    colnames(res) <- c("Ap", "Hoyre", "Frp", "SV", "Sp", "KrF", 
	                       "Venstre", "MdG", "Rodt", "Andre")
	    
	    resp.df <- data.frame(institute=ins, client=cli, date=dat, res,
	      stringsAsFactors=FALSE)
	    resp.df <- resp.df[nr:1,]
	    rownames(resp.df) <- NULL
	    attr(resp.df, "info") <- list(serieid=serieid[i], 
	                                institute=ins1, 
	                                   client=cli1)

	    resp.all[[i]] <- resp.df
	    names(resp.all)[i] <- paste0("id", serieid[i])
	}

	# Remove all NULL-elements
	resp.all <- Filter(Negate(
	  function(x) is.null(unlist(x))), resp.all)
	
	if (verbose) {
        recent <- max(do.call(c, lapply(resp.all, function(x) max(x[,"date"]))))
		sp <- sum(sapply(resp.all, nrow))
		cat(paste0(length(resp.all), " series retrieved, ",
		  sp, " sample points in total, ", "most recent at ", recent, "\n"))
	}
	
	Sys.setlocale("LC_CTYPE", loc)
	
	if (list) {
	    return(resp.all)
	}
	
	date.min <- min(do.call(c, lapply(resp.all, function(x) min(x[,"date"]))))
    date.max <- Sys.Date()
    
    date.seq <- seq(date.min, date.max, by="day")

    resp.arr <- lapply(resp.all, function(x) 
      merge(data.frame(date=date.seq), x[,-(1:2)], by="date", all=TRUE)[,-1]
    )

    resp.arr <- list2array(resp.arr)
    info.df <- sapply(resp.all, function(x) unlist(attr(x, "info")))
    info.df <- as.data.frame(t(info.df), stringsAsFactors=FALSE)
    info.df[,1] <- as.integer(info.df[,1])
    
    attr(resp.arr, "date") <- date.seq
    attr(resp.arr, "info") <- info.df
    class(resp.arr) <- "pollplot"
    resp.arr
}