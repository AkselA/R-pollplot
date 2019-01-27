

boxplot.pollplot <- function(x, log1p.y=TRUE, border=ppcol, 
  col=adjustcolor(ppcol, 0.5), cex.axis=c(0.65, 0.8), whisklty=1, 
  main=NULL, ...) {
  	if (length(cex.axis) != 2) {
  		cex.axis <- rep(cex.axis, length.out=2)
  	}
	d <- dim(x)

	app <- t(matrix(aperm(x, c(2, 1, 3)), nrow=d[2]))
	nna <- !is.na(app[,1])
	app <- app[nna,]
	colnames(app) <- colnames(x)
	timevec <- rep(time(x), d[3])[nna]
	
	daterange <- range(timevec)
	if (is.null(main)) {
	    maintext <- paste(daterange, collapse="–")
	} else {
		maintext <- main
	}

    if (log1p.y) {
        app <- log1p(app)
        bx <- boxplot(app, yaxt="n", border=border, col=col, cex.axis=cex.axis[1],
          main=maintext, whisklty=whisklty, ...)
		s <- log1p(signif(expm1(seq(min(app), max(app), length=9)), 2))
		sx <- expm1(s)
		s <- s[!duplicated(sx)]
		sx <- sx[!duplicated(sx)]
		 
		axis(2, at=s, labels=sx, cex.axis=cex.axis[2])

    } else {
        bx <- boxplot(app, yaxt="n", cex.axis=cex.axis[1], border=border, 
          col=col, main=maintext, whisklty=whisklty, ...)
		axis(2, cex.axis=cex.axis[2])
    }
    
    bx$daterange <- daterange
    invisible(bx)
}

# pp <- get_polls()
# pp2 <- tail(pp, 30)
# x <- pp2
# summary(pp2)
# cclog <- boxplot(pp2, ylim=log1p(c(0.6, 32)))
# cclin <- boxplot(pp2, log1p.y=FALSE, ylim=c(0.5, 32))

