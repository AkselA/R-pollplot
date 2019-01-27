#' Norwegian general election results 1945–2017
#' 
#' Two matrices: \code{gen.votes} is the vote percentage each party received,
#' \code{gen.mand} is the number of mandates each party received.
#' 
#' @name election_results
#' 
#' @usage 
#' gen.votes 
#' gen.mand
#' 
#' @aliases
#' gen.votes gen.mand
#' 
#' @examples
#' 
#' par(mfrow=c(3, 1), mar=c(1.8, 1.8, 1, 1), mgp=c(1.9, 0.6, 0), 
#'   oma=c(0, 0, 2, 0), xaxs="i")
#' 
#' matplot(gen.votes[,1], gen.votes[,-1], type="o", pch=16, cex=0.9,
#'   lty=1, lwd=3, col=ppcol)
#' legend("top", "Percentage of votes", inset=-0.08,
#'   xpd=NA, bg="white", x.intersp=-0.5)
#' legend("top", colnames(gen.votes)[-1], inset=-0.28, horiz=TRUE, xpd=NA, 
#'   x.intersp=0.5, bty="n", col=ppcol, lwd=2.5, cex=0.85, seg.len=1)
#' 
#' gen.mand.p <- gen.mand
#' gen.mand.p[,-1] <- round(gen.mand[,-1]*100 / rowSums(gen.mand[,-1]), 2)
#' 
#' matplot(gen.mand.p[,1], gen.mand.p[,-1], type="o", pch=16, cex=0.9,
#'   lty=1, lwd=3, col=ppcol)
#' legend("top", "Percentage of mandates", inset=-0.08,
#'   xpd=NA, bg="white", x.intersp=-0.5)
#' 
#' gen.diff <- gen.mand[, -11]
#' gen.diff[,-1] <- gen.mand.p[,-c(1, 11)] - gen.votes[,-c(1, 11)]
#' 
#' matplot(gen.diff[,1], gen.diff[,-1], type="o", pch=16, cex=0.9,
#'   lty=1, lwd=3, col=ppcol)
#' legend("top", "Difference between %votes and %mandates", inset=-0.08,
#'   xpd=NA, bg="white", x.intersp=-0.5)
#' 

"gen.votes"