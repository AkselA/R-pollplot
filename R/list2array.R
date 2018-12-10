list2array <- function(x) {
    arr <- array(data=unlist(x), 
      dim=c(nrow(x[[1]]), ncol(x[[1]]), length(x)), 
      dimnames=list(
        rownames(x[[1]]), colnames(x[[1]]), names(x)))
    comment(arr) <- comment(x)
    arr
}