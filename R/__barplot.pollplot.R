pp <- get_polls()
da <- as.Date("2018-12-12")-30
x <- window(pp, da-30, da)

d <- dim(x)
summary(x)
app <- t(matrix(aperm(x, c(2, 1, 3)), nrow=d[2]))
nna <- !is.na(app[,1])
app <- app[nna,]
colnames(app) <- colnames(x)
timevec <- rep(time(x), d[3])[nna]

zoo(app, order.by=timevec)


appm <- colMeans(app)
or <- order(appm, decreasing=TRUE)
gr <- c(1, 2, 2, 1, 1, 2, 2, 3, 3, 3)

apply(app, 2, order)

# apps <- sapply(1:3, function(x) app[9,] * (gr == x))[or,]

# barplot(apps, col=ppcol[or], border=NA)



r <- apply(app, 1, function(x) colSums(sapply(1:3, function(y) x * (gr == y))))
# plot(sort(r[1,] - r[2,]))

# qqnorm(r[1,] - r[2,])
# qqline(r[1,] - r[2,], col=2)

plot(density(r[1,] - r[2,], adjust=1.5), xlim=c(-12, 12), ylim=c(0, 0.2))

ahist(r[1,] - r[2,], n.breaks=4, xlim=c(-12, 12), ylim=c(0, 0.2))
abline(v=median(r[1,] - r[2,]), col=2)