###Open Science in Marketing
###Jan R. Landwehr (Goethe University Frankfurt) & Stefan Mayer (University of Tübingen)
###CC BY 4.0: https://osf.io/t93pf

x <- 1:105
y0 <- 0.95^x
y <- 1-y0

plot(y~x, type="l", xlab="Number of statistical tests", ylab="P(falsly rejecting the Null at least once)")
