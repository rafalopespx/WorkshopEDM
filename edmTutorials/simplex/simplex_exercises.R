nich97I <- read.csv("https://www.stat.berkeley.edu/~brill/blowfly97I")
plot(nich97I$total, type="b")
X4 <- diff(nich97I$total)
find.embN <- simplex(time_series = X4, E = 1:30)
plot(rho ~ E, data=find.embN, type="b",
     xlab = "Embedding dimensions",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")))
pred.decay4 <- simplex(time_series = X4, E = 17, tp = 1:50)
plot(rho ~ tp, data=pred.decay4,
     type = "l",
     xlab = "Time to prediction",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")))
find.theta4 <- s_map(X4, E=17)
plot(rho ~ theta, data=find.theta4,
     type = "l",
     xlab = expression(paste("Nonlinearity (",theta,")",sep="")),
     ylab = expression(paste("Forecast skill (",rho,")",sep="")))
