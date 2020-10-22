## ----setup, echo=FALSE, warning=FALSE, message=F-------------------------
library(knitr)
library(plotly)
library(rEDM)
library(dplyr)


opts_chunk$set(fig.align = 'center',
               fig.show = 'hold',
               fig.height = 5,
               warning = FALSE, message = FALSE, error = FALSE, echo=FALSE)
options(formatR.arrow = TRUE,width = 90)###, cache=TRUE)

## ----call vignette, echo=TRUE, eval=FALSE--------------------------------
## vignette("rEDM-tutorial", package="rEDM")

## ----generate data, echo=TRUE--------------------------------------------
## Two vectors to store data
X <- c()
Y <- c()
## Initial values
X[1] <- 0.1
Y[1] <- 0.3
X[2] <- 0.3
Y[2] <- 3.78*Y[1] - 3.78*Y[1]^2
## Iterate the dynamics 150 time steps
for(i in 3:150){
    X[i] <- 3.77*X[i-1] - 3.77*X[i-1]^2 - 0.85*Y[i-1]*X[i-1] - 0.5*X[i-2]
    Y[i] <- 3.78*Y[i-1] - 3.78*Y[i-1]^2
}

## ----plot 1st time series, echo=TRUE-------------------------------------
plot(X, xlab="Time", ylab="X", type="b", lty=3)

## ----shadow manifold, echo=TRUE------------------------------------------
## Data frame with X at t0, t1 and t2
df1 <- data.frame(X.t0=X[1:(length(X)-2)],X.t1=X[2:(length(X)-1)],  X.t2=X[3:(length(X))])
## point to point Euclidian distance matrix
dist.m1 <- as.matrix(dist(df1[,1:3], upper=TRUE))
## Indexes of the 4 nearest neighbors of the last point in the time series
neigb1 <- order(dist.m1[(ncol(dist.m1)-1),])[2:5]

## ----plot shadow df3-----------------------------------------------------
## Plot of the manifold: add colored markers on last point and their neighbors
p3 <- plot_ly(df1, x = ~X.t0, y=~X.t1, z=~X.t2, marker=(list(color=grey)), opacity=0.25) %>%
    layout(scene = list(xaxis = list(title = 'X'),
                        yaxis = list(title = 'X (t+1)'),
                        zaxis = list(title = 'X (t+2)'))) %>%
    add_markers(text = paste("time =",3:length(X)), showlegend = FALSE) %>%
    add_trace( x = ~X.t0, y=~X.t1, z=~X.t2, data=df1[c(length(X)-3,neigb1),],
              opacity=1,
              marker=list(color=c("blue","red","green","orange", "magenta")),
              type="scatter3d", mode="markers",
              text = paste("time =",rownames(df1[c(length(X)-3,neigb1),])), showlegend = FALSE) %>%
    add_trace(data=df1[c(length(X)-3, neigb1[1]),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE) %>%
    add_trace(data=df1[c(length(X)-3, neigb1[2]),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE)%>%
    add_trace(data=df1[c(length(X)-3, neigb1[3]),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE) %>%
    add_trace(data=df1[c(length(X)-3, neigb1[4]),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE)
p3

## ----time series with neighbors highlighted, echo=TRUE-------------------
time1 <- min(neigb1,length(X)):length(X) # syntatic sugar
plot(time1, X[time1] , xlab="Time", ylab="X", type="b", lty=3)
cores <- c("blue", "red","green","orange", "magenta")
z <- 1
for(i in c(length(X)-3,neigb1)){
    ind <- i:(i+2)
    lines(ind, X[ind], type="b", col=cores[z], lwd=2, pch=19)
    z <- z+1}

## ----time series with projected point, echo=TRUE-------------------------
plot(time1, X[time1] , xlab="Time", ylab="X", type="b", lty=3)
cores <- c("blue", "red","green","orange", "magenta")
z <- 1
for(i in c(length(X)-2,neigb1+1)){
    ind <- i:(i+2)
    lines(ind, X[ind], type="b", col=cores[z], lwd=2, pch=19)
    z <- z+1}
arrows(x0=neigb1+3, y0=X[neigb1+3], x1=length(X)*.99, y1=X[neigb1+3],
       col=cores[-1])
points(length(X), X[length(X)], pch=17, cex=1.5)

## ----shadow manifold with projected simplex------------------------------
s1 <- simplex(X, E=3, stats_only=FALSE)$model_output[[1]]
p1.last <- s1$pred[nrow(s1)]
pred.df <- df1[c(length(X)-2,neigb1+1),]
pred.df[1,3] <- p1.last
p4 <-
    p3 %>%
    add_trace( x = ~X.t0, y=~X.t1, z=~X.t2, data=pred.df,
              marker=list(color=c("black","red","green","orange", "magenta")),
              type="scatter3d", mode="marker",opacity=1,
              text = paste("time = ",rownames(pred.df)), showlegend = FALSE) %>%
    add_trace(data=pred.df[c(1,2),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE) %>%
    add_trace(data=pred.df[c(1,3),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE) %>%
    add_trace(data=pred.df[c(1,4),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE) %>%
    add_trace(data=pred.df[c(1,5),], mode="lines",
              line = list(width = 6, color = "blue"), showlegend = FALSE) %>%
    add_trace( x = ~X.t0, y=~X.t1, z=~X.t2, data=df1[nrow(df1),],
              opacity=1,
              marker=list(color=c("blue")),
              type="scatter3d", mode="markers")
#p4
htmlwidgets::saveWidget(as_widget(p4), file = "p4.html")
include_url("p4.html", height="600px")

## ----load rEDM, echo=TRUE------------------------------------------------
library(rEDM)

## ----simplex predictions, echo=TRUE--------------------------------------
predE3 <- simplex(time_series = X, E = 3, stats_only = FALSE)

## ----simplex object, echo=TRUE-------------------------------------------
names(predE3)

## ----simplex object E3, echo=TRUE----------------------------------------
## dataframe with obs and predicted in a separated object
fits <- predE3$model_output[[1]]
head(fits)

## ----obs x pred time series, echo=TRUE-----------------------------------
plot(pred ~ time, data = fits, type = "l", col = "blue", lwd=3,
     xlab="Time", ylab="X", ylim=range(fits[,2:3]))
lines(obs ~ time, data = fits, col=grey.colors(1, alpha=0.25), lwd = 6)
legend("topright", c("Observed", "Predicted"), lty=1, lwd=c(6,3),
       col=c(grey.colors(1, alpha=0.25), "blue"),bty="n")

## ----forecast skill value, echo=TRUE-------------------------------------
predE3$rho

## ----obs x pred varying embedding, fig.height=3, echo=TRUE---------------
predE2 <- simplex(time_series = X, E = c(2,3,10), stats_only = FALSE)
par(mfrow=c(1,3))
plot(pred ~ obs, data = predE2$model_output[[1]],
     main = bquote("Embedding = 2, " ~ rho == .(round(predE2$rho[1],2))))
plot(pred ~ obs, data = predE2$model_output[[2]],
     main = bquote("Embedding = 3, " ~ rho == .(round(predE2$rho[2],2))))
plot(pred ~ obs, data = predE2$model_output[[3]],
     main = bquote("Embedding = 10, " ~ rho == .(round(predE2$rho[3],2))))
par(mfrow=c(1,1))

## ----find embedding dimensions, echo=TRUE--------------------------------
find.emb <- simplex(time_series = X, E = 1:10)
plot(rho ~ E, data=find.emb, type="b",
     xlab = "Embedding dimensions",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")))

## ----prediction decay plot, echo=TRUE------------------------------------
pred.decay <- simplex(time_series = X, E = 3, tp = 1:10)
plot(rho ~ tp, data=pred.decay,
     type = "b",
     xlab = "Time to prediction",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")))

## ----non-chaotic data, echo=TRUE-----------------------------------------
X2 <- c()
X2[1] <- 0.5
for(i in 2:150)
    X2[i] <- 3.569949 * X2[i-1] * ( 1- X2[i-1] )
## Plots the series
plot(X2, xlab="Time", ylab="X", type="b", lty=3)

## ----nc find embedding, echo=TRUE----------------------------------------
find.emb2 <- simplex(time_series = X2, E = 1:10)
plot(rho ~ E, data=find.emb2, type="b",
     ylim=c(0,1),
     xlab = "Embedding dimensions",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")))

## ----nc pred decay-------------------------------------------------------
pred.decay2 <- simplex(time_series = X2, E = 6, tp = 1:50)
plot(rho ~ tp, data=pred.decay2,
     type = "l",
     xlab = "Time to prediction",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")),
     ylim = c(0,1))

## ----add noise, echo=TRUE------------------------------------------------
## Adding noise
X3 <- X2 + rnorm(n = length(X2), mean = 0, sd = sd(X2))
## Plot series
plot(X3, xlab="Time", ylab="X", type="b", lty=3)

## ----noise find embedding, eval=FALSE------------------------------------
## find.emb3 <- simplex(time_series = X3, E = 1:10)
## plot(rho ~ E, data=find.emb3, type="b", ylim=c(0,1),
##      xlab = "Embedding dimensions",
##      ylab = expression(paste("Forecast skill (",rho,")",sep="")))

## ----noise pred decay, echo = TRUE---------------------------------------
pred.decay3 <- simplex(time_series = X3, E = 6, tp = 1:50)
plot(rho ~ tp, data=pred.decay3,
     type = "l",
     xlab = "Time to prediction",
     ylab = expression(paste("Forecast skill (",rho,")",sep="")),
     ylim = c(0,1))

## ----nicholson data, echo=TRUE-------------------------------------------
nich97I <- read.csv("https://www.stat.berkeley.edu/~brill/blowfly97I")
plot(nich97I$total, type="b", xlab="Time (days)", ylab="Total number of flies")

## ----nicholson data diff, echo = TRUE------------------------------------
X4 <- diff(nich97I$total)

## ----include glossary, child = '../glossary.md'--------------------------



