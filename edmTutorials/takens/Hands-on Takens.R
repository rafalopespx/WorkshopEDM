## ----setup, echo=FALSE, warning=FALSE, message=F-------------------------
library(knitr)
library(rEDM)
library(dplyr)
library(plotly)

opts_chunk$set(fig.align = 'center',
               fig.show = 'hold',
               fig.height = 5,
               fig.width = 5, 
               warning = FALSE, message = FALSE, error = FALSE, echo=TRUE)
options(formatR.arrow = TRUE,width = 90)###, cache=TRUE)

## ----load rEDM, echo=TRUE------------------------------------------------
library(rEDM)

## ----call vignette, eval=FALSE-------------------------------------------
## vignette("rEDM-tutorial", package="rEDM")

## ----generates data for the harmonic oscillator, echo=TRUE---------------
## Generate the data at tau = 0.2 time intervals
time1<-seq(0, 500, by = 0.2) 
x1<-sin(time1)
y1<-cos(time1)
## Plot the state space
plot(x1, y1, type = 'l', main = "State-space of harmonic oscillator",
     cex=0.2, xlab="X(t)", ylab="Y(t)")

## ----time series of each variable----------------------------------------
## Time series of the 1st 250 observations
ind <- 1:250
plot(time1[ind], x1[ind], type = 'l',
     main = "Time series of the variables of the system",
     col = "red", xlab = "Time", ylab = "Amplitude",
     ylim=c(-1,1.25))
lines(time1[ind], y1[ind], col = "blue")
legend("topright", c("X","Y"), col=c("red", "blue"),
       lty=1, bty="n")

## ----load make_block, eval=TRUE, echo=TRUE-------------------------------
source("https://raw.githubusercontent.com/mathbio/edmTutorials/master/utilities/make_block.R")

## ----plot one time series alone------------------------------------------
## Data frame with observations with tau = 0.2 with one lag
df1 <- make_block(data.frame(x=x1), t=time1, max_lag = 2)

## ----check lagged data frame---------------------------------------------
## Take a look in the lagged data to understand how the function works
head(df1)
tail(df1)

## ----shadow manifold-----------------------------------------------------
plot(x_1 ~ x, data=df1, type="l",
     xlab = "X", ylab = "X(t + tau)" )

## ----plot one 2nd time series alone--------------------------------------
## a new vector of observed values + Gaussian iid error
x2 <- x1 + rnorm(n = length(x1), mean = 0, sd = sd(x1)/12)
## Time series
plot(x1[ind] ~ time1[ind], type="l", xlab = "Time", ylab = "X",
     col="grey", lwd = 5, ylim=range(x2))
lines(x2[ind] ~ time1[ind], type="l", col="red", lwd=2)

## ----2D plot noisy series------------------------------------------------
## Data frame with lagged variables
df2 <- make_block( data.frame(x = x2))
## Shadow manifold with embedding = 2
plot(x_1 ~ x, data=df2, type="l",
     ylab = "X(t + tau)", xlab = "X" , col="grey",
     lwd=0.5)

## ----more noise----------------------------------------------------------
x3 <- x1 + rnorm(n = length(x1), mean = 0, sd = sd(x1)/6)
df3 <- make_block(data.frame(x=x3), max_lag = 5)
plot(x_1 ~ x, data=df3, type="l", col="grey", lwd=0.5,
     ylab = "X(t + tau)", xlab = "X" )

## ----more noise 3D plot, echo=FALSE, fig.align="center"------------------
plot_ly(df3, x = ~x, y=~x_1, z=~x_2, type="scatter3d",
        mode="lines", opacity=0.25) %>%
    layout(scene=list(camera = list(eye = list(x = 0, y = 0, z = -2)),
                      xaxis = list(title = 'X'),
                      yaxis = list(title = 'X (t + tau)'),
                      zaxis = list(title = 'X (t + 2tau)')))

## ----simulates oscillator series with smaller tau------------------------
time4<-seq(0, 500, by = 0.01) 
x4<-sin(time4)
y4<-cos(time4)

## ----smaller tau manifold------------------------------------------------
df4 <- make_block(data.frame(x=x4), t=time4)
plot(x_1 ~ x, data=df4, type="l",
     xlab = "X", ylab = "X(t + tau)" )

## ----abline, eval=FALSE--------------------------------------------------
## abline(0,1, col="red")

## ----problem time series-------------------------------------------------
prob2 <- read.csv("https://raw.githubusercontent.com/mathbio/edmTutorials/master/takens/problem-time-series.csv")

## ----plot time series problem 2------------------------------------------
plot( X ~ time, data = prob2, type="l")

## ----3D plot with scatterplot3d------------------------------------------
## Loads the package
library(scatterplot3d)
## 3D plot
scatterplot3d(x = df2$x, y = df2$x_1, z = df2$x_2,
              type = "l", color="grey", lwd = 0.5,
              xlab = "x(t)", ylab = "x(t+tau)", zlab = "x(t + 2 tau)")

## ----install packages, eval=FALSE----------------------------------------
## install.packages("scatterplot3d")

## ----include glossary, child = '../glossary.md'--------------------------



