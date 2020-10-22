
## ----setup, echo=FALSE, warning=FALSE, message=F-------------------------
library(knitr)
library(plotly)
library(rEDM)
library(dplyr)


opts_chunk$set(fig.align = 'center',
               fig.show = 'hold',
               fig.height = 5,
               warning = FALSE, message = FALSE, error = FALSE, echo=TRUE)
options(formatR.arrow = TRUE,width = 90)


## ----call vignette, eval=FALSE-------------------------------------------
## vignette("rEDM-tutorial", package="rEDM")


## ----generate data-------------------------------------------------------
## Two vectors to store data
X <- c()
Y <- c()
## Initial values
X[1] <- 0.1
Y[1] <- 0.1
## Iterate the dynamics 150 time steps
for(i in 2:150){
  X[i] <- 3.77*X[i-1]*(1-X[i-1])
  Y[i] <- 3.82*Y[i-1]*(1-Y[i-1]-0.05*X[i-1])
}
XY<-as.data.frame(cbind(X,Y))


## ----plot 1st time series------------------------------------------------
par(cex=1.1,lwd=2)
plot(20:50,X[20:50],type="b", pch=18, col="blue",ylim=c(min(X,Y),max(X,Y)),
     main='Two Species',xlab = 'time',ylab='Population')
lines(20:50,Y[20:50],pch=19, col="red", type="b",lty=2,lwd=2)
legend(x = "bottomright", legend = c("X", "Y"),lty=c(1,2),pch=c(18,19),
       col = c("blue", "red"), inset = 0.02,lwd=2)


## ----plot correlation----------------------------------------------------
fit<-lm(Y ~ X)
plot(X,Y,main='Correlation (X,Y)')
abline(0,1,lty=2)
#abline(fit$coefficients[1],fit$coefficients[2])
legend(x = "bottomleft", legend = paste('r =',round(cor(X,Y)*100)/100),
       inset = 0.02,col = 'black')


## ----optimal embeddings X------------------------------------------------
options(warn = -1)
simplex_X<-simplex(X,silent=T)
plot(simplex_X$rho,type='o', ylab = "Forecast Skill (rho)",
     xlab="Embedding Dimension (E)")
E_star_X<-which.max(simplex_X$rho)
print(paste('E*(X) =',E_star_X))


## ----optimal embeddings Y------------------------------------------------
simplex_Y<-simplex(Y,silent=T)
plot(simplex_Y$rho,type='o', ylab = "Forecast Skill (rho)",
     xlab="Embedding Dimension (E)")
E_star_Y<-which.max(simplex_Y$rho)
print(paste('E*(Y) =',E_star_Y))


## ----load make_block-----------------------------------------------------
source("https://raw.githubusercontent.com/mathbio/edmTutorials/master/utilities/make_block.R")


## ----make_block----------------------------------------------------------
# max_lag is the optimal embedding dimension
Shadow_MXY<-make_block(XY,max_lag = 2)
Shadow_MX<-Shadow_MXY[,2:3]
Shadow_MY<-Shadow_MXY[,4:5]

head(Shadow_MXY)


## ----MX X_xmap_Y_code----------------------------------------------------
predictor<-70
print(Y[predictor])


## ----neighbors_X---------------------------------------------------------
dist.matrix_X <- as.matrix(dist(Shadow_MX, upper=TRUE))
neigb_X <- order(dist.matrix_X[predictor,])[2:4]
neigh_X_print<-c(neigb_X)
print(paste('simplex_Mx:',list(neigh_X_print)))


## ----MX X_xmap_Y, echo=FALSE, fig.align='center'-------------------------
p_MX_X_to_Y <- plot_ly(Shadow_MX, x=~X, y=~X_1, marker=(list(color=grey)), opacity=0.1) %>%
  layout(xaxis = list(title = 'X'),yaxis = list(title = 'X(t-1)'),title='Mx') %>%
  add_markers(text = paste("time =",1:length(X)), showlegend = FALSE) %>%
  add_trace( x = ~X, y=~X_1,data=Shadow_MX[c(predictor,neigb_X),],opacity=1,marker=list(color=c("red","blue","blue","blue")),type="scatter", mode="markers",text = paste("time =",c(predictor,neigb_X)))
p_MX_X_to_Y


## ----MY_X_xmap_Y, echo=FALSE---------------------------------------------
p_MY_X_to_Y <- plot_ly(Shadow_MY, x=~Y, y=~Y_1, marker=(list(color=grey)), opacity=0.1) %>%
  layout(xaxis = list(title = 'Y'),yaxis = list(title = 'Y (t-1)'),title='My') %>%
  add_markers(text = paste("time =",1:length(Y)), showlegend = FALSE) %>%
  add_trace( x = ~Y, y=~Y_1,data=Shadow_MY[c(neigb_X),],opacity=1,marker=list(color=c("green","green","green")),type="scatter", mode="markers",text = paste("time =",c(neigb_X)))
p_MY_X_to_Y



## ----estimating Y from X (X_xmap_Y)--------------------------------------
lib <- c(1, NROW(Shadow_MXY))
block_lnlp_output_XY <- block_lnlp(Shadow_MXY, lib = lib, pred = lib, columns = c("X",
 "X_1"), target_column = "Y", stats_only = FALSE, first_column_time = TRUE)
observed_all_Y <- block_lnlp_output_XY$model_output[[1]]$obs
predicted_all_Y <- block_lnlp_output_XY$model_output[[1]]$pred
pred_obs_Y<-as.data.frame(cbind(predicted_all_Y,observed_all_Y)) #
colnames(pred_obs_Y)<-c('Predicted Y','Observed Y')
head(pred_obs_Y)


## ----predicted y---------------------------------------------------------

pred_obs_Y[(predictor-2),]



## ----plot_obs_pred_MX_MY-------------------------------------------------
fit_YX<-lm(predicted_all_Y ~ observed_all_Y)
plot_range <- range(c(observed_all_Y, predicted_all_Y), na.rm = TRUE)
plot(observed_all_Y,predicted_all_Y, xlim = plot_range, ylim = plot_range, xlab = "Observed Y",
ylab = "Predicted Y")
#abline(fit_YX$coefficients[1],fit_YX$coefficients[2])
abline(0,1,lty=2)
legend(x = "bottomright", legend = paste('r =',round(cor(observed_all_Y, predicted_all_Y)*100)/100),inset = 0.02,col = 'black')
observed_pred_Y<-observed_all_Y[predictor-2]
predicted_pred_Y<-predicted_all_Y[predictor-2]
points(observed_pred_Y,predicted_pred_Y,col='red',pch=16,cex=1.2)


## ----MX Y_xmap_X_code----------------------------------------------------
print(X[predictor])


## ----neighbors_y---------------------------------------------------------
dist.matrix_Y <- as.matrix(dist(Shadow_MY, upper=TRUE))
neigb_Y <- order(dist.matrix_Y[predictor,])[2:4]
neigh_Y_print<-c(neigb_Y)
print(paste('simplex_My:',list(neigh_Y_print)))


## ----MY Y_xmap_X, echo=FALSE, fig.align='center'-------------------------
p_MY_Y_to_X <- plot_ly(Shadow_MY, x=~Y, y=~Y_1, marker=(list(color=grey)), opacity=0.1) %>%
  layout(xaxis = list(title = 'Y'),yaxis = list(title = 'Y(t-1)'),title='My') %>%
  add_markers(text = paste("time =",1:length(X)), showlegend = FALSE) %>%
  add_trace( x = ~Y, y=~Y_1,data=Shadow_MY[c(predictor,neigb_Y),],opacity=1,marker=list(color=c("red","blue","blue","blue")),type="scatter", mode="markers",text = paste("time =",c(predictor,neigb_Y)))
p_MY_Y_to_X


## ----MX_Y_xmap_X, echo=FALSE---------------------------------------------
p_MX_Y_to_X <- plot_ly(Shadow_MX, x=~X, y=~X_1, marker=(list(color=grey)), opacity=0.1) %>%
  layout(xaxis = list(title = 'X'),yaxis = list(title = 'X (t-1)'),title='Mx') %>%
  add_markers(text = paste("time =",1:length(Y)), showlegend = FALSE) %>%
  add_trace( x = ~X, y=~X_1,data=Shadow_MX[c(neigb_Y),],opacity=1,marker=list(color=c("green","green","green")),type="scatter", mode="markers",text = paste("time =",c(neigb_Y)))
p_MX_Y_to_X


## ----estimating X from Y (Y_xmap_X)--------------------------------------
lib<-lib <- c(1, NROW(Shadow_MXY))
block_lnlp_output_YX <- block_lnlp(Shadow_MXY, lib = lib, pred = lib, columns = c("Y", "Y_1"),
                                   target_column = "X", stats_only = FALSE, first_column_time = TRUE)
observed_all_X <- block_lnlp_output_YX$model_output[[1]]$obs
predicted_all_X <- block_lnlp_output_YX$model_output[[1]]$pred
pred_obs_X<-as.data.frame(cbind(predicted_all_X,observed_all_X))
colnames(pred_obs_X)<-c('Predicted X','Observed X')
head(pred_obs_X)


## ----plot_obs_pred_MY_MX-------------------------------------------------
fit_XY<-lm(predicted_all_X ~ observed_all_X)
plot_range <- range(c(observed_all_X, predicted_all_X), na.rm = TRUE)
plot(observed_all_X, predicted_all_X, xlim = plot_range, ylim = plot_range,
     xlab = "Observed X", ylab = "Predicted X")
#abline(fit_XY$coefficients[1],fit_XY$coefficients[2])
abline(0,1,lty=2)
legend(x = "bottomright", legend = paste('r =',round(cor(observed_all_X,predicted_all_X)*100)/100),
       inset = 0.02,col = 'black')
observed_pred_X<-observed_all_X[predictor-2]
predicted_pred_X<-predicted_all_X[predictor-2]
points(observed_pred_X,predicted_pred_X,col='red',pch=16,cex=1.2)


## ----convergent----------------------------------------------------------
# cross map from X to Y
X_xmap_Y<- ccm(XY, E = 2, lib_column = "X", target_column = "Y",
               lib_sizes = seq(10, 130, by = 10), num_samples = 100, random_libs = TRUE,
               replace = TRUE)
# cross map from Y to X
Y_xmap_X<- ccm(XY, E = 2, lib_column = "Y", target_column = "X",
               lib_sizes = seq(10, 130, by = 10), num_samples = 100, random_libs = TRUE,
               replace = TRUE)

# mean values
X_xmap_Y_means <- ccm_means(X_xmap_Y)
Y_xmap_X_means <- ccm_means(Y_xmap_X)

# plot graphs
plot(X_xmap_Y_means$lib_size, pmax(0, X_xmap_Y_means$rho), type = "l", col = "red",
     main='Two Species', xlab = "Library Size (L)",
     ylab = "Cross Map Skill (Pearson rho)", ylim = c(0,1))
lines(Y_xmap_X_means$lib_size, pmax(0, Y_xmap_X_means$rho), col = "blue")
legend(x = "topleft", legend = c("X_xmap_Y", "Y_xmap_X"), col = c("red", "blue"),
       cex=1.1,lwd=2, inset = 0.02)


## ----ex4-----------------------------------------------------------------
data(sardine_anchovy_sst)
head(sardine_anchovy_sst)




