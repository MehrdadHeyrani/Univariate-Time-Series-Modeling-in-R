################### R codes ########################
#                                                  #
#                     R Book                       #
#                                                  #
#                Mehrdad Heyrani                   #
#       MehrdadHeyrani@alum.sharif.edu             #
#                                                  #
#                Nasim RoshanZamir                 #
#                                                  #
####################################################


####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 1              ##########
##########  Univariate time series        ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################


####################################################
####################################################
##########                                ##########
##########         Chapter 2              ##########
##########                                ##########
####################################################
####################################################


# create a Loop for finding  ARMA Order based on AIC and BIC

P=10 # maximum estimated order for AR part of the model

Q=10 # maximum estimated order for MA part of the model

aic.x <- matrix(ncol=P, nrow=Q, byrow = T)
bic.x <- matrix(ncol=P, nrow=Q, byrow = T)
for (i in 1:P)
  for (j in 1:Q)
  {
    arimafit <- arima(x, order=c(i-1,0,j-1), method = c("ML"), optim.control = list(maxit=10000), optim.method = "BFGS")
    aic.x[i,j] <- AIC(arimafit)
    bic.x[i,j] <- BIC(arimafit)
  }
aic.min<- min(aic.x)
bic.min<- min(bic.x)
for (h in 1:P)
  for (f in 1:Q)
  {
    if (aic.x[h,f]==aic.min)
    {
      p.aic <- h-1
      q.aic <- f-1
    }
  }
for (h in 1:P)
  for (f in 1:Q)
  {
    if (bic.x[h,f]==bic.min)
    {
      p.bic <- h-1
      q.bic <- f-1
    }
  }
p.aic
q.aic
p.bic
q.bic
fit <- arima(x, order=c(p.aic,0, q.aic), method = c("ML")
             coeftest(fit)
             fit