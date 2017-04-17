'"""
#Purpose: Below application code is designed to read csv data file and find best fit ARIMA
MODEL for closing prices variable. And then need to check existence of ARCH or GARCH effects 
using Engles Methodology. 
#Process: 1. Check data is stationary or not because arima model is performed on stationary
             data.
          2. Make data stationary by stationary we mean, Variance & Mean of data.
          3. Perform PACF(Partial Autocorrelation) to get an idea of possible value of p.
          4. Pick the value of p and form possible combinations of ARIMA model. 
          5. Pick model with low AIC value.And fit the model on data.
          6. Cross validate picked model using L-jung Box test.
          7. Now again perform steps 3-5 to get value of q.
          8. Next check coefficients of parent model & squared residuals models. To prove
             the existence of ARCH or GARCH effects.  
#Author:  Aakash Parwani
#Date:    01 December 2016

"""'

####################################PROBLEM 1#########################################
#load the data
data<-read.csv("C:/Users/hp/Downloads/spy.csv",header = TRUE,sep = ",")

#let's check the data whether it is stationary or not.
plot(data$Close)

#make the data stationary
plot(diff(data$Close,differences = 1))

#check Partial Auto Correlation without calcuating difference.
#pacf(data$Close,lag.max = 20)
#pacf(data$Close,lag.max = 20,plot=FALSE)

#check Partial Auto Correlation after calcuating difference.
pacf(diff(data$Close), main = "PACF of data",lag.max = 20)
pacf(diff(data$Close),lag.max = 20,plot=FALSE)

#fit ARIMA model with different possible combinations.
arima1<-arima(data$Close,c(7,0,0))
arima11<-arima(data$Close,c(7,1,0))
#arima2<-arima(log(data$Close),c(2,0,0))
#arima21<-arima(log(data$Close),c(2,1,0))

#verify the aic of all models and pick the model with lowest aic value.
arima1$aic
arima11$aic
#arima2$aic
#arima21$aic

#fetch the residuals of all the models
residual.arima1<-arima1$res
residual.arima11<-arima11$res
#residual.arima2<-arima2$res
#residual.arima21<-arima21$res

#now let us validate the best fit model arima11 using ljung-box test. 
#from results we can say arima(7,1,0) model is best as it has less aic value.
#also "Ljung-Box" test has proved that it is best fit model.
Box.test(residual.arima1,lag=10,type="Ljung-Box")
Box.test(residual.arima11,lag=10,type="Ljung-Box")

print("Value of p is 7")

####################################PROBLEM 2#########################################
#now check the volatility in residuals of best fitted model i.e. 7,1,0
#perform square of residuals
sq.res.arima11<-arima11$res^2

#plot the squared residuals,
#squared residual plot shows some cluster of volatility at some points
plot(sq.res.arima11,main="Square residuals",ylab="Square residuals of arima 7,1,0")

#check Partial Auto Correlation of squared residuals,
#from figure above we can see that after lag 11 values are coming down to 0.
pacf(sq.res.arima11,main="PACF Squared Residuals",lag.max = 20)
pacf(sq.res.arima11,lag.max = 20,plot=FALSE)



#fit ARIMA model on squared residuals with different possible combinations.
sq.res.arima.model1<-arima(sq.res.arima11,c(11,0,0))
sq.res.arima.model11<-arima(sq.res.arima11,c(11,1,0))

#verify the aic of all models and pick the model with lowest aic value.
sq.res.arima.model1$aic
sq.res.arima.model11$aic

print("Value of q is 11")

####################################CONCLUSION#######################################
#verify the coefficient of parent model as well as squared residual model.
#from the results we can see coefficients are not zero for both parent and child model.
#Hence we can say Daily Closing Price exhibits ARCH or GARCH effects.
arima11$coef
sq.res.arima.model1$coef