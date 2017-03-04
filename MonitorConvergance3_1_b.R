######################Exercise 3.1#######################

#b. Monitor the convergence with the standard error of the estimate. Obtain three
#   digits of accuracy with probability :95.
library(MASS)
x1=dnorm(co,mean=x)
estint2=cumsum(x1)/(1:Niter)
esterr2=sqrt(cumsum((x1-estint2)^2))/(1:Niter)

x1=co*x1
estint1=cumsum(x1)/(1:Niter)
esterr1=sqrt(cumsum((x1-estint1)^2))/(1:Niter)

par(mfrow=c(1,2))

plot(estint1,type="l",xlab="iteration",ylab="",col="blue")
lines(estint1-2*esterr1,lty=2,lwd=2)
lines(estint1+2*esterr1,lty=2,lwd=2)

plot(estint2,type="l",xlab="iteration",ylab="",col="blue")
lines(estint2-2*esterr2,lty=2,lwd=2)
lines(estint2+2*esterr2,lty=2,lwd=2)

x=0;
max(4*var(dnorm(co,m=x))*10^6,4*var(co*dnorm(co,m=x))*10^6)
#Output:- 98301.97

x=2; 

4*10^6*max(var(dnorm(co,m=x)),var(co*dnorm(co,m=x)))
#Output:- 219929.2

x=4; 

10^6*4*max(var(dnorm(co,m=x)),var(co*dnorm(co,m=x)))
#Output:- 307311.1

par(mfrow=c(1,1))