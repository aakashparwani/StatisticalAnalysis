######################Exercise 3.1#######################

#c.Repeat the experiment with a Monte Carlo integration based on a normal
# simulation and compare both approaches.


#normal simulation
Niter=10^4
co=rnorm(Niter)
I=mean(co*dcauchy(co))/mean(dcauchy(co))

x=0
mean(co*dcauchy(co,location = x))/mean(dcauchy(co,location = x))
#Output:- 0.01652606


x=2
mean(co*dcauchy(co,location = x))/mean(dcauchy(co,location = x))
#Output:- 0.725805

x=4
mean(co*dcauchy(co,location = x))/mean(dcauchy(co,location = x))
#Output:- 0.5891296



x1=dcauchy(co,location = x)
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
max(4*var(dcauchy(co,location = x))*10^6,4*var(co*dcauchy(co,location=x))*10^6)
#Output:- 62433.53

x=2; 

4*10^6*max(var(dcauchy(co,location = x)),var(co*dcauchy(co,location = x)))
#Output:- 97573.6

x=4; 

10^6*4*max(var(dcauchy(co,location = x)),var(co*dcauchy(co,location = x)))
#Output:- 9309.791

par(mfrow=c(1,1))