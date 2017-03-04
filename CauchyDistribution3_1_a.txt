######################Exercise 3.1#######################

#a.Plot the integrands, and use Monte Carlo integration based on a Cauchy
#  simulation to calculate the integrals.
f1=function(t){ t/(1+t*t)*exp(-(x-t)^2/2)}
f2=function(t){ 1/(1+t*t)*exp(-(x-t)^2/2)}
plot(f1,-4,4,col=1,ylim=c(-0.5,1),xlab="t",ylab="",ty="l")
plot(f2,-4,4,add=TRUE,col=2,ty="l")
legend("topright", c("f1=t.f2","f2"), lty=1,col=1 :2)

#cauchy simulation
Niter=10^4
co=rcauchy(Niter)
I=mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))

x=0
mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))
#Output:- -0.00134292


x=2
mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))
#Output:- 1.288597

x=4
mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))
#Output:- 3.424328