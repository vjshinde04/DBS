library(MASS)
db = Boston
head(db,5)
##################
##UNIFORM MODEL
#model 'indus' using uniform, estimate parameter and predict future value
myData=db$indus
head(myData,5)
minVal=min(myData)
minVal
maxVal=max(myData)
maxVal
#parameter estimation
c(minVal,maxVal)
sim=runif(1000,minVal,maxVal)
pred=mean(sim)
pred
sim2=runif(10000,minVal,maxVal)
pred2=mean(sim2)
pred2
##################
##NORMAL MODEL
#estimate parameters and for a data plot
x=myData
mu=mean(X)
sigma=sd(X)
#paramaeters are
print(mu)
print(sigma)
hist(x)
#chebyshev model
L=mu-3*sigma
U=mu+3*sigma
c(L,U)
xaxis=seq(-18,120,.5)
dens=dnorm(xaxis,mu,sigma)
plot(xaxis,dens,type = 'l',col='red')
#create cdf curve for above normal disrtibution
dens1=pnorm(xaxis,mu,sigma)
plot(xaxis,dens1,type = 'l',col='blue')

#par(mfrow=c(4,2))
#plot(xaxis,dens,type = 'l',col='red')
#plot(xaxis,dens1,type = 'l',col='blue')


plot(xaxis,dens1,type = 'l',col='blue')
lines(xaxis,dens,type = 'l',col='red')

##################
##EXPONENTIAL MODEL
#create multiple exponential curve for lambda=1,3,6
y=seq(0,6,.5)
m1=dexp(y,1)
m2=dexp(y,3)
m3=dexp(y,6)
plot(y,m1, type = 'l', col='red')
lines(y,m2, type = 'l', col='blue')
lines(y,m3, type = 'l', col='greeb')
