##################
#OneNote>HT>Group B>Page13
data=rnorm(100,3,10)
xbar=mean(data)
xbar
xbar=mean(data[71:100])
sigma=10
mu0=2
n=30
testValue=(xbar-mu0)/(sigma/sqrt(n))
testValue
alpha=0.01
cValue=qnorm(0.995)

##################
#OneNote>HT>Group B>Page14
#2 tailed test
alpha=0.05
s=120
n=30
xbar=9900
testValue=(xbar-10000)/(120/sqrt(30))
testValue
cValue=qnorm(0.95)
-cValue
cValue1=qnorm(0.05)
cValue1
#Since testValue is < cValue
#we accept HZero and rejecting HOne

