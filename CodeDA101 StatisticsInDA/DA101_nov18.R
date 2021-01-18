##################
#OneNote>HT>Group B>Page13
qnorm(0.95)
qnorm(0.05)

##################
#example in class notebook
nu=(20-1)*(1.5)
nu
de=(1.56)
de
print(19*1.5/1.56)
testValue=nu/de
testValue
cValue=qchisq(0.05,19)
cValue

##################
#use iris data set, test whether std. dev. of sepal.length 
#is greater than 0.8 at the level alpha=0.01
X=iris$Sepal.Length
head(X)
##It's a one-tailed test
##H0=std. dev. > 0.8
##H1=std. dev. <= 0.8
sd=0.8
varianceValue=var(X)
varianceValue
len=length(X)
testValue=((len-1)*varianceValue)/(0.8)*(0.8)
testValue
cValue=qchisq(0.01,(len-1))
cValue
#since testValue is coming out to be lesser than cValue, we'll have
#to reject the hypothesis:Reject H0
