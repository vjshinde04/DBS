##################
#example in class notebook
testValue=(82.6-84)/sqrt((((1.41)^2)/35)+(((3.63)^2)/32))
cValue=qnorm(1-(0.05/2))


##################
#example in class notebook
p1=50/93
p2=63/207
pStar=113/300
testValue1=(p1-p2)/sqrt(pStar*(1-pStar)*((1/93)+(1/207)))
cValue=-qnorm(1-0.01)


##################
#Fetch one year data from yahoo finance. Test whether the mean of close price is
#greater than open price on daily scale at alpha=0.05. Use ETH-EUR ticker.
#H0:mu1>mu2 (closePrice>openPrice)
#H1:Reject H0
alpha=0.05
library(pdfetch)
startDate=Sys.Date()-365
endDate=Sys.Date()
fData <- pdfetch_YAHOO(identifiers = "ETH-EUR", from = startDate, to=endDate, interval = '1d')
head(fData)
x1=fData$`ETH-EUR.close`
x2=fData$`ETH-EUR.open`
xBar1=mean(x1)
xBar2=mean(x2)
dim(fData)
n=length(x1)
s1=var(x1)
s2=var(x2)
testValue2=(xBar1-xBar2)/sqrt((s1)/n+(s2)/n)
cValue=qnorm(1-alpha)
#Since |testValue| < cValue, we accept H0.
#Concluding that ETH-EUR didn't have significant increments.