#exp model Q1
#10000miles
lambda = 1/10000
m1=1-pexp(5000,lambda)
m1
##################
##fetch data from yahoo finance into R and propose the prediction one step 
#ahead (one day) of close price ETH-USD using normal model
installed.packages('pdfetch')
library(pdfetch)
startDate = Sys.Date() - 365
endDate = Sys.Date()
fData <- pdfetch_YAHOO(identifiers = "AAPL",from = startDate, to=endDate, interval = '1d')
head(fData)
#Normal model
#myData = fData$
#Exponential model
myData = fData$
lambda = 1/mean(X)
sim=rexp(10000,lambda)
pred=mean(sim)

