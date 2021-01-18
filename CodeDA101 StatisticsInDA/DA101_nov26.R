##################
#credit card data kaggle
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
db = read.csv("creditcard.csv")
alpha=0.01
n=nrow(db)
set.seed(1245)
indexes=sample(n,n*(0.7))
db1 = db[indexes,]
db2 = db[-indexes,]
x1=db1$Class
x2=db2$Class
n1=nrow(db1)
n2=nrow(db2)
p1=sum(x1)/n1
p2=sum(x2)/n2
pStar=(sum(x1)+sum(x2))/n1+n2
testValue=p1-p1/sqrt(pStar*(1-pStar)*(1/n1+1/n2))
cValue=qnorm(1-alpha)


##################
#credit card data kaggle 90:10


##################
#Fetch one month data from yahoo finance. Test whether the variance of close price
#equals to variance of open price on daily scale at alpha=0.05. Use ETH-EUR ticker.
#H0 : var(open price) = var(close price)
#H1 : not H0
library(pdfetch)
alpha=0.05
startDate=Sys.Date()-30
stopDate=Sys.Date()
freqDate='1d'
dbYahoo <- pdfetch_YAHOO(identifiers = "ETH-EUR", from = startDate, to=stopDate, 
                         interval = freqDate)
head(dbYahoo)
tail(dbYahoo)
x1=dbYahoo$`ETH-EUR.open`
x2=dbYahoo$`ETH-EUR.close`
n1=nrow(x1)
n2=nrow(x2)
sigma1=var(x1)
sigma2=var(x2)
testValue=sigma1/sigma2
cValue1=qf(alpha/2,n1-1,n2-1)
cValue2=qf((1-alpha/2),n1-1,n2-1)
#Since testValue lies between cVAlue1 and cValue2. We accept H0.