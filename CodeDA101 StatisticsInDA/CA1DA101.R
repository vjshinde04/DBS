##################
#Q1
##################
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
db = read.csv('Video_Games_Sales.csv')

#list of all the attributes
names(db)

#summary of all attributes
summary(db)

#structure of data
str(db)

#top 3 rows of dataset
head(db,3)

#looking for missing values
colSums(is.na(db))

##################
##Q1(A)
str(db)
#Following are qualitative data
unique(db$Genre)
unique(db$Platform)
length(unique(db$Genre))
length(unique(db$Platform))

#Qualitative data can be visualized in following methods
#1. bar graph
#2. pie chart
#3. pareto diagram

#Frequency table for categorical variables
table(db$Genre)
table(db$Platform)

#Pie chart
#Categorical/Qualitative attribute : Genre
dataPie=table(db$Genre)
labelValue=unique(db$Genre)
perc=round(dataPie/sum(dataPie)*100,1)
labelPie=paste(labelValue,"(",perc,"%)")   #attach the the label and percentage 
pie(dataPie,labels=labelPie,main="Distribution of Genre",col=rainbow(7))

#Bar plot
#Categorical/Qualitative attribute : Genre
dataBar=table(db$Genre)
nameGenre = unique(db$Genre)
par(mar=c(4,10,4,4))
barplot(dataBar, main="Count in each Genre", names=nameGenre, horiz = T , las=1,
        xlab="Count", col = "blue")

#Histogram
#Continuous/Quantitative attribute : Critic_Score
dataHist = na.omit(db$Critic_Score)
length(db$Critic_Score)
length(dataHist)
hist(dataHist,main = "Critic score",xlab = "Rating")

#Scatter plot
#Continuous/Qualitative attribue : 
plot(db$Year_of_Release, db$Other_Sales, main="Scatter plot for Sales in other region", 
     xlab = "Release Year", ylab = "")



##################
##Q1(B)
str(db)
#consider continuous attribute "NA_Sales"

##Central Measures : Mean, Median, Mode

#Mean
xBar = mean(db$NA_Sales)
cat('Mean for attribute "NA_Sales" is:',xBar)
#Median
med = median(db$NA_Sales)
cat('Median for attribute "NA_Sales" is:',med)
#Mode
#names(table(db$NA_Sales))[table(db$NA_Sales)==max(table(db$NA_Sales))]
uniqueX = unique(db$NA_Sales)
tableX = table(match(db$NA_Sales,uniqueX))
mod = uniqueX[tableX == max(tableX)]
cat('Mode for attribute "NA_Sales" is:',mod)

##Variational Measures : Range, Variance, Standard Deviation
#Range
cat('Range for attribute "NA_Sales" is',range(db$NA_Sales))
#Variance
cat('Variance for attribute "NA_Sales" is',var(db$NA_Sales))
#Standard Deviation
cat('Standard Deviation for attribute "NA_Sales" is',sd(db$NA_Sales))


##################
##Q1(C)
str(db)
#consider continuous attribute "db$EU_Sales"
#one-sigma interval estimation
xBarQC = mean(db$EU_Sales)
xBarQC
sdQC = sd(db$EU_Sales)
sdQC
LQC=xBarQC-sdQC
UQC=xBarQC+sdQC
c(LQC,UQC)

FuncOutliers <- function(L,U,X){
  print(c(L,U))
  outliersIndexList<-c()
  outliersList<-c()
  for (i in 1:length(X)){
    if(X[i]> U | X[i] < L){
      outliersIndexList <- c(outliersIndexList,i)
      outliersList <- c(outliersList,X[i])
    }
  }
  #print(outliersIndexList)
  #print(outliersList)
  cat('Total no. of outliers:',length(outliersIndexList))
}

FuncOutliers(LQC,UQC,db$EU_Sales)

#2 sigma interval
LQC2=xBarQC-2*sdQC
UQC2=xBarQC+2*sdQC
c(LQC2,UQC2)
FuncOutliers(LQC2,UQC2,db$EU_Sales)

#3 sigma interval
LQC3=xBarQC-3*sdQC
UQC3=xBarQC+3*sdQC
c(LQC3,UQC3)
FuncOutliers(LQC3,UQC3,db$EU_Sales)


##################
##Q1(D)
str(db)
#consider the above continuous attribute "EU_Sales"

#box plot method
Q=quantile(db$EU_Sales)
Q
cat(Q[2],Q[4])
QL=Q[2]
QU=Q[4]
IQR=QU-QL
LQD=QL-1.5*IQR
UQD=QU+1.5*IQR
cat(LQD,UQD)
FuncOutliers(LQD,UQD,db$EU_Sales)





##################
#Q2
##################
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
db = read.csv('Video_Games_Sales.csv')

##################
##Q2(A)
#Bernoulli's Model
#getwd() returns the path of current working directory
getwd()

#reading the CSV file
DA=read.csv()

#Getting the top 5 rows of data
head(DA,5)

#Getting the table headings
names(DA)

#Returning the data from OT table and assigning it to tab 
tab=table(DA$OT)
tab
#Parameter Estimation
#For Bernoulli Model n=1
#finding Probability for OT table
prob=tab/sum(tab)
prob

#Finding simulation with rbinom(SampleSize,1,prob) where n=1
sim=rbinom(1000,1,prob)
sim
#Predction for a attribute
pred=table(sim)
pred



##################
##Q2
#getting the NA_sales variable for binomial model assigning it to V
V=DA$EU_Sales
n=round(max(V))
n

#Finding the probabilty
prob=mean(V)/n
prob
#parameter estimation
c(n,prob)
#prediction
sim=rbinom(100,n,prob)
sim
pred=round(mean(sim))
c("predicted variable is:",pred)





##################
#Q3
##################
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
db3=read.csv('Video_Games_Sales.csv')
##################
##Q3(A)
alpha=0.01
str(db3)
#Over here we consider
#H0:the 2 variables are independent
#H1:Reject H0
unique(db3$Platform)
unique(db3$Genre)

dbTable=table(db3$Platform,db3$Genre)
dbTable

library("gplots")
balloonplot(dbTable,label=FALSE,show.margins = FALSE)
#for simplicity we narrow down onto our data for a better insight.
#Considering just a few companies:famous publisher that still exist in the 
#market, for example, Microsoft and Sony
#[330] "Microsoft Game Studios" 
#[467] "Sony Computer Entertainment"     
#[468] "Sony Computer Entertainment America"   
#[469] "Sony Computer Entertainment Europe"    
#[470] "Sony Music Entertainment"              
#[471] "Sony Online Entertainment"
#c("Microsoft Game Studios","Sony Computer Entertainment","Sony Computer Entertainment Europe","Sony Music Entertainment","Sony Online Entertainment")
unique(sort(db3$Publisher))
#dbexample=filter(db3[:,:], db3$Publisher %in% c("Microsoft Game Studios","Sony Computer Entertainment","Sony Computer Entertainment Europe","Sony Music Entertainment","Sony Online Entertainment"))
#dbexample=db3[grep("Microsoft-",db3$Publisher)]
db3a=db3[which(db3$Publisher %in% c("Microsoft Game Studios","Sony Computer Entertainment","Sony Computer Entertainment Europe","Sony Music Entertainment","Sony Online Entertainment")),]
str(db3a)
dim(db3a)
unique(db3a$Publisher)
unique(db3a$Platform)

library(xlsx)
write.csv(x = dbexample, file = "D:/code/DBSCode/CodeDA101 StatisticsInDA/db3asubset.csv")

table(db3a$Genre,db3a$Platform)

db3aTable1=table(db3a$Platform,db3a$Genre)
balloonplot(db3aTable1,xlab="",ylab="",label=FALSE,show.margins = FALSE)


obs=table(db3a$Platform,db3a$Genre)
obs
obsRow = nrow(obs)
obsCol = ncol(obs)
nLen = nrow(db3a)
expt=matrix(NA,obsRow,obsCol)
for (i in (1:obsRow)){
  for (j in (1:obsCol)){
    expt[i,j]=(sum(obs[i,])*sum(obs[,j]))/nLen
  }
}
testValue3a=sum(((obs-expt)^2)/expt)
testValue3a
conf=1-alpha
df=(obsRow-1)*(obsCol-1)
cValue3a=qchisq(conf,df)
cValue3a
#Since testValue > cValue, H0 is accepted.
#Concluding that the 2 variables are indepenent.

#summary(table(db3a$Platform,db3a$Genre))
#chisq.test(db3a$Platform,db3a$Genre)

#just for the sake of clarity we take 2 variables which we know are 
#actually related : Platform and Publisher
db3aTable2=table(db3a$Platform,db3a$Publisher)
balloonplot(db3aTable2,label=FALSE,show.margins = FALSE)


obs2=table(db3a$Platform,db3a$Publisher)
obsRow2 = nrow(obs2)
obsCol2 = ncol(obs2)
nLen2 = nrow(db3a)
expt2=matrix(NA,obsRow2,obsCol2)
for (i in (1:obsRow2)){
  for (j in (1:obsCol2)){
    expt2[i,j]=(sum(obs2[i,])*sum(obs2[,j]))/nLen2
  }
}
testValue3a2=sum(((obs2-expt2)^2)/expt2)
testValue3a2
conf=1-alpha
df2=(obsRow2-1)*(obsCol2-1)
cValue3a2=qchisq(conf,df2)
cValue3a2
#summary(table(db3a$Platform,db3a$Publisher))
#chisq.test(db3aTable2)



##################
##Q3(B)
#Goodness of fit
str(db3)
dim(db3)
#considering "Genre"(categorical variable)
xb=db3$Genre
which(is.na(xb))
n=length(unique(xb))
#There are 12 classes so we expect probability of each class to be 1/12
#H0 : p1=1/12,p2=1/12,p3=1/12,p4=1/12,p5=1/12,p6=1/12,p7=1/12,p8=1/12,
#p9=1/12,p10=1/12,p11=1/12,p12=1/12
#H1 : Not H0
alpha=0.05
obs=table(xb)
obs
expt=sum(obs)*rep(1/12,12)
expt
testValue=sum(((obs-expt)^2)/expt)
testValue
cValue=qchisq(1-alpha,n-1)
cValue
#Since test value is greater than cValue, H0 is rejected.
#The fitness of "Genre" is not appropriate.
#This implies that categories in the "Genre" variable is not evenly spread.


##################
##Q3(C)
#test of mean
str(db3)
#Considering "Global_Sales"(continuous variable)
X=db3$Global_Sales
xBar=mean(X)
#H0 : average sales is above 0.5; mu > 0.5
#H1 : H0 is false; ; mu <= 0.5
alpha=0.05
n=length(X)
n
sd1=sd(X)
sd1
var1=var(X)
var1
testValue=(xBar-0.5)/(sd(X)/sqrt(n))
testValue
cValue=qnorm(1-alpha)
cValue
#Since cValue is less than testValue, H0 is rejected.