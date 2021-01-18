##################
##Example 3 (oneNote,GLM)
#using glm_dataset, model weight to height, sleeptime and sprint
#1 estimate the parameters
#2 select the significant variables
#3 predictive model
#4 compute the coefficient for reduced model
#5 predict the last roaw in the dataset using full and reduced model
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
y=data$Weight
x1=data$Height
x2=data$SleepTime
x3=data$Sprint
df=data.frame(x1,x2,x3,y)
dataset=na.omit(df)
#full model
fitFull=glm(y~., data=dataset, family = 'gaussian')
summary(fitFull)
betaFull=coef(fitFull)
library(MASS)
fitReduced=stepAIC(fitFull)
##4
betaReduced=coef(fitReduced)
##5
#predictive model for full model
xIn=dataset[nrow(dataset),-4]
pred=predict(fitFull,xIn)
#predictive model for reduced model
predReduced=predict(fitReduced,xIn)


##################
##Example 4 (oneNote,GLM)
#Model sprint to writing, reading, math and english.
#predict sprint for the first 5 rows in the dataset
#using full and reduced model and create a dataframe
#using predictions and actual values.
y=data$Sprint
x1=data$Writing
x2=data$Reading
x3=data$Math
x4=data$English
df=data.frame(x1,x2,x3,x4,y)
dataset=na.omit(df)
fitFull=glm(y~., data=dataset, family = 'gaussian')
fitReduced=stepAIC(fitFull)
betaFull=coef(fitFull)
betaReduced=coef((fitReduced))
#dataset[0:5,-5]
#dataset[1:5,-5]
xIn=dataset[0:5,1:4]
predFull=predict(fitFull,xIn)
predReduced=predict(fitReduced,xIn)
#dataset[0:5,5]
actualValues=dataset$y[0:5]
predDF=data.frame(actualValues,predFull,predReduced)
View(predDF)


##################
##Example 5 (oneNote,GLM)
#Model sprint to writing, reading, math and english
#1 split the dataset into 80:20 for train and test set(use set.seed(5432))
#2 compute RMSE for full and reduced model
#3 which model is a better model?
#(hint: the model with lower rmse is the better model)
y=data$Sprint
x1=data$Writing
x2=data$Reading
x3=data$Math
x4=data$English
df=data.frame(x1,x2,x3,x4,y)
dataset=na.omit(df)
#
set.seed(5432)
nrow(dataset)
sampleSize=floor(0.8*nrow(dataset))
trainSet=dataset[0:sampleSize,]
nrow(trainSet)
testSet=dataset[-nrow(trainSet):-1,]
nrow(testSet)
#
set.seed(5432)
n=nrow(dataset)
indexes=sample(n,n*80/100)
trainSet1=dataset[indexes,]
testSet1=dataset[-indexes,]
#
fitFull=glm(y~., data=trainSet1, family = 'gaussian')
fitReduced=stepAIC(fitFull)
testSet1[0:5,]
testSet1[0:5,-5]
testSet1[0:5,5]
#testSet1=testSet1[,-5]
predFull=predict(fitFull,testSet1)
predReduced=predict(fitReduced,testSet1)

actualVal=testSet1$y
rmseFull=sqrt((sum(predFull-actualVal)^2)/nrow(testSet1))
rmseReduced=sqrt((sum(predReduced-actualVal)^2)/nrow(testSet1))
c(rmseFull,rmseReduced)
