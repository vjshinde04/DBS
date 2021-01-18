##################
##multinomial logistic regression where output variable is
##categorical with more than 2 classes
#Consider cyl as the output variable and mpg, wt, hp, drat as input
#apply multinomial logistics regressoin
#evaluate the performance of model in 100mc runs
#plot the line chart for different ratio of trainset
library(nnet)
y=datasets::mtcars$cyl
x1=datasets::mtcars$mpg
x2=datasets::mtcars$wt
x3=datasets::mtcars$hp
x4=datasets::mtcars$drat
df=data.frame(x1,x2,x3,x4,y)
dim(df)
df=na.omit(df)
dim(df)

ratio=c(60,70,80,90)
accuracyVector=c()
for(j in ratio){
  acc=0
  mc=100
  for(i in 1:mc){
    n=nrow(df)
    indexes=sample(n,n*70/100)
    trainset=df[indexes,]
    testset=df[-indexes,]
    dim(trainset)
    dim(testset)
    actual=testset$y
    
    logiModel=multinom(y~.,data=trainset)
    pred=predict(logiModel,testset)
    
    confusionMatrix=table(actual,pred)
    accuracy=mean(actual==pred)
    acc=acc+(1/100)*accuracy
  }
  accuracyVector=c(accuracyVector,acc)
}
accuracyVector

plot(ratio,accuracyVector,type = 'b',col='red')

##################
#Consider gear as the output variable and mpg, wt, hp, drat as input
#apply multinomial logistics regressoin
#evaluate the performance of model in 100mc runs
#plot the line chart for different ratio of trainset
library(nnet)
y=datasets::mtcars$gear
x1=datasets::mtcars$mpg
x2=datasets::mtcars$wt
x3=datasets::mtcars$hp
x4=datasets::mtcars$drat
df=data.frame(x1,x2,x3,x4,y)
dim(df)
df=na.omit(df)
dim(df)

ratio=c(60,70,80,90)
accuracyVector=c()
for(j in ratio){
  acc=0
  mc=1000
  for(i in 1:mc){
    n=nrow(df)
    indexes=sample(n,n*j/100)
    trainset=df[indexes,]
    testset=df[-indexes,]
    dim(trainset)
    dim(testset)
    actual=testset$y
    
    logiModel=multinom(y~.,data=trainset)
    pred=predict(logiModel,testset)
    
    confusionMatrix=table(actual,pred)
    accuracy=mean(actual==pred)
    acc=acc+(1/1000)*accuracy
  }
  accuracyVector=c(accuracyVector,acc)
}
accuracyVector

plot(ratio,accuracyVector,type = 'b',col='red')

##################
##poisson regression
#use dataset_glm, consider sleep timeas the output cariable and weight and
#height as inputs
#apply poisson regression
#evaluate the performance of model in 100mc runs using rmse
#plot the line chart for different ratio of trainset in terms of rmse
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
y=data$SleepTime
x1=data$Weight
x2=data$Height
df=data.frame(x1,x2,y)
dim(df)
df=na.omit(df)
dim(df)

ratioList=c(50,60,70,80,90)
rmseMat=matrix(NA,5,2)
for (j in ratioList){
  monteCarlo=100
  rmseVal=rep(0,2)
  for (i in 1:monteCarlo){
    n=nrow(df)
    indexes=sample(n,n*j/100)
    trainset=df[indexes,]
    testset=df[-indexes,]
    dim(trainset)
    dim(testset)
    actual=testset$y
    
    poissonModel=glm(y~.,data=trainset,family = "poisson")
    pred=predict(poissonModel,testset)
    
    confusionMatrix=table(actual,pred)
    rmse=sqrt((sum(pred-actual)^2)/nrow(testset))
    rmseVal=rmseVal+(rmse/monteCarlo)
  }  
  rmseMat[j,]=rmseVal
}
rmseMat

plot(ratioList,rmseMat,col='red')
  
##################
##run above with linear regression and compare the models
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
y=data$SleepTime
x1=data$Weight
x2=data$Height
df=data.frame(x1,x2,y)
dim(df)
df=na.omit(df)
dim(df)

ratioList=c(50,60,70,80,90)
rmseMat=matrix(NA,5,2)
for (j in ratioList){
  monteCarlo=100
  rmseVal=rep(0,2)
  for (i in 1:monteCarlo){
    n=nrow(df)
    indexes=sample(n,n*j/100)
    trainset=df[indexes,]
    testset=df[-indexes,]
    dim(trainset)
    dim(testset)
    actual=testset$y
    
    poissonModel=glm(y~.,data=trainset,family = "poisson")
    predPoisson=predict(poissonModel,testset)
    
    linearModel=glm(y~.,data=trainset,family = "guassian")
    predLinear=predict(linearModel,testset)
    
    rmsePoisson=sqrt((sum(predPoisson-actual)^2)/nrow(testset))
    rmseLinear=sqrt((sum(predLinear-actual)^2)/nrow(testset))
    rmse=c(rmsePoisson,rmseLinear)
    rmseVal=rmseVal+(rmse/monteCarlo)
  }  
  rmseMat[j,]=rmseVal
}
rmseMat

plot(ratioList,rmseMat,col='red')

