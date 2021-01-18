##################
##Example 6 (oneNote,GLM)
# Exp: model sprint to writing, reading, math and english.  
#1  split the dataset into 80% as a trainset, and 20% as a testset (use set.seed(5432)) 
#2 compute RMSE for full and reduced model 
#3 which model is a better model? (hint: the model with lower rmse is a better model) 
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
library(MASS)
y=data$Sprint
x1=data$Writing
x2=data$Reading
x3=data$Math
x4=data$English
df=data.frame(x1,x2,x3,x4,y)
dataset=na.omit(df)
length(dataset)
dim(dataset)
##monte carlo is more stable than cross validation
mc=100
rmseVal=rep(0,2)
for(i in 1:mc){
  n=nrow(dataset)
  indexes = sample(n,n*80/100)
  trainset = dataset[indexes,]
  testset = dataset[-indexes,]
  #fit the model using trainset
  fitFull=glm(y~., data=trainset, family='gaussian')
  fitReduced=stepAIC(fitFull)
  predFull=predict(fitFull,trainset)
  predReduced=predict(fitReduced,trainset)
  actualVal=testset$y
  rmseFull=sqrt((sum(predFull-actualVal)^2)/nrow(testset))
  rmseReduced=sqrt((sum(predReduced-actualVal)^2)/nrow(testset))
  rmse=c(rmseFull,rmseReduced)
  rmseVal=rmseVal+(rmse/mc)
} 
rmseVal
#use a for loop to evaluate RMSE for train ration (50,60,70,80,90) in 100 mc runs
ratioArray=c(50,60,70,80,90)
rmseMat=matrix(NA,5,2)
for(j in ratioArray){
  mc=100
  rmseVal=rep(0,2)
  for(i in 1:mc){
    n=nrow(dataset)
    indexes = sample(n,n*j/100)
    trainset = dataset[indexes,]
    head(trainset)
    testset = dataset[-indexes,]
    head(testset)
    #fit the model using trainset
    fitFull=glm(y~., data=trainset, family='gaussian')
    fitReduced=stepAIC(fitFull)
    predFull=predict(fitFull,trainset)
    predReduced=predict(fitReduced,trainset)
    actualVal=testset$y
    rmseFull=sqrt((sum(predFull-actualVal)^2)/nrow(testset))
    rmseReduced=sqrt((sum(predReduced-actualVal)^2)/nrow(testset))
    rmse=c(rmseFull,rmseReduced)
    rmseVal=rmseVal+(rmse/mc)
  }
  rmseMat[j,]=rmseVal
}
cat('Matrix is')
rmseMat

##!!!!!!!!!
plot(ratioArray,rmseMat)


##################
##Example 7 (oneNote,GLM)
##use boston dataset, consider medv as the output variable and all other as input
#apply multiple linear regression for different ratio of trainset (50,60,70,80,90)
#in 10 monte carlo runs.
#compute rmse for full and reduced model
#which model outperforms? Also visualize the result.
library(MASS)
dataset=Boston
head(dataset)
#y=data$medv
#colList=names(data)
#x=subset(data,select = -c(medv))
#head(x)
#dataset=c(x,y)
ratioArray=c(50,60,70,80,90)
rmseMat=matrix(NA,5,2)
for(j in 1:5){
  cat("",j)
}
for(j in ratioArray){
  cat("",j)
}
for(j in ratioArray[1:5]){
  cat("",j)
}
for(j in ratioArray){
  mc=10
  rmseVal=rep(0,2)
  for(i in 1:mc){
    n=nrow(dataset)
    indexes = sample(n,n*j/100)
    trainset = dataset[indexes,]
    head(trainset)
    testset = dataset[-indexes,]
    head(testset)
    #fit the model using trainset
    fitFull=glm(y~., data=trainset, family='gaussian')
    fitReduced=stepAIC(fitFull)
    predFull=predict(fitFull,trainset)
    predReduced=predict(fitReduced,trainset)
    actualVal=testset$y
    rmseFull=sqrt((sum(predFull-actualVal)^2)/nrow(testset))
    rmseReduced=sqrt((sum(predReduced-actualVal)^2)/nrow(testset))
    rmse=c(rmseFull,rmseReduced)
    rmseVal=rmseVal+(rmse/mc)
  }
  rmseMat[j,]=rmseVal
}
cat('Matrix is')
rmseMat


