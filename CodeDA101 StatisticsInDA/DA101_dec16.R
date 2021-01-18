##################
##logistic regression
#a) use glm_dataset, model athelete as output to height and weight as the input
#b) split the dataset in 70:30
#c) fit logistic regression using 70% of the dataset
#d) predict the actual value in the testset, create a dataframe based on actual and predictions
#e) evaluate the performance of prediction using confusion matrix and accuracy
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
#data=read.csv(file.choose())
y=data$Athlete
x1=data$Height
x2=data$Weight
df=data.frame(x1,x2,y)
dim(df)
#sum(na)
df=na.omit(df)
dim(df)
acc=0
for(i in 1:100){
  n=nrow(df)
  indexes=sample(n,n*70/100)
  trainset=df[indexes,]
  testset=df[-indexes,]
  dim(trainset)
  dim(testset)
  
  logisticReg = glm(as.factor(y)~., data=trainset, family = 'binomial') #poisson reg
  summary(logisticReg)
  #pred=predict(fit,testset)
  pred=predict(logisticReg,testset, type='response')
  l=length(pred)
  
  yHat=rep(0,1)
  yHat[pred>0.5]=1
  actual=testset$y
  
  confusionMatrix=table(actual,yHat)
  accuracy=mean(actual==yHat)
  acc=acc+(1/100)*accuracy
}
acc
#evaluate accuracy in 100 monte carlo runs for different ratio of trainset, ie.,
#(60,70,80,90). Visualize using line chart
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
#data=read.csv(file.choose())
y=data$Athlete
x1=data$Height
x2=data$Weight
df=data.frame(x1,x2,y)
dim(df)
#sum(na)
df=na.omit(df)
dim(df)
accVector=c()
mcList=c(60,70,80,90)
for (j in mcList){
  mc=100
  for(i in 1:100){
    n=nrow(df)
    indexes=sample(n,n*70/100)
    trainset=df[indexes,]
    testset=df[-indexes,]
    dim(trainset)
    dim(testset)
    
    logisticReg = glm(as.factor(y)~., data=trainset, family = 'binomial') #poisson reg
    summary(logisticReg)
    #pred=predict(fit,testset)
    pred=predict(logisticReg,testset, type='response')
    l=length(pred)
    
    yHat=rep(0,1)
    yHat[pred>0.5]=1
    actual=testset$y
    
    confusionMatrix=table(actual,yHat)
    accuracy=mean(actual==yHat)
    acc=acc+(1/100)*accuracy
  }
  accVector=c(accVector,acc)
}
accVector
#visualization
plot(ratio,accVector, type='b', col='red')