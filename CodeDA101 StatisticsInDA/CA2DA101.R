##################
#Q1
##################
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
db = read.csv('rwm5yr.csv')
##SOURCE : https://vincentarelbundock.github.io/Rdatasets/doc/COUNT/rwm5yr.html
##DISCRIPTION : German health registry for the years 1984-1988. 
##Health information for years immediately prior to health reform. 

#list of all the attributes
names(db)

#summary of all attributes
summary(db)

#top 3 rows of dataset
head(db,3)

#looking for missing values
colSums(is.na(db))

#Condisdering the doctor visits as output variable
y=db$docvis

##################
##Q1(A)
mc=100
rmseVal=rep(0,2)
for(i in 1:mc){
  n=nrow(db)
  index=sample(n,n*80/100)
  trainSet=db[index,]
  testSet=db[-index,]
  
  fitFull=glm(y~., data=trainSet, family = 'gaussian')
  fitReduced=stepAIC(fitFull)
  predFull=predict(fitFull,testSet)
  predReduced=predict(fitReduced,testSet)
  
  actualVal=testSet$y
  rmseFull=sqrt((sum(predFull-actualVal)^2)/nrow(testSet))
  rmseReduced=sqrt((sum(predReduced-actualVal)^2)/nrow(testSet))
  rmse=c(rmseFull,rmseReduced)
  rmseVal=rmseVal+(rmse/mc)
}
rmseVal
#the model with lower rmse is the better model, hence reduced model is better

##################
##Q1(B)
#since y(db$docvis) is count data, we need to apply poisson regression
poissonReg = glm(db$docvis~., data = db, family = 'gaussian')
summary(poissonReg)

##################
##Q1(D)
ratioList=c(50,60,70,80,90)
rmseMat=matrix(NA,5,2)
for (j in ratioList){
  monteCarlo=100
  rmseVal=rep(0,2)
  for (i in 1:monteCarlo){
    n=nrow(db)
    indexes=sample(n,n*j/100)
    trainset=db[indexes,]
    testset=db[-indexes,]
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