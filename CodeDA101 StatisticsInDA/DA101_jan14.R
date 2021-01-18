##################
## no of correctness
#install.packages("e1071")
library(e1071)  #similiar to scikit library of python
data(Titanic)
titanicDataset=as.data.frame(Titanic)
head(titanicDataset)

m <- naiveBayes(Survived ~ ., data = titanicDataset)
dim(titanicDataset)
titanicDataset[32,-4]
input=titanicDataset[32,-4]
pred=predict(m,input)

##split the dataset into 70/30 and find the accuracy accordingly in 100 mc runs
acc = 0 ; mc = 100
for (i in 1:mc)
  {
  n = nrow(titanicDataset)
  index = sample(n,n*70/100)
  trainSet = titanicDataset[index,]
  testSet = titanicDataset[index,]
  m <- naiveBayes(Survived ~ ., data = trainSet)
  pred=predict(m,testSet$Survived)
  tab=table(pred,testSet$Survived)  #confusion matrix
  accuracy = mean(pred==testSet$Survived)
  acc=acc+(1/mc)*accuracy
  }
acc


##################
##using iris data set, perform:
#apply naive bayes' classifier where Species is output variable.
#compute confusion matrix and accuracy for 90/10 data split in 1000 mc runs
acc=0 ; mc=1000
dataset=iris
nrow(dataset)
for (i in 1:mc){
  n = nrow(dataset)
  index = sample(n,n*90/100)
  trainset = dataset[index,]
  testset = dataset[-index,]
  m <- naiveBayes(Species~., data = trainset)
  pred=predict(m,testset)
  tab=table(pred,testset$Species)
  accuracy=mean(pred==testset$Species)
  acc=acc+(1/mc)*accuracy
}
acc

##redo the above example using multinomial logistic regression and compare the 
#result versus using naive bayes'
