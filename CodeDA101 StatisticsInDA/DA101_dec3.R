##################
#Use 'faithful' dataset, model eruptions as output
#1. estimate the coefficients
#2. test whether waiting is significant variable on output variable
#3. compute the predictive
#4. predict output variable if waiting is 61
head(faithful)
Y=faithful$eruptions #output
X=faithful$waiting #input
linearReg = glm(Y~X, data = faithful, family = 'gaussian')
summary(linearReg)

#Write a code for prediction of output for the last row in dataset
min(faithful$waiting)
max(faithful$waiting)
beta=coef(linearReg)
xLast=tail(X,1)
xTest=c(1,xLast)
pred=sum(xTest*beta)
pred
tail(faithful,1)



##################
#Use 'dataset_GLM' dataset model, height as output variable
#to weight, math, english & reading as input variable
#1. estimate the coefficients
#2. select the significant variables
#3. compute the predictive model
getwd()
setwd("D:/code/DBSCode/CodeDA101 StatisticsInDA")
data=read.csv('dataset_GLM.csv')
y=data$Height
x1=data$Weight
x2=data$Math
x3=data$English
x4=data$Reading
df=data.frame(x1,x2,x3,x4,y)
colSums(is.na(df))
cleanDf = na.omit(df)
colSums(is.na(cleanDf))
head(df)
#since y is continous, we need to apply linear regression
linReg = glm(y~. , data = cleanDf, family = 'gaussian')
summary(linReg)
