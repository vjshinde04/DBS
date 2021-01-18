##################
#example in class notebook
e=5.33
o1=5
o2=8
o3=3
testValue=((o1-e)*(o1-e)+(o2-e)*(o2-e)+(o3-e)*(o3-e))/e
testValue
qchisq(0.99,2)


##################
##Use mtcars dataset: test whether probability of gear class selection 
#is the same at the level at alpha=5%
alpha=0.05
X=mtcars$gear
unique(X)
table(X)
h0=p1=p2=p3=1/3
o=table(X)
e=sum(o)*rep(1/3,3)
testValue=sum(((o-e)^2)/e)
cValue=qchisq(1-alpha,2)


##################
##test whether cyl us independent of gear in the mtcars dataset
#at level alpha=0.01
#H0 : cyl and gear are independent
#H1 : not H0
alpha=0.01
X1=mtcars$cyl
X2=mtcars$gear
o=table(X1,X2)
class(o)
oRow=nrow(o)
oCol=ncol(o)
dim(o)
range(nrow(o))
e=matrix(NA,3,3)
for (i in (1:oRow)){
  for (j in (1:oCol)){#range(ncol(e))
    e[i,j]=(sum(o[i,])*sum(o[,j]))/nrow(mtcars)
  }
}
testValue=sum(((o-e)^2)/e)
df=4
cValue=qchisq(1-0.01,df)
#cValue=qchisq((1-alpha),((nrow(o)-1)*(ncol(o)-1)))
#**Since testValue is greater than cValue, we reject H0
chisq.test(X1,X2)

