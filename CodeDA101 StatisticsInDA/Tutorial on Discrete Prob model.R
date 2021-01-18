####################################################
Example 3 in R 
j=3
n=7
k=700
N=1000
prob=dhyper(j, k, N-k, n, log = FALSE)
############################################

Example 4
lambda=2
#(i)
#P(X=0)
prob=dpois(0,lambda)
exp(-2)
#(ii)
#P(X=2)
prob=dpois(2,lambda)
 #(iii)
#P(X<=3)
prob=ppois(3,lambda)
 ################################
Example 5 in R 
j=2
n=5
k=10
N=20
prob=dhyper(j, k, N-k, n, log = FALSE)


















Example 8: Five users are active in a network. 
Each user is independently connected to the
 network with probability .8. we test whether 
each user is in the network or not.  
What is the probability of having the 
first connection in the third test? 
j=3
p=.8
p=dgeom(j,p)


The network is likely to be failed 
if at least 4 users are in. 
what is the probability of the failure of network? 

#P(X>=4)=1-p(X<=3)
n=5
j=3
p=0.8
prob=1-pbinom(j,n,p)
dbinom(4,n,p)+dbinom(5,n,p)

 

Let us assume that three users are internal.
 What is the probability of having 2 internal
 users in 3 sample test? 
j=2
n=3
k=3
N=5
dhyper(j, k, N-k, n, log = FALSE)

j=0:min(k,n)
prob=dhyper(j, k, N-k, n, log = FALSE)
probtable=data.frame(j,prob ) # probability table for hypergeom

View(probtable)









