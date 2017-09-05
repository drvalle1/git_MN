rm(list=ls(all=TRUE))
set.seed(1)

n=500
x=seq(from=-1,to=1,length.out=n)

true.b1=b1=-2
true.b2=b2=4

med=b1*x+b2*(x^2)
true.z=z=rnorm(n,mean=med,sd=1)
plot(x,med)
plot(x,z)

nclass=20
true.breaks=breaks=sort(rnorm(nclass-1,mean=0,sd=1))

y=rep(NA,n)
cond=z<breaks[1]; y[cond]=1
for (i in 2:(nclass-1)){
  cond=z<breaks[i] & z>breaks[i-1]
  y[cond]=i
}
cond=z>breaks[nclass-1]
y[cond]=nclass

table(y)
plot(x,y)
dat=data.frame(x=x,y=y)

setwd('U:\\modeling abundance\\git_MN')
write.csv(dat,'fake data.csv',row.names=F)