multinom.gibbs=function(dat,ngibbs,covs,burnin,prior.var){
  nobs=nrow(dat)
  
  tmp=sort(unique(dat$y))
  nclass=length(tmp)
  class1=1:nclass
  uni=data.frame(y=tmp,y1=class1)
  
  dat1=merge(dat,uni,all=T); dim(dat); dim(dat1)
  dat2=dat1[,c('y1',covs)]
  
  #get initial values
  z=(dat2$y1-mean(dat2$y1))/sd(dat2$y1)
  b=tmp[1:(nclass-1)]+0.1
  b=(b-mean(dat2$y1))/sd(dat2$y1)
  xmat=data.matrix(dat2[,covs])
  npar=ncol(xmat)
  betas=rep(0,npar)
  xtx=t(xmat)%*%xmat
  prec=xtx+diag(1/prior.var,npar)
  var1=solve(prec)
  
  #gibbs sampler
  store.b=matrix(NA,ngibbs,nclass-1)
  store.betas=matrix(NA,ngibbs,npar)
  options(warn=2)
  for (i in 1:ngibbs){
    print(i)
    z=sample.z(y=dat2$y1,xmat=xmat,betas=betas,b=b,class1=class1,nobs=nobs,nclass=nclass)
    betas=sample.betas(z=z,xmat=xmat,var1=var1)
    b=sample.b(z=z,y=dat2$y1,nclass=nclass,class1=class1)
    
    #store results
    store.b[i,]=b
    store.betas[i,]=betas
  }
  
  after.burn=burnin:ngibbs
  list(b=store.b[after.burn,],
       betas=store.betas[after.burn,])
}

