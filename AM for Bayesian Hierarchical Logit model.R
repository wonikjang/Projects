# Adaptive Metropolis Hasting algorithm
library(MCMCpack)
library(mvtnorm)
library(plyr)
set.seed(450889)

# ablMvncoeff
ablMvnCoeff<-function(bmean,invLambda,invC,eta,nind){
  postPrec<-invC+nind*invLambda
  temp<-invC%*%eta+nind*invLambda%*%bmean
  postmean<-solve(postPrec,temp)
  invpostPrec<-solve(postPrec)
  output<-rmvnorm(1,postmean,invpostPrec)
  return  (output)
}

# ablMvnErrorPrec
ablMvnErrorPrec<-function(bmat,mu,invomega,rou,nind){
  lmat<-matrix(0.0,nind,npar)
  for(i in 1:nind)
    for(j in 1:npar)
     { lmat[i,j]<-(bmat[i,j]-mu[j])}
  ip<-t(lmat)%*%lmat+invomega
  scale<-solve(ip)
  output<-riwish(rou+nind, ip)
  return (output)
}

#Data
nind=400
nobs1=20
nobs=nind*nobs1
mu1<-c(1.1,-0.1,-0.6)
npar<-length(mu1)
indicator<-rep(1:400, rep(20,400))

sigma<-matrix(0.0, npar,npar)
for(i in 1:npar)
  for(j in 1:npar)
  {ifelse(i==j,sigma[i,j]<-1.0,sigma[i,j]<-0.5)}
var=0.1*sigma
theta<-rmvnorm(nind,mu1,var)

#xvec<-matrix(0.0,nobs1,npar)
#for(i in 1:nobs1)
#  for(j in 1:npar)
#  {xvec[i,1]<-1
#   xvec[i,2:3]<-rnorm((npar-1),0.0,1.0)}

xvecre<-matrix(0.0,nobs,npar)
for(i in 1:nobs)
  for(j in 1:npar)
  {xvecre[i,1]<-1
   xvecre[i,2:3]<-rnorm((npar-1),0.0,1.0)}
# xvec cut by 20(obs1) from xvecre 
xtheta<-matrix(0.0,nind,nobs1)
for(i in 1:nind){
  k<-20*i-19
  j<-20*i
  xtheta[i,]<-(xvecre[k:j,])%*%(theta[i,])
}
prob1<-exp(xtheta)/(1+exp(xtheta))
prob<-as.vector(prob1)
y1<-rbinom(length(prob),1,prob)

data<-cbind(indicator,y1,xvecre)
data[7999:8000,]
Y<-data[,2]
X<-data[,3:5]
length(which(y1=="0"))
length(which(y1=="1"))


#1 create function (list of all the parameters)
adaptiveCreate<-function(targetrate,startiter,enditer,nind,npar,tunescalar){
  start<-startiter
  end<-enditer
  target<-targetrate
  
  accept1all<-rep(0.0,nind)
  g<-1.0
  sigmaall<-rep(log(2.38^2/npar),nind)
  muall<-matrix(0.0,nind,npar)

  gamma1<-diag(npar)
  gammaall<-apply(gamma1,2,rep,nind)

  tune1<-tunescalar*diag(npar)
  tuneall<-apply(tune1,2,rep,nind)
 
  list (start=start,end=end,target=target,accept1all=accept1all,g=g,sigmaall=sigmaall,muall=muall,gammaall=gammaall,tuneall=tuneall)
} 

# 1.5 update gamma function
updateGamma<-function(muall,gammaall,bmat,g){
  diff<-bmat-muall
  multiarray = list();
  for(i in 1:nind){
    multiarray[[i]] = diff[i,]%o%diff[i,];
  }
  outp <- matrix(unlist(multiarray), ncol = npar, byrow = TRUE)
  return(outp)
}

#2 update function
adaptiveUpdate<-function(atchadePanel,bmat,iternum,acceptall){
  nstart<-atchadePanel$start
  nend<-atchadePanel$end
  taubar<-atchadePanel$target
  
  accept1all<-atchadePanel$accept1all
  g<-atchadePanel$g
  lsigmaall<-atchadePanel$sigmaall
  muall<-atchadePanel$muall
  gammaall<-atchadePanel$gammaall
  tuneall<-atchadePanel$tuneall
  
  g=10.0/iternum
  
  if(iternum>=nstart && iternum<=nend){
    lsimgaall<-lsigmaall+g*(acceptall-taubar)
    gammaall<-gammaall+g*(updateGamma(muall,gammaall,bmat,g)-gammaall)
    muall=muall+g*(bmat-muall)
    tuneall=exp(lsigmaall)*gammaall    
  }
  
  list (start=nstart,end=nend,target=taubar,acceptall=accept1all,g=g,sigmaall=lsigmaall,muall=muall,gammaall=gammaall,tuneall=tuneall)
}

# Prior for Normal
eta<-rep(0.0, npar)
invC<-matrix(0.0,npar,npar)
for(i in 1:npar)
  for(j in 1:npar)
  {ifelse(i==j,invC[i,j]<-0.001,invC[i,j]<-0.0)}

# Prior for Wishart
rou<-npar+2
invomega<-(rou-npar-1)*diag(npar)
omega<-solve(invomega)

# sufficient statistics->p pstart
m2<-as.data.frame(table(data[,1]))
m3<-as.numeric(as.matrix(m2,ncol=2))
pstart<-matrix(m3,ncol = 2)
pstart[1,1]<-1
for(i in 1:(nind-1))
  {pstart[i+1,1]<-pstart[i,1]+pstart[i,2]}
colnames(pstart)<-c("ind","nrows")

# Likelihood
plike<-function(y,x,beta){
  xbeta<-x%*%beta
  logden<-log(1+exp(xbeta))
  result<-sum(y*xbeta-logden)
  return (result)
}

# 3 mhbeta function
mhbeta<- function(yperson,xperson,oldbeta,eta,invC,tune,ite,prevlike){     
  if(ite==1){
    oldlike<-plike(yperson,xperson,oldbeta)
  }else{
    oldlike<-prevlike
  } 
  cand1<-rmvnorm(n=1,oldbeta,tune)
  cand2<-t(cand1)
  candlike<-plike(yperson,xperson,cand1[1,])
  
  candprior<- -0.5*( (cand1-eta) %*% invC %*% t(cand1-eta) )
  
  oldprior<- -0.5*( (t(oldbeta)-eta) %*% invC %*% t(t(oldbeta)-eta) )

  logratio<-candlike+candprior-(oldlike+oldprior)
  
  u<-runif(1,0,1)

  output<-rep(0.0,npar+2)
  
  if (logratio >0.0){ 
    output[1:3]=cand1
    output[4]=candlike
    output[5]=1.0 
  }else if (logratio > log(u)){ 
    output[1:3]=cand1
    output[4]=candlike
    output[5]=exp(logratio)
  }else{
    output[1:3]=oldbeta
    output[4]=oldlike
    output[5]=exp(logratio)
    }
  return (output)
}

# 4 cpmat fucntion 
cpmat<-function(yall,xall,bmat,popmean,popprec,tuneall,iter,prevlikeall,pstart){
  xperson<-matrix(0.0, nobs1,npar)
  tune<-matrix(0.0,npar,npar)
  result<-matrix(0.0,nind,npar+2)

  for(i in 1:nind){
    first<-pstart[i,1]
    nrows<-pstart[i,2]
    last<-first+nrows-1
    
    u<-3*(i-1)+1
    v<-3*i
    tune<-tuneall[u:v,]

    xperson<-xall[first:last,]
    yperson<-yall[first:last]
    
    oldbeta<-bmat[i,]
    oldlike<-prevlikeall[i]
    
    result[i,]<-mhbeta(yperson,xperson,oldbeta,popmean,popprec,tune,iter,oldlike)
  } 
  
  return (result)
}

# MCMC
nind=400
npar=3
# starting value
invLambda<-diag(npar)
like<-rep(0.0,nind)
niter<-5000
mu<-c(0,0,-0.5)
bmat<-matrix(0.0, nind,npar)
acceptall<-rep(0.1,nind)

atchadePanel<-adaptiveCreate(0.234,1000,5000,nind,npar,0.001)

# Draws
draws<-matrix(0.0,niter,npar)
m<-matrix(0.0,nind,npar+2)

for(i in 1:niter){
  tuneall = atchadePanel$tuneall
  m <- cpmat(Y,X,bmat,mu,invLambda,tuneall,i,like,pstart)
  bmat<-m[,1:npar]
  like<-m[,npar+1]
  accpetall<-m[,npar+2]
  
  atchadePanel<-adaptiveUpdate(atchadePanel,bmat,i,acceptall)
  bmean<-colMeans(bmat)

  mu<-ablMvnCoeff(bmean,invLambda,invC,eta,nind)
  invLambda<-ablMvnErrorPrec(bmat,mu,invomega,rou,nind)
  
  draws[i,]<-mu
}

burn=niter/2
parms1<-draws[burn:niter,1]
parms2<-draws[burn:niter,2]
parms3<-draws[burn:niter,3]

colMeans(draws)

plot(parms1,type='l')
plot(parms2,type='l')
plot(parms3,type='l')

