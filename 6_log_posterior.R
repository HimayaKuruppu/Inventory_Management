log_posterior <- function(par,X,Y) {

  lp <- X%*%par[1:(num_par)]
  #prob=exp(lp)/(1+exp(lp))
  p<-plogis(lp)
  
  log_like=sum(dbinom(Y,1,p, log = T))
  
log_prior<-dnorm(par[1], 0.9364,sd=0.2052, log = T) + 
  dnorm(par[2], -3.2895,sd=0.1603, log = T)+
  dnorm(par[3], -0.0012,sd=0.0006, log = T)+
  dnorm(par[4], 0.0218,0.0009,log=T)+
  dnorm(par[5], 0.0059,0.0009,log=T) +
 dnorm(par[6], 0.0005,0.0012,log=T)

  names(log_prior) <- NULL
  log_post<- log_like+log_prior
  
  if(is.finite(log_post)==TRUE)
  {
    log_post=log_post
  }
  else
  {
    log_post=0.000000001
  }
  -1*log_post
}
#log_posterior(inits,X,Y)

