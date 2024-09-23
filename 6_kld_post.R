kld.post <- function(X,Y,inits,iSigma_prior,dSigma_prior,mu_prior){
  Sol<-LP_approx(X,Y,inits)
  mu_post <- Sol$par
  if(is.non.singular.matrix(Sol$hessian)==TRUE){
    Sigma_post<-solve(Sol$hessian)
    iSp <- iSigma_prior
    mp <- mu_prior
    num_par<-length(mp)
    dSigma_post<-det(Sigma_post)
    #if(is.na(dSigma_post) || dSigma_post <= 0){
     # dSigma_post<-0.00000001
    #}
a<-base::log((dSigma_prior)/(dSigma_post))
    
  det.out <- 0.5*(trace(iSp%*%Sigma_post) + t(mp-mu_post)%*%iSp%*%(mp-mu_post)-num_par +a)
  }
  
  if(is.finite(det.out)==TRUE)
  {
    det.out=det.out
  }
  else
  {
    det.out=0.00000001
  }
  det.out
}
