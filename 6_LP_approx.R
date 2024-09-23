LP_approx <- function(X,Y,inits){
  lp.approx <- optim(par=inits, X=X, Y=Y, fn=log_posterior, hessian=TRUE,
                     method="L-BFGS-B",lower=c(1.2367,-4,-0.0012,0.0110,-0.0007,-0.0030),upper=c(2.4232,-3,0.0022,0.0169,0.0009,0.0006)) 
  
  lp.approx
}
#,lower=c(1.2367,-4,-0.0012,0.0110,-0.0007,-0.0030),
#upper=c(2.4232,-3,0.0022,0.0169,0.0009,0.0006) ) 