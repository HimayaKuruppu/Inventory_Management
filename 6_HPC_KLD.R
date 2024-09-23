library("Rcpp")
library("rngWELL")
library("randtoolbox")
library("acebayes")
library("matrixcalc")#is.singular
library("lhs")
library("Matrix")
library("lhs")
library("acebayes")
library("iterators")
library("foreach")
library("doParallel")
library("ggplot2")

source('6_log_posterior.R')
source('6_LP_approx.R')
source('6_kld_post.R')
source('6_trace.R')


leny <-6 # number of data points
N2 <- 20; Q<-20   ####ACE parameters
B=100



mu_prior <- c(0.9364,-3.2895,-0.0012,0.0218,0.0059,0.0005)
Sigma_prior <- diag(c(0.205^2,0.160^2,0.0006^2,0.0009^2,0.0009^2,0.0012^2))
iSigma_prior <- solve(Sigma_prior)
dSigma_prior <- det(Sigma_prior)
num_par <- length(mu_prior)

r_rand1<- matrix(rnorm(leny*B,0,1),leny,B)

theta_sim <- matrix(0,nrow=B,ncol=length(mu_prior))
Rnc <- halton(B, dim = num_par, normal=TRUE)
T.mat <- chol(Sigma_prior)

for (j in 1:B){
  theta_sim[j,] <- t(Rnc[j,]%*%T.mat) + mu_prior
}
kld_ud_val<-matrix(0,B)

kld.utility<-function(d,B){
  X<-cbind(1,data.matrix(d))
  
  for (j in 1:B){
    theta = theta_sim[,(1:(num_par))][j,]
    lp = X%*%theta
    
    #prob=exp(lp)/(1+exp(lp))
    P<-plogis(lp)
    Y <-rbinom(leny,1,P)
    Y
    inits=theta_sim[j,]
    kld_ud_val[j]=kld.post(X,Y,inits,iSigma_prior,dSigma_prior,mu_prior)
  }
  print(mean(kld_ud_val))
  mean(kld_ud_val)
}




  d   ### Use initial design selection code and generate random initial design 1
  design1<-ace(utility=kld.utility, start.d=d, deterministic =TRUE,N1=0,N2=20, Q=20, B=c(100,100))
  d   ### Use initial design selection code and generate random initial design 2
  design2<-ace(utility=kld.utility, start.d=d, deterministic =TRUE,N1=0,N2=20, Q=20, B=c(100,100))
  d   ### Use initial design selection code and generate random initial design 3
  design3<-ace(utility=kld.utility, start.d=d, deterministic =TRUE,N1=0,N2=20, Q=20, B=c(100,100))

F1<-design1$phase2.d ##Optimal design 1
F2<-design2$phase2.d ##Optimal design 2
F3<-design3$phase2.d ##Optimal design 3

##Generate utility values for optimal designs
FM1<-numeric(100)
for(i in 1:100){
  FM1[i]<-kld.utility(F1,100)
}
FM2<-numeric(100)
for(i in 1:100){
  FM2[i]<-kld.utility(F2,100)
}
FM3<-numeric(100)
for(i in 1:100){
  FM3[i]<-kld.utility(F3,100)
}

boxplot(list(FM1,FM2,FM3),xlab="Designs",ylab="Utility Values")
mean(FM1)
mean(FM2)
mean(FM3)
sd(FM1)
sd(FM2)
sd(FM3)

####Then Similarly, systematically drop one variable at a time and evaluate the utility for finding the optimal design
boxplot(list(FM2,MT3,RP3,IC2,UP3,NP3),xlab="Designs",ylab="Utility Values")

Data<-data.frame(
  category=rep(c("Full model","Without marketing type","Without register price","Without item count","Without user price","Without net price"),each=100),
  value=c(FM2,MT3,RP3,IC2,UP3,NP3)
)


ggplot(Data, aes(x = as.factor(category), y = value)) +
  geom_boxplot(fill = "slateblue", alpha = 0.9, width = 0.01) +  
  labs(y = "Utility values", x = "Designs") +  
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_jitter(color = "black", size = 0.5, width = 0.1)+
scale_y_continuous(limits = c(21.5, 31.5), expand = c(0, 0))










