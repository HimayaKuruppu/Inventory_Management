##load required libraries##

library("rstanarm")
library("rstan")
library("ggplot2")
library("bayesplot")
library("acebayes")

##loading data##

Historycal_sales<-read.csv("/kaggle/input/sales-analysis/SalesKaggle3.csv")

##preparing data for analysis##
#This data set included both historical and active data. In our case we need only historical data.

Historycal_sales<-Historycal_sales[,1:10000]
y<-Historycal_sales[,4]       # y ~ Sales
x1<-Historycal_sales[,6]      # x1~ Marketing type 
x4<-Historycal_sales[,10]     # x4~ Register price 
x6<-Historycal_sales[,12]     # x6~ Inventory level 
x7<-Historycal_sales[,13]     # x7~ Low user price 
x8<-Historycal_sales[,14]     # x8~ Low net price 

##Convert categarical data numerical data
x11 <- as.integer( factor ( Historycal_sales[,6]))

##Descriptive statistics 
summary(Historycal_sales)
table(x1)
prop.table(table(x1))
barplot(table(x1),xlab = "Marketing type",ylab = "Frequncy")

########### Bayesian approach #########

t_prior<-student_t(df=6,location = 0,scale=2.5)
t_prior
Bayesian_app<-stan_glm(y~x11+x4+x6+x7+x8,data=Historycal_sales,family=binomial(link="logit"),
                       prior=t_prior,prior_intercept = t_prior,QR=TRUE)
summary(Bayesian_app)
View(Bayesian_app)

pp_check(Bayesian_app)
fit<-summary(Bayesian_app,probs=c(0,0.5,1))
View(round(fit,4))

####### Logistic regression formula ########

logistic_reg<-glm(y~x1+x4+x6+x7+x8,data=Historycal_sales,family="binomial")
logistic_reg
summary(logistic_reg)
View(logistic_reg)

#There are two methods to find R^2, 
#cox and snell R^2 and McFadden R^2,

######## McFadden R^2 is used for this case #######

ll.null<-logistic_reg$null.deviance/-2
ll.null
ll.proposed<-logistic_reg$deviance/-2
ll.proposed
R2<-(ll.null-ll.proposed)/ll.null
R2
