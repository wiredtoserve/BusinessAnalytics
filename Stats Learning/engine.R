# Creating a file for useful functions
setwd("~/R Projects")

# Loading RData file
load("week2.RData")

clc <- function(en, cons){
  if (en == 'yes'){
    rm(list=ls())
  }
}

# to check the size
data(NPreg)
nrow(NPreg)

par(mfrow=c(1,1))

# Week 1 ------------------------------------------------------------------

#######################################################################
## Week 1 - Multiple Linear Regression
library(MASS)
#Reading in the salary data
data = read.csv("salary.csv", header = TRUE, sep=",")

fit0<-lm(salary~Expr,data=data)
#Scatter plot of Salary (y) vs Expr (x) 
plot(salary~Expr,xlab="Experience",ylab="Salary $000",data=data)
#plot the best fit line on existing plot produced above
abline(fit0)
#look at the summary of the linear model fit
summary(fit0)

fit1<-lm(salary~age+Expr+education,data=data)
#look at the summary of the linear model fit
summary(fit1)

#Defining a function for the wald test - require MASS library
#H0: RB=c vs Ha: RB!=c
#R=the R matrix
#B=the estimated coefficients
#S=the covariance matrix of B
#c=the vector of constants

wald<-function(R,B,S,c){
  stats<-matrix(0,1,2)
  dif=(R%*%B-c)
  VV=R%*%(S)%*%t(R)
  W=t(dif)%*%ginv(VV)%*%dif
  stats[1]=W
  stats[2]=pchisq(W,nrow(c),lower.tail=FALSE)
  colnames(stats)<-c("Wald stat","p-value")
  return(stats)
}

RR=cbind(rbind(0,0,0),diag(3))
cc=rbind(0,0,0)
bhat=(fit1$coefficients)
Shat=vcov(fit1)
wald1=wald(RR,bhat,Shat,cc)
wald1

# To check if Age and Expr have equal impact
# i.e. B1 = B2
# Rtest = cbind(0,1,-1,0)
# ctest = rbind(0)
# wald2=wald(Rtest,bhat,Shat,ctest)


# Week 2 ------------------------------------------------------------------

#######################################################################
## Week 2 - MLE and Logistic Regression
library(stats4) #required for MLE

#Defining the log likelihood function 
LL_lpm<-function(beta0,beta1,sig){
  #a vector of log likelihood for each observation
  R = suppressWarnings(dnorm(wells$switch,beta0+beta1 * wells$dist,sig,log=TRUE))
  -sum(R) #passing out the negative of the log likelihood
}

mle_lpm=mle(minuslogl = LL_lpm, start = list(beta0=0.5, beta1=0, sig=0.5), method = "BFGS")
summary(mle_lpm)

#LOGISTIC MODEL

#Fitting the logistic model using MLE (slide 27)
LL_logistic<-function(beta0,beta1){
  xb=beta0+beta1*wells$dist
  lpy=wells$switch*log(exp(xb)/(1+exp(xb)))+(1-wells$switch)*log(1/(1+exp(xb)))
  -sum(lpy)
}
mle_logistic=mle(minuslogl = LL_logistic, start = list(beta0=0, beta1=0), method = "BFGS")
summary(mle_logistic)

#Fitting the logistic model using glm() (slide 28)
fit1=glm(switch~dist,binomial(link="logit"),data=wells)
summary(fit1)

# function outputs the implication of the probabilities
# z = beta0 + beta1 * predictor + error
# Eg - compute_prob(fit1, 500)
# Probability the person will swich at value x
compute_prob <- function(fit, predictor_val){
  fit1_p2=exp(fit$coefficients[1]+fit$coefficients[2]*predictor_val)/(1+exp(fit$coefficients[1]+fit$coefficients[2]*predictor_val))
  return(fit1_p2)
}

# function outputs the Odds, post fit analysis
compute_odds <- function(fit, predictor_val){
  odds=exp(fit$coefficients[1]+fit$coefficients[2]*predictor_val)
  ln_odds = (fit$coefficients[1]+fit$coefficients[2]*predictor_val)
  not_ln_odds = 1/odds
  
  rowlist <- c('Odds', 'Log Odds', 'Inverse Odds')
  colist <- c(odds, ln_odds, not_ln_odds)
  df = data.frame(rowlist, colist)
  return(df)
}

# find the point at which the switch happens, i.e. ln Odds = 0
tipping_point = - fit1$coefficients[1]/fit1$coefficients[2]

#Odds amongst households in the data set
odds_HH=exp(fit1$linear.predictors)
par(mfrow=c(1,2))
boxplot(odds_HH,main="Odds ratio to switch",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
plot(wells$dist,odds_HH,main="Odds ratio to switch vs distance",xlab="distance",ylab="Odds ratio")
abline(h=1,col="red",lty=2)

# function outputs the Margincal Effects, post fit analysis
# Eg - compute_ME(fit1, 100, -30)
compute_ME <- function(fit, predictor_val, delta_val=0){
  exb=exp(fit$coefficients[1]+fit$coefficients[2]*predictor_val)
  ME=fit1$coefficients[2]*exb/((1+exb)^2)
  
  exb1=exp(fit$coefficients[1]+fit$coefficients[2]*(predictor_val + delta_val))
  Deltap=(exb1/(1+exb1))-(exb/(1+exb))
  
  rowlist <- c('Marginal Effect', 'Discrete Change')
  colist <- c(ME, Deltap)
  df = data.frame(rowlist, colist)
  return(df)
}

#Logistic Model with Multiple predictors

fit2=glm(switch~dist+arsenic+assoc+educ,binomial(link="logit"),data=wells)
summary(fit2)

#computing marginal effect at dist=100, and remaining X variables at median
# compute_multi_predictors(fit2, 100, 3, 5, 6, wells) where 3, 5 and 6 are column indexes
compute_multi_predictors <- function(fit, predictor_val, val1, val2, val3, data){
  exb2=exp(fit$coefficients[1]+fit2$coefficients[2]*predictor_val
           +fit$coefficients[3]*median(data[[val1]])
           +fit$coefficients[4]*median(data[[val2]])
           +fit$coefficients[5]*median(data[[val3]]))
  ME2=fit2$coefficients[2]*exb2/((1+exb2)^2)
  
  rowlist <- c('Odds Ratio', 'Marginal Effect')
  colist <- c(exb2, ME2)
  df = data.frame(rowlist, colist)
  
  return(df)
}

#Odds amongst households in the data set
odds_HH2=exp(fit2$linear.predictor)
par(mfrow=c(1,2))
boxplot(odds_HH2,main="Odds ratio to switch",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
plot(wells$dist,odds_HH2,main="Odds ratio to switch vs distance",xlab="distance",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
par(mfrow=c(1,2))
plot(wells$arsenic,odds_HH2,main="Odds ratio to switch vs arsenic",xlab="arsenic",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
plot(wells$educ,odds_HH2,main="Odds ratio to switch vs education",xlab="education",ylab="Odds ratio")
abline(h=1,col="red",lty=2)

# Week 3 ------------------------------------------------------------------

#######################################################################
## Week 3 - Diagnostic Tools for Categorical Outcomes
library(boot)

#Loading in the environment from last week
load("week2.RData")

#Likelihood Ratio test
# LR = Dev_R - Dev_UR
LR1=fit1$null.deviance-fit1$deviance 
LR1_pval=1-pchisq(LR1,fit1$df.null-fit1$df.residual)
# p-value ~ 0, strong evidence to show that the single predictor model is statistically significant
# Reject H_0

# General model, (fit2, multiple predictors)
# Restricted model, B2 = B3 = B4 = 0 (fit1, just one predictor)
LR2 = fit1$deviance - fit2$deviance
LR2_pval = 1 - pchisq(LR2, fit1$df.residual - fit2$df.residual)

#Hit and miss table

HitMiss<-function(y,prob,c){
  HM=matrix(0,2,2)
  HM[1,1]=mean(prob>c & y==1)
  HM[1,2]=mean(prob>c & y==0)
  HM[2,1]=mean(prob<=c & y==1)
  HM[2,2]=mean(prob<=c & y==0)
  return(HM)
}

#compute the fitted probabilities: pi_hat in the lecture notes
probs1=fit1$fitted.values
probs2=fit2$fitted.values

HM1=HitMiss(wells$switch,probs1,0.5)
HM2=HitMiss(wells$switch,probs2,0.5)

# Null Model 
# or use mean(wells$switch) 
probs0 <- rep(1,length(probs1))
HM0=HitMiss(wells$switch,probs0,0.5)

# Eg - HitMiss_Analysis(HM1)
HitMiss_Analysis <- function(HM){
  rowlist <- c('Accuracy Rate', 'Error Rate', 'Precision Rate', 'Recall Rate')
  accuracy = sum(diag(HM))
  error = 1 - accuracy
  precision = HM[1]/(HM[1]+HM[3])
  recall = HM[1]/(HM[1]+HM[2])
  colist <- c(accuracy, error, precision, recall)
  
  df = data.frame(rowlist, colist)
  return(df)
}

# APPLICATION: an extensive investigation of the wells example

fit3=glm(switch~dist+arsenic+assoc+educ+I(arsenic^2)+dist:arsenic+dist:educ+educ:arsenic,binomial(link="logit"),data=wells)
fit6=glm(switch~dist+arsenic+assoc+I(arsenic^2)+dist:educ,binomial(link="logit"),data=wells)
summary(fit6)
#LR test of fit6 vs fit 3
LR6=fit6$deviance-fit3$deviance
pval6=1-pchisq(LR6,fit6$df.residual-fit3$df.residual)

#Probit class model
fit3p=glm(switch~dist+arsenic+assoc+educ+I(arsenic^2)+dist:arsenic+dist:educ+educ:arsenic,binomial(link="probit"),data=wells)
fit6p=glm(switch~dist+arsenic+assoc+I(arsenic^2)+dist:educ,binomial(link="probit"),data=wells)
summary(fit6p)
#LR test of fit6p vs fit 3p
LR6p=fit6p$deviance-fit3p$deviance
pval6p=1-pchisq(LR6p,fit6p$df.residual-fit3p$df.residual)

#Hit and miss table for each model
HM6=HitMiss(wells$switch,fit6$fitted.values,0.5)
HM6p=HitMiss(wells$switch,fit6p$fitted.values,0.5)

#Marginal effect of assoc
# assoc is a dummy variable
p_assoc1=exp(fit6$linear.predictors-fit6$coefficients[4]*wells$assoc+fit6$coefficients[4])/(1+exp(fit6$linear.predictors-fit6$coefficients[4]*wells$assoc+fit6$coefficients[4]))
p_assoc0=exp(fit6$linear.predictors-fit6$coefficients[4]*wells$assoc)/(1+exp(fit6$linear.predictors-fit6$coefficients[4]*wells$assoc))
ME_assoc=p_assoc1-p_assoc0
mean(ME_assoc)
mean(wells$assoc)

#Marginal effect of arsenic
# ME_arsenic = B2 + 2*B5*arsenic exp(XB)/ (1 + exp(XB))^2
ME_arsenic=(fit6$coefficients[3]+2*fit6$coefficients[5]*wells$arsenic)*fit6$fitted.values/(1+exp(fit6$linear.predictors))
plot(wells$arsenic,ME_arsenic,main="ME arsenic vs arsenic level", xlab="arsenic",ylab="Marginal effect")
abline(h=mean(ME_arsenic),col="red",lty=2)

#Marginal effect of distance
# ME_distance = B1 + B7*educ exp(XB)/ (1 + exp(XB))^2
ME_dist=(fit6$coefficients[2]+fit6$coefficients[6]*wells$educ)*fit6$fitted.values/(1+exp(fit6$linear.predictors))
par(mfrow=c(1,2))
plot(wells$educ,ME_dist,main="ME of distance vs education level", xlab="Education",ylab="Marginal effect")
abline(h=mean(ME_dist),col="red",lty=2)

#Marginal effect of education
# ME_educ = B7*dist exp(XB)/ (1 + exp(XB))^2
ME_educ=(fit6$coefficients[6]*wells$dist)*fit6$fitted.values/(1+exp(fit6$linear.predictors))
plot(wells$dist,ME_educ,main="ME of education vs distance", xlab="distance",ylab="Marginal effect")
abline(h=mean(ME_educ),col="red",lty=2)

#Odds ratio
odds_HH6=exp(fit6$linear.predictor)
par(mfrow=c(1,2))
boxplot(odds_HH6,main="Odds ratio to switch",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
plot(wells$dist,odds_HH6,main="Odds ratio to switch vs distance",xlab="distance",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
par(mfrow=c(1,2))
plot(wells$arsenic,odds_HH6,main="Odds ratio to switch vs arsenic",xlab="arsenic",ylab="Odds ratio")
abline(h=1,col="red",lty=2)
plot(wells$educ,odds_HH6,main="Odds ratio to switch vs education",xlab="education",ylab="Odds ratio")
abline(h=1,col="red",lty=2)



# Week 4 ------------------------------------------------------------------

#######################################################################
## Week 4 - Modelling Ordinal and Multinomial Outcomes
# Cleaning the variable space
rm(list=ls())

# liraries required
library(MASS)
library(foreign)
library(mlogit)

#Load data on propensity survey 
college=read.dta(file="apply.dta")

# find the number of responses
sum(college$apply == 'unlikely')
sum(college$apply == 'somewhat likely')
sum(college$apply == 'very likely')
# to see how many people responded positively to the superior choice
sum(college$apply == 'very likely')/nrow(college)


#The ordered logit model
fit_olog1=polr(apply~pared+public+gpa,data=college,method="logistic")
summary(fit_olog1)

#A function to produce p-values from a polr fit
polr_pval<-function(fit){
  fit.c=coef(summary(fit))
  fit.c=cbind(fit.c,"p-val"=pnorm(abs(fit.c[,"t value"]),lower.tail=FALSE)*2)
  return(fit.c)
}

#computing the p-values of each coefficient
fit_olog1.c=polr_pval(fit_olog1)
summary(fit_olog1)
fit_olog1.c

# Nested Model
#The ordered logit model - with nonlinear predictors
fit_olog2=polr(apply~pared+public+gpa+I(gpa^2)+I(pared*gpa)+I(public*gpa),data=college,method="logistic")
summary(fit_olog2)
#computing the p-values of each coefficient
fit_olog2.c=polr_pval(fit_olog2)
fit_olog2.c

# After the model fit
#The ordered logit model - with nonlinear predictors
fit_olog6=polr(apply~gpa+I(pared*gpa),data=college,method="logistic")
summary(fit_olog6)
#computing the p-values of each coefficient
fit_olog6.c=polr_pval(fit_olog6)
fit_olog6.c

# convert the fit into a dataframe
df_ordered_model = data.frame(fit_olog6.c) # fit_olog6$coefficients
tau_1 = df_ordered_model$Value[3] # fit_olog6$zeta[1]
tau_2 = df_ordered_model$Value[4] # fit_olog6$zeta[2]

# Odds ration between the choices
odds21=fit_olog6$fitted.values[,2]/fit_olog6$fitted.values[,1]
odds32=fit_olog6$fitted.values[,3]/fit_olog6$fitted.values[,2]
par(mfrow=c(1,2))
boxplot(odds21,main="Odds ratio - somewhat likely/unlikely")
abline(h=1.0,col="red")
boxplot(odds32,main="Odds ratio - very likely/somewhat likely",ylim=c(min(0.5,min(odds32)),max(1.5,max(odds32))))
abline(h=1.0,col="red")

# check for value = 1, above or below
median(odds21)
median(odds21)

# Looking at the utility
par(mfrow=c(1,1))
plot(college$gpa,fit_olog6$lp,main="Utility vs GPA",ylab="Utility (zi)",xlab="GPA")
abline(h=tau_1,col="red")
abline(h=tau_2,col="red")

# function to check the effect of probability for a particular predictor with respect to the other one
# run: effect_prob_change(fit_olog6, college$gpa, college)
effect_prob_change <- function(fit, data_col, data){
  lp_pared0=fit$coefficients[1]*data_col
  lp_pared1=(fit$coefficients[1]+fit$coefficients[2])*data_col
  pr_pared0=cbind(plogis(fit$zeta[1]-lp_pared0),plogis(fit$zeta[2]-lp_pared0)-plogis(fit$zeta[1]-lp_pared0),1-plogis(fit$zeta[2]-lp_pared0))
  pr_pared1=cbind(plogis(fit$zeta[1]-lp_pared1),plogis(fit$zeta[2]-lp_pared1)-plogis(fit$zeta[1]-lp_pared1),1-plogis(fit$zeta[2]-lp_pared1))
  
  matplot(data_col,pr_pared1-pr_pared0,main="Effect of predictor", ylab="Change in Probability",xlab=colnames(data)[4])
}


#Function for computing the hit-miss table in ordinal data
#Note that the input y must be recoded to ranked numeric
#predicted y is the category with highest probability

HitMissMult<-function(y,prob){
  m=ncol(prob)
  ypred=as.numeric(max.col(prob))
  if(nrow(prob)==1) #the case of unconditional prob
  {
    ypred=rep(ypred,length(y))
  }
  HM=matrix(0,m,m) #table is now m by m
  for(i in 1:m){
    for(j in 1:m){
      HM[i,j]=mean(ypred==i & y==j)
    }
  }
  return(HM)
}
y_app=1*(college$apply=="unlikely")+2*(college$apply=="somewhat likely")+3*(college$apply=="very likely")
HM_olog6=HitMissMult(y_app,fit_olog6$fitted.values)

#HM with no models
unc_pr=cbind(mean(y_app==1),mean(y_app==2),mean(y_app==3))
HM_unc=HitMissMult(y_app,unc_pr)


# LR Test
# For 5%, Pr(>Chisq) < 0.05
lrtest(fit_olog2, fit_olog6)


## Week 4 - Modelling Unordered outcomes

Yogurt=read.csv("Yoghurt.csv")

# Prices in the dataset
# Relative Frequency
sum(Yogurt$choice == 'dannon')/nrow(Yogurt)
sum(Yogurt$choice == 'weight')/nrow(Yogurt)
sum(Yogurt$choice == 'yoplait')/nrow(Yogurt)
sum(Yogurt$choice == 'hiland')/nrow(Yogurt)


#the mlogit requres the data to be formed in a certain way...
yog=mlogit.data(Yogurt,choice="choice",sep="",shape="wide")
fit_m1=mlogit(choice~0|price.yoplait+price.dannon+price.weight+price.hiland+feat.yoplait+feat.dannon+feat.weight+feat.hiland,data=yog)
summary(fit_m1)
AIC(fit_m1)

#Model excluding feat.dannon and feat.yoplait
fit_m2=mlogit(choice~0|price.yoplait+price.dannon+price.weight+price.hiland+feat.weight+feat.hiland,data=yog)
summary(fit_m2)
AIC(fit_m2)

# Fail to reject the null hypothesis (p-value = 0.333), restrictions are valid
lrtest(fit_m1,fit_m2)

# ylim=c(min(0.5,min(odds32)),max(1.5,max(odds32)))
#Odds analysis
prob_m2=fit_m2$probabilities
odds_hd=prob_m2[,2]/prob_m2[,1]
odds_wd=prob_m2[,3]/prob_m2[,1]
odds_yd=prob_m2[,4]/prob_m2[,1]
par(mfrow=c(1,3))
boxplot(odds_hd, main="Odds Hiland vs Dannon", ylim=c(min(0.5,min(odds_hd)),10))
abline(h=1.0,col="red")
boxplot(odds_wd, main="Odds WeightWatchers vs Dannon", ylim=c(min(0.5,min(odds_wd)),max(1.5,max(odds_wd))))
abline(h=1.0,col="red")
boxplot(odds_yd, main="Odds Yoplait vs Dannon", ylim=c(min(0.5,min(odds_yd)),10))
abline(h=1.0,col="red")

# Sensitivity analysis
# For 4 groups
# function(fit_m2, 'hiland', 'weight', 'yoplait', 'dannon', 'price.hiland', 'price.weight', 'price.yoplait', 'price.dannon')
sensitivity_analysis_mult <- function(fit, group1, group2, group3, ref, char1, char2, char3, ref_char4){

  prob_m2=fit$probabilities
  odds1=prob_m2[,2]/prob_m2[,1]
  odds2=prob_m2[,3]/prob_m2[,1]
  odds3=prob_m2[,4]/prob_m2[,1]
  
  
  rowlist <- c('-',ref_char4, char1, char2, char3)
  
  col11 = median(odds1*fit$coefficients[paste(group1,ref_char4,sep=':')])
  col12 = median(odds2*fit$coefficients[paste(group2,ref_char4,sep=':')])
  col13 = median(odds3*fit$coefficients[paste(group3,ref_char4,sep=':')])
  
  col21 = median(odds1*fit$coefficients[paste(group1,char1,sep=':')])
  col22 = median(odds2*fit$coefficients[paste(group2,char1,sep=':')])
  col23 = median(odds3*fit$coefficients[paste(group3,char1,sep=':')])
  
  col31 = median(odds1*fit$coefficients[paste(group1,char2,sep=':')])
  col32 = median(odds2*fit$coefficients[paste(group2,char2,sep=':')])
  col33 = median(odds3*fit$coefficients[paste(group3,char2,sep=':')])
  
  col41 = median(odds1*fit$coefficients[paste(group1,char3,sep=':')])
  col42 = median(odds2*fit$coefficients[paste(group2,char3,sep=':')])
  col43 = median(odds3*fit$coefficients[paste(group3,char3,sep=':')])
  

  odds_sensitivity1 <- c(paste(group1, ref, sep = '/'), round(col11, digits=4), round(col21, digits=4), round(col31, digits=4), round(col41, digits=4))
  odds_sensitivity2 <- c(paste(group2, ref, sep = '/'), round(col12, digits=4), round(col22, digits=4), round(col32, digits=4), round(col42, digits=4))
  odds_sensitivity3 <- c(paste(group3, ref, sep = '/'), round(col13, digits=4), round(col23, digits=4), round(col33, digits=4), round(col43, digits=4))
  
  df = data.frame(rowlist, odds_sensitivity1, odds_sensitivity2, odds_sensitivity3)
  
  return(df)
  
}



# Week 5 ------------------------------------------------------------------

#######################################################################
## Week 5 - Poisson Regression 
rm(list=ls())
# Log - Linear relationship in the poisson regression

#loading in the data
lung=read.csv("lung.csv")
#plot a histogram of the observations
# since you have only count data in Poisson Regression
hist(lung$y,15, main="Histogram - Number of Deaths from Lung Cancer",xlab="# deaths")

#Fitting the Poisson regression
# Poisson Regression to explain the number of deaths by 'years.smok' and 'cigarettes'
preg=glm(y~years.smok+cigarettes,family=poisson(link="log"),data=lung)
summary(preg)
# levels(lung$years.smok) to find the reference group 

# A function that computes the overdispersion and its p-value 
# inputs: y = observed data; fit=glm fit of poisson regression
overdis<-function(y,fit){
  pred=fit$fitted.values
  zi=(y-pred)/sqrt(pred)
  odis=sum(zi^2)/fit$df.residual
  odis_pval=1-pchisq(sum(zi^2),fit$df.residual) #p-value for the test
  return(list(odis=odis,pval=odis_pval))
}

# test
underdis<-function(y,fit){
  pred=fit$fitted.values
  zi=(y-pred)/sqrt(pred)
  undis=sum(zi^2)/fit$df.residual
  undis_pval=pchisq(sum(zi^2),fit$df.residual) #p-value for the test
  return(list(odis=odis,pval=odis_pval))
}

# to find the overdispersion parameter
overdis(lung$y,preg)
# w = 2.23 and p-value ~ 0.00
# evidence for overdispersion, reject the null hypothesis

#Quasi-Poisson model
preg_q=glm(y~years.smok+cigarettes,family=quasipoisson(link="log"),data=lung)
summary(preg_q) #The only difference is in the standard errors

# ln(Time_i) can be used as the offset variable as Time is the exposure variable
#Poisson regression accounting for exposure
preg_ex=glm(y~years.smok+cigarettes,offset=log(Time),family=poisson(link="log"),data=lung)
summary(preg_ex)
overdis(lung$y,preg_ex) #Testing for overdispersion

preg_ex_q=glm(y~years.smok+cigarettes,offset=log(Time),family=quasipoisson(link="log"),data=lung)
summary(preg_ex_q)

preg_ex_q$coefficients # comparing the coefficients

#relative magnitude of the fitted lambda_i
boxplot(preg_ex_q$fitted.values)

# Model comparison via AIC
AIC(preg) # Poisson
AIC(preg_ex) # Poisson with exposure

## Week 5 - Tobit Model
#Redefine the data into rate
lung1=lung
lung1$y=(lung$y/lung$Time)*100
hist(lung1$y,15,main="Rate of death by lung cancer per 100 @risk candidates",
     xlab="Death rate per 100 @risk candidates")

#Using VGAM package
library(VGAM)
tobit1 <- vglm(y ~ years.smok+cigarettes, tobit(Lower = 0,Upper=100), data = lung1)
summary(tobit1) #Note: (Intercept):2 in the vglm output is the log of sigma
xb=fitted(tobit1) # Xi Beta
sig=exp(coefficients(tobit1)["(Intercept):2"]) # sigma

#NOTE: residual is computed based on X*Beta
#plot(lung1$y,resid(tobit1,type="response"))

#This is a case of censoring: y=0 is meaningful as it represents no death
#Conditional expectation needs to be formed accordingly
Ey=xb*pnorm(xb/sig)+sig*dnorm(xb/sig) # Slide 28
plot(xb,Ey)
plot(xb,lung1$y,main="Predicted vs Actual", xlab="X*Beta",ylab="Actual Y in dot, E(Y|X) in line")
lines(lowess(xb,Ey),col="red")

#Poisson prediction - conversion to expected duration
#Assume the death counts are recorded over a yearly interval

# Note: use poission fit with exposrure accounted for
# initializing the dataframe group1, group2 and exposure variable
x.1<-data.frame(years.smok="30-34",cigarettes="1-9",Time=100)
# Eg1 - expected_rates(preg_ex_q, tobit1, x.1, "30-34", "1-9", 100)
# Eg2 - expected_rates(preg_ex_q, tobit1, x.1, "55-59", "1-9", 100)

expected_rates <- function(poission_fit, tobit_fit, x.1, value1, value2, exposure_rate){
  
  x.1[[1]] = value1
  x.1[[2]] = value2
  x.1[[3]] = exposure_rate
  
  # expectation using poisson
  lambda1=exp(predict(poission_fit,x.1))
  # predict function would give the log of lambda, that's why we exponentiate it
  # for time
  ET1=1/lambda1
  
  # expectation using tobit
  xb1=predict(tobit_fit,x.1) # xb1[1] is mew, xb1[2] is sd
  Ey1=xb1[1]*pnorm(xb1[1]/exp(xb1[2]))+exp(xb1[2])*dnorm(xb1[1]/exp(xb1[2])) # Slide 28
  
  rowlist <- c('Expected Rate', 'Expected Time')
  poisson_col <- c(lambda1, ET1)
  tobit_col <- c(round(Ey1, digits=4), '-')
  
  df <- data.frame(rowlist, poisson_col, tobit_col)
  return(df)
}

# Week 6 ------------------------------------------------------------------

#######################################################################
## Week 6 - Accounting for Heterogeneity

library(lme4)
library(ggplot2)

salary=read.csv("salary.csv")
sqrt(var(salary$salary)) # total variation

#the linear model
fit_s0=lm(salary~Expr+I(Expr^2)+age+I(age^2)+I(Gender=="F")
          +I(education==14)+I(education==15)
          +I(education==16)+I(education==17),data=salary)
summary(fit_s0)

#linear model with random intercept
# group predictor is education
fit_s1=lmer(salary~Expr+I(Expr^2)+age+I(age^2)+I(Gender=="F")
            +(1|education),data=salary)

summary(fit_s1) # Intercept is the u_alpha 
coef(fit_s1) # what are the intercepts for each group
fixef(fit_s1) # u_alpha and beta's (marginal effects)
ranef(fit_s1) # eta_j values

AIC(fit_s0) # Linear AIC
AIC(fit_s1) # Multilevel AIC

#Multilevel model

#just with varying intercept by county
fit1=lmer(log.radon~floor+(1|county.name),data=radonAZ)

summary(fit1)
AIC(fit1)
coef(fit1)
fixef(fit1)
ranef(fit1)

# dataframe of 15 counties, estimated coefficient of the counties 
fit1_alpha<-data.frame(x=1:15,est=coef(fit1)$county.name[,1],
                       U=rep(fixef(fit1)[1],15)+qnorm(0.975)*0.1901, # std dev of county.name
                       L=rep(fixef(fit1)[1],15)+qnorm(0.025)*0.1901,
                       fixed=rep(fixef(fit1)[1],15))

#Plotting the effect using ggplot2
ggplot(fit1_alpha, aes(x = x, y = est)) +
  geom_point(size = 4) +
  geom_hline(aes(yintercept=fixed),colour="red")+
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Baseline variation across group") +
  xlab("County") +
  ylab("Intercept")

#varying intercept with group-specific predictors
fit2=lmer(log.radon~floor+u.full+(1|county.name),data=radonAZ)

summary(fit2)
AIC(fit2)
coef(fit2)
fixef(fit2) # gamma_0, beta, gamma_1
ranef(fit2) # alpha: varies with uranium level

fit2_a=coef(fit2)$county.name[,1]+ coef(fit2)$county.name[,3]*unique(radonAZ$u.full)
fit2_af=fixef(fit2)[1]+ coef(fit2)$county.name[,3]*unique(radonAZ$u.full)
fit2_alpha<-data.frame(x=1:15,est=fit2_a,
                       U=fit2_af+qnorm(0.975)*0.1918,
                       L=fit2_af+qnorm(0.025)*0.1918,
                       fixed=fit2_af)
ggplot(fit2_alpha, aes(x = x, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Baseline variation across group - varies with uranium") +
  xlab("County") +
  ylab("Intercept") +
  geom_point(aes(x=x,y=fixed),size=4,colour="red")
# red dots (mean) change with the uranium level (group level predictor)

#Allowing for random slope, but independent of intercept
fit3=lmer(log.radon~floor+u.full+(floor||county.name),data=radonAZ)
# Formula: log.radon ~ floor + u.full + ((1 | county.name) + (0 + floor |  county.name))
# random slope - floor wrt county, random intercept - county (uranium: group level predictor)

summary(fit3)
AIC(fit3)
coef(fit3)
fixef(fit3)
ranef(fit3) # you have eta_1j and eta_floor (eta_2j)

fit3_a=coef(fit3)$county.name[,1]+ coef(fit3)$county.name[,3]*unique(radonAZ$u.full)
fit3_af=fixef(fit3)[1]+ coef(fit3)$county.name[,3]*unique(radonAZ$u.full)
fit3_alpha<-data.frame(x=1:15,est=fit3_a,
                       U=fit3_af+qnorm(0.975)*0.18940,
                       L=fit3_af+qnorm(0.025)*0.18940,
                       fixed=fit3_af)
ggplot(fit3_alpha, aes(x = x, y = est)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Baseline variation across group - varies with uranium") +
  xlab("County") +
  ylab("Intercept") +
  geom_point(aes(x=x,y=fixed),size=4,colour="red")


fit3_beta<-data.frame(x=1:15,est=coef(fit3)$county.name[,2],
                      U=rep(fixef(fit3)[2],15)+qnorm(0.975)*0.01818,
                      L=rep(fixef(fit3)[2],15)+qnorm(0.025)*0.01818,
                      fixed=rep(fixef(fit3)[2],15))
ggplot(fit3_beta, aes(x = x, y = est)) +
  geom_point(size = 4) +
  geom_hline(aes(yintercept=fixed),colour="red")+
  geom_errorbar(aes(ymax = U, ymin = L)) +
  ggtitle("Slope variation across group") +
  xlab("County") +
  ylab("Slope")


# Week 7 ------------------------------------------------------------------

#######################################################################
## Week 7 - Finding statistics about the components 
library(plyr)
count(salary.comp1, c("education"))

library(flexmix)

credit=read.csv("Credit.csv")

# iterate the model to find the number of clusters
simfit=stepFlexmix(credit$Balance~credit$Income+credit$Cards+credit$Age+credit$Student+credit$Gender,k = 1:5,data=credit)
print(simfit)
# model selection
getModel(simfit)
getModel(simfit, "BIC")

# using two clusters
fit=flexmix(credit$Balance~credit$Income+credit$Cards+credit$Age+credit$Student+credit$Gender,k=2,data=credit)

fit@cluster
# get the coefficients to write the equation
parameters(fit)

# Use this for component analysis
#Run the refit to get the p-values of the estimates
fit.refit=refit(fit)
summary(fit.refit) #viewing the refitted object


#Segmenting the data
credit.comp1=credit[fit@cluster==1,]
c1.param=parameters(fit)[,"Comp.1"]

credit.comp2=credit[fit@cluster==2,]
c2.param=parameters(fit)[,"Comp.2"]

par(mfrow=c(1,2))
plot(credit.comp1$Income,credit.comp1$Balance,main="Cluster 1", ylab="y", xlab="x")
plot(credit.comp2$Income,credit.comp2$Balance,main="Cluster 2", ylab="y", xlab="x")

#summary statistics for each component
#install.packages("pastecs")
library(pastecs)
options(digits=3)
stat.desc(credit.comp1[,c("Income","Age","Cards")])
sum(credit.comp1$Gender=="Female")/nrow(credit.comp1)
sum(credit.comp1$Student=="Yes")/nrow(credit.comp1)


options(digits=3)
stat.desc(credit.comp2[,c("Income","Age","Cards")])
sum(credit.comp2$Gender=="Female")/nrow(credit.comp2)
sum(credit.comp2$Student=="Yes")/nrow(credit.comp2)


# Create a function that helps provide descriptive statistics: Latent Mixture Models
# Example : lmm_desc_stats(credit, fit, c("Income","Age","Cards"), c("Gender", "Age"), c("Female", "Yes"))
lmm_desc_stats <- function(dataset, fit, predictors_col, variable_col, values) {
  
  #Segmenting the data
  dataset.comp1=dataset[fit@cluster==1,]
  dataset.comp2=dataset[fit@cluster==2,]

  
  library(pastecs)
  options(digits=3)
  t1 = stat.desc(dataset.comp1[,predictors_col])
  t2 = sum(dataset.comp1[[variable_col[1]]]==values[1])/nrow(dataset.comp1)
  t3 = sum(dataset.comp1[[variable_col[2]]]==values[2])/nrow(dataset.comp1)
  
  
  options(digits=3)
  h1 = stat.desc(dataset.comp2[,predictors_col])
  h2 = sum(dataset.comp2[[variable_col[1]]]==values[1])/nrow(dataset.comp2)
  h3 = sum(dataset.comp2[[variable_col[2]]]==values[2])/nrow(dataset.comp2)
  
  print('Cluster 1 ')
  print(t1)
  print(c(values[1],t2))
  print(c(values[2],t3))
  print('Cluster 2')
  print(h1)
  print(c(values[1],h2))
  print(c(values[2],h3))
}


