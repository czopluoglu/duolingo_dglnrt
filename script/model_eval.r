require(cmdstanr)
require(rstan)
require(psych)
################################################################################

# Overview of the item parameters

View(summary(stanfit, pars = c("item"), probs = c(0.025, 0.975))$summary)

ipar <- summary(stanfit, pars = c("item"), probs = c(0.025, 0.975))$summary
ipar <- matrix(ipar[,1],ncol=2,byrow=TRUE) 

describe(ipar)

# Overview of the latent speed parameters

View(summary(stanfit, pars = c("person"), probs = c(0.025, 0.975))$summary)

tau <- summary(stanfit, pars = c("person"), probs = c(0.025, 0.975))$summary
tau <- matrix(tau[,1],ncol=2,byrow=TRUE)
psych::describe(tau)

# Overview of the probability of an examinee having item preknowledge

View(summary(stanfit, pars = c("pH"), probs = c(0.025, 0.975))$summary)

pH <- as.numeric(summary(stanfit, pars = c("pH"), probs = c(0.025, 0.975))$summary[,1])
head(pH)

describeBy(pH,d_wide$group,mat=TRUE)[,c('group1','n','mean','sd','min','max')]

plot(density(pH[d_wide$group==0]),xlim=c(0,1),main="",ylim = c(0,5))
points(density(pH[d_wide$group==1]),lty=2,type='l')


auc(d_wide$group,pH)

roc_analysis <- roc(response = d_wide$group,
                    predictor = pH)

plot(1-roc_analysis$specificities,
     roc_analysis$sensitivities,
     xlim = c(0,1),ylim=c(0,1),
     xlab = 'False Positive Rate (1-Specificity)',
     ylab = 'True Positive Rate (Sensitivity)',
     type='l')

my_thresholds <- seq(from=0.5,to=0.8,by=0.01)

coords(roc_analysis, 
       my_thresholds, 
       input="threshold", 
       ret=c("threshold","specificity", "sensitivity"))

# Overview of the probability of and item being compromised

View(summary(stanfit, pars = c("pC"), probs = c(0.025, 0.975))$summary)

# Retrive the true item compromise status

C_vec <- c()

for(kk in 1:50){
  C_vec[kk] = unique(d_long[d_long$Item==kk,]$compromised)
}

pC <- as.numeric(summary(stanfit, pars = c("pC"), probs = c(0.025, 0.975))$summary[,1])

describeBy(pC,C_vec,mat=TRUE)[,c('group1','n','mean','sd','min','max')]

plot(density(pC[C_vec==0]),xlim=c(0,1),main="",ylim = c(0,5))
points(density(pC[C_vec==1]),lty=2,type='l')


auc(C_vec,pC)

roc_analysis <- roc(response = C_vec,
                    predictor = pC)

my_thresholds <- seq(from=0.5,to=0.8,by=0.01)

coords(roc_analysis, 
       my_thresholds, 
       input="threshold", 
       ret=c("threshold","specificity", "sensitivity"))







