require(cmdstanr)
require(rstan)
require(psych)
################################################################################
# Estimation Time 

get_elapsed_time(stanfit)

(sum(get_elapsed_time(stanfit))/4)/3600
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
describe(tau)

# Overview of the probability of an examinee having item preknowledge

View(summary(stanfit, pars = c("pH"), probs = c(0.025, 0.975))$summary)

T <- as.numeric(summary(stanfit, pars = c("pH"), probs = c(0.025, 0.975))$summary[,1])
hist(T)

auc(d_wide$group,T)

roc_analysis <- roc(response = d_wide$group,
                    predictor = T)

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

C_ <- as.numeric(summary(stanfit, pars = c("pC"), probs = c(0.025, 0.975))$summary[,1])
hist(C_)

auc(C_vec,C_)

roc_analysis <- roc(response = C_vec,
                    predictor = C_)

my_thresholds <- seq(from=0.5,to=0.8,by=0.01)

coords(roc_analysis, 
       my_thresholds, 
       input="threshold", 
       ret=c("threshold","specificity", "sensitivity"))







