require(MASS)
require(MBESS)
require(matrixStats)
require(psych)
################################################################################
set.seed(6202021)

N = 500       # number of examinees
n = 50        # number of items
pe <- 0.10    # proportion of examinees with item preknowledge
pi <- 0.50    # proportion of compromised items

# Generate the binary status of examinee item preknowledge
# 1: examinee has item preknowledge
# 0: examinee has item preknowledge

tmp <- runif(N,0,1)
H  <- ifelse(tmp<=quantile(tmp,pe),1,0)
H
table(H)

# Generate the binary status of item compromise
# 1: item is compromised
# 0: item is not compromised

tmp <- runif(n,0,1)
C  <- ifelse(tmp<=quantile(tmp,pi),1,0)
C
table(C)

# Generate item parameters

mu_beta        <- 3.5
mu_logalpha    <- 0.5
sigma_beta     <- 0.3
sigma_logalpha <- 0.2
omega_I        <- matrix(c(1,0.25,0.25,1),2,2)

mu_I     <- c(mu_beta,mu_logalpha)
Sigma_I  <- diag(c(sigma_beta,sigma_logalpha))%*%omega_I%*%diag(c(sigma_beta,sigma_logalpha))

item_par <- mvrnorm(n,mu=mu_I,Sigma=Sigma_I)  
  
beta  <- item_par[,1]    # time intensity parameters
alpha <- exp(item_par[,2])  # time discrimination parameters


# Generate perso parameters

mu_taut      <- 0
sigma_taut   <- 0.1
mu_tauc      <- 0.4
sigma_tauc   <- 0.15
omega_P      <- matrix(c(1,0.7,0.7,1),2,2)

mu_P     <- c(mu_taut ,mu_tauc)
Sigma_P  <- diag(c(sigma_taut,sigma_tauc))%*%omega_P%*%diag(c(sigma_taut,sigma_tauc))

tau <- mvrnorm(N,mu_P,Sigma_P)

tau_t <- tau[,1]    # true latent speed parameters 
tau_c <- tau[,2]    # true cheating speed parameters 


# Generate observed response times according to the model

rt <- matrix(nrow=N,ncol=n)

for(i in 1:N){
  for(j in 1:n){
    
    p_t <- beta[j] - tau_t[i]
    p_c <- beta[j] - tau_c[i]
    
    if(H[i] == 1 & C[j] == 1){
      rt[i,j] = exp(rnorm(1,p_c,1/alpha[j]))
    } else {
      rt[i,j] = exp(rnorm(1,p_t,1/alpha[j]))
    }
    
  }
}

# Convert it to data frame and add group membership and a unique ID

rt       <- as.data.frame(rt)
rt$group <- H
rt$id    <- 1:nrow(rt)

# Check the data

head(rt)

# Reshape it to long format (for plotting purposes)

rt.long <- reshape(data        = rt,
                   idvar       = 'id',
                   varying     = list(colnames(rt)[1:n]),
                   timevar     = "Item",
                   times       = 1:n,
                   v.names      = "RT",
                   direction   = "long")

# Add item status

rt.long$compromised <- NA

for(j in 1:n){
  
  rt.long[rt.long$Item==j,]$compromised = C[j]
  
}

head(rt.long)

describeBy(rt.long$RT,list(rt.long$group,rt.long$compromised),mat=TRUE)

################################################################################

write.csv(rt.long,'./data/simdata_long.csv',
          row.names = FALSE)  

write.csv(rt,'./data/simdata_wide.csv',
          row.names = FALSE)  


