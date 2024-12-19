require(cmdstanr)
require(rstan)

################################################################################

# Import data (long format)

d_long <- read.csv('./data/simdata_long.csv')
d_wide <- read.csv('./data/simdata_wide.csv')
################################################################################

# Input Data

data_rt <- list(
  I              = length(unique(d_long$Item)),
  J              = length(unique(d_long$id)),
  n_obs          = nrow(d_long),
  p_loc          = d_long$id,
  i_loc          = d_long$Item,
  Y              = log(d_long$RT)
)

# Compile the model syntax

mod <- cmdstan_model('./script/dglnrt.stan')

# Single chain initialization

fit_init <- mod$sample(
  data            = data_rt,
  chains          = 1,
  iter_warmup     = 150,
  iter_sampling   = 150,
  refresh         = 10,
  adapt_delta     = 0.99)
#########################################################################

# Multi-chain estimation with starting parameters from single-chain estimation

# a vector of initial time intensity and time discrimination parameters

ipar <- matrix(fit_init$summary("item")$mean,ncol=2,byrow=FALSE) 
rownames(ipar) <- NULL
colnames(ipar) <- NULL
head(ipar)

# a two column matrix of initial tau parameters

tau <- matrix(fit_init$summary("person")$mean,ncol=2,byrow=FALSE)

rownames(tau) <- NULL
colnames(tau) <- NULL

head(tau)

# A vector of initial P(H=1) parameters, probability of an examinee having item preknowledge

H <- as.vector(fit_init$summary("pH")$mean)
head(H)

# A vector of initial P(C=1) parameters, probability of an item being compromised

C <- as.vector(fit_init$summary("pC")$mean)
head(C)

# Put the initial estimates together as a list

start <- list(item   = ipar,
              person = tau,
              pH     = H,
              pC     = C)

str(start)

# Fit the model

fit <- mod$sample(
  data            = data_rt,
  seed            = 1234,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 500,
  iter_sampling   = 1000,
  refresh         = 10,
  init            = list(start,start,start,start),
  adapt_delta     = 0.99)

fit$save_object(file = './no_upload/model_fit.RDS')

