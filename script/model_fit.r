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

# Fit the model

fit <- mod$sample(
  data            = data_rt,
  seed            = 1234,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 1000,
  refresh         = 10,
  adapt_delta     = 0.99)

# Compile the output files into an rstan object


fit$cmdstan_summary()

stanfit <- rstan::read_stan_csv(fit$output_files())
