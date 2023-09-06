
set.seed(42)
library(BVAR)

# data
x <- fred_qd[, c("GDPC1", "GDPCTPI", "FEDFUNDS")]
x <- fred_transform(x, codes = c(4, 4, 1))

# priors
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2), 
  psi <- bv_psi(scale = 0.004, shape = 0.004, mode = "auto", min = "auto", max = "auto"),
  var = 1e07
)

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)

priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)

# MH setup
mh <- bv_metropolis(
  scale_hess = c(0.05, 0.0001, 0.0001),
  adjust_acc = TRUE, 
  acc_lower = 0.25, 
  acc_upper = 0.45
)

# estimation
run <- bvar(
  x, 
  lags = 5, 
  n_draw = 20000, 
  n_burn = 10000, 
  n_thin = 1,
  priors = priors, 
  mh = mh, 
  verbose = TRUE
)

# forecasting 
predict(run) <- predict(
  run, 
  horizon = 20, 
  conf_bands = seq(from = 0.05, to = 0.4, by = 0.01)
)
plot(
  predict(run), 
  area = TRUE, 
  t_back = 32,
  vars = c("FEDFUNDS")
)


# output
plot.ts(run$hyper)
plot.ts(run$sigma[,1,])
plot.ts(run$beta[,2,])

apply(run$beta, 2:3, mean)
