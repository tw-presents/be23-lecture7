
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


Y.h   = aperm(run$fcast$fcast, c(2,3,1))
h     = dim(Y.h)[1]

limits.1    = range(Y.h[,1,])
point.f     = apply(Y.h[,1,],1,mean)
interval.f  = apply(Y.h[,1,],1,HDInterval::hdi,credMass=0.90)

x           = seq(from=limits.1[1], to=limits.1[2], length.out=100)
z           = matrix(NA,h,99)
for (i in 1:h){
  z[i,]     = hist(Y.h[i,1,], breaks=x, plot=FALSE)$density
}
x           = hist(Y.h[i,1,], breaks=x, plot=FALSE)$mids
yy          = 1:h
z           = t(z)

library(plot3D)

theta = 180
phi   = 15.5
f4    = persp3D(x=x, y=yy, z=z, phi=phi, theta=theta, xlab="\nrgdp[t+h|t]", ylab="h", zlab="\npredictive densities of rgdp", shade=NA, border=NA, ticktype="detailed", nticks=3,cex.lab=1, col=NA,plot=FALSE)
perspbox (x=x, y=yy, z=z, bty="f", col.axis="black", phi=phi, theta=theta, xlab="\nrgdp[t+h|t]", ylab="h", zlab="\npredictive densities of rgdp", ticktype="detailed", nticks=3,cex.lab=1, col = NULL, plot = TRUE)
polygon3D(x=c(interval.f[1,],interval.f[2,h:1]), y=c(1:h,h:1), z=rep(0,2*h), col = "#FF00FF", NAcol = "white", border = NA, add = TRUE, plot = TRUE)
for (i in 1:h){
  f4.l = trans3d(x=x, y=yy[i], z=z[,i], pmat=f4)
  lines(f4.l, lwd=0.5, col="black")
}
f4.l1 = trans3d(x=point.f, y=yy, z=0, pmat=f4)
lines(f4.l1, lwd=2, col="black")
