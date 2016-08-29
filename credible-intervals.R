library(rstan)

rm(list=ls())

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

n_reps <- 2

stan.file.name <- "point.stan"
stan.precompiled <- stan(stan.file.name, iter=1)

get_coverage_frequency <- function(mu, sigma, sample_size, n_reps) {
  cover <- 0
  sign <- 0
  for (i in 1:n_reps) {
    x <- rnorm(sample_size, mu, sigma)
    y <- rnorm(sample_size, 0.0, sigma)
    stan.data <- list(x=x,
                      y=y,
                      N=sample_size)
    stan.pars <- c("mu_x",
                   "mu_y",
                   "sigma_x",
                   "sigma_y",
                   "mu_diff")
    fit <- stan(fit=stan.precompiled,
                data=stan.data,
                pars=stan.pars,
                iter=5000,
                warmup=2500,
                chains=4,
                save_dso=TRUE)
    mu_post <- extract(fit, "mu_diff")$mu_diff
    ci <- quantile(mu_post, c(0.025, 0.975))
    if ((ci[1] < 0.0) && (ci[2] > 0.0)) {
      cover <- cover + 1
    }
    if (mu > 0) {
      if (mean(mu_post) > 0) {
        sign <- sign + 1
      }
    } else {
      if (mean(mu_post) < 0) {
        sign <- sign + 1
      }
    }
  }
  return(data.frame(cover=cover,
                    sign=sign))
}

mean <- character(0)
n <- character(0)
cover <- numeric(0)
sign <- numeric(0)
for (mu in c(0.05, 0.10, 0.20)) {
  for (sample_size in c(10, 50, 100)) {
    tmp <- get_coverage_frequency(mu, sigma=1.0, sample_size, n_reps)
    cover <- c(cover, tmp$cover)
    sign <- c(sign, tmp$sign)
    label <- sprintf("mu=%4.2f", mu)
    mean <- c(mean, label)
    label <- sprintf("n=%d", sample_size)
    n <- c(n, label)
  }
}

data <- data.frame(mean=mean,
                   n=n,
                   cover=cover,
                   sign=sign)
