library(ggplot2)
library(rstan)

rm(list=ls())

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

n_reps <- 1000

stan.file.name <- "point.stan"
stan.precompiled <- stan(stan.file.name, iter=1)

progress_file <- "progress.txt"

get_coverage_frequency <- function(mu, sigma, sample_size, n_reps) {
  cover <- 0
  sign <- 0
  x_bar <- numeric(0)
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
    } else {
      x_bar <- c(x_bar, mean(mu_post))
      if (mu > 0) {
        if (mean(mu_post) < 0) {
          sign <- sign + 1
        }
      } else {
        if (mean(mu_post) > 0) {
          sign <- sign + 1
        }
      }
    }
  }
  return(list(cover=cover,
              sign=sign,
              x_bar=x_bar))
}

mean <- character(0)
n <- character(0)
cover <- numeric(0)
sign <- numeric(0)
mu_post <- numeric(0)
mu_true <- numeric(0)
sample <- numeric(0)
for (mu in c(0.05, 0.10, 0.20)) {
  for (sample_size in c(10, 50, 100)) {
    sink(file=progress_file, append=TRUE)
    label <- sprintf("mu=%5.2f, n=%3d\n", mu, sample_size)
    cat(label)
    sink()
    tmp <- get_coverage_frequency(mu, sigma=1.0, sample_size, n_reps)
    cover <- c(cover, tmp$cover)
    sign <- c(sign, tmp$sign)
    label <- sprintf("mu=%4.2f", mu)
    mean <- c(mean, label)
    mu_true <- c(mu_true, rep(label, length(tmp$x_bar)))
    label <- sprintf("n=%d", sample_size)
    n <- c(n, label)
    sample <- c(sample, rep(label, length(tmp$x_bar)))
    mu_post <- c(mu_post, tmp$x_bar)
  }
}
file.remove(progress_file)

data <- data.frame(mean=mean,
                   n=n,
                   cover=cover,
                   sign=sign)

for.plot <- data.frame(mu=mu_true,
                       n=sample,
                       x_bar=mu_post)
for.plot$n <- factor(for.plot$n, levels=c("n=10", "n=50", "n=100"))
p <- ggplot(for.plot, aes(x=x_bar)) +
  geom_histogram(bins=50) +
  xlab("Sample mean") +
  ylab("") +
  facet_grid(n ~ mu, scales="free_y", switch="y") +
  theme(strip.text=element_text(face="bold", size=rel(1.25)),
        strip.background=element_rect(fill="lightblue",
                                      color="black",
                                      size=1),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
print(p)

