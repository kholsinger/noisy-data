library(ggplot2)

rm(list=ls())

n_reps <- 1000

generate_p_values <- function(mu, sigma, sample_size, n_reps) {
  p_value <- numeric(n_reps)
  x_bar <- numeric(n_reps)
  for (i in 1:n_reps) {
    x <- rnorm(sample_size, mu, sigma)
    y <- rnorm(sample_size, 0.0, sigma)
    tmp <- t.test(x, y, var.equal=TRUE)
    x_bar[i] <- mean(x) - mean(y)
    p_value[i] <- tmp$p.value
  }
  stats <- data.frame(p_value=p_value,
                      x_bar=x_bar)
  return(stats)
}

p_value <- numeric(0)
x_bar <- numeric(0)
mean <- character(0)
n <- character(0)
mu_vec <- character(0)
for (mu in c(0.05, 0.10, 0.20)) {
  for (sample_size in c(10, 50, 100)) {
    p <- generate_p_values(mu, sigma=1.0, sample_size, n_reps)
    p_value <- c(p_value, p$p_value)
    x_bar <- c(x_bar, p$x_bar)
    label <- sprintf("mu=%4.2f", mu)
    mean <- c(mean, rep(label, n_reps))
    label <- sprintf("n=%d", sample_size)
    n <- c(n, rep(label, n_reps))
    mu_vec <- c(mu_vec, rep(mu, n_reps))
  }
}

for.plot <- data.frame(p_value=p_value,
                       x_bar=x_bar,
                       mu=mu_vec,
                       mean=mean,
                       n=n)
for.plot$n <- factor(for.plot$n, levels=c("n=10", "n=50", "n=100"))
for.plot <- subset(for.plot, p_value < 0.05)
p <- ggplot(for.plot, aes(x=x_bar)) +
  geom_histogram(bins=50) +
  xlab("Sample mean") +
  ylab("") +
  facet_grid(n ~ mean, scales="free_y", switch="y") +
  theme(strip.text=element_text(face="bold", size=rel(1.25)),
        strip.background=element_rect(fill="lightblue",
                                      color="black",
                                      size=1),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
print(p)

