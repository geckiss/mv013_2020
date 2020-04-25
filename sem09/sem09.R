setwd("/home/martingregorik/FI/mv013_2020/sem09")

# task02
# subtask 1
mu <- 4
sigma <- 1
n <- 1000

set.seed(13)
X <- rnorm(n, mu, sigma)
hat_mu <- sum(X) / length(X)

norm.variance <- function(mu, X) {
  n <- length(X)
  sigma2 <- sum((X - mu) ^ 2) / n
  return (sigma2)
}

hat_sigma2 <- norm.variance(mu, X)

# subtask 2
# one value of mu, sigma2(scalar and scalar), data is vector
norm.log.likelihood <- function(mu, sigma2, data) {
  n <- length(data)
  like <- (-n * log(sigma2) / 2) - (1 / (2 * sigma2))*sum((data - mu) ^ 2)
  return(like)
}

# fix sigma2 = 1
mu.est <- optimize(f = norm.log.likelihood, interval = c(2, 6), maximum = T, sigma2 = 1, data = X)$maximum
mu.seq <- seq(from = 2, to = 6, length = 200)
l.mu <- apply(X = as.matrix(mu.seq), MARGIN = 1, FUN = norm.log.likelihood, sigma2 = 1, data = X)
plot(mu.seq, l.mu, type = 'l', xlab = bquote(mu), ylab = bquote(paste('l(', mu, '|x)')))
abline(v = mu.est, col = 'red', lty = 2)
mtext(bquote(hat(mu) == .(round(mu.est, 2))), side = 1, line = 2, at = mu.est, cex = 0.7, col = 'red')

# subtask 3
# fix mu = 4
sigma2.est <- optimize(f = norm.log.likelihood, interval = c(0.5, 1.5), maximum = T, mu = 4, data = X)$maximum
sigma2.seq <- seq(from = 0.5, to = 1.5, length = 500)
l.sigma2 <- apply(X = as.matrix(sigma2.seq), MARGIN = 1, FUN = norm.log.likelihood, mu = 4, data = X)
plot(sigma2.seq, l.sigma2, type = 'l', xlab = bquote(sigma2), ylab = bquote(paste('l(', sigma2, '|x)')))
abline(v = sigma2.est, col = 'red', lty = 2)
mtext(bquote(hat(sigma2) == .(round(sigma2.est, 2))), side = 1, line = 2, at = sigma2.est, cex = 0.7, col = 'red')

# subtask 4
norm.log.likelihood2 <- function(theta, data) {
  norm.log.likelihood(theta[1], theta[2], data)
}

par.est <- optim(par = c(6, 0.5), fn = norm.log.likelihood2, control = list(fnscale = -1), data = X)$par
xx <- seq(from = 3, to = 5, length = 200)
yy <- seq(from = 0.5, to = 3, length = 200)
Z <- expand.grid(xx, yy)
zz <- apply(X = as.matrix(Z), MARGIN = 1, FUN = norm.log.likelihood2, data = X)
zz <- matrix(zz, nrow = 200, ncol = 200)
image(xx, yy, zz, col = terrain.colors(12), xlab = bquote(mu), ylab = bquote(sigma ^ 2))
contour(xx, yy, zz, levels = seq(min(zz), max(zz), length = 13), add = T, drawlabels = F)
points(par.est[1], par.est[2], col = 'red', pch = 20)

# subtask 5
norm.prof.log.likelihood <- function(mu, X) {
  sigma2 <- norm.variance(mu, X)
  like <- norm.log.likelihood(mu, sigma2, X)
  return (like)
}
mu.prof.est <- optimize(f = norm.prof.log.likelihood, interval = c(2, 6), maximum = T, X = X)$maximum
sigma2.prof.est <- norm.variance(mu.prof.est, X)
