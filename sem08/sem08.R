setwd("/home/martingregorik/FI/mv013_2020/sem08")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 01
# kernel, we can disregard the rest. Why?
# I think it's because when we derive with respect to theta,
# every other variable is perceived as constant
# and constant derived is 1
# subtask 01
likelihood.binom.log.fun <- c("(x / log p) + ((N - x) / log (1 - p))")

# subtask 02
# take derivative with regards to p and set equal to 0, then derive
likelihood.binom.eq <- c("l(p|x) = x / p - ((N - x) / (1 - p))")

# subtask 03
# l(p|x) = 0
# ->
# x / p - ((N - x) / (1 - p)) = 0
# find p(derive by theta -> derive by p)
likelihood.binom.eq.res <- c("p = x / N")

# subtask 04
# 2nd derivation
likelihood.binom.pHAT <- c("(-x / (p ^ 2)) - ((N - x) / (1 - p) ^ 2)")
# I(theta) = I(p)
theta <- c("N / (pHAT * (1 - pHAT))")
likelihood.binom.pHAT.Var <- c("inverse(I(theta))")
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 02
binom.likelihood <- function(p, x, N) {
  return ((p ^ x) * ((1 - p) ^ (N - x)))
}

binom.log.likelihood <- function(p, x, N) {
  return ((x / log(p)) + ((N - x) / log(1 - p)))
}

Task.2.Solver <- function(x, N) {
  # How to get x / N? Solve the likelihood equation -> set likelihood function = 0 and derive by theta(p)
  p <- x / N
  phat <- (-x / (p ^ 2)) - ((N - x) / (1 - p) ^ 2)
  # Could not find function f ?????????????????????????????????????
  phat.likelihood <- optimize(binom.likelihood(p, x, N), interval = c(0, 1), maximum = TRUE)
  # plot(...)
  phat.log.likelihood <- optimize(binom.log.likelihood(p, x, N), interval = c(0, 1), maximum = TRUE)
  # plot(...)
}

Task.2.Solver(2, 20)
Task.2.Solver(10, 20)
Task.2.Solver(18, 20)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# Am i missing something here? Whats the difference between this exercise and previous one?
# It is still binomial distribution, so the same equation.
# Only input parameters change.
# Is the 'x' a set below correct? x is meant to be observations, but the values are not in exercise.
# task 03
x <- 15
N <- 20

p.hat.1 <- optimize(f = binom.likelihood, interval = c(0, 1), maximum = T, x = x, N = N)$maximum
p.hat.11 <- optimize(f = binom.log.likelihood, interval = c(0, 1), maximum = T, x = x, N = N)$maximum

pfit <- seq(0.001, 1, length.out = 500)

like2.1 <- binom.likelihood(pfit, x, N)
likehood2.1 <- binom.likelihood(p.hat.1, x, N)

plot(pfit, like2.1, type = 'l', xlab = 'prob', ylab = 'likelihood function')
abline(v = p.hat.1, col = 'red', lty = 2)
abline(h = likehood2.1, col = 'red', lty = 2)
points(p.hat1, likehood2.1, col = 'red', pch = 20)

# relative likelihood
rel.like <- like2.1 / likehood2.1
likehood2.2 <- 1
plot(pfit, rel.like, type = 'l', xlab = 'prob', ylab = 'relative likelihood function')
abline(v = p.hat.1, col = 'red', lty = 2)
abline(h = likehood2.2, col = 'red', lty = 2)
points(p.hat.1, likehood2.2, col = 'red', pch = 20)


llike <- binom.log.likelihood(pfit, x, N)
llikehood <- binom.log.likelihood(p.hat.11, x, N)
plot(pfit, llike, type = 'l', xlab = 'prob', ylab = 'log-likelihood function')
abline(v = p.hat.11, col = 'red', lty = 2)
abline(h = llikehood, col = 'red', lty = 2)
points(p.hat.1, llikehood, col = 'red', pch = 20)


rel.llike <- llike - llikehood
llikehood2 <- 0
plot(pfit, rel.llike, type = 'l', xlab = 'prob', ylab = 'relative log-likelihood function')
abline(v = p.hat.11, col = 'red', lty = 2)
abline(h = llikehood2, col = 'red', lty = 2)
points(p.hat.1, llikehood2, col = 'red', pch = 20)


ratio <- -2*logb(rel.like)
plot(pfit, ratio, type = 'l', xlab = 'prob', ylab = 'likelihood ratio test statistic')
abline(v = p.hat.1, col = 'red', lty = 2)
abline(h = 0, col = 'red', lty = 2)
points(p.hat.1, 0, col = 'red', pch = 20)
