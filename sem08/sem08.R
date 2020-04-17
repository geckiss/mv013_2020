setwd("/home/martingregorik/FI/mv013_2020/sem08")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 01
# kernel, we can disregard the rest. Why?
# I think it's because when we derive by theta,
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
# Am i missing something here? Whats the difference between this exercise nad previous one?
# It is still binomial distribution, so the same equation.
# Only input parameters change.
# Is the 'x' a set below correct? x is meant to be observations, but the values are not in exercise.
# task 03
x <- 15
N <- 20
Task.3.Solver <- function(x, N) {
  # How to get x / N? Solve the likelihood equation -> set likelihood function = 0 and derive by theta(p)
  p <- x / N
  phat <- (-x / (p ^ 2)) - ((N - x) / (1 - p) ^ 2)
  return (phat)
}
Task.3.Solver(x, N)
binom.likelihood(x / N, x, N)
# This will plot the red point in exercise instructions 1st plot from top left
plot(x, binom.likelihood(x / N, x, N))

# relative likelihood
plot(x, binom.likelihood(x / N, x, N) /  binom.likelihood(Task.3.Solver(x, N), x, N))

binom.log.likelihood(x / N, x, N)

binom.log.likelihood(x / N, x, N) - binom.log.likelihood(Task.3.Solver(x, N), x, N)
