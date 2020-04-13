setwd("/home/martingregorik/R/sem06")

<<<<<<< HEAD
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
=======
>>>>>>> 095108dd2812482d04109ff394e654564fa1501d
# task 01
mean <- 140.83
variance <- 33.79
sd <- sqrt(variance)
<<<<<<< HEAD
# ---------------------------- #
# vals are random, idk where they got values in seminar
(vals <- seq(120, 160, length = 100))
# ---------------------------- #
=======
# length in seq ???
(vals <- seq(120, 160, length = 100))
>>>>>>> 095108dd2812482d04109ff394e654564fa1501d
#####
k <- 1
(interval <- seq(mean - (k*sd), mean + (k*sd)))
Fxa <- pnorm(q = min(interval), mean = mean, sd = sd)
Fxb <- pnorm(q = max(interval), mean = mean, sd = sd)
prob <- Fxb - Fxa
# normal density
plot(vals, dnorm(vals, mean = mean, sd = sd), type = 'l', ylab = 'density', xlab = paste0('area under the curve\n', 'Pr(', round(min(interval), 2), ' < X < ', round(max(interval), 2), ') = ', round(prob, 4)))
(x <- c(mean - k*sd, interval, mean + k*sd))
(y <- c(0, dnorm(interval, mean = mean, sd = sd), 0))
polygon(x, y, col = 'gray')
#####
#####
k <- 2
(interval <- seq(mean - (k*sd), mean + (k*sd)))
Fxa <- pnorm(q = min(interval), mean = mean, sd = sd)
Fxb <- pnorm(q = max(interval), mean = mean, sd = sd)
prob <- Fxb - Fxa
plot(vals, dnorm(vals, mean = mean, sd = sd), type = 'l', ylab = 'density', xlab = paste0('area under the curve\n', 'Pr(', round(min(interval), 2), ' < X < ', round(max(interval), 2), ') = ', round(prob, 4)))
(x <- c(mean - k*sd, interval, mean + k*sd))
(y <- c(0, dnorm(interval, mean = mean, sd = sd), 0))
polygon(x, y, col = 'gray')
#####
#####
k <- 3
(interval <- seq(mean - (k*sd), mean + (k*sd)))
Fxa <- pnorm(q = min(interval), mean = mean, sd = sd)
Fxb <- pnorm(q = max(interval), mean = mean, sd = sd)
prob <- Fxb - Fxa
plot(
  vals, 
  dnorm(vals, mean = mean, sd = sd), 
  type = 'l', 
  ylab = 'density', 
  xlab = paste0(
    'area under the curve\n', 
    'Pr(', 
    round(min(interval), 2), 
    ' < X < ', 
    round(max(interval), 2), 
    ') = ', 
    round(prob, 4)
    )
  )
(x <- c(mean - k*sd, interval, mean + k*sd))
(y <- c(0, dnorm(interval, mean = mean, sd = sd), 0))
polygon(x, y, col = 'gray')
#####
def.par <- par()
layout(matrix(c(1, 2, 3), nrow=1, ncol=3), widths=c(0.33,0.33,0.33), heights=c(1))
layout.show(n=3)
# then plot all 3 plots
<<<<<<< HEAD
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
=======

>>>>>>> 095108dd2812482d04109ff394e654564fa1501d
# task 02
mean <- 150
variance <- 6.25
(sd <- sqrt(variance))
<<<<<<< HEAD

probs <- c(0.90,0.95,0.99)
bs <- qnorm(probs, mean = mean, sd = sd)
as <- qnorm(probs, mean = mean, sd = sd, lower.tail = FALSE)

Task.2.Plot <- function(vals, a, b, mean, sd, prob) {
  plot(
    vals, 
    dnorm(vals, mean = mean, sd = sd), 
    type = 'l', 
    ylab = 'density', 
    xlab = paste0(
      'area under the curve\n', 
      'Pr(', 
      round(a, 2), 
      ' < X < ', 
      round(b, 2), 
      ') = ', 
      prob
    )
  )
}
# 90
Task.2.Plot(vals, as[1], bs[1], mean, sd, 1 - alfa.90)
(interval <- seq(as[1], bs[1]))
(x <- c(as[1], interval, bs[1]))
(y <- c(0, dnorm(interval, mean = mean, sd = sd), 0))
polygon(x, y, col = 'gray')

# 95
Task.2.Plot(vals, as[2], bs[2], mean, sd, 1 - alfa.95)
(interval <- seq(as[2], bs[2]))
(x <- c(as[2], interval, bs[2]))
(y <- c(0, dnorm(interval, mean = mean, sd = sd), 0))
polygon(x, y, col = 'gray')

# 95
Task.2.Plot(vals, as[3], bs[3], mean, sd, 1 - alfa.99)
(interval <- seq(as[3], bs[3]))
(x <- c(as[3], interval, bs[3]))
(y <- c(0, dnorm(interval, mean = mean, sd = sd), 0))
polygon(x, y, col = 'gray')
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 03
proportion.men <- 0.515
proportion.women <- 0.485
# pnorm/pbinom = probability mass function = distribution function
# dnorm/dbinom = density function
# they give approximately values
### TODO thats just donmr with q = 3
###1 N = 5, p = 0.515

Task.3.Plot <- function(prob, size, cummulative = FALSE) {
  trials <- seq(0, size)
  if (cummulative) {
    binom <- pbinom(q = trials, size = size, prob = prob) # lower.tail = TRUE
    # TODO for norm, trials needs to be continuous
    norm <- pnorm(q = trials, mean = size * prob, sd = sqrt(size * prob * (1 - prob)))
  } else {
    binom <- dbinom(x = trials, size = size, prob = prob) # lower.tail = TRUE
    # TODO for norm, trials needs to be continuous
    norm <- dnorm(x = trials, mean = size * prob, sd = sqrt(size * prob * (1 - prob)))
  }
  plot(
    trials, 
    binom, 
    type = 'h', 
    xlab = paste0(
      'Bin(', 
      size, 
      ', ', 
      prob, 
      ')'
    ),
    ylab = 'prob'
  )
  points(trials, binom, col = 'black')
  lines(trials, norm, col="red", type = 'l')
  legend('topleft', pch = 15, legend = c('Binomial', 'Normal'), col = c('black', 'red'))
}

size <- 5
Task.3.Plot(proportion.men, size)
size <- 10
Task.3.Plot(proportion.men, size)
size <- 50
Task.3.Plot(proportion.men, size)

size <- 5
Task.3.Plot(proportion.men, size, cummulative = TRUE)
size <- 10
Task.3.Plot(proportion.men, size, cummulative = TRUE)
size <- 50
Task.3.Plot(proportion.men, size, cummulative = TRUE)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 04
# families
M <- 6115
# children
N <- 12
n <- seq(0, N)
mn.observed <- c(3, 24, 104, 286, 670, 1033, 1343, 1112, 829, 478, 181, 45, 7)
(prob <- sum(n * mn.observed) / (N * M))
(mn.expected <- round((dbinom(x = n, size = N, prob = prob)) * M, 0))
(data.frame(ob = mn.observed, ex = mn.expected))
def.par <- par()
layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(0.5, 0.5))
barplot(
  height = mn.observed, 
  main = "Observed",
  xlab = 'Number of boys',
  ylab = 'Number of families',
  space = 0,
  cex.names = 0.8,
  xaxt = 'n',
  col = 'blue')
axis(side = 1, at = n + 0.5, labels = n, las = 2, cex.axis = 0.8)
text(x = n + 0.5, y = mn.observed + 1, label = mn.observed, pos = 3, cex = 0.5, col = 'blue', xpd = TRUE)
barplot(
  height = mn.expected, 
  main = "Expected",
  xlab = 'Number of boys',
  ylab = 'Number of families',
  space = 0,
  cex.axis = 0.8,
  xaxt = 'n',
  col = 'red')
axis(side = 1, at = n + 0.5, labels = n, las = 2, cex.axis = 0.8)
text(x = n + 0.5, y = mn.expected + 1, label = mn.expected, pos = 3, cex = 0.5, col = 'red', xpd = TRUE)

# Superimposed
par(def.par)
(mn.difference <- mn.observed - mn.expected)
barplot(
  height = mn.observed, 
  main = "Observed superimposed with expected, with difference",
  xlab = 'Number of boys',
  ylab = 'Number of families',
  space = 0,
  xaxt = 'n',
  col = 'blue')
axis(side = 1, at = n + 0.5, labels = n, cex.axis = 0.8)
text(x = n + 0.5, y = mn.expected + 1, label = mn.difference, pos = 3, col = 'red', xpd = TRUE)
points(n + 0.5, mn.expected, col = 'red', pch = 16, xpd = TRUE)
lines(n + 0.5, mn.expected, col= 'red', type = 'h', xpd = TRUE)
#sum(mn.expected)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# Task 5
# rnorm is theoretical density, ofc
# kernel is density()

Task.5.Plot <- function(N, mean = 0, sd = 1, lambda = -1) {
  if (lambda != -1) {
    samples <- rexp(N)
    theo.normalized <- dexp(samples)
    main.title <- paste0('Exponential distribution EXP(',lambda,')')
  } else {
    samples <- rnorm(N)
    theo.normalized <- dnorm(samples)
    main.title <- paste0('Normal distribution N(',mean,',',sd,')')
  }
  
  plot(
    samples, 
    theo.normalized, 
    type = 'p',
    main = main.title,
    xlab = paste0('N = ', N),
    ylab = 'density'
  )
  
  den <- density(samples, n = 50)
  points(den$x, den$y, col = 'red', pch = 16)
  rug(samples)
}

Task.5.Plot(50)
Task.5.Plot(1000)
Task.5.Plot(50, 3, sqrt(0.25))
Task.5.Plot(1000, 3, sqrt(0.25))
Task.5.Plot(50, lambda = 3)
Task.5.Plot(1000, lambda = 3)
=======
probs <- c(0.90,0.95,0.99)
# 1 - alfa = 0.90 => alfa = 0.10
alfa <- 0.1
# calculate a and b
# x 1-alfa/2 - this is the quantile
seq(0, 1, 0.99)
(quant <- quantile(vals, probs = c(0.99), na.rm = TRUE, names = FALSE))
(a <- mean - (quant[2]*sd))
(b <- mean + (quant[2]*sd))

#(quants <- quantile(vals, probs = probs))
# b = x
(x <- qnorm(probs, mean = mean, sd = sd))
#(quants <- pnorm(q = x, mean = mean, sd = sd))


## task 03
men <- 0.515
women <- 0.485




>>>>>>> 095108dd2812482d04109ff394e654564fa1501d
