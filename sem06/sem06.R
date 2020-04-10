setwd("/home/martingregorik/R/sem06")

# task 01
mean <- 140.83
variance <- 33.79
sd <- sqrt(variance)
# length in seq ???
(vals <- seq(120, 160, length = 100))
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

# task 02
mean <- 150
variance <- 6.25
(sd <- sqrt(variance))
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




