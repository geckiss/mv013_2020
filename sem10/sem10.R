setwd("/home/martingregorik/FI/mv013_2020/sem10")

# we never accept, we reject or not reject
# alfa is prob of happening of this type of error !
# beta is err II
# if I alawys reject H0, alfa will equal 1
# if i always not reject H0, beta will equal 1
# we set Alfa to some very small number, so beta could be 

# how to reject/not reject, 3 possibilities:
# 1
# T = statistics, tOBS = aquired from data sample
# we know(from theory) the distribution of the statistics
# 2
# confidence interval I, if w is in interval or is not we decide if we reject or not reject null hypothesis
# w = w* --- some parameter = some other parameter
# critical region W
# if the value of our statistics is in critical region, we reject( if tOBS E W, we reject)
# 3
# p-value = probability that tOBS
# if p-value is smaller than significance level, we reject
# larger or equal, we do not reject

# what test statistics we are going to use
# if u calcualte test statistics, u obtain some ditribution - so comopute staatistics
# based on knowledge of distribution, u reject or not reject null hypothesis

# l(p,x) = xlnp + (N - x) ln(1 - p)
# phat = X / N
# variance_phat = phat*(1 - phat) / N - thats 1 / I(phat)


#########################
# task 1
# Wald
N <- 1000
# theta0 is parameter from our task, in our case p
# Uw = (theta_hat - theta0) * I(theta_hat) * (theta_hat - theta0)
# H0: p = 0.5 = p0, H1: p /= 0.5
# Uw = (phat - p)^2 * N/(phat*(1 - phat)) ~ Chi2
# sqrt(Uw) = (phat - p0)* sqrt((phat(1 - phat))/N) ~ N(0,1)
# N = 1000, X = 534, so phat = 534/1000 - dosad do vzorcov
# (0.534 - 0.5)*sqrt(1000/(0.534*(0.5))) = 2.1520 - value of test statistics
# now criticla region and p-value
u_alpha <- qnorm(0.05/2, lower.tail = F) # quantiles for desirable ciritical region
# if it is two-sided

my_z_score <- 2.15534 # my statistics
z_scores <- seq(-3, 3, by =.1)
dvalues <- dnorm(z_scores)
plot(z_scores, dvalues, type = 'l', main = 'standard normal', xlab='z-scores')
abline(v = my_z_score)
abline(v = -my_z_score)
abline(v = u_alpha, col = 'red')
# our test statistics falled into critical region, so we reject our null hypothesis

# p-value is area below those two parts(critical regions)
p_value <- 2*(1 - pnorm(abs(my_z_score))) # this is form kross
# p-value is smaller than 0.5(desirable confidence interval) -> we rejct null hypothesis

# confidence interval - if Zw(statistics) is between critical region, we dont reject null hypothesis(probability is 1 - alfa)
# on the left and right, u can see the borders of your confidece interval(the last line before Results)

# likelihood ratio test
binom.log.likelihood <- function(p, x, N) {
  return (2*((x * log(x / (N * p))) + ((N - x) * log((N - x) / (N * (1 - p))))))
}
x <- rbinom(N, 1, 0.534)
Ulr <- -2 * binom.log.likelihood(0.5, x, N)
p_value <- pnorm(Ulr)

#######################
#######################
# Task 2
N <- 350
phat <- 269 / N
p0 <- 0.8
alpha <- 0.05
# H0: prob of delivering package the next day is at least 0.8(>=)
# H1: prob of delviering package the next day is less than 0.8(<)

Zw <- (phat - p0) / sqrt((phat * (1 - phat)) / N)
z_scores <- seq(-2, 2, by = .1)
u_alpha <- qnorm(alpha) # quantiles for desirable ciritical region(one sided)
dvalues <- dnorm(z_scores)

# our test statistics DID NOT land in critical region -> we dont reject the null hypothesis
p_value <- pnorm(Zw) # this is form kross
# p-value is smaller than p0 -> we reject the hypothesis??? but that different
# confidence interval: Pr(phat - Ualpha*sqrt((phat*(1-phat))/N) < p0) = 1 - a
ci <- phat - (u_alpha*sqrt((phat * (1 - phat)) / N))

def.par <- par()
layout(matrix(c(1, 2), nrow=1, ncol=2), widths=c(0.5, 0.5))
plot(z_scores, dvalues, type = 'l', main = 'standard normal', xlab='Zw', ylab = 'density')
abline(v = Zw, col = 'red', lty = 2)
mtext("Zw", side = 1, line = 2, at = Zw, cex = 0.7, col = 'red')
mtext("-u0.65", side = 1, line = 3, at = u_alpha, cex = 0.7, col = 'black')
polygon(x = seq(-2, u_alpha, by = .01), y = dnorm(seq(-2, u_alpha, by = .01)))
# p-value
plot(z_scores, dvalues, type = 'l', main = 'standard normal', xlab='Zw', ylab = 'density')
abline(v = Zw, col = 'red', lty = 2)
mtext("Zw", side = 1, line = 2, at = Zw, cex = 0.7, col = 'red')
mtext("-u0.65", side = 1, line = 3, at = u_alpha, cex = 0.7, col = 'red')
polygon(x = seq(-2, min(Zw, p_value), by = .01), y = dnorm(seq(-2, min(Zw, p_value), by = .01)))




