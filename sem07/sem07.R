setwd("/home/martingregorik/FI/mv013_2020/sem07")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 01
N <- 500
# create contingency table from data
data.observed <- data.frame(rbind(c(60, 60, 60, 20), c(90, 90, 90, 30)))
data.observed.colnames <- c("D-C", "D-Li", "R-C", "R-Li")
data.observed.rownames <- c("H", "Lo")
colnames(data.observed) <- data.observed.colnames
rownames(data.observed) <- data.observed.rownames

combine <- function(..., prefix = "", sep = "-") {
  paste0(prefix, levels(interaction(..., sep = sep)))
}

# calculate probs
data.observed.probs <- data.observed / N
# as.matrix because with data.frame it screamed that x values must be positive, i guess it dont like row/col names
# also labels would print vector, noy value, if not in matrix
(data.observed.percentages <- as.matrix(data.observed.probs * 100))
pie(
  data.observed.percentages, 
  clockwise = TRUE, 
  labels = paste0(combine(data.observed.rownames, data.observed.colnames), " (", data.observed.percentages, "%)"), 
  col = rainbow(8)
)

# calculate var
# stat.models II., page 8/slide 62
# Var(Xj) if Xj ~ Bin(N, p)
# X2 is H-D-Li
# Var(X2) - Bin(N, p2) -> p(H-D-Li) = 0.12
X2.p <- 0.12
X8.p <- 0.06
X2.variance <- N * X2.p * (1 - X2.p)
# X8 is Lo-R-Li
X8.variance <- N * X8.p * (1 - X8.p)
# Cov(Xi, Xj) = -Npipj
X2X8.cov <- -N * X2.p * X8.p
# Cor(Xi, Xj) = (-pipj) / sqrt(pi(1 - pi)pj(1 - pj))
X2X8.cor <- (-X2.p * X8.p) / sqrt(X2.p * (1 - X2.p) * X8.p * (1 - X8.p))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 02
# product multinomial distribution
N1 <- 200
N2 <- 300
# Distribution conditioned on socioeconomic status(H/L) - rows
# if I am H, whats the prob that I am D-C, if N = 200 etc.
(data.observed.probs2 <- rbind(data.observed[1, ] / N1, data.observed[2, ] / N2))
# TODO barplot
# var cov, cor
# ProductMult is Mult and Mult is Bin -> same as last exercise
# Xjk = pkj
X11.p <- 0.3
X21.p <- 0.3
X22.p <- 0.3
X32.p <- 0.3
# Xj|k∼Bin(Nk,pj|k) (j = 1,2,3,4, k = 1,2)
X21.variance <- N1 * X21.p * (1 - X21.p)
# Cov(Xi, Xj) = -Npipj
X22X32.cov <- -N2 * X22.p * X32.p
# Cor(Xi, Xj) = (-pipj) / sqrt(pi(1 - pi)pj(1 - pj))
X11X32.cor <- (-X11.p * X32.p) / sqrt(X11.p * (1 - X11.p) * X32.p * (1 - X32.p))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 03
# bivariate normal distribution
bnorm <- function(x,y,mu1,mu2,sigma1,sigma2,rho) {
  A <- 2 * pi * sqrt((sigma1 ^ 2) * (sigma2 ^ 2) * (1 - (rho ^ 2)))
  B <- 2 * (1 - rho ^ 2)
  z <- (((x - mu1) ^ 2) / sigma1 ^ 2) - (2 * rho * (x - mu1) * (y - mu2) / (sigma1 * sigma2)) + ((y - mu2)^2) / (sigma2 ^ 2)
  fxy <- 1 / A * exp(-z / B)
  return (fxy)
}
n <- 10
x <- runif(n, min = -3, max = 3)
y <- x

bnorm(x[1], y[1], 0, 0, 1, 1, 0)
bnorm(x[1], y[1], 0, 0, 1, 1, 0.5)
bnorm(x[1], y[1], 0, 0, 1, 1.2, 0.5)

# help pls
outer(x, y, FUN = bnorm, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 0, rho = 0)
outer(x, y, FUN = bnorm, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 0, rho = 0.5)
outer(x, y, FUN = bnorm, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1.2, rho = 0.5)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# task 04
library(MASS)
