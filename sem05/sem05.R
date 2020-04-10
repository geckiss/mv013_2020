setwd("/home/martingregorik/R/sem05")
options(max.print=10000)
skulls <- read.table("one-sample-mean-skull-mf.txt", header = T)
str(skulls)
skulls

n.na <- is.na(skulls$skull.B)
sum(n.na) # == 3
skulls_no_na <- na.omit(skulls)

# task 1
(skulls.male <- skulls_no_na$skull.B[skulls_no_na$sex == 'm'])
qqnorm(y = skulls.male, xlab = "Theoretical quantities", ylab = "Sample quantities")
qqline(skulls.male)

# task 2
x <- skulls.male
seqx <- seq(min(x), max(x), length=1000)
denx <- dnorm(seqx, mean(x), sd(x))
denx
hist(skulls.male, prob = TRUE, xlab = "Skull Breadth(mm)", ylab = "Density")
lines(seqx, denx, col = 'blue')
lines(density(x), col = 'red')
legend('topleft', legend = c('normal', 'empirical'), col = c('red', 'blue'), lty = 1, box.lty = 0)
rug(skulls.male)

#task 3
# Tools - Install packages - Hmisc
library(Hmisc)
skulls.female <- skulls_no_na$skull.B[skulls_no_na$sex == 'f']
histbackback(skulls.male, skulls.female, probability = TRUE, axes = TRUE, xlab = c('males', 'females'), ylab = 'Skulls Breadth (mm)')

# task 4
boxplot(
  skulls_no_na$skull.B ~ skulls_no_na$sex, 
  width = c(length(skulls.female), length(skulls.male)), 
  notch = TRUE, 
  col = c('pink', 'cyan'), 
  xaxt='n', 
  xlab = '',
  ylab = "Skulls Breadth (mm)",
  pch = 16
)
axis(1, at = c(1, 2), labels = c('females', 'males'))
points(sum(skulls.female)/length(skulls.female), col = 'red', pch = 16)
points(2, sum(skulls.male)/length(skulls.male), col = 'red', pch = 16)

# task 5
def.par <- par()
layout(matrix(c(2,1,0,3), nrow=2, ncol=2), widths=c(0.8,0.2), heights=c(0.2,0.8))
layout.show(n=3)
# bottom,. left, top, right
plot(skulls_no_na$skull.B, skulls_no_na$skull.L, xlab='Skull Breadth (mm)', ylab='Skull Length (mm)', asp = 1)
boxplot(
  skulls_no_na$skull.B, 
  xaxt='n', 
  xlab = '',
  horizontal = TRUE
)
boxplot(
  skulls_no_na$skull.L, 
  xaxt='n', 
  yaxt = 'n',
  xlab = ''
)

