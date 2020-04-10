getwd()
set_table <- function() {
  d <- matrix(c(0.12,0.15,0.22,0.34,0.06,0.11), ncol=3)
  d
  colnames(d) <- c('blonde', 'brown', 'red')
  rownames(d) <- c('blue', 'brown')
  return (d)
}
rel.freq <- set_table()
rel.freq
# task 1.1
(abs.freq <- rel.freq *1000)

# task 1.2
(abs.freq.eyes <- rowSums(abs.freq))
# task 1.3
(abs.freq.hair <- colSums(abs.freq))

# task 1.4+5
# what proportion of people have blue eyes and what proportion have brown eyes within each hair color
addmargins(abs.freq)
(cond.hair <- prop.table(abs.freq, 2))
(cond.eye <- prop.table(d, 1))

# task 1.6
# each column of the matrix will be a column of the barplot
# barplot takes columns as bars
barplot(abs.freq, space = 0, ylab='absolute frequencies', legend.text = row.names(abs.freq), args.legend = list(x='topleft', bty='n'), col=c('dodgerblue', 'brown'))

# task 1.7
barplot(cond.hair, space = 0, ylab='relative frequencies')

# task 1.8
barplot(t(abs.freq), space = 0, ylab='absolute frequencies', legend.text = colnames(abs.freq), args.legend = list(x='topleft', bty='n'), col=c('yellow', 'brown', 'red'))

# task 1.9
barplot(t(cond.eye), space = 0, ylab='relative frequencies')

# task 1.10 - incorporated in previous tasks
# task 1.11
# Colors in R pdf somwhere google
# abs.freq.eyes/sum(abs.freq.eyes) * 100
eye.labels <- paste(names(abs.freq.eyes), ' (', abs.freq.eyes/sum(abs.freq.eyes) * 100, '%)', sep='')
# same, without specifying separator
#paste0(names(abs.freq.eyes), ' (', abs.freq.eyes/sum(abs.freq.eyes) * 100, '%)')
pie(abs.freq.eyes, col=c('dodgerblue', 'brown'), labels=eye.labels, main='Eye colour')

# task 1.12
# you can change radius or rotate the piechart
hair.labels <- paste(names(abs.freq.hair), ' (', abs.freq.hair/sum(abs.freq.hair) * 100, '%)', sep='')
pie(abs.freq.hair, col=c('yellow', 'brown', 'red'), labels=hair.labels, main='Hair colour')

# task 2.1
k = 24
pie(rep(1, k), col=rainbow(k))
pie(rep(1, k), col=heat.colors(k))
pie(rep(1, k), col=topo.colors(k))
pie(rep(1, k), col=terrain.colors((k)))
pie(rep(1, k), col=grey.colors(k, start=0, end=1))

# task 3.1
# iris
(iris.petals <- data.frame(iris[, 3:5]))
my_cols <- c('red', 'green', 'blue')
(iris.num.species <- as.numeric(iris$Species))
palette(my_cols)
plot(iris$Petal.Length, iris$Petal.Width, xlab='Petal Length(cm)', ylab='Petal Width(cm)', asp = 1, col=iris$Species, pch = iris.num.species)
legend('topleft', pch = unique(as.numeric(iris$Species)), legend = levels(iris$Species), col = my_cols)

# task 3.2
# filled circle = 16
# empty circle = 1
# star = 8

