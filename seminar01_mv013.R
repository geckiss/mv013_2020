## task 01
v <- sample(c(0, 1), replace = T, 1000)
v
vc <- cumsum(v)
vc
vs <- c(1:1000)
vs
vv <- vc/vs
vv

plot(c(1:1000), vv, xlab="number of coin flips", ylab="number of heads", type="l")

## ex2
# 
id_v <- c(1:30)
df <- data.frame(id=id_v, gender=(sample(c("M", "F"), length(id_v), replace = T, prob = c(0.7, 0.3))), mark=sample(c("A", "B", "C", "D", "E", "F"), length(id_v), replace = T, prob = c(0.09, 0.09, 0.11, 0.09, 0.18, 0.44)))
df

# OMFG ugly
map <- function(df){
  ret_vector = c()
  for (i in 1:length(df[, 1])){
    if(df$mark[i] == 'A'){
      ret_vector[i] <- 1
    } else if(df$mark[i] == 'B'){
      ret_vector[i] <- 1.5
    } else if (df$mark[i] == 'C'){
      ret_vector[i] <- 2
    } else if(df$mark[i] == 'D'){
      ret_vector[i] <- 2.5
    } else if (df$mark[i] == 'E'){
      ret_vector[i] <- 3
    } else if (df$mark[i] == 'F'){
      ret_vector[i] <- 3.5
    }
  }
  return(ret_vector)
}

df$values <- map(df)
df

# table of frequency of marks
marks_tab <- table(df$mark)
marks_tab

# contingency table of marks by gender
cont_tab <- table(df$gender, df$mark)
cont_tab

# average of all marks
average <- mean(df$values)
average

# average by gender
val_tab <- table(df$gender, df$values)
val_tab

vals <- c(1, 1.5, 2, 2.5, 3, 3.5)
vals

avg_tabl <- data.frame(
  females=sum(val_tab[1, ]*vals) / sum(val_tab[1, ]), 
  males=sum(val_tab[2, ]*vals) / sum(val_tab[2, ])
  )
avg_tabl

## ex3
#
par(bg='white')
a <- seq(-2, 2, 0.1)
a
b <- a^2 - 1
b
plot(a, b, type = 'l', main = "Exercise 3 graph")

p_x <- a[a >= -1 & a <= 1]
p_x
p_y <- b[b <= 0]
p_y
polygon(p_x, p_y, col='gray')

t <- seq(min(b), max(b), length=length(a))
t
l <- rep(0, length(a))
l
lines(a, rep(0, length(a)), type='l', lty=2, col='red')
lines(l, t, type='l', lty=2, col='red')

points(0, min(b), type='p', col='green', bg='green', pch=21)
points(1, 0, type='p', col='green', bg='green', pch=21)
points(-1, 0, type='p', col='green', bg='green', pch=21)

## ex04
demo(graphics)


