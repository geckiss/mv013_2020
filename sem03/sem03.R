setwd("/home/martingregorik/R/sem03")

tab <- data.frame(height=c(131, 132, 135, 141, 141, 141, 141, 142, 143, 146, 146, 151), rank=c(1, 2, 3, 5.5, 5.5, 5.5, 5.5, 8, 9, 10.5, 10.5, 12))
tab
tab[1, 'height']
tab$height[1]

check_length <- function(vec) {
  if (length(vec) != 0) {
    return (length(vec))
  } else {
    return (0)
  }
}

# vec = vector = data frame column
my_sample_min <- function(vec) {
  # TODO handle NA???
  # TODO check length
  min <- Inf
  for (i in 1:length(vec)) {
    if (vec[i] < min) {
      min <- vec[i]
    }
  }
  return (min)
}
my_sample_min(tab$height)

my_sample_max <- function(vec) {
  # TODO handle NA???
  max <- -Inf
  for (i in 1:length(vec)) {
    if (vec[i] > max) {
      max <- vec[i]
    }
  }
  return (max)
}
my_sample_max(tab$height)

my_arit_avg <- function(vec) {
  sum <- 0
  for (i in 1:length(vec)) {
    sum <- sum + vec[i]
  }
  return (sum/length(vec))
}
my_arit_avg(tab$height)

my_quartile <- function(vec, q) {
  # denominator
  denom <- 1 / q
  len <- length(vec)
  if (len %% 2 == 0) {
    # even
    return ((vec[len/denom] + vec[len/denom + 1]) / 2)
  } else {
    # odd
    return (vec[(len + 1) / denom])
  }
}
median <- my_quartile(tab$height, 0.5)
median
lower_quartile <- my_quartile(tab$height, 0.25)
lower_quartile
upper_quartile <- my_quartile(tab$height, 0.75)
upper_quartile

# TODO make nums on one row, col names = desc
my_five_num_sum <- function(vec) {
  return (data.frame(nums=c(my_sample_min(vec), my_quartile(vec, 0.25), my_quartile(vec, 0.50), my_quartile(vec, 0.75), my_sample_max(vec)), desc=c("min", "lower_q", "median", "upper_q", "max")))
}
my_five_num_sum(tab$height)

# sum of (xi - x~) squared
my_sum_sample_avg <- function(vec, exponent=2) {
  sum <- 0
  div <- 0
  avg <- my_arit_avg(vec)
  for (i in 1:length(vec)) {
    div <- vec[i] - avg
    sum <- sum + div^exponent
  }
  return (sum)
}

my_sample_variance <- function(vec, exponent=2) {
  if (check_length(vec) != 0) {
    avg <- my_arit_avg(vec)
    sum <- my_sum_sample_avg(vec, exponent)
    return (sum / (length(vec) - 1))
  } else {
    return ("length of vec is 0")
  }
}
my_sample_variance(tab$height)

my_sample_standard_deviation <- function(vec, exponent=2) {
  if (check_length(vec) != 0) {
    pw <- my_sample_variance(vec, exponent)
    return (sqrt(pw))
  }
}
my_sample_standard_deviation(tab$height)

my_sample_variance_of_arit_avg <- function(vec) {
  if (check_length(vec) != 0) {
    return (my_sample_variance(vec) / length(vec))
  }
}
my_sample_variance_of_arit_avg(tab$height)

# standard error
my_sample_standard_dev_of_arit_avg <- function(vec) {
  if (check_length(vec) != 0) {
    return (my_sample_standard_deviation(vec) / sqrt(length(vec)))
  }
}
my_sample_standard_dev_of_arit_avg(tab$height)

# s is standard deviation ^ 3 = sqrt ^ 3 = ^3/2
my_sample_skew_cramer <- function(vec) {
  nom <- my_sum_sample_avg(vec, 3)
  denom <- length(vec)*my_sample_variance(vec)^(3/2)
  return (nom / denom)
}
# negatively skewed
my_sample_skew_cramer(tab$height)

my_sample_kurtosis <- function(vec) {
  nom <- my_sum_sample_avg(vec, 4)
  denom <- length(vec)*my_sample_variance(vec)^2
  return ((nom / denom) - 3)
}
# broad = thick tails = platykurtic
my_sample_kurtosis(tab$height)

my_sample_range <- function(vec) {
  return (my_sample_max(vec) - my_sample_min(vec))
}
my_sample_range(tab$height)

my_sample_interquartile_range <- function(vec) {
  return (my_quartile(vec, 0.75) - my_quartile(vec, 0.25))
}
my_sample_interquartile_range(tab$height)
# ---
# positive interquartile skewness
my_quartile(tab$height, 0.75) - my_quartile(tab$height, 0.50)
my_quartile(tab$height, 0.50) - my_quartile(tab$height, 0.25)
# ---

my_inner_bound_lower <- function(vec) {
  return (my_quartile(vec, 0.25) - 1.5*(my_sample_interquartile_range(vec)))
}
my_inner_bound_lower(tab$height)
my_inner_bound_upper <- function(vec) {
  return (my_quartile(vec, 0.75) + 1.5*(my_sample_interquartile_range(vec)))
}
my_inner_bound_upper(tab$height)

my_outer_bound_lower <- function(vec) {
  return (my_quartile(vec, 0.25) - 3*(my_sample_interquartile_range(vec)))
}
my_outer_bound_lower(tab$height)
my_outer_bound_upper <- function(vec) {
  return (my_quartile(vec, 0.75) + 3*(my_sample_interquartile_range(vec)))
}
my_outer_bound_upper(tab$height)

my_sample_quartile_coef_skew <- function(vec) {
  nom <- (my_quartile(vec, 0.75) - my_quartile(vec, 0.5)) - (my_quartile(vec, 0.5) - my_quartile(vec, 0.25))
  denom <- my_sample_interquartile_range(vec)
  return (nom / denom)
}
my_sample_quartile_coef_skew(tab$height)

# ---
# task 03
body <- read.table("body.txt", header=T)
body
str(body)
dim(body)
# no missing values
sum(is.na(body))
# sort
body <- body[order(body$hip.C, body$sex), ]
body
# calculate basic chars 
# FOR WHOLE SAMPLE
# sample size
length(body[, "hip.C"])
my_arit_avg(body$hip.C)
my_five_num_sum(body$hip.C)
# b1~
my_sample_skew_cramer(body$hip.C)
# b2~
my_sample_kurtosis(body$hip.C)
my_sample_standard_deviation(body$hip.C)
# standard error
my_sample_standard_dev_of_arit_avg(body$hip.C)
# potential outliers = out of inner bounds
inner_bounds <- c(my_inner_bound_lower(body$hip.C), my_inner_bound_upper(body$hip.C))
inner_bounds
potential_outliers <- body$hip.C[body$hip.C < inner_bounds[1] | body$hip.C > inner_bounds[2]]
potential_outliers
# distant points = out of outer bounds
outer_bounds <- c(my_outer_bound_lower(body$hip.C), my_outer_bound_upper(body$hip.C))
outer_bounds
distant_points <- body$hip.C[body$hip.C < outer_bounds[1] | body$hip.C > outer_bounds[2]]
distant_points
# ---
# FOR WOMEN
body_f <- body[body$sex == 'f', ]
body_f
length(body_f[, 1])
my_arit_avg(body_f$hip.C)
my_five_num_sum(body_f$hip.C)
# b1~
my_sample_skew_cramer(body_f$hip.C)
# b2~
my_sample_kurtosis(body_f$hip.C)
my_sample_standard_deviation(body_f$hip.C)
# standard error
my_sample_standard_dev_of_arit_avg(body_f$hip.C)
# potential outliers = out of inner bounds
inner_bounds_f <- c(my_inner_bound_lower(body_f$hip.C), my_inner_bound_upper(body_f$hip.C))
inner_bounds_f
potential_outliers_f <- body_f$hip.C[body_f$hip.C < inner_bounds_f[1] | body_f$hip.C > inner_bounds_f[2]]
potential_outliers_f
# distant points = out of outer bounds
outer_bounds_f <- c(my_outer_bound_lower(body_f$hip.C), my_outer_bound_upper(body_f$hip.C))
outer_bounds_f
distant_points_f <- body_f$hip.C[body_f$hip.C < outer_bounds_f[1] | body_f$hip.C > outer_bounds_f[2]]
distant_points_f
# ---
# FOR MEN
body_m <- body[body$sex == 'm', ]
body_m
length(body_m[, 1])
my_arit_avg(body_m$hip.C)
my_five_num_sum(body_m$hip.C)
# b1~
my_sample_skew_cramer(body_m$hip.C)
# b2~
my_sample_kurtosis(body_m$hip.C)
my_sample_standard_deviation(body_m$hip.C)
# standard error
my_sample_standard_dev_of_arit_avg(body_m$hip.C)
# potential outliers = out of inner bounds
# TODO have found one, should have found two
inner_bounds_m <- c(my_inner_bound_lower(body_m$hip.C), my_inner_bound_upper(body_m$hip.C))
inner_bounds_m
potential_outliers_m <- body_m$hip.C[body_m$hip.C < inner_bounds_m[1] | body_m$hip.C > inner_bounds_m[2]]
potential_outliers_m
# distant points = out of outer bounds
outer_bounds_m <- c(my_outer_bound_lower(body_m$hip.C), my_outer_bound_upper(body_m$hip.C))
outer_bounds_m
distant_points_m <- body_m$hip.C[body_m$hip.C < outer_bounds_m[1] | body_m$hip.C > outer_bounds_m[2]]
distant_points_m