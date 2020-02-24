setwd(getwd())
# ==============================================
# task 3
skulls <- read.table("skulls.txt", header=T)

str(skulls)
dim(skulls)

head(skulls, 50)
tail(skulls, 50)

n.na <- is.na(skulls$skull.H)
sum(n.na)

skulls_no_na <- na.omit(skulls)

unique(skulls_no_na$id) # will select only unique IDs
length(unique(skulls_no_na$id)) == nrow(skulls_no_na)
# or
# true if it encounters duplicate, 0 sum if no duplicates
sum(duplicated(skulls_no_na$id))

# point 7
table(skulls_no_na$sex, useNA = 'ifany')
table(skulls_no_na$sex, useNA = 'always')

#table(skulls_no_na$sex, useNA = 'ifany', exclude = 'm')
women <- skulls_no_na[skulls_no_na$sex == 'f', ]
women

big_men <- skulls_no_na[skulls_no_na$sex == 'm' & skulls_no_na$skull.H > 125, ]
nrow(big_men) # or you can see it in environment

wdaw <- skulls_no_na[skulls_no_na$skull.H >= 130 & skulls_no_na$skull.H < 140, ]
wdaw_f <- wdaw[wdaw$sex == 'f', ]
wdaw_m <- wdaw[wdaw$sex == 'm', ]
# nrow(wdaw) == nrow(wdaw_f) + nrow(wdaw_m)

# ==============================================
# task 4
newbs <- read.csv('newborns.csv', header = T)
str(newbs)
dim(newbs)

table(newbs$edu.M, useNA = 'always')

edu.levels <- c("elem", "high", "uni")
newbs$edu.M2 <- factor(newbs$edu.M, labels = edu.levels)

table(newbs$edu.M2, useNA = 'ifany')

sum(is.na(newbs))

newbs2 <- na.omit(newbs)
# point 7
sum(duplicated(newbs2$id))

newbs3 <- newbs2[!duplicated(newbs2$id), ]
newbs3
#unique(newbs3)
table(newbs3$edu.M2, newbs3$sex.C, useNA = 'ifany')

# ==============================================
# task 5
height <- read.table('height.txt', header = T)
str(height)
levels(height$gender)

gender_table <- table(height$gender, useNA = 'ifany')
gender_table
sum(gender_table)
gender_table[-c(5,10)] # removes F and M
sum(gender_table[-c(5,10)]) / sum(gender_table) * 100 # labeled incorrectly
#height2 <- na.omit(height)
#height2
#men <- height2[height2$gender == 'M', ]
#nrows_men <- nrow(men)
#nrows_men
#women <- height2[height2$gender == 'F', ]
#women
#women_nrow <- nrow(women)
#women_nrow
#(women_nrow + nrows_men) / nrow(height) * 100 # labeled correctly
#(women_nrow + nrows_men) / sum(height) * 100

# point 4
gender_consistent <- height$gender
gender_consistent
gender_consistent[gender_consistent == 'female'] <- 'F'
gender_consistent[gender_consistent == 'Female'] <- 'F'
gender_consistent[gender_consistent == ' female'] <- 'F'
gender_consistent[gender_consistent == ' Female'] <- 'F'
gender_consistent[gender_consistent == 'female '] <- 'F'
gender_consistent[gender_consistent == 'Female '] <- 'F'

gender_consistent[gender_consistent == 'male' | gender_consistent == 'Male' | gender_consistent == ' male' | gender_consistent == ' Male' | gender_consistent == 'male ' | gender_consistent == 'Male '] <- 'M'

gender_consistent[gender_consistent == 'na' | gender_consistent == 'Na'] <- NA

gender_consistent
# will keep old levels = BAD
#gender_consistent2 <- height$gender
#gender_consistent2
#gender_consistent2 <- factor(gender_consistent2, labels = c('F', 'F', rep('M', 2), rep('F', 5), rep('M', 5), rep(NA, 2)))
#table(gender_consistent2)
#gender3 <- factor(gender_consistent2)
#levels(gender3)

gender4 <- factor(gender_consistent)
levels(gender4)


