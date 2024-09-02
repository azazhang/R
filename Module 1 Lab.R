
# Question 1: BMI ---------------------------------------------------------

wt <- 150
ht <- 68
bmi <- wt * 703 / ht^2
print(bmi)


# Question 2: Cost of Pizza -----------------------------------------------

diameter <- 12
cost <- 8
area <- pi * (diameter / 2)^2
cost / area

diameter_2 <- 15
cost_2 <- 12
area_2 <- pi * (diameter_2 / 2)^2
cost_2 / area_2


# Question 3: Calculating Roots -------------------------------------------

(14 * 0.51)^0.5
(14 * 0.51)^(1/3)
n <- 1:5
(14 * 0.51)^(1/n)


# Question 4: Analyzing a Vector of Weights -------------------------------

kg <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)
lbs <- kg * 2.20462
mean(lbs)
var(lbs)
sd(lbs)
range(lbs)
lbs[lbs > mean(lbs)]


# Question 5: More BMI ----------------------------------------------------

hts <- c(62, 58, 61, 61, 59, 64, 63, 61, 60, 62)
bmi <- (lbs * 703) / hts^2
mean(bmi)

# Question 6: Sequences ---------------------------------------------------

seq1 <- seq(0, 1, 0.1)
seq2 <- rev(seq(1, 10, 0.5))
seq1
seq2
rep(1:3, times = 3)
rep(c("a", "c", "e", "g"), each = 3)
rep(c("a", "c", "e", "g"), times = 3)
rep(1:3, each = 3, times = 2)
rep(1:5, times = 5:1)
rep(c(7, 2, 8, 1), times = c(4, 3, 1, 5))


# Question 7: Ordering ----------------------------------------------------

child_names <- c("John", "Jack", "Jane")
ht <- c(63, 59, 60)
height_ord <- order(ht)
names_sort <- child_names[height_ord]
names_sort
child_names <- c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise"
                 , "Mary", "Ronald", "William")
ht <- c(62, 58, 61, 61, 59, 64, 63, 61, 60, 62)
height_ord <- order(ht)
names_sort <- child_names[height_ord]
names_sort[1]
names_sort[10]
names_sort

# Question 8: Missing values ----------------------------------------------

mydata <- c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mean(mydata)
mean(mydata, na.rm = TRUE)