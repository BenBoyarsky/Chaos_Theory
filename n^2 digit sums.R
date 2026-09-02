n <- 10000
num <- 1:n
squares <- num^2

sum_digits <- function(x) {
  sapply(strsplit(as.character(x), ""), function(d) sum(as.numeric(d)))
}

sums <- sum_digits(squares)

hist(sums, breaks = seq(min(sums)-0.5, max(sums)+0.5, 1),
     main = "Digit sums of n^2",
     xlab = "sum of digits of n^2")