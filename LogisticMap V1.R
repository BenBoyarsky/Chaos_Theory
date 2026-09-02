## -- Logistic Map -- ##

#Initial Conditions
n <- 1500
Pi <- 0.12345

#Logistic Difference Equation
logistic <- function(r, x) {
  return(r*x*(1-x))
}

#Assign Populations for each r
k <- 100
population <- function(r) {
  X <- numeric(n)
  X[1] <- Pi
  for (i in 2:n) {
    X[i] <- logistic(r, X[i-1])
  }
  TailValues <- tail(X, k)
  return(TailValues)
}


R <- vector("list", n)
R_Values <- seq(0, 4, length.out = n)
for (i in 1:n) {
  r <- numeric(k)
  for (j in 1:k) {
  r[j] <- R_Values[i]
}
  R[[i]] <- r
}

Populations <- vector("list", n)
for (i in 1:n) {
  Populations[[i]] <- population(R[[i]][1])
}


plot(
  NA,
  xlim = c(2.6, 4),
  ylim = c(0, 1),
  xlab = "Rate, r",
  ylab = "Population",
  main = "Logistic Map"
)
for (i in 1:n) {
  points(R[[i]], Populations[[i]],
  pch = ".",
  col = "black")
}