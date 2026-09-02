## ----- Lyapunov Exponent ----- ##

#Initial Conditions
Iterations <- 5000
Population_Initial <- 0.12345

#Backend Initial Conditions
n <- Iterations
Pi <- Population_Initial
if(n %% 2 == 1) {
  n <- n + 1
}
BurnIn <- n / 2

#Logistic Difference Equation
logistic <- function(x, r) {
  r*x*(1-x)
}

#Multipler Equation
multiplier <- function(x, r) {
  abs(r*(1-2*x))
}

#Lyapunov Function
lyapunov <- function(r) {
  X <- numeric(n)
  X[1] <- Pi
  for (i in 2:n) {
    X[i] <- logistic(X[i-1], r)
  }
  
  M <- numeric(n)
  for (i in 1:n) {
    M[i] <- multiplier(X[i], r)
  }
  
  ΣlnM <- 0
  for (i in BurnIn:n) {
    ΣlnM <- ΣlnM + log(M[i])
  }
  
  λ <- ΣlnM / (n - BurnIn)
  return(λ)
}

#Plot: Lyapunov vs. r
R <- seq((4/n), 4 ,(4/n))
lambda <- numeric(n)
for (i in 1:n) {
  lambda[i] <- lyapunov(R[i])
}

plot(R, lambda, type = 'h', col = 'pink', xlim = c(2.6, 4), ylim = c(-0.5,0.5), xlab = "Rate, r", ylab = "Lyapunov Exponent, λ", main = "Lyapunov Exponent, λ Vs. Rate, r")
abline(0,0)


multiplier <- function(x, r) {
  abs(r*(1-2*x))
}
lambda <- mean(log(multiplier(x, r)))









#Assign M & X Values

#for (i in 2:n) {
#  X[i] <- logistic(X[i-1])
#}

#for (i in 1:n) {
#  M[i] <- multiplier(X[i])
#}

#Lyapunov Exponent
#lnM <- numeric(n)
#for (i in 1:n) {
#  lnM[i] <- log(M[i])
#}
#
#SumOflnM <- 0 #With BurnIn
#for (i in BurnIn:n) {
#  SumOflnM <- SumOflnM + lnM[i]
#}

#λ <- (SumOflnM / (n - BurnIn))
#cat("r:", r, "λ:", λ)


#Lyapunov Exponent Compact
#ΣlnM <- 0
#for (i in BurnIn:n) {
#  ΣlnM <- ΣlnM + log(M[i])
#}
#λ <- ΣlnM / (n - BurnIn)
#cat("r:", r, "λ:", λ)

