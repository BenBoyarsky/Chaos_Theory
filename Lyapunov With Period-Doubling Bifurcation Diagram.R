## -- Period-Doubling Bifurcation Diagram With Lyapunov Exponent Overlay -- ##
## -- Period Doubling Bifurcation Diagram -- ##

#Initial Conditions
n <- 1500
Pi <- 0.2

#Logistic Difference Equation
logistic <- function(r, x) {
  return(r*x*(1-x))
}

#Assign Populations for each r
k <- 200
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


#Initial Conditions
Iterations <- 1000
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

par(new = TRUE)

plot(
  R,
  lambda,
  type = "h",
  col = "pink",
  axes = FALSE,
  xlab = "",
  ylab = "",
  xlim = c(2.6, 4),
  ylim = c(-0.5, 0.5)
)
abline(0,0)




