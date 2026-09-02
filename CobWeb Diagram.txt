#-----CobWeb Diagram of Logistic Difference Equation-----

#Logistic Difference Equation
logistic <- function(P) {
  r*P*(1-P)
}

#Initial Conditions
r <- 3.4497

P <- 0.001
n <- 10000
if (n %% 2 == 1) {n <- n + 1}
X <- numeric(n)
Y <- numeric(n)
X[1] <- P
Y[1] <- logistic(X[1])

#Set Order Pairs

for (i in 2:n) {
  if (i %% 2 == 0) {
    X[i] <- Y[i-1]
    Y[i] <- X[i]
  } else {
    X[i] <- X[i-1]
    Y[i] <- logistic(X[i])
    
  }
}

#Logistic Curve

x <- numeric(1000)
for (i in 1:1000) {
  x[i] <- i / 1000
}

#Slice Cobweb
start_index <- n/2

# Plot curves

plot(X[start_index:n], Y[start_index:n], type = "l", xlab= "X", ylab = "Y", ylim = c(0, 1), col = "red", xlim = c(0,1))
#plot(X, Y, type = "l", xlab= "X", ylab = "Y", ylim = c(0, 1), col = "red", xlim = c(0,1))

abline(0,1)
lines(x, logistic(x))