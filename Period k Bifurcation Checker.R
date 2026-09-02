#-----Period k Bifurcation Checker-----#
#Instructions: 
#1. Plug in the period you are checking for k
#   - Likely only works for k = 1, 2, 4, 8, 16,...
#2. Plug in a value of r, greater than the value of r for
#   the bifurcation prior to the period k
#   - Ex) If k = 4, r > 3.44949
#3. Run the code once r is selected.
#   - If bifurcated, lower the value of r
#   - If Non-bifurcated, increase the value of r
#4. Do this until you have the value of r to around
#   5 decimals
#5. Put value of r into the #Data at the bottom

#Quick Note: The bifurcation values may differ slightly
#            after around 4 or 5 decimals relative to the
#            widely cited values. This is due to the method
#            of determining bifurcation on lines 54-60

#Logistic Difference Equation
logistic <- function(P) {
  r*P*(1-P)
}

#Initial Conditions
r <- 3.5687595
P <- 0.1
n <- 1000000
if (n %% 2 == 1) {n <- n + 1}
n_vector <- 1:n
BurnIn <- n - 1000
X <- numeric(n)
X[1] <- P

#Period-k
k <- 16
Period_k <- function(x) {
  for (j in 1:k) {
    x <- logistic(x)
  }
  x
}

#Iterations
for (i in 2:n) {
  X[i] <- Period_k(X[i-1])
}

#Plot
plot(n_vector[BurnIn:n], X[BurnIn:n],
     pch = 16, cex = 0.4, type = 'p', col = "red",
     xlab = "n", ylab = "X[n]",
     ylim = range(X[BurnIn:n]), xlim = c(BurnIn,n))

#Determine if seperation is macroscopic or microscopic noise
tail_vals <- X[BurnIn:n]
sep <- diff(range(tail_vals))
if (sep >= 0.000001) {
  cat(r, "Bifurcated", "- Lower the Value")
} else {
  cat(r, "Non-Bifurcated", "- Increase the value")
}



#Bifurcation Data
r <- numeric(10)
#Period 2:
r[1] = 3
#Period 4:
r[2] = 3.44949
#Period 8:
r[3] = 3.54409
#Period 16:
r[4] = 3.5644072
#Period 32:
r[5] = 3.5687595