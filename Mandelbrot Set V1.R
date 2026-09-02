## -- Mandelbrot Set -- ##
#Initial Conditions
n <- 100

#Iterations
iteration <- function(c) {
  Z <- 0
  for(i in 1:n) {
    Z <- Z^2 + c
    if (Mod(Z) > 2) {
      return(Z)
    }
    if (i == n & Mod(Z) <= 2) {
      return(Z)
    }
  }
}  

TFDeterminer <- function(x) {
  if (Mod(x) > 2) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}
PixelRoot <- 1000
Width <- PixelRoot
Height <- PixelRoot
LowerRealBound <- -2
UpperRealBound <- 1
LowerImaginaryBound <- -1
UpperImaginaryBound <- 1
Dr <- (UpperRealBound - LowerRealBound) / Width
Di <- (UpperImaginaryBound - LowerImaginaryBound) / Height

Mpar <- matrix(0, nrow = Height, ncol = Width)
Mnum <- matrix(0, nrow = Height, ncol = Width)
Mlogical <- matrix(FALSE, nrow = Height, ncol = Width)
for (i in 1:Width) {
  R <- seq(LowerRealBound, UpperRealBound, length.out = Width)
  for (j in 1:Height) {
    I <- seq(LowerRealBound * 1i, UpperRealBound * 1i, length.out = Height)
    Mpar[i, j] <- R[i] + I[j]
    Mnum[i, j] <- iteration(Mpar[i,j])
    Mlogical[i, j] <- TFDeterminer(Mnum[i,j])
  }
}

plot(
  NA,
  xlim = c(LowerRealBound, UpperRealBound),
  ylim = c(LowerImaginaryBound, UpperImaginaryBound),
  xlab = "Real",
  ylab = "Imaginary",
  main = "Mandelbrot set"
)
for (i in 1:Width) {
  for (j in 1:Height) {
    if (Mlogical[i,j] == TRUE) {
      points(Re(Mpar[i,j]), Im(Mpar[i,j]),
             pch = ".",
             col = "black")
    } else {
      points(Re(Mpar[i,j]), Im(Mpar[i,j]),
      pch = ".",
      col = "blue")
    }
  }
}