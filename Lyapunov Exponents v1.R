{\rtf1\ansi\ansicpg1252\cocoartf2867
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 ## ----- Lyapunov Exponent ----- ##\
\
#Initial Conditions\
#r <- 4\
#n <- 50000\
x = 0.12345\
\
#X <- numeric(n)\
#X[1] <- 0.12345\
\
#Logistic Difference Equation\
logistic <- function(x, r) \{\
  r*x*(1-x)\
\}\
\
#Multipler\
#M <- numeric(n)\
\
multiplier <- function(x, r) \{\
  abs(r*(1-2*x))\
\}\
\
#Assign M & X Values\
\
#for (i in 2:n) \{\
#  X[i] <- logistic(X[i-1])\
#\}\
\
#for (i in 1:n) \{\
#  M[i] <- multiplier(X[i])\
#\}\
\
#Lyapunov Exponent\
#lnM <- numeric(n)\
#for (i in 1:n) \{\
#  lnM[i] <- log(M[i])\
#\}\
#\
#SumOflnM <- 0 #With BurnIn\
#for (i in BurnIn:n) \{\
#  SumOflnM <- SumOflnM + lnM[i]\
#\}\
\
#\uc0\u955  <- (SumOflnM / (n - BurnIn))\
#cat("r:", r, "\uc0\u955 :", \u955 )\
\
\
#Lyapunov Exponent Compact\
#\uc0\u931 lnM <- 0\
#for (i in BurnIn:n) \{\
#  \uc0\u931 lnM <- \u931 lnM + log(M[i])\
#\}\
#\uc0\u955  <- \u931 lnM / (n - BurnIn)\
#cat("r:", r, "\uc0\u955 :", \u955 )\
#Lyapunov Function\
Pi <- 0.12345\
n <- 50\
if(n %% 2 == 1) \{\
  n <- n + 1\
\}\
BurnIn <- n / 2\
R <- seq((4/n), 4 ,(4/n))\
\
lyapunov <- function(r) \{\
  X <- numeric(n)\
  X[1] <- Pi\
  for (i in 2:n) \{\
    X[i] <- logistic(X[i-1], r)\
  \}\
  M <- numeric(n)\
  for (i in 1:n) \{\
    M[i] <- multiplier(X[i], r)\
  \}\
  \uc0\u931 lnM <- 0\
  for (i in BurnIn:n) \{\
    \uc0\u931 lnM <- \u931 lnM + log(M[i])\
  \}\
  \uc0\u955  <- \u931 lnM / (n - BurnIn)\
  return(\uc0\u955 )\
\}\
lambda <- numeric(n)\
\
for (i in 1:n) \{\
  lambda[i] <- lyapunov(R[i])\
\}\
\
plot(R, lambda, type = 'b', col = 'red')\
\
}