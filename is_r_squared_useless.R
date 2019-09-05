## Is R-Squared Useless?
## Translated/adapted from this resource: https://data.library.virginia.edu/is-r-squared-useless/
## All credit to the above sources

##First, practically, how to get R-Squared out of R
x = 1:20 #IV
set.seed(1) #for reproducibility
y = 2 + 0.5*x + rnorm(20,0,3) #dv; function of x with gaussian random error
mod = lm(y~x) #simple linear regression
summary(mod)$r.squared #request just r squared val

##Calculating R-squared by hand
f = mod$fitted.values #extract fitted/predicted values from model
mss = sum((f - mean(f))^2)
tss = sum((y - mean(y))^2)
mss/tss

##Problems with R-squared

#1. R does not measure goodness of fit. If sigma-squared is high
#it can drive R-squared low, even when model is perfect.

r2.0 <- function(sig) {
  x = seq(1,10,length.out = 100) #our predictor
  y = 2 + 1.2*x + rnorm(100,0,sd = sig) #our response, function of x+noise
  summary(lm(y~x))$r.squared #print r-squared
}

sigmas = seq(0.5, 20, length.out = 20) #apply function to sigmas
rout = sapply(sigmas, r2.0)
plot(rout ~ sigmas, type="b")

#2. R-squared can be arbitrarily close to 1 when the model is wrong
set.seed(1) 
x = rexp(50, rate=0.005) #predictor comes from exponential distribution
y = (x-1)^2 * runif(50, min=0.8, max=1.2) #non-linear data gen
plot(x,y) #clearly non-linear
summary(lm(y~x))$r.squared

#3. R-squared says nothing about prediction error, even with sigma-squared
#exactly the same, and no change in coeffs. Changing range of X messes with 
#R-squared
x = seq(1,10,length.out=100)
set.seed(1)
y = 2 + 1.2*x + rnorm(100,0,sd=0.9)
mod1 = lm(y~x)
summary(mod1)$r.squared
sum((fitted(mod1) - y)^2)/100

#Repeat the above, but change range of x

x = seq(1,2,length.out=100)
set.seed(1)
y = 2 + 1.2*x + rnorm(100,0,sd=0.9)
mod1 = lm(y~x)
summary(mod1)$r.squared
sum((fitted(mod1) - y)^2)/100
