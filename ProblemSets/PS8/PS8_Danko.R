library(nloptr)
library(tidyverse)
#calculate betas
set.seed(100)
X = matrix( rnorm(100000*10,mean=0,sd=1), 100000, 10) 
X[,1] <- 1 
eps = matrix( rnorm(100000*1,mean=0,sd=.5), 100000, 1) 
beta <-  c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)
Y = X%*%beta+eps
view(Y)
solve(t(X)%*%(X))%*%t(X)%*%Y

#use gradient descent t calculate betas
m <- length(Y)
#Create random initialization for theta
theta <- matrix(rnorm(10*1, mean=0, sd=1), 10,1)
x <- cbind(rep(1, 100000), X)
compCost<-function(X, Y, theta){
  m <- length(Y)
  J <- sum((X%*%theta- Y)^2)/(2*m)
  return(J)
}
gradDescent<-function(X, Y, theta, alpha, num_iters){
  m <- length(Y)
  J_hist <- rep(0, num_iters)
  for(i in 1:num_iters){
    
    # this is a vectorized form for the gradient of the cost function
    # X is a 100x5 matrix, theta is a 5x1 column vector, y is a 100x1 column vector
    # X transpose is a 5x100 matrix. So t(X)%*%(X%*%theta - y) is a 5x1 column vector
    theta <- theta - alpha*(1/m)*(t(X)%*%(X%*%theta - Y))
    
    # this for-loop records the cost history for every iterative move of the gradient descent,
    # and it is obtained for plotting number of iterations against cost history.
    J_hist[i]  <- compCost(X, Y, theta)
  }
  # for a R function to return two values, we need to use a list to store them:
  results<-list(theta, J_hist)
  return(results)
}
alpha <- .0000003
num_iters <- 15000
results <- gradDescent(X, Y, theta, alpha, num_iters)
theta <- results[[1]]
print(theta)

#Compute using LBFGS
# Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}


# initial values
beta0 <- runif(10) #start at uniform random numbers equal to number of coefficients

# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

#try nelder-mead
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)

# Find the optimum
res <- nloptr( x0=beta0,eval_f=objfun,Y=Y,X=X,opts=options)
print(res)

#use L-BFGS to evaluate MLE

# objective function
objfun  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta))]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y- X%*%beta)/sig)^2) ) 
  return (loglike)
}

# initial values
theta0 <- runif(10)

# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

# Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

library(stargazer)
est <- lm(Y~X-1)
stargazer(est)
