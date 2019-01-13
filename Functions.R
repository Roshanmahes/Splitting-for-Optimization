### A set of objective functions designed for multidimensional optimisation ###
# Makes use of https://www.sfu.ca/~ssurjano/optimization.html.

### 30-D unimodal functions ###

DeJong1 <- function(x) {
  # interval: [-100,100]^n
  return(sum(x[1:(length(x)-1)]^2))
}

Schwefel222 <- function(x) {
  # interval: [-10,10]^n
  return(sum(abs(x)) + prod(abs(x)))
}

Schwefel12 <- function(x) {
  # interval: [-100,100]^n
  n <- length(x)
  xj_s <- matrix(rep(x,n),n)
  xj_s[lower.tri(xj_s)] <- 0
  
  return(sum(colSums(xj_s)^2))
}

Schwefel221 <- function(x) {
  # interval: [-100,100]^n
  return(max(abs(x)))
}

DeJong2 <- function(x) {
  # interval: [-30,30]^n (Rosenbrock function)
  n <- length(x)
  xi <- x[1:(n-1)]
  xnext <- x[2:n]
  
  return(sum(100*(xnext - xi^2)^2 + (xi - 1)^2))
}

StepFun <- function(x) {
  # interval: [-100,100]^n
  return(sum(floor(x[1:(length(x)-1)] + 0.5)^2))
}

DeJong4 <- function(x) {
  # interval: [-1.28,1.28]^n
  return(sum(1:length(x)*x^4) + runif(1))
}


### 30-D multimodal functions ###

Schwefel226 <- function(x) {
  # interval: [-500,500]^n
  return(sum(-x*sin(sqrt(abs(x)))))
}

Rastrigin <- function(x) {
  # interval: [-5.12,5.12]^n
  return(sum(x^2 - 10*cos(2*pi*x) + 10))
}

Ackley <- function(x) {
  # interval: [-30,30]^n
  n <- length(x)
  return(-20*exp(-0.2*sqrt(sum(x^2)/n)) - exp(sum(cos(2*pi*x))/n) + 20 + exp(1))
}

Griewank <- function(x) {
  # interval: [-600,600]^n
  return(sum(x^2)/4000 - prod(cos(x/sqrt(1:length(x)))) + 1)
}

### Helper function for the penalized functions ###

u <- function(xi,a,k,m) {
  if (xi > a) {
    return(k*(xi - a)^m)
  } else if (xi < -a) {
    return(k*(-xi - a)^m)
  } else {
    return(0)
  }
}

Penalized1 <- function(x) {
  # interval: [-50,50]^n, uses helpers.R
  n <- length(x)
  y <- 1 + 0.25*x
  yi <- y[1:(n-1)]
  ynext <- y[2:n]
  
  sum1 <- 0; sum2 <- 0
  if (n > 1) {
    sum1 <- sum((yi - 1)^2*(1 + 10*sin(pi*ynext)^2))
    for (i in 1:n) {
      sum2 <- sum2 + u(x[i],10,100,4)
    }
  }
  
  return((pi/n)*(10*sin(pi*y[1])^2 + sum1 + (y[n] - 1)^2) + sum2)
}

Penalized2 <- function(x) {
  # interval: [-50,50]^n, uses helpers.R
  n <- length(x)
  xi <- x[1:(n-1)]
  xnext <- x[2:n]
  
  sum1 <- 0; sum2 <- 0
  if (n > 1) {
    sum1 <- sum((xi - 1)^2*(1 + sin(3*pi*xnext)^2))
    for (i in 1:n) {
      sum2 <- sum2 + u(x[i],5,100,4)
    }
  }
  return(0.1*(sin(3*pi*x[1])^2 + sum1 + (x[n] - 1)^2*(1 + sin(2*pi*x[n])^2)) + sum2)
}


### Low/Fixed-D multimodal functions ###

DeJong5 <- function(x) {
  # interval: [-65.536,65.536]^2 (Shekel's Foxholes)
  j <- 1:25
  c <- seq(-32,32,16)
  a1 <- rep(c,5)
  a2 <- rep(c,each=5)
  
  return(1/(2e-3 + sum(1/(j + (x[1] - a1)^6 + (x[2] - a2)^6))))
}

Kowalik <- function(x) {
  # interval: [-5,5]^4
  a <- c(0.1957,0.1947,0.1735,0.16,0.0844,0.0627,0.0456,0.0342,0.0323,0.0235,0.0246)
  b <- c(2^(2:0),1/seq(2,16,2))
  b_sq <- b^2
  
  return(sum((a - x[1]*(b_sq + b*x[2])/(b_sq + b*x[3] + x[4]))^2))
}

SixHumpCamel <- function(x) {
  # interval: [-5,5]^2
  return(4*x[1]^2 - 2.1*x[1]^4 + x[1]^6/3 + x[1]*x[2] - 4*x[2]^2 + 4*x[2]^4)
}

BraninHoo <- function(x) {
  # interval: [-5,10] x [0,15]
  return((x[2] - 5.1*x[1]^2/(4*pi^2) + 5*x[1]/pi - 6)^2 + 10*(1 - 1/(8*pi))*cos(x[1]) + 10)
}

GoldsteinPrice <- function(x) {
  # interval: [-2,2]^2
  Fact1 <- 1 + (x[1] + x[2] + 1)^2*(19 - 14*(x[1] + x[2]) + 3*(x[1]^2 + x[2]^2) + 6*x[1]*x[2])
  Fact2 <- 30 + (2*x[1] - 3*x[2])^2*(18 + x[1]*(12*x[1] - 36*x[2] - 32) + x[2]*(27*x[2] + 48))
  return(Fact1*Fact2)
}

Hartmann3 <- function(x) {
  # interval: [0,1]^3
  c <- c(1,1.2,3,3.2)
  A <- matrix(rep(c(3,10,30,0.1,10,35),2),3)
  P <- matrix(10^(-4)*c(3689,1170,2673,4699,4387,7470,1091,8732,5547,381,5743,8828),3)
  return(-sum(c*exp(-colSums(A*(matrix(rep(x,4),3) - P)^2))))
}

Hartmann6 <- function(x) {
  # interval: [0,1]^6
  c <- c(1,1.2,3,3.2)
  A <- matrix(c(10,3,17,3.5,1.7,8,0.05,10,17,0.1,8,14,3,3.5,1.7,10,17,8,17,8,0.05,10,0.1,14),6)
  P <- matrix(10^(-4)*c(1312,1696,5569,124,8283,5886,2329,4135,8307,3736,1004,9991,
                        2348,1451,3522,2883,3047,6650,4047,8828,8732,5743,1091,381),6)
  return(-sum(c*exp(-colSums(A*(matrix(rep(x,4),6) - P)^2))))
}

Shekel5 <- function(x)
{
  # interval: [0,10]^4
  c <- 0.1*c(1,2,2,4,4,6,3,7,5,5)[1:5]
  A <- matrix(c(rep(c(4,1,8,6),each=4),3,7,3,7,2,9,2,9,5,5,3,3,8,1,8,1,6,2,6,2,7,3.6,7,3.6),4)[,1:5]
  return(-sum(1/(colSums((matrix(rep(x,5),4) - A)^2) + c)))
}

Shekel7 <- function(x)
{
  # interval: [0,10]^4
  c <- 0.1*c(1,2,2,4,4,6,3,7,5,5)[1:7]
  A <- matrix(c(rep(c(4,1,8,6),each=4),3,7,3,7,2,9,2,9,5,5,3,3,8,1,8,1,6,2,6,2,7,3.6,7,3.6),4)[,1:7]
  return(-sum(1/(colSums((matrix(rep(x,7),4) - A)^2) + c)))
}

Shekel10 <- function(x)
{
  # interval: [0,10]^4
  c <- 0.1*c(1,2,2,4,4,6,3,7,5,5)[1:10]
  A <- matrix(c(rep(c(4,1,8,6),each=4),3,7,3,7,2,9,2,9,5,5,3,3,8,1,8,1,6,2,6,2,7,3.6,7,3.6),4)[,1:10]
  return(-sum(1/(colSums((matrix(rep(x,10),4) - A)^2) + c)))
}

