library(MASS)
library(aod)

N <- 30
M <- 100

b0 <- 1
b1 <- 1.5
v <- 4
Sigma <- matrix(data=v * 0.9, nrow=N, ncol=N)
diag(Sigma) <- v
br <- N / 2

# a
x <- matrix(nrow=M, ncol=N)
y <- matrix(nrow=M, ncol=N)
wt <- numeric(M)
for (ite in 1:M)
{
  x[ite,] <- runif(N, 5, 20)
  centr <- x[ite,] - mean(x[ite,])
  y[ite,] <- b0 + centr * b1 + rnorm(N, 0, v)
  model <- lm(y[ite,] ~ centr)
  wt[ite] <- wald.test(b=coef(model), Sigma=vcov(model),
                       Terms = 1:2, H0 = c(1,0))$result$chi2[3]
}
hist(y, breaks=br)
hist(wt, breaks=br)


# b
x <- matrix(nrow=M, ncol=N)
y <- matrix(nrow=M, ncol=N)
wt <- numeric(M)
for (ite in 1:M)
{
  x[ite,] <- runif(N, 5, 20)
  centr <- x[ite,] - mean(x[ite,])
  y[ite,] <- b0 + centr * b1 + rnorm(N, 0, v * x[ite,])
  model <- lm(y[ite,] ~ centr)
  wt[ite] <- wald.test(b=coef(model), Sigma=vcov(model),
                       Terms = 1:2, H0 = c(1,0))$result$chi2[3]
}
hist(y, breaks=br)
hist(wt, breaks=br)

# c
x <- matrix(nrow=M, ncol=N)
y <- matrix(nrow=M, ncol=N)
wt <- numeric(M)
for (ite in 1:M)
{
  x[ite,] <- runif(N, 5, 20)
  centr <- x[ite,] - mean(x[ite,])
  y[ite,] <- b0 + centr * b1 + rt(N, df=6)
  model <- lm(y[ite,] ~ centr)
  wt[ite] <- wald.test(b=coef(model), Sigma=vcov(model),
                       Terms = 1:2, H0 = c(1,0))$result$chi2[3]
}
hist(y, breaks=br)
hist(wt, breaks=br)

# d
x <- matrix(nrow=M, ncol=N)
y <- matrix(nrow=M, ncol=N)
wt <- numeric(M)
for (ite in 1:M)
{
  x[ite,] <- runif(N, 5, 20)
  centr <- x[ite,] - mean(x[ite,])
  y[ite,] <- b0 + centr * b1 + mvrnorm(1, mu = rep(0, N), Sigma=Sigma)
  model <- lm(y[ite,] ~ centr)
  wt[ite] <- wald.test(b=coef(model), Sigma=vcov(model),
                       Terms = 1:2, H0 = c(1,0))$result$chi2[3]
}
hist(y, breaks=br)
hist(wt, breaks=br)