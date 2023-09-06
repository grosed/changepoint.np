
library(changepoint)

## import original code 
source("hyeyoung-maeng-original.R")


## generate data
set.seed(0)
# single change point at (1/2)
int.length = 200
true.cpt <- c(int.length)+1
jump.size = 1
mu0 <- c(rep(0, 2*int.length))
mu <- c(rep(0, int.length), rep(jump.size, int.length))
n <- length(mu)
x = rnorm(n) + mu


## generate quantiles
QC = c(2, 4, 8)
m = 2
K = min(round(n*0.9), round(QC[m]*log(n)))
qnt = qnt.seq(x, K=K)



## generate a dictionary for the results
test.data <- new.env()

## generate test data for np.average
key.root <- "average-alpha="
alphas <- seq(0.1,1.0,0.1)
for(alpha in alphas)
{
  key <- paste(key.root,alpha,sep="")
  test.data[[key]] <- PELT(x, qnt=qnt, thr.c=3*alpha)
}

## generate test data for np.max
key.root <- "max-alpha="
for(alpha in alphas)
{
  key <- paste(key.root,alpha,sep="")
  test.data[[key]] <- PELT_max(x, qnt=qnt, thr.c=1*alpha)
}

## generate test data for np.conditional
key.root <- "conditional-alpha="
for(alpha in alphas)
{
  key <- paste(key.root,alpha,sep="")
  test.data[[key]] <- PELT_c_with_penalty(x, qnt=qnt, 24*alpha)
}


## generate test data for normal.mean
key.root <- "normal-mean-beta="
betas <- 2*log(length(x))*seq(0.1,1.0,0.1)
for(beta in betas)
{
  key <- paste(key.root,beta,sep="")
  test.data[[key]] <-  cpt.mean(x,penalty="Manual",method="PELT",pen.value=beta)@cpts
}


## serialise the dictionary
save(test.data,file="test.data.RData")





