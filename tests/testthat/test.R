

load("test.data.RData")

library(changed)
source("hyeyoung-maeng-original.R")

test_that("test quantiles against original R code",
{

	## generate data
	set.seed(0)
	# single change point at (1/2)
	int.length = 200
	true.cpt <- c(int.length)+1
	jump.size = 1
	mu0 <- c(rep(0, 2*int.length))
	mu <- c(rep(0, int.length), rep(jump.size, int.length))
	n <- length(mu)
	X = rnorm(n) + mu

        for(k in 2:30)
        {
          expected <- c(-Inf,qnt.seq(X,k),Inf)
	  actual <- quantiles(X,k,"notequal")
	  expect_equal(expected,actual)	
        }

})

test_that("test np.average against original R code",
{
	## generate data
	set.seed(0)
	# single change point at (1/2)
	int.length = 200
	true.cpt <- c(int.length)+1
	jump.size = 1
	mu0 <- c(rep(0, 2*int.length))
	mu <- c(rep(0, int.length), rep(jump.size, int.length))
	n <- length(mu)
	X = rnorm(n) + mu

	## generate quantiles
	qnts <- quantiles(X,24,method="nonequal")

        ## check package against test data
	key.root <- "average-alpha="
	alphas <- seq(0.1,1.0,0.1)
	for(alpha in alphas)
	{
           key <- paste(key.root,alpha,sep="")
	   ## expected value from test data
	   expected <- c(1,test.data[[key]])
	   ## actual value using package
	   res <- cpt.np.average(X,qnts,alpha*3*log(length(X)))
	   actual <- changepoints(res)$start
           ## test it
     	   expect_equal(expected,actual)	
	}

})


test_that("test np.max against original R code",
{
	## generate data
	set.seed(0)
	# single change point at (1/2)
	int.length = 200
	true.cpt <- c(int.length)+1
	jump.size = 1
	mu0 <- c(rep(0, 2*int.length))
	mu <- c(rep(0, int.length), rep(jump.size, int.length))
	n <- length(mu)
	X = rnorm(n) + mu

	## generate quantiles
	qnts <- quantiles(X,24,method="nonequal")

        ## check package against test data
	key.root <- "max-alpha="
	alphas <- seq(0.1,1.0,0.1)
	for(alpha in alphas)
	{
           key <- paste(key.root,alpha,sep="")
	   ## expected value from test data
	   expected <- c(1,test.data[[key]])
	   ## actual value using package
	   res <- cpt.np.max(X,qnts,alpha*3*log(length(X)))
	   actual <- changepoints(res)$start
	   ## show it
	   #print("--------------------------------------------")
	   #print(expected)
	   #print("*****************************************")		
	   #print(actual)
	   #print("--------------------------------------------")
           ## test it
     	   expect_equal(expected,actual)	
	}

})


test_that("test np.conditional against original R code",
{
	## generate data
	set.seed(0)
	# single change point at (1/2)
	int.length = 200
	true.cpt <- c(int.length)+1
	jump.size = 1
	mu0 <- c(rep(0, 2*int.length))
	mu <- c(rep(0, int.length), rep(jump.size, int.length))
	n <- length(mu)
	X = rnorm(n) + mu

	## generate quantiles
	qnts <- quantiles(X,24,method="nonequal")

        ## check package against test data
	key.root <- "conditional-alpha="
	alphas <- seq(0.1,1.0,0.1)
	for(alpha in alphas)
	{
           key <- paste(key.root,alpha,sep="")
	   ## expected value from test data
	   expected <- c(1,test.data[[key]])
	   ## actual value using package
	   res <- cpt.np.conditional(X,qnts,24*alpha)
	   actual <- changepoints(res)$start
           ## test it
     	   expect_equal(expected,actual)	
	}

})


test_that("test normal.mean against changepoint",
{
	## generate data
	set.seed(0)
	# single change point at (1/2)
	int.length = 200
	true.cpt <- c(int.length)+1
	jump.size = 1
	mu0 <- c(rep(0, 2*int.length))
	mu <- c(rep(0, int.length), rep(jump.size, int.length))
	n <- length(mu)
	X = rnorm(n) + mu
	key.root <- "normal-mean-beta="
	betas <- 2*log(length(X))*seq(0.1,1.0,0.1)
	for(beta in betas)
	{
	   key <- paste(key.root,beta,sep="")
	   ## expected value from test data
	   expected <- test.data[[key]]
	   ## actual value using package
	   res <- cpt.normal.mean(X,beta)
	   actual <- changepoints(res)$end
           ## test it
           expect_equal(expected,actual)			
	}
})