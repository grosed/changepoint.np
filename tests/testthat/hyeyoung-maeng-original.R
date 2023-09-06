
#######################################################
########## Functions



######### ADDED BY DJG 20-06-2023 to get a wider range of test data for PELT_c
### PELT-conditional (section 2.2)
PELT_c_with_penalty = function (x, qnt, penalty) {
  
  # initialization
  K = length(qnt)
  
  C = costMatrix_c(x, qnt)
  
  N = length(x)
  
  # penalty = K + 2*sqrt(K*thr.c*log(N)) + 2*thr.c*log(N)
  
  Fvec = rep(0, N + 1)
  Fvec[1] = -penalty
  cngPoints = list(NULL) # initializing a null list
  Rvec = 0
  
  for (t in 1:N) {
    
    # compute all the costs up to time t
    aggregatedC <- C[(Rvec + 1), t, drop=F]

    partitionsCosts = Fvec[Rvec + 1] + aggregatedC + penalty
    
    # get the new F(t) and its relative changepoint between R
    Fvec[(t) + 1] = min(partitionsCosts)
    cngPoint = Rvec[which.min(partitionsCosts)]
    cngPoints[[t+1]] = c(cngPoints[[cngPoint + 1]], cngPoint + 1)
    
    # make a vector of the same lenth to filter based on F(t)
    filter = (Fvec[Rvec + 1] + aggregatedC) <= Fvec[(t) + 1]
    
    # append the new time to values in R that meet the condition
    Rvec = c(Rvec[filter], t)
    
  }
  
  return(cngPoints[[N]][-1])
  
}


### Haynes et al (2017)
PELT_with_penalty = function (x, qnt, penalty) {
  
  # initialization
  K = length(qnt)
  
  C = costMatrix(x, qnt)
  
  N = length(x)
  
  # penalty = thr.c*log(N)
  
  Fvec = rep(0, N + 1)
  Fvec[1] = -penalty
  cngPoints = list(NULL) # initializing a null list
  Rvec = 0
  
  for (t in 1:N) {
    
    # compute all the costs up to time t
    meanC <- -2*log(2*N-1)*colMeans(-C[, (Rvec + 1), t, drop=F])
    partitionsCosts = Fvec[Rvec + 1] + meanC + penalty
    
    # get the new F(t) and its relative changepoint between R
    Fvec[(t) + 1] = min(partitionsCosts)
    cngPoint = Rvec[which.min(partitionsCosts)]
    cngPoints[[t+1]] = c(cngPoints[[cngPoint + 1]], cngPoint + 1)
    
    # make a vector of the same lenth to filter based on F(t)
    filter = (Fvec[Rvec + 1] + meanC) <= Fvec[(t) + 1]
    #print(c(filter))
    
    # append the new time to values in R that meet the condition
    # Rvec = c(Rvec[filter], t)
    Rvec = c(Rvec, t)
  }
  
  return(cngPoints[[N]][-1])
  
}




### a sequence of quantiles
qnt.seq <- function(x, K=NULL, method="nonequally"){
  
  n <- length(x)
  if(is.null(K)){
    K <- round(4*log(n))
  }
  Q <- matrix(0, K, n+1)
  sorted.x = sort(x)
  yK = -1 + (2*(1:K)/K-1/K)
  c = -log(2*n-1)
  if(method=="equally"){
    pK = seq(0, 1, length.out = K+2)[-c(1, K+2)]
  }else{
    pK  = (1+exp(c*yK))^-1
  }
  
  qnt = rep(NA, K)
  for (i in 1:K){
    j  = as.integer((n-1)*pK[i] + 1)
    qnt[i] = sorted.x[j]
  }
  return(qnt)
  
}

### minus log-likelihood
minloglik <- function(x, Q, i, j){
  
  if (i>=j) {
    min.log.lik <- 0 
  }else{
    t <- j-i+1
    mi <- sum(x[i:j] < Q) + 0.5*sum(x[i:j]==Q)
    
    if(mi==0 | mi==t){
      min.log.lik <- 0 
    }else{
      min.log.lik <- -mi*log(mi/t)-(t-mi)*log(1-mi/t) 
    }
    
  }
  
  return(min.log.lik)  
  
}

### cost function for PELT
costMatrix = function(x, qnt) {
  
  N = length(x)
  K = length(qnt)
  C <- array(NA, dim=c(length(qnt), N, N))
  
  for(k in 1:K){
    for (i in 1:N){
      for (j in 1:N){
        C[k, i, j] = minloglik(x=x, Q=qnt[k], i=i, j=j)
      }
    }
  }
  
  return(C)
}

### cost function for PELT-conditional
costMatrix_c = function(x, qnt) {
  
  N = length(x)
  K = length(qnt)
  
  C = matrix(nr = N, nc = N)
  #############################################################################
  ### NOTE - THIS LINE HAS BEEN MODIFIED FORM THE ORIGINAL - DJG 20-06-2023  ###
  # qnt0 = c(0, qnt, Inf)
  qnt0 = c(-Inf, qnt, Inf)
  ############################################################################
  for (i in 1:N){
    
    for (j in 1:N){
      
      if (i>=j) {
        
        min.log.lik <- 0 
        
      }else{
        
        t <- j-i+1
        mi <- rep(NA, (length(qnt0)-1))
        f_mi <- rep(NA, (length(qnt0)-1))
        
        for(l in 1:(length(qnt0)-1)){
          
          mi[l] <- sum(((qnt0[l] < x[i:j])+(x[i:j] <= qnt0[l+1]))==2)
          
          if(mi[l]==0 | mi[l]==t){
            f_mi[l] <- 0 
          }else{
            f_mi[l] <- -mi[l]*log(mi[l]/t)
          }
          
        }
        
        min.log.lik <- sum(f_mi)
        
      }
      
      C[i, j] = min.log.lik
      
    }
  }
  
  return(C)
}

### Haynes et al (2017)
PELT = function (x, qnt, thr.c=3) {
  
  # initialization
  K = length(qnt)
  
  C = costMatrix(x, qnt)
  
  N = length(x)
  
  penalty = thr.c*log(N)
  
  Fvec = rep(0, N + 1)
  Fvec[1] = -penalty
  cngPoints = list(NULL) # initializing a null list
  Rvec = 0
  
  for (t in 1:N) {
    
    # compute all the costs up to time t
    meanC <- -2*log(2*N-1)*colMeans(-C[, (Rvec + 1), t, drop=F])
    partitionsCosts = Fvec[Rvec + 1] + meanC + penalty
    
    # get the new F(t) and its relative changepoint between R
    Fvec[(t) + 1] = min(partitionsCosts)
    cngPoint = Rvec[which.min(partitionsCosts)]
    cngPoints[[t+1]] = c(cngPoints[[cngPoint + 1]], cngPoint + 1)
    
    # make a vector of the same lenth to filter based on F(t)
    filter = (Fvec[Rvec + 1] + meanC) <= Fvec[(t) + 1]
    #print(c(filter))
    
    # append the new time to values in R that meet the condition
    Rvec = c(Rvec[filter], t)
    
  }
  
  return(cngPoints[[N]][-1])
  
}

### PELT-conditional (section 2.2)
PELT_c = function (x, qnt, thr.c=8) {
  
  # initialization
  K = length(qnt)
  
  C = costMatrix_c(x, qnt)
  
  N = length(x)
  
  penalty = K + 2*sqrt(K*thr.c*log(N)) + 2*thr.c*log(N)
  
  Fvec = rep(0, N + 1)
  Fvec[1] = -penalty
  cngPoints = list(NULL) # initializing a null list
  Rvec = 0
  
  for (t in 1:N) {
    
    # compute all the costs up to time t
    aggregatedC <- C[(Rvec + 1), t, drop=F]

    partitionsCosts = Fvec[Rvec + 1] + aggregatedC + penalty
    
    # get the new F(t) and its relative changepoint between R
    Fvec[(t) + 1] = min(partitionsCosts)
    cngPoint = Rvec[which.min(partitionsCosts)]
    cngPoints[[t+1]] = c(cngPoints[[cngPoint + 1]], cngPoint + 1)
    
    # make a vector of the same lenth to filter based on F(t)
    filter = (Fvec[Rvec + 1] + aggregatedC) <= Fvec[(t) + 1]
    
    # append the new time to values in R that meet the condition
    Rvec = c(Rvec[filter], t)
    
  }
  
  return(cngPoints[[N]][-1])
  
}

### PELT-max (section 2.3)
PELT_max = function (x, qnt, thr.c=3) {
  
  # initialization
  K = length(qnt)
  
  C = costMatrix(x, qnt)
  
  N = length(x)
  
  penalty = thr.c*log(N)
  #penalty = 1 + 2*sqrt(1*thr.c*log(N)) + 2*thr.c*log(N)
  
  Fvec = rep(0, N + 1)
  Fvec[1] = -penalty
  cngPoints = list(NULL) # initializing a null list
  Rvec = 0
  
  for (t in 1:N) {
    
    # compute all the costs up to time t
    maxC <- apply(2*C[, (Rvec + 1), t, drop=F], 2, max)
    partitionsCosts = Fvec[Rvec + 1] + maxC + penalty
    
    # get the new F(t) and its relative changepoint between R
    Fvec[(t) + 1] = min(partitionsCosts)
    cngPoint = Rvec[which.min(partitionsCosts)]
    cngPoints[[t+1]] = c(cngPoints[[cngPoint + 1]], cngPoint + 1)
    
    # make a vector of the same lenth to filter based on F(t)
    filter = (Fvec[Rvec + 1] + maxC) <= Fvec[(t) + 1]
    #print(c(filter))
    
    # append the new time to values in R that meet the condition
    Rvec = c(Rvec[filter], t)
    
  }
  
  return(cngPoints[[N]][-1])
  
}