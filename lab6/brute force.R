# Brute force problem
library(foreach)
library(doParallel)
library(profvis)

# inspo https://rdrr.io/github/amemem/lab6package/src/R/knapsack_brute_force.R

brute_force_knapsack <- function(x,W, parallel=FALSE){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W)
  
  # If parralell is false then do..
  
  if(parallel==FALSE){
  # creating objects for the brute force loop
  value <- 0
  # for loop to get the combinations and make the calculations
  for(i in 1:2^nrow(x)-1){
    # get the combinations
    idx  <- intToBits(i) == 1

    # Use the combinations to pick out the rows in x
    # if the weight for the combination is lower than W
    if(sum(x$w[idx]) <= W && sum(x$v[idx]) > value){
      value <- sum(x$v[idx])

      elements <- as.numeric(rownames(x[idx,]))
      
    } 
  }
  
  lis <- list('value'=round(value,0), 'elements'= elements)
  return(lis)
  }
  
  else{
    value <- 0
    elements <- c()
     # making a function which can be called for the parallelize
    func <- function(i){
      
      # get the combinations
      idx  <- intToBits(i) ==1

      # Use the combinations to pick out the rows in x
      # if the weight for the combination is lower than W
      if(sum(x$w[idx]) <= W && sum(x$v[idx]) > value){
        value <- sum(x$v[idx])

        elements <- as.numeric(rownames(x[idx,]))
        
      } 
      
      return(list('value'=round(value,0), 'elements'= elements))
    }
    
    # checking coores on the cumputer
    core <- parallel::detectCores()
    cl <- parallel::makeCluster(core, type = "PSOCK")
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl,c('x','func','W','value','elements'),envir=environment())
    
    
    results <- parallel::parSapply(cl,(1:2^nrow(x)-1),func)
    comb <- results[,which.max(results['value',])]
    return(comb)
    parallel::stopCluster(cl)
      
    }
    
}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)


system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))

system.time(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE))


# it goes faster without parallelizing as to split the task takes more time 
# then the actual task. 


## With sapply instead of for loop. 
brute_force_knapsack <- function(x,W, parallel=FALSE){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W)
  func <- function(i){
    
    # get the combinations
    idx  <- intToBits(i) ==1
    
    # Use the combinations to pick out the rows in x
    # if the weight for the combination is lower than W
    if(sum(x$w[idx]) <= W && sum(x$v[idx]) > value){
      value <- sum(x$v[idx])
      elements <- as.numeric(rownames(x[idx,]))
      }
     
    return(list('value'=round(value,0), 'elements'= elements))
  }
  # If parralell is false then do..
  
  if(parallel==FALSE){
    value <- 0
    elements <- c()
    # making a function which can be called for the parallelize
   
    res <- sapply((1:2^nrow(x)-1),func)
    comb <- res[,which.max(res['value',])]
    return(comb)
  }
  
  else{
    value <- 0
    elements <- c()
    # checking coores on the cumputer
    core <- parallel::detectCores()
    cl <- parallel::makeCluster(core-1, type = "PSOCK")
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl,c('x','func','W','value','elements'),envir=environment())
    
    
    results <- parallel::parSapply(cl,(1:2^nrow(x)-1),func)
    comb <- results[,which.max(results['value',])]
    return(comb)
    parallel::stopCluster(cl)
    
  }
  
}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)


system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))

system.time(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE))





profvis(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))

        