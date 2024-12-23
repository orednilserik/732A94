# Adv R lab 6
RNGversion(min(as.character(getRversion()),"3.5.3"))

##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
rm(n)

#### Dynamic programming ####
knapsack_dynamic <- function(x, W){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W, is.numeric(x$w), is.numeric(x$v),W>0)
  n <- nrow(x) # how many objects that are to be determined
  m <- matrix(0, nrow = n, ncol = W) # an empty matrix with the properties resulting from the weight and objects
  
  for(i in 2:n){ # row wise indexing
    for(j in 1:W){ # column wise indexing
      if(x$w[i] <= j){
        m[i, j] <- max(m[i-1, j], m[i-1,j-x$w[i]] + x$v[i])
      } else {
        m[i, j] <- m[i-1, j]
      }
    }
  }
  
  
  value <- round(m[n, W], digits = 0)

  
  final<-c()
  i <- n 
  j <- W
  while ( i > 0 && j > 0){
    if(isTRUE(m[i,j]!=m[i-1,j])){
      final<-c(final,i)
      j<-j-x$w[i]
    }
    i<-i-1
  }
  return(list("value"=value,"elements"=final))
}

#### Greedy heuristic #### 

# Creating the function with 2 arguments
greedy_knapsack <- function(x, W){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W, is.numeric(x$w), is.numeric(x$v),W>0)
  
  # Reorder the rows with value/weight in decreasing order
  # to add as much value per weight as possible
  x <- x[order(x$v/x$w, decreasing = TRUE),]
  
  # creating objects for the loop
  value <- 0 # values for the print
  
  elements <- c() # element list for the print
  
  knapsack_capacity <- W # current capacity of the knapsack
  k<- 1
  
  # While loop until the knapsack is "full" S1
  while(knapsack_capacity >= x$w[k]){
    
    # Updating the current capacity and adding the elements
    
    value <- value +x$v[k]
    elements[k] <- as.numeric(rownames(x)[k]) 
    knapsack_capacity <- knapsack_capacity - x$w[k]
    k <- k+1
  }
  lis <- list('value'=round(value,0), 'elements'= elements)
  return(lis)
  
}

#### Brute force problem ####
library(foreach)
library(doParallel)
# inspo https://rdrr.io/github/amemem/lab6package/src/R/knapsack_brute_force.R

brute_force_knapsack <- function(x,W, parallel=FALSE){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W, is.numeric(x$w), is.numeric(x$v),W>0)
  
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
      idx  <- base::intToBits(i) ==1
      
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
    idx  <- base::intToBits(i) ==1
    
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


package.skeleton('knapsacked')


