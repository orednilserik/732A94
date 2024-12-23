# 1.1.4 Greedy heuristic 
#

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n = 10000000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


# Creating the function with 2 arguments
greedy_knapsack <- function(x, W){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W)
  
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
# The running time depends on both n and W. 
profvis(greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

system.time(greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))
system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 2000))



# creating objects for the second loop(S2) if necessary
value2 <- 0 # values for the print
weight2 <- 0
elements2 <- c() # element list for the print

knapsack_capacity2 <- W # current capacity of the knapsack

# While loop until the knapsack is "full"
while(knapsack_capacity2 >= x$w[k]){
  
  # Updating the current capacity and adding the elements
  weight2 <- x$w[k]
  value2 <- value2 +x$v[k]
  elements2[k] <- as.numeric(rownames(x)[k])
  knapsack_capacity2 <- knapsack_capacity2 - weight2 
  k <- k+1
}

if(which.max(c(value,value2)) == 1){ # checking which got the 
  lis <- list('value'=round(value,0), 'elements'= elements)
} else 
{
  lis <- list('value'=round(value2,0), 'elements'= elements2)
}




