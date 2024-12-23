# Dynamic programming

knapsack_dynamic <- function(x, W){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W, is.numeric(x$w), is.numeric(x$v),W>0)
  
  n <- nrow(x) # how many objects that are to be determined
  
  m <- matrix(0, nrow = n, ncol = W) # an empty matrix with the properties resulting from the weight and objects
  
  
  for(i in 2:n){ # row wise indexing
    
    for(j in 2:W){ # column wise indexing
      
       if(x$w[i] <= j){
       
        m[i, j] <- max(m[i-1, j], m[i-1,j-x$w[i]] + x$v[i])
        
      } else {
        
        m[i, j] <- m[i-1, j]
        
      }
      
    }
    
  }
  
  final<-c()
  i<- n 
  j <- W
  while ( i > 1 && j > 0){
    if(m[i,j] != m[i-1,j]){
      final<-c(final,i)
      j<-j-x$w[i]
    }
    i<-i-1
  }
  
  res <- max(m)
  return(list('value'=round(res,0), 'elements' = final))
}



profvis(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))

system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))

knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)




