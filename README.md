# This repository contains some of the work completed by me and another student during an advanced R course in my master's program!

- lab 2: Functions to solve simple problems with base R <br> 

## The following labs are created as packages which might make it a bit tricky to find the code, try looking in the "R"-folder in each lab!  
<br>
- lab 3: Dijkstra and euclidean algorithm<br><br>
- lab 4: Two RC-class objects, one to do an linear regression and one for ridge regression<br><br>
- lab 5: A package to download data from an API and then genererate some graphs, a shiny app is also created here but is in a seperate repository named Shiny-BaDaAn<br><br>
- lab 6: Solving the knapsack problem with brute force, greedy heuristic and programming search<br><br>

<br>

#### If you have any questions regarding the work, please contact me

# Example of the dijkstra algortihm

```R
# Data from wikipedia

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra <-
function(graph, init_node){
  # checking the input
  stopifnot(is.data.frame(graph), colnames(graph) %in% c('v1','v2','w'),
            is.numeric(init_node) && length(init_node) == 1,
            init_node %in% graph$v1 ||init_node %in% graph$v2 )
  
  Q <- unique(graph$v1) # Creating variable Q(each node)
  
  dist <- rep(Inf,  length(Q)) # Creating the dist and prev vectors
  
  dist[init_node] <- 0 # The distance to itself is 0
  
  while(length(Q) > 0) { # While the Q variable is longer than 0
    u <- Q[which.min(dist[Q])] # Takes the node with the shortest distance
    
    Q <- setdiff(Q,u) # Remove that node from Q
    
    # Indice all elements in v1 for the rows in v1 that are the value of u
    neighbors <- c(graph$v2[graph$v1 == u])
    
    for (v in neighbors){ # looping over all neighbors
      # Distance u + distance for all neighbors of u and v
      alt <-  dist[u] + graph$w[which((graph$v1 == u & graph$v2 == v ))]
      if (alt < dist[v]){ # updating the distance if its shorter than to the previous neighbor
        dist[v] <- alt
        
      }
    }
    
  }
  return(dist)   
}

```



# Example of a ridge regression-object with some simple functionalities

```R
ridgereg <- setRefClass('ridgereg', fields = list(formula = 'formula',
                                                  data = 'data.frame',
                                                  lambda = 'numeric',
                                                  Y= 'numeric',
                                                  X = 'matrix',
                                                  Xnorm = 'matrix',
                                                  beta_hat='matrix', y_hat='array',
                                                  df_name='character')
                                                  
                                                  ,
                        methods = list(
                          initialize = function(formula, data, lambda){
                            
                            .self$data <- data
                            .self$formula <- formula
                            # checking the input
                            stopifnot(length(all.vars(formula)) >= 1,
                                      is.data.frame(data),all.vars(formula) %in% colnames(data),
                                      lambda>=0)
                            
                            # picking out my variables from the formula
                            .self$Y <- .self$data[,all.vars(.self$formula)[1]]
                            
                            .self$X <- model.matrix(.self$formula, .self$data)[,-1]
                            
                            # getting the sd and mean of the X-matrix
                            Xsd <-apply(X,2,sd)
                            mean_X <- apply(X, 2, mean)
                            
                            # Normalize the numeric variables in the formula and creating the X matrix
                            .self$Xnorm <- scale(X,center=TRUE)
    
                            
                            # Regression calculations
                            # creating the identity matrix with values from lambda instead of 1
                            Iden <- diag(x=lambda, nrow = ncol(t(.self$Xnorm) %*% .self$Xnorm))
                            
                            .self$beta_hat <- solve(t(.self$Xnorm) %*% .self$Xnorm + Iden) %*%
                             (t(.self$Xnorm) %*% Y)
                            
                            # the beta parameters
                            b <- .self$beta_hat/Xsd
                            
                            intercept <- mean(.self$Y) - sum(b*mean_X)
                            
                            .self$beta_hat <- rbind(intercept,b)
                            
                            
                            # Estimating Y_hat
                            .self$y_hat <- as.matrix(cbind(const=1,.self$X)) %*% .self$beta_hat

                            
                            # taking the name of the data frame
                            .self$df_name <-  deparse(substitute(.self$data))
                          },
                          
                          print = function(){
                            
                            "Printing the formula and the coefficients" 
                            # making it look like printing the lm-function
                            cat("Call:","\nlinreg(formula = ", format(.self$formula),", data = ",
                                format(.self$df_name),")\n\n", sep="")
                            
                            cat('Coefficients:\n')
                            
                            mat <- c(t(.self$beta_hat))
                            names(mat) <- colnames(t(.self$beta_hat))
                            base::print(round(mat,3))
                            
                            
                          },
                          
                          show = function(){
                            
                            "The coefficients as a named vector when printing the object"
                            # the coefficients as a named vector
                            vec <- as.vector(.self$beta_hat)
                            names(vec) <- rownames(.self$beta_hat) # giving it the names
                            base::print(vec)
                            },
                          
                          pred = function(newdata=data){
                            "Return the predicted Y values of training data or new data"
                            
                            # Checking the input
                            stopifnot(all.vars(.self$formula)[-1] %in% colnames(newdata))
                            
                            # creating a formula without y, as the new data probably don't have y
                            form <- as.formula(stringr::str_flatten(.self$formula[-2]))
                            
                            x <- model.matrix(form, newdata)
            
                            pred_Y <- x  %*%  .self$beta_hat # calculate the predictions
                            
                            base::print(as.vector(pred_Y))
                            },
                          
                          
                          
                          coef = function(){
                            "The coefficients as a named vector"
                            # the coefficients as a named vector
                            vec <- as.vector(.self$beta_hat)
                            names(vec) <- rownames(.self$beta_hat) # giving it the names
                            base::print(vec)}
                          
                            
                     )
                 )

```



<br><br><br>


<div align="center">
  <img src="https://media4.giphy.com/media/v1.Y2lkPTc5MGI3NjExNWF1ejkyMHJueWdtd29xeWN6bHBmZ2M0cjZqbW1wbm93MzJ6M3B2bCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/xVRRDVP6lqtNQJrzN7/giphy.gif" width="600" height="300"/>
</div>

