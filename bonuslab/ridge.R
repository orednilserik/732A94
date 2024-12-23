# Ridge for linmod package

library(MASS)
data=iris
formula = Petal.Length~Species + Sepal.Length
lambda = 5

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
                            # cheking the input
                            stopifnot(length(all.vars(formula)) >= 1,
                                      is.data.frame(data),all.vars(formula) %in% colnames(data))
                            
                            # picking out my variables from the formula
                            .self$Y <- data[,all.vars(formula)[1]]
                            .self$X <-  model.matrix(formula, data)
                            cols <- all.vars(formula)[-1]
                            Xnorm <- as.data.frame(data)
                            
                            
                            # Normalize the numeric variables in the formula and creating the X matrix
                           
                            for (i in cols) {
                              if(is.numeric(data[,i])){
                                Xnorm[,i] <-  (Xnorm[,i] - mean(Xnorm[,i]))/sqrt(var(Xnorm[,i]))
                              }
                                
                            }
                            .self$Xnorm <- model.matrix(formula, Xnorm)
                           
                            # Regression calculations
                            # creating the identity matrix with values from lambda instead of 1
                            Iden <- diag(x=lambda, nrow = ncol(t(.self$Xnorm) %*% .self$Xnorm))
                            .self$beta_hat <- solve(t(.self$Xnorm) %*% .self$Xnorm + Iden) %*% (t(.self$Xnorm) %*% .self$Y)
                            
                            
                            # Estimating Y_hat
                            .self$y_hat <- .self$Xnorm %*% .self$beta_hat

                            
                            # taking the name of the dataframe
                            .self$df_name <-  deparse(substitute(data))
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
                          
                          pred = function(newdata=.self$data){
                            "Return the predicted Y values of training data or new data"
                            
                            # Checking the input
                            stopifnot(all.vars(.self$formula) %in% colnames(newdata))
                            
                            cols <-all.vars(formula)[-1]
                            
                            # Normalize the numeric variables in the formula and creating the X matrix
                            
                            for (i in cols) {
                              if(is.numeric(newdata[,i])){
                                newdata[,i] <-  (newdata[,i] - mean(newdata[,i]))/sqrt(var(newdata[,i]))
                              }
                              
                            }
                            x <- model.matrix(.self$formula, newdata)
                            
                              
                            pred_Y <- x  %*%  .self$beta_hat # calculate the predictions
                            
                            base::print(pred_Y)
                            },
                          
                          
                          
                          coef = function(){
                            "The coefficients as a named vector"
                            # the coefficients as a named vector
                            vec <- as.vector(.self$beta_hat)
                            names(vec) <- rownames(.self$beta_hat) # giving it the names
                            base::print(vec)}
                          
                            
                     )
                 )



x <- ridgereg(formula,iris,lambda=5)

x$coef()
lm.ridge(formula,data,lambda = 5)


