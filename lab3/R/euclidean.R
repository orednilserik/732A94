
#' 
#' @title Euclidean algorithm 
#' 
#' @name Euclidean
#' 
#' @description 
#' Euclidean algorithm which finds the greatest common divisor between two numeric 
#' scalars or integers. https://en.wikipedia.org/wiki/Euclidean_algorithm 
#'
#'@param x  numeric scalar or integer 
#'
#'@param y  numeric scalar or integer 
#'
#'@return The Greatest common divisor of x and y
#'
#'@export
#'
#'@source <https://en.wikipedia.org/wiki/Euclidean_algorithm>

euclidean <-
function(x,y){
  # checking the input 
  stopifnot(is.integer(x) || is.numeric(x) && length(x) == 1 ,is.integer(y) || is.numeric(y) && length(y) == 1)
  
  # Checking which is bigger or smallest
  big <- max(x,y)
  small <- min(x,y)
  
  while (big != 0) { # While the biggest value differ from 0
    t  <- big
    big <- small %% big  # small modulus big
    small <- t # When the small %% big == 0 the latest 'big value' is the GCD
  }
  return(abs(small)) # returning the absolute value of the  GCD
}
