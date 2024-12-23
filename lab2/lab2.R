library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)
name <- "Mikael Montén"
liuid <- "mikmo937"

#### 1.1 Conditional statements ####

# 1.1.1.
sheldon_game <- function(player1, player2){
  stopifnot(player1 %in% c("scissor", "paper", "rock", "spock", "lizard"), player2 %in% c("scissor", "paper", "rock", "spock", "lizard")) # making sure each argument only allows the choices included in the game
  num <- c(scissor = 1, paper = 2, rock = 3, lizard = 4, spock = 5) # encode the choices into numbers for easier handling
  value1 <- num[[player1]] # assign the players choices to an object
  value2 <- num[[player2]]
  if(value1 == value2){ # if the numbers are identical its consideered a draw
    return("Draw!")
  } else if(value1 == 1 & value2 == 2 | value1 == 1 & value2 == 4 | # based on the game rule conditions,
            value1 == 2 & value2 == 3 | value1 == 2 & value2 == 5 | # this else if statement returns that
            value1 == 3 & value2 == 4 | value1 == 3 & value2 == 1 | # player 1 wins if they any one of them are true
            value1 == 4 & value2 == 2 | value1 == 4 & value2 == 5 |
            value1 == 5 & value2 == 1 | value1 == 5 & value2 == 3){
    return("Player 1 wins!")
  } else { # if none of the above holds true, player 2 wins
    return("Player 2 wins!")
  }
}


#### 1.2 for loops ####

# 1.2.1
my_moving_median <- function(x, n, ...){ # the three dots in the argument call is to allow for variability in the functions, which is to be used for na.rm = TRUE
  stopifnot(is.numeric(x), is.vector(x), is.numeric(n), length(n) == 1) # assuring the arguments are of correct type and length
  median <- vector() # empty to store the moving median in
  for(i in 1:(length(x)-n)){ # index from 1 to the length of x - n so the length is exactly as long as the resulting vector from a moving value is
    median[i] <- median(x[i:(n+i)], ...) # store the moving median element-wise in the vector. i is used to account for the moving index.
  }
  return(median)
}

# 1.2.2
for_mult_table <- function(from, to){
  if(!(is.numeric(from) & length(from) == 1) | !(is.numeric(to) & length(to) == 1)){ # making sure the arguments are correct
    stop("'from' and 'to' has to be numeric vectors of length one!") # return a message to adjust the arguments to correct format
  }
  m_mult <- matrix(nrow = length(from:to), ncol = length(from:to)) # create empty matrix
  v_seq <- from:to # sequential vector based on the arguments
  rownames(m_mult) <- from:to # set rownames according to the numbers provided
  colnames(m_mult) <- from:to # set colnames - || -
  for(i in 1:length(v_seq)){ # row-wise indexing from 1 to length of seqnetial vector to have the right structure in the matrix
    for(j in 1:length(v_seq)){ # column-wise indexing
      m_mult[i,j] <- v_seq[i]*v_seq[j] # element-wise add the product for each iteration to the matrix
    }
  }
  return(m_mult)
}

#### 1.3 while loops ####
# 1.3.1
find_cumsum <- function(x, find_sum){ 
  stopifnot(is.numeric(x), is.vector(x), is.numeric(find_sum)) # assuring the arguments are correct
  v_cumsum <- 0 # initial cumulative sum is 0
  i <- 1 # iterative object to make sure the loop knows when to stop
  while(i <= length(x) & v_cumsum < find_sum){ # break when either i has traversed the length of x or the sum is larger
    v_cumsum <- v_cumsum + x[i] # update the cumulative sum iteratively with the current sum + the sum of x for current iteration
    i <- i + 1 # add one "step" to the iteration
  }
  return(v_cumsum)
}


# 1.3.2
while_mult_table <- function(from, to){
  if(!(is.numeric(from) & length(from) == 1) | !(is.numeric(to) & length(to) == 1)){ # assure arguments are correct
    stop("'from' and 'to' has to be numeric vectors of length one!") # error message to adjust the arguments
  }
  m_mult <- matrix(nrow = length(from:to), ncol = length(from:to)) # create empty matrix
  rownames(m_mult) <- from:to # assign row names according to the arguments 
  colnames(m_mult) <- from:to # assign col names according to the arguments
  v_seq <- from:to # sequential vector for the integers including and between the arguments
  i <- 1 # iterating row object
  while(i <= length(v_seq)){ # make sure the amount of rows correspond to the length of the sequential vector
    j <- 1 # iterating column object
    while(j <= length(v_seq)){ # make sur the amount of columns correspond to the length of the sequential vector
      m_mult[i,j] <- v_seq[i]*v_seq[j] # elementwise add the product of the indexed sequential vector to the matrix
      j <- j + 1 # add iterative step
    }
    i <- i + 1 # add iterative step
  }
  return(m_mult)
}

#### 1.4 repeat and loop controls ####

# 1.4.1
repeat_find_cumsum <- function(x, find_sum){
  stopifnot(is.numeric(x), is.vector(x), is.numeric(find_sum)) # check for correct arguments
  v_cumsum <- 0 # initial sum is 0
  i <- 1 # add iterative object
  repeat{
    v_cumsum <- v_cumsum + x[i] # cumulative sum is current + argument indexed at the current iteration
    i <- i + 1 # add step to the iterative object
    if(i > length(x) | v_cumsum >= find_sum){ # once either x has been traversed or the sum been met the function,
      break # BREAKS!
    }
  }
  return(v_cumsum)
}


# 1.4
repeat_my_moving_median <- function(x, n, ...){ # ... argument provides variability in the function to account for na.rm = TRUE
  stopifnot(is.numeric(x), is.vector(x), is.numeric(x), length(n) == 1) # ensure correct arguments are passed
  median <- vector() # create vector to store moving median in
  i <- 1 # iterative object
  repeat{
    median[i] <- median(x[i:(n+i)], ...) # pass the moving median according to the iteration to the median vector
    i <- i + 1 # add one step to the iterative object
    if(i > length(x)-n){ # once i is longer than the supposed length, the function,
      break # BREAKS!
    }
  }
  return(median)
}


#### 1.5 Environment ####
in_environment <- function(env){
  contents <- ls(env) # return the contents of ls env to the an object that stores them as a character vector inherently
  return(contents)
}


#### 1.6 Functionals ####
cov <- function(X){
  stopifnot(is.data.frame(X)) # ensure X is a data frame
  df_cov <- lapply(X = X, function(x) sd(x)/mean(x)) # list apply a function to the data frame that calculate the cov by sd / mean
  return(unlist(df_cov)) # unlist to get it as an array instead of stored in multiple elements
}

#### 1.7 Closures ####
# the formula for the central moment formula was taken from wikipedia
moment <- function(i){
  stopifnot(is.numeric(i)) # make sure i is numeric
  function(x){ # create function inside function
    central <- mean((x-mean(x))^i) # assign the expected value of the observation minus its mean and exponent it with the index to get the central moment of choice
    return(central)
  }
}


#### Optionals ####

cor_matrix <- function(X){
  if(!is.data.frame(X) | !all(sapply(X, is.numeric))){ # ensure X is a data frame with only numeric variables
    stop("Argument has to be a data frame with only numeric variables!")
  }
  cor_mat <- matrix(NA, nrow = ncol(X), ncol = ncol(X)) # create empty matrix object size of the called dataframe
  for(i in 1:ncol(X)){ # create iteration i & j to calculate correlations between different columns
    for(j in 1:ncol(X)){
      sd_i <- sqrt(mean(X[,i]^2) - mean(X[,i])^2) # sd for column i
      sd_j <- sqrt(mean(X[,j]^2) - mean(X[,j])^2) # sd for column j
      cov_ij <- mean((X[,i] - mean(X[,i])) * (X[,j] - mean(X[,j]))) # cov of both columns
      cor_ij <- cov_ij / (sd_i * sd_j) # corr of both columns
      cor_mat[i,j] <- cor_ij # store correlations in the matrix
    }
  }
  
  return(cor_mat)
}

# The pseudo-code for this assignment was found at https://en.wikipedia.org/wiki/Trial_division
trial_division_factorization <- function(x){
  primes <- c() # empty vector to assign prime factors of x into
  f <- 2 # lowest possible factor
  i <- 1 # iterative object
  while(x > 1){ # while the integer still has remaining factors
    if(x %% f == 0){ # the modulus operator ensures the factor can divide the integer
      primes[i] <- f # assign the first found factor into the primes list
      x <- x / f # divide the integer by the factor
      i <- i + 1 # add one iteration
    } else {
      f <- f + 1 # if the factor can not divide the integer without rest, add one to f and try the loop again
    }
  }
  return(primes)
}