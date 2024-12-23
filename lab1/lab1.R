library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)
name <- "Mikael Montén"
liuid <- "mikmo937"


#### 1.1 Vectors ####

## 1.1.1
my_num_vector <- function(){
  options(digits = 22) # sufficient amount of decimals to accuont for "all"
  first_element <- log10(11) # each element that is in the final vector
  second_element <- cos(pi/5)
  third_element <- exp(pi/3)
  fourth_element <- (1173 %% 7)/19
  complete_vector <- c(first_element, second_element, third_element, fourth_element) # create vector
  return(complete_vector)
}

## 1.1.2
filter_my_vector <- function(x, leq){
  x[x >= leq] <- NA # for each x, index every element that is greater or equal than leq and set it to NA
  return(x)
}

## 1.1.3
dot_prod <- function(a, b){
  i <- 1:length(a) # index for the a vector
  j <- 1:length(b) # index for the b vector
  c = a[i]*b[j] # multiply every element into a new vector c
  return(sum(c)) # return summation of c
}

## 1.1.4
approx_e <- function(N){
  e <- c(0) # create empty vector to store each iteration in
  for(n in 0:N){ # iterate through the length of N
    e[n+1] <- 1/factorial(n) # add each approximation to the e vector as one elemental extra to include 0
  }
  return(sum(e)) # return the sum of the vector
} # N = 8 is required to approxiate e to the fifth decimal


#### 1.2 Matrices ####

# 1.2.1 
my_magic_matrix <- function(){
  magic <- matrix(ncol = 3, nrow = 3) # create empty matrix
  magic[1,] <- c(4, 9, 2) # fill row 1 with corresponding element
  magic[2,] <- c(3, 5, 7)
  magic[3,] <- c(8, 1, 6)
  return(magic)
}


# 1.2.2
calculate_elements <- function(A){
 length <- length(A) # calculate the amount of elements in the argument
 return(length)
}


# 1.2.3
row_to_zero <- function(A, i){
  A[i,] <- 0 # set the i row to of the A matrix to be 0
  return(A)
}

# 1.2.4
add_elements_to_matrix <- function(A, x, i, j){
  A[i,j] <- A[i,j] + x # index the row (i) and column (j) of interest and add x to that element
  return(A)
}


#### 1.3 Lists ####

# 1.3.1
my_magic_list <- function(){
  magic_list <- list("info" = "my own list", my_num_vector(), my_magic_matrix()) # create list
  return(magic_list)
}


# 1.3.2
change_info <- function(x, text){
  x$info <- text # alter the info element of the x list to the new argument
  return(x)
}

# 1.3.3
add_note <- function(x, note){
  x$note <- note # add the new element into the list
  return(x)
}

# 1.3.4
sum_numeric_parts <- function(x){
  matrix_sum <- c(0) # create empty vector with summarized list elements
  for(i in 1:length(x)){ # index over every element in the argument matrix
    if(is.numeric(x[[i]]) == TRUE){ # check if iterated element is numeric
      matrix_sum[i] <- sum(x[[i]]) # sum the iterated element and append it to the vector
    }
  }
  return(sum(matrix_sum)) # sum the vector and return it
}



#### 1.4 data.frames ####

# 1.4.1
# create the data frame with column names and values according to the instructions
my_data.frame <- function(){
  df <- data.frame("id" = c(1, 2, 3), "name" = c("John", "Lisa", "Azra"),
                   "income" = c(7.30, 0.00, 15.21), "rich" = c(FALSE, TRUE, FALSE))
  return(df)
}

# 1.4.2
sort_head <- function(df, var.name, n){
  ordered_df <- df[order(df[,var.name], decreasing = TRUE),] # order the df according to the relevant variable name in decreasing order
  return(head(ordered_df, n = n)) # return the n largest values
}


# 1.4.3
add_median_variable <- function(df, j){
  median_var <- median(df[,j]) # assign the median of the chosen column to a variable
  df$compared_to_median <- NA # create the new column
  df$compared_to_median <- ifelse(df[, j] < median_var, "Smaller", # if the median is larger than the column value, assign "Smaller"
                                 ifelse(df[, j] > median_var, "Greater", "Median")) # if median is larger, assign Greater, otherwise "Median"
  return(df)
}
  

# 1.4.4
analyze_columns <- function(df, j){
  df_vars <- data.frame(df[,j]) # create a new data frame with the 2 columns provided
  list_vars <- list() # create empty list
  for(i in 1:2){ # for column 1 and 2 of the new df,
    desc_var <- c(mean = mean(df_vars[,i]), median = median(df_vars[,i]), sd = sd(df_vars[,i])) # calculate descriptive statistics for each
    list_vars[[names(df)[j[i]]]] <- df_vars[,i] # set the list element names to correspond with the new df,
    list_vars[[i]] <- desc_var # save the descriptive statistics in the aforementioned list
  }
  correlation_matrix <- cor(df_vars) # create correlation matrix for the columns
  list_vars[["correlation_matrix"]] <- correlation_matrix # create new element and save the correlation matrix
  return(list_vars)
}