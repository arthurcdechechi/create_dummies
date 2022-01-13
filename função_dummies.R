#'-----------------------------------------------------------------------------
#' 
#' Project : Function to create dummie variables in dataframes
#' Author : Arthur Dechechi
#' Date : 2021/12
#' 
#' ----------------------------------------------------------------------------

# Necessary Libraries ----
library( fastDummies )

# Defining the function ----
create_dummies <- function(data, variable, n_options){
  
  # Applying dummy_cols from fastDummies to create the dummies to the observed ----
  # values
  var_dummy <- as.data.frame(
    fastDummies::dummy_cols(
      data[ , variable],
      split = ","
    )
  )
  
  # Creating a vector with all necessary dummy names in the range ----
  names_n_options <- paste0(
    '.data_',
    1:n_options
  )
  
  # Creating an empty dataframe to be used as the estructure of the dummies ----
  # dataframe
  Empty_dataframe <- as.data.frame(matrix(
    data = 0,
    nrow = nrow(var_dummy),
    ncol = n_options 
    )
  )
  names(Empty_dataframe) <- names_n_options
  
  # Verify if all the possible values were found, otherwise it creates a ----
  # dummy to the not found value
  for (name in 1:n_options){
    ifelse(
       names_n_options[name] %in% names(var_dummy),
       Empty_dataframe[ ,
         names(Empty_dataframe) == names_n_options[name]
       ] <- var_dummy[ ,
         names(var_dummy) == names_n_options[name] 
       ],
       NA
    )
    
  }
  
  # Adapting the variable names ----
  new_names <- paste0(
    variable,
    "_d",
    1:n_options
  )
  names(Empty_dataframe) <- new_names
  
  # Inserting the dummies dataframe to the original data ----
  data <- cbind(
    data,
    Empty_dataframe
  )
  
  # Relocating these dummies to the side of the original variables ----
  data <- relocate(
    .data = data,
    (ncol(data) - n_options +1):ncol(data),
    .after = variable
  )
  
  return(data)
}
