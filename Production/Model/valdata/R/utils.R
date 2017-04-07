
#' @title check_classes_match
#' @description Check if the variable classes in the new state match the expected class.
#' If the classes do not match then an error message will be displayed. This function is applied
#' within the pull_data function. The variable classes will be compared to the expected variable classes in the data dictionary.
#' @param df - A data.frame
#' @export
#

#Create function that will alert the user if the classes of the data pulled
#in a new state differ from the expected data format found in the data_class dataset.

#Create data.frame of the classes and the associated column
#Check if the variable classes match with the expected class
#If the class of the variable doesn't match return a message notifying the user
check_classes_match <- function(df){
  new.classes <- sapply(df, class)
  new <- data.frame("var.name" = names(new.classes), "class"=  as.character(new.classes))
  new$check <- paste(new$var.name, new$class, sep = ".")
  no_match <- new[!(new$check %in% variable_classes$check),]

  if(nrow(no_match) == 0){
    cat(paste("Variable classes in The ", deparse(substitute(df)),"  data frame match correctly"))} else {
      cat(paste("variables ", paste(no_match$var.name, collapse = ", "), " in data.frame ",deparse(substitute(df)),
                "  have a different class than expected", sep = ""))}
  cat("\n")
}



#' @title dimension
#' @description Calculates the number of rows and columns, and displays the values to the user
#' when new data is pulled. *For the users reference
#' @param df - A data.frame
#' @export

dimension <- function(df){
#This section of the function
n.row <- nrow(df)
n.col <- ncol(df)
cat(paste("The ", deparse(substitute(df)),
          "  data frame contains ", n.row," rows and ", n.col, " columns.", sep = ""))
cat("\n")
cat("\n")
cat("\n")
}



#' @title check_var_range
#' @description Check the range of a variable pulled from the NRDB. Is it within
#' the acceptable range? Variable ranges are compared to ranges set in the data dictionary.
#' @param df'x' - any data frame that contains variables you would like to check the range of.
#' @export

check_var_range <- function(df1, df2 = NA, df3= NA, df4= NA, df5= NA,
                            df6= NA, df7= NA){

  #Use list because data frames have a different number of rows
  lst <- list(df1, df2, df3, df4, df5, df6, df7)

  lst <- lapply(lst, function(x){      #----------\  Only keep the df's that are
    if(class(x) == "data.frame") return(x) #------\  of class data.frame, effectively
  })                                   #----------\  removing the ones that are NA

  #define empty variables that will store information
  var_info <- data.frame()
  msg1 <- character()
  msg2 <- character()
  pct_na <- numeric()

  #create a mini dictionary that only contains the numeric variables as these have a max and min
  dict <- dictionary %>% filter(var_class == "numeric")

  for(df in lst){          #-----\ loop through data frames in the list

    for(i in 1:ncol(df)){  #-----\ loop through columns in the data frame
      df[df == -1] <- NA
      x <- df[,i]
      name <- names(df)[i]

      #If the name of the variable is in the dictionary
      #find the min and max of that variable
      if(name %in% dict$var_name){
        index <- which(as.character(dict$var_name) %in% name)
        tbl <- dict[index,]
        min.value <- min(x, na.rm = TRUE)
        max.value <- max(x, na.rm = TRUE)

       #If the variable contains values lower or higher
       #than the acceptable range, then record an error message
        if(min.value < tbl$acceptable_min) {
          err_msg1 <- paste0("The variable ", name, "contains values less than ", tbl$acceptable_min)
        } else err_msg1 <- "Good"

        if(max.value > tbl$acceptable_max) {
         err_msg2 <- paste0("The variable ", name,
                            " contains values greater than ", tbl$acceptable_max)
        }else err_msg2 <- "Good"

        #record the % of NA values
        na <- (sum(is.na(x))/length(x)) * 100

        #write all error messages and NA %'s in a data frame
        if(max.value > tbl$acceptable_max | min.value < tbl$acceptable_min | na > 0){
        msg1 <- c(msg1,err_msg1)
        msg2 <- c(msg2,err_msg2)
        pct_na <- c(pct_na,na)
        var_info <- rbind(var_info,tbl[tbl$var_name == name,])
        }
      }
    }
  }
    #Write the error table to the global environment  for the user to view
 if(nrow(var_info) > 0){
  error_tbl <<- cbind("error message1" = msg1, "error message2" = msg2, "percent_na" = pct_na, var_info)
  cat("There were potential data errors identified. View them in the newly created 'error_tbl' object")
 }

}


#' @title similarity_measures
#' @description Estimate the bhattacharyya coefficient and shared area
#' between two probability density functions.
#' @param pdf1_x - x values from a pdf
#' @param pdf2_x - x values from a pdf
#' @param pdf1_y - y values from a pdf
#' @param pdf2_y - y values from a pdf
#' @export



#save the x and y values from the pdfs (probability density function)

#The bhattacharyya and shared area are calculated as follows :
#1. Find the max and min x values (both variables have different x values)
#   BUT we need the x values to be the same so..
#2. we create a new set of x values that spans the correct range
#3. Since we are linearly interpolating the y points...
#   for some variables we end up with an area under the curve
#   greater than 1 and for some lower than 1.
#   To account for this we divide the new y values
#   by the new area under the curve which forces the area to be =  1.
#   Now that the area under the curve is = 1 for both pdf's we can
#4. calculate the bhattacharyya coefficient
#   and
#5. calculate the shares area between the two pdfs

similarity_measures <- function(pdf1_x, pdf1_y, pdf2_x, pdf2_y) {

  x1 <- pdf1_x
  y1 <- pdf1_y
  x2 <- pdf2_x
  y2 <- pdf2_y

  max.x <- max(x1,x2)
  min.x <- min(x1,x2)

  delta <- (max.x - min.x)/5000
  xout <- seq(min.x,max.x,delta)

  new_coords1 <- approx(x = x1, y = y1, xout = xout)
  new_coords1$y[is.na(new_coords1$y)] <- 0
  new_coords2 <- approx(x = x2, y = y2, xout = xout)
  new_coords2$y[is.na(new_coords2$y)] <- 0

  y1 <- new_coords1$y
  y2 <- new_coords2$y

  y1 <- y1 / sum(y1*delta)
  y2 <- y2 / sum(y2*delta)

  bhattacharyya <- sqrt(y1*delta * y2*delta)
  bhat_coef <- sum(bhattacharyya)

  shared_min <- pmin(y1,y2)
  shared_area <- sum(shared_min * delta)

  return(c(bhat_coef, shared_area))
}



#' @title form_data_dictionary
#' @description create a data dictionary of the data used to generate scores. This
#' dictionary contains name, class, parent table in the NRDB, table it is stored in locally,
#' description, min acceptable value and max acceptable value
#' @export

form_data_dictionary <- function(){

  var_descriptions <- create_data_descriptions()
  var_range_tbl <- create_var_range_tbl()
  dictionary <- merge(var_descriptions, var_range_tbl, by = "var_name", all = T)
  dictionary <<- dictionary[,-which(names(dictionary) == "check")]

}


#' @title char_stats_tbl
#' @description Statistics for categorical variables. Categorical variable statistics from all states live in this data frame.
#'
#'
#' @format A data frame with a unique item for each categorical variable:
#'    1. State Initial
#'    2. Timestamp, when the summary stats were created
#'    3. Min date range used to query data
#'    4. Max date range used to query data
#'    5. Variable Name
#'    6. Category
#'    7. Category Percent
#'    8. Sample Size
"char_stats_tbl"


#' @title num_stats_tbl
#' @description Statistics for numeric variables. Numeric variable statistics from all states live in this data frame.
#'
#'
#' @format A data frame containing descriptive statistics for each numeric variable
#'    1. State Initial
#'    2. Timestamp, when the summary stats were created
#'    3. Min date range used to query data
#'    4. Max date range used to query data
#'    5. Variable Name
#'    6. Statistic
#'    7. Statistic Value
"num_stats_tbl"


#' @title density_tbl
#' @description Probability Density Function x and y values for numeric variables in each
#' state live in this data frame.
#'
#'
#' @format A data frame containing information required to
#' make density plots for each numeric variable
#'    1. State Initial
#'    2. Timestamp, when the summary stats were created
#'    3. Min date range used to query data
#'    4. Max date range used to query data
#'    5. Variable Name
#'    6. pdf x-values
#'    7. pdf y-values
"density_tbl"



#' @title variable_classes
#' @description data frame of expected variable classes for each variable used in scoring
#'
#'
#' @format A data.frame of expected variable classes
"variable_classes"



