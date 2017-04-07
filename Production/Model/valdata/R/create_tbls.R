#' @title create_num_tbl
#' @description Create a table containing statistics on each numeric variable and store them in a data frame.
#' This function is applied in the generate_new_stats function.
#'
#' @param df - A data.frame containing numeric variables to validate
#' @param st_initials - Initials of the state you are generating stats for
#' @param min_date - the minimum date that the data is pulled from
#' @param max_date - the max date that the data is pulled from
#' @export

#create function to loop over a data.frame containing numeric
#variables and generate descriptive statistics
#The function is used in both the generate_new_stats function
create_num_tbl <- function(df, st_initials, min_date, max_date) {

  num_cols <- sapply(df, is.numeric)
  df <- df[,num_cols, drop = FALSE]
  df[df == -1] <- NA

  df %>%
    summarise_each(funs("_n" = round(length(.),1),   #------------------------------------------number of rows
                         "_pct_zero" = round(sum(. == 0, na.rm = T)/length(.),3) * 100,  #-----pct zero occurs
                         "_pct_NA" = round(sum(is.na(.))/length(.),3) * 100,   #-------------pct NA occurs
                         "_min" = round(min(., na.rm = T),3),  #--------------------------min
                         "_max" = round(max(., na.rm = T),3),  #--------------------------ma.
                         "_range" = round(max(., na.rm = T) - min(., na.rm = T),3), #-------range
                         "_mean" = round(mean(., na.rm = T),3),        #-------------------mean
                         "_quartile_1" = as.numeric(round(quantile(.,.25,na.rm = T),3)),   #-----------------1st quartile
                         "_quartile_2" = as.numeric(round(quantile(.,.50,na.rm = T),3)),   #-----------------2nd quartile, or median
                         "_quartile_3" = as.numeric(round(quantile(.,.75,na.rm = T),3)),  #------------------3rd quartile
                         "_var" = round(var(., na.rm = T),3),  #--------------------------variation
                         "_sd" = round(sd(., na.rm = T),3),   #--------------------------standard deviation
                         "_kurtosis" = round(moments::kurtosis(., na.rm = T),3),   #--------------------kurtosis
                         "_skewness" = round(moments::skewness(., na.rm = T),3))) -> tbl #--------------------skewness
    if(ncol(df) == 1) {names(tbl) <- paste0(names(df),"_", names(tbl))}
  tbl %>%
    gather() %>%
    separate(key, sep = "__", into = c("var_name", "stat" )) %>%
    mutate("st_init" = st_initials,  "timestamp" = Sys.Date(), "min_date" = min_date, "max_date" = max_date) %>%
    select(st_init, timestamp, min_date, max_date, var_name, stat, value) -> tbl

  return(tbl)
}



#' @title create_density_tbl
#' @description Calculate a density function for each variable and store them in a data frame.
#' This function is applied in the generate_new_stats function.
#'
#' @param df - A data.frame containing numeric variables to validate
#' @param st_initials - Initials of the state you are generating stats for
#' @export

create_density_tbl <-function(df, st_initials, min_date, max_date) {
  num_cols <- sapply(df, is.numeric)
  df <- df[,num_cols, drop = FALSE]
  den_tbl <- data.frame("st_init" = character(), "name" = character(),
                        "date" = character(), "x" = numeric(), "y" = numeric())
  variable_names <- names(df)
  for(i in variable_names){
    data <- df[,names(df) == i]
    den <- density(data, n = 2^12, na.rm = TRUE)
    den_x <- den$x
    den_y <- den$y
    var_name <- rep(i, length(den_x))
    timestamp <- rep(Sys.Date(), length(den_x))
    st_init <- rep(st_initials, length(den_x))
    tbl <- data.frame(st_init, timestamp, min_date, max_date, var_name, den_x, den_y)
    den_tbl <- rbind(den_tbl,tbl)
   }
  return(den_tbl)
}



#' @title create_char_tbl
#' @description Calculate the percent that each category makes up within each categorical variable. Store them in a data frame.
#' This function is applied in the generate_new_stats function.
#' @param df - a data.frame of the categorical variables
#' @param st_initials - Initials of the state you are generating stats for
#' @export


create_char_tbl <- function(df, st_initials, min_date, max_date) {

  char_cols <- sapply(df, is.character)
  df <- df[,char_cols, drop = FALSE]

  df %>%
    gather() %>%
    group_by(key, value) %>%
    summarise(n.observed = n())%>%
    group_by(key) %>%
    mutate(n = sum(n.observed)) %>%
    replace_na(list(value = "NA")) %>%
    mutate(category_pct = (n.observed / n) * 100, st_init = st_initials,
                           timestamp =  Sys.Date(), "max_date" = max_date,
                           "min_date" = min_date) %>%
    select(st_init, timestamp, min_date, max_date, "var_name" = key, "category" = value, category_pct, n) -> tbl

  return(data.frame(tbl))
}










