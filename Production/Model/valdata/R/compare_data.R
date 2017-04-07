
#' @title compare_data
#' @description  Compare variable statistics between multiple states.
#' This function displays the visualizations that show the differences between states.
#' @param st_initials - a vector of state initials.  input should look like c("id", "ks", "nc", .....)
#' @param numeric - Do you want to compare the numeric variables. TRUE or FALSE
#' @param char - Do you want to compare the categorical variables. TRUE or FALSE
#' @export


# compare two states data

compare_data <- function(st_initials = NULL, char = FALSE, numeric = FALSE ) {

  st <- toupper(st_initials)


  #####plotting categorical variables

  if(char == TRUE) {

    #select the state(s) requested
    char_stats_tbl %>%
      filter(st_init %in% st) -> compare_data

    variable_names <- unique(compare_data$var_name)

    #loop through variables to plot them one at a time
    for(i in variable_names){
      compare_data %>%
        filter(var_name == i) %>%
        mutate_each(funs(ifelse(. == -1, "NA", .))) -> plot_data

      # print out variable definition to console
      info <- dictionary[dictionary$var_name == i,]
      cat("\n",as.character(info$var_definition), "\n")

      #format data suitable to be printed to the console
      plot_data %>%
        select(st_init, category, category_pct) %>%
        spread(st_init,category_pct) -> tbl

      #print variable stats to console
      print(data.frame(tbl))

      cat("\n")

      #print sample size
      print(plot_data %>%
        distinct(st_init) %>%
        select(st_init, n))

      cat("\n")

      #print chi_sq test of independence
      #Technically this chi square test is wrong bc we are not
      #using the whole sample size, but we limit ourselves to a
      #sample size of 100 to better see variable differences
      if(length(st) > 1){
      plot_data %>%
        select(st_init, category, category_pct) %>%
        spread(category, category_pct) %>%
        mutate_each(funs(ifelse(is.na(.), 0, .))) %>%
        select(-st_init) -> chi

      print(chisq.test(chi))
      }

      #plot bar charts
      print(ggplot(plot_data, aes(x = category, y = category_pct, fill = st_init)) +
        geom_bar(stat = "identity", position = "dodge") + ggtitle(i))

      cat("\n \n \n Hit 'Enter' to view the next variable, return 'e' to exit \n \n")

      #allow user to leave the function
      decide <- readline()
      if (decide == "e"){
        stop
        break}

    }

  }

##############pick up making density plotting

  ##Displaying the density plots of the two states, and print the statistic differences
  #This will allow the user to tell how much the states differ from eachother
  if(numeric == TRUE){

    variable_names <- unique(density_tbl$var_name)

    #loop through variables to plot them one at a time
    for(i in variable_names){

      #plot densities
      print(density_tbl %>%
        filter(st_init %in% st, var_name == i) %>%
        ggplot(aes(x = den_x, y = den_y, color = st_init)) +
                 geom_line() + xlab(" ") + ylab("p(x)")  + ggtitle(i))

      #print definition
      info <- dictionary[dictionary$var_name == i,]
      cat(as.character(info$var_definition), "\n")

      #print associated stats
        print(num_stats_tbl %>%
        filter(st_init %in% st, var_name == i) %>%
          select(stat, st_init, value) %>%
          spread(st_init, value) %>%
          slice(match(c("n", "pct_NA", "pct_zero", "min", "max", "range",
                        "mean", "quartile_1", "quartile_2", "quartile_3",
                        "var", "sd", "kurtosis", "skewness"), stat)))

      #if two states are selected, calculate similatity metrics
      if(length(st) == 2){
        pdf1 <- density_tbl %>% filter(st_init == st[1], var_name == i)
        pdf1_x <- pdf1$den_x
        pdf1_y <- pdf1$den_y
        pdf2 <- density_tbl %>% filter(st_init == st[2], var_name == i)
        pdf2_x <- pdf2$den_x
        pdf2_y <- pdf2$den_y

        measures <- similarity_measures(pdf1_x, pdf1_y, pdf2_x, pdf2_y)
        cat("Bhattacharrya coefficient = ", measures[1], "\n")
        cat("Shared area = ", measures[2], "\n")
      }

      #allow user to leave the function
      cat("\n \n \n Hit 'Enter' to view the next variable, return 'e' to exit \n \n")
      decide <- readline()
      if (decide == "e"){
        stop
        break}
    }

  }
}


