library(magrittr)
main <-function(){
  my_folder <- "inequality"
  
  GINI_tidy <- basics$read_interim(my_folder, extension = "tidy")
  
  GINI_ready <- GINI_tidy %>% 
    add_year() %>% 
    fill_na_U() %>% 
    fill_na_J() %>% 
    unify_scale() %>% 
    extract_every_5years_data()
    
  basics$save_interim(GINI_ready, my_folder, extension = "ready")
}


add_year <- function(input_data){
  every_yaer <- c(1990:2019) %>% tidyr::as_tibble() %>% 
    dplyr::rename(year=value)
  output_data <- input_data %>% dplyr::full_join(every_yaer,by="year") %>% 
    dplyr::arrange(year)
  return(output_data)
}

fill_na_U <- function(input_data){
  input_data$U_GINI[1]=input_data$U_GINI[3]
  input_data$U_GINI[2]=input_data$U_GINI[3]
  output_data <- input_data
  return(output_data)
}

fill_na_J <- function(input_data){
  for (i in 1:length(input_data$J_GINI)){
    if (i%%3==1){
      input_data$J_GINI[i] <- input_data$J_GINI[i]
    }
    else if(i%%3==2){
      input_data$J_GINI[i] <- (input_data$J_GINI[i-1]*2/3+input_data$J_GINI[i+2]*1/3)
    }
    else if(i%%3==0){
      input_data$J_GINI[i] <- (input_data$J_GINI[i-2]*1/3+input_data$J_GINI[i+1]*1/3)
    }
  }
  output_data <- input_data
  return(output_data)
}

unify_scale <- function(input_data){
  input_data$U_GINI <- input_data$U_GINI/100
  output_data <- input_data
  return(output_data)
}

extract_every_5years_data <- function(input_data){
  output_data <- input_data %>% 
    dplyr::filter(year%%5 ==0)
  return(output_data)
}

box::use(functions/basics)
main()


