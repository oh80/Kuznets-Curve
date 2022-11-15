library(magrittr)

main <- function(){
  GINI_data <- basics$read_interim("inequality", extension = "ready") 
  gdp_data <- basics$read_interim("gdp", extension = "ready")
  
  GINI_long <-  GINI_data %>% to_long_GINI()
  gdp_long <- gdp_data %>% to_long_gdp()
  
  master_data <- master(GINI_long,gdp_long) %>% print()
  basics$save_interim(master_data, "master")
}

to_long_GINI <-function(input_data){
  output_data <-input_data %>% 
    dplyr::rename(Japan="J_GINI",US="U_GINI") %>% 
    tidyr::pivot_longer(cols = c("Japan","US"),
                        names_to = "country",
                        values_to = "GINI")
  return(output_data)
}

to_long_gdp <-function(input_data){
  output_data <-input_data %>% 
    dplyr::rename(Japan="gdp_per_capita_J",US="gdp_per_capita_U") %>% 
    tidyr::pivot_longer(cols = c("Japan","US"),
                      names_to = "country",
                      values_to = "gdp_per_capita")
  return(output_data)
}

master <- function(input_data1,input_data2){
  output_data <- input_data1 %>% 
    dplyr::inner_join(input_data2,by=c("year","country"))
  return(output_data)
}


main()
