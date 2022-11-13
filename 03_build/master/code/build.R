library(magrittr)

main <- function(){
  GINI_data <- basics$read_interim("inequality", extension = "ready")
  gdp_data <- basics$read_interim("gdp", extension = "ready")
  
  master_data <- master_gdp_GINI(GINI_data,gdp_data)
  basics$save_interim(master_data, "master")
  
}

master_gdp_GINI <- function(input_data1,input_data2){
  output_data <- input_data1 %>% 
    dplyr::inner_join(input_data2,"year")
  return(output_data)
}

main()
