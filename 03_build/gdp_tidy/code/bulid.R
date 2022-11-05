library(magrittr)
box::

main <- function(){
  my_folder <- "gdp"
  J_data <- read_J_raw_data(my_folder,
                            fime_name <- "raw_data_japan.csv")
  U_data <- read_US_raw_data(my_folder,
                            fime_name <- "raw_data_US.csv")
  J_tidy_data <- J_data %>% 
    select_column() %>% 
    rename_column_J() %>% 
    to_numeric()
  
  U_tidy_data <- U_data %>% 
    select_column() %>% 
    rename_column_U() %>% 
    to_numeric()
  
  GDP_data <- J_tidy_data %>% dplyr::inner_join(U_tidy_data,by="year") 
  basics$save_interim(GDP_data, my_folder, extension = "tidy")
}

read_J_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  J_data <- readr::read_csv(file_path,skip = 16)
  return(J_data)
}

read_US_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  U_data <- readr::read_csv(file_path,skip = 16)
  return(U_data)
}

#関数の定義
select_column　<- function(gdp_data){
  data_output <- gdp_data %>% dplyr::select(1,2)
  #gdp_dataの１列目には日付のデータ、2列目にはGDPのデータが含まれている
  return(data_output)
}

rename_column_J <- function(data_input){
  data_output <- data_input %>% dplyr::rename("year"="date",
                                              "GDP_J"="GDP ( Billions of US $)") 
  return(data_output)
}

rename_column_U <- function(data_input){
  data_output <- data_input %>% dplyr::rename("year"="date",
                                              "GDP_U"="GDP ( Billions of US $)") 
  return(data_output)
}

to_numeric <- function(data_input){
  data_input$year <- format(as.Date(data_input$year, format="%Y-%m-%d"),"%Y")
  data_output <- data_input %>% dplyr::mutate(year=as.numeric(year))
  return(data_output)
}

main()
