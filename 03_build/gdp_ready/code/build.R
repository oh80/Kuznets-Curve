library(magrittr)

main <- function(){
  my_folder <- "gdp"
  
  gdp_tidy  <- basics$read_interim(my_folder, extension = "tidy")
  
  J_pop_raw <- read_J_pop_raw_data(my_folder,
                                   fime_name <- "raw_pop_japan.csv")
  U_pop_raw <- read_U_pop_raw_data(my_folder,
                                   fime_name <- "raw_pop_US.csv")
  J_pop_tidy <- J_pop_raw %>% 
    select_column() %>% 
    rename_column_J() %>% 
    to_numeric()
  
  U_pop_tidy <- U_pop_raw %>% 
    select_column() %>% 
    rename_column_U() %>% 
    to_numeric()
  
  master_data <- join_pop_and_gdp(gdp_tidy,J_pop_tidy,U_pop_tidy)
  gdp_ready <- add_per_capita(master_data)
  
  basics$save_interim(gdp_ready, my_folder, extension = "ready")

}

read_J_pop_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  J_pop_raw <- readr::read_csv(file_path,skip = 15)
  return(J_pop_raw)
}

read_U_pop_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  U_pop_raw <- readr::read_csv(file_path,skip = 15)
  return(U_pop_raw)
}

select_column <- function(data_input){
  data_output <- data_input %>% dplyr::select(date,Population)
  return(data_output)
}

rename_column_J <- function(data_input){
  data_output <- data_input %>% dplyr::rename(year="date",
                               pop_J="Population")
  return(data_output)
}

rename_column_U <- function(data_input){
  data_output <- data_input %>% dplyr::rename(year="date",
                               pop_U="Population")
  return(data_output)
}

to_numeric <- function(data_input){
  data_input$year <- format(as.Date(data_input$year, format="%Y-%m-%d"),"%Y")
  data_output <- data_input %>% dplyr::mutate(year=as.numeric(year))
  return(data_output)
}

join_pop_and_gdp <- function(gdp_data,J_pop,U_pop){
  pop_data <- J_pop %>% dplyr::inner_join(U_pop,by="year")
  data_output <- gdp_data %>% dplyr::inner_join(pop_data,by="year")
  return(data_output)
}

add_per_capita <- function(data_input){
  gdp_per_capita_J <- data_input$GDP_J/data_input$pop_J
  gdp_per_capita_U <- data_input$GDP_U/data_input$pop_U
  data_output <- data_input %>% dplyr::mutate(gdp_per_capita_J,gdp_per_capita_U)
  return(data_output)
}

box::use(`functions`/basics)
main()
