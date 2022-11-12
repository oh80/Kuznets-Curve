library(magrittr)

main <- function(){
  my_folder <- "inequality"
  
  J_raw_data <- read_J_raw_data(my_folder,
                            file_name="Japan_GINI.xls")
  U_raw_data <- read_U_raw_data(my_folder,
                            file_name="US_GINI.xlsx")
  
  J_tidy <- J_raw_data %>% 
    extract_data_J() %>% 
    rename_column_J() %>% 
    to_numeric_J()
  
  U_tidy <- U_raw_data %>% 
    extract_data_U() %>% 
    to_long_U() %>% 
    rename_column_U() 
  
  U_tidy <-to_numeric_U(U_tidy)
  
  basics$save_interim(J_tidy, my_folder, extension = "tidy")
  basics$save_interim(U_tidy, my_folder, extension = "tidy")

}

read_J_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  J_data <- readxl::read_excel(file_path)
  return(J_data)
}

read_U_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,"data",file_name)
  U_data <- readxl::read_excel(file_path,
                               col_names = FALSE)
  return(U_data)
}

extract_data_J <- function(input_data){
  output_data <-input_data %>% 
    dplyr::select(1,3) %>% 
  #1 and 4 col contain year and GIGI data
    tidyr::drop_na()
  return(output_data)
}

rename_column_J <- function(input_data){
  output_data <- input_data %>% 
    dplyr::rename("year"="図表1-8-9 所得再分配によるジニ係数の改善の推移",
                  "J_GINI"="...3")
  return(output_data)
}

to_numeric_J <- function(input_data){
  output_data <- input_data %>% 
    dplyr::mutate(year=as.numeric(year),J_GINI=as.numeric(J_GINI))
}

extract_data_U <-function(input_data){
  output_data <- input_data %>% 
    dplyr::select(6:14) %>% 
    #needed data is contained this range
    tidyr::drop_na() 
  return(output_data)
}

to_long_U <- function(input_data){
  output_data <- as.data.frame(t(input_data))
  return(output_data)
}

rename_column_U <- function(input_data){
  output_data <- input_data %>% 
    dplyr::rename("year"="V1","U_GINI"="V2")
  return(output_data)
}

to_numeric_U <- function(input_data){
  year_num <- stringr::str_sub(input_data$year,start = 1,end = 4)
  output_data <-input_data %>% 
    dplyr::mutate("year"=year_num)
  output_data <- output_data %>% 
    dplyr::mutate(year=as.numeric(year),U_GINI=as.numeric(U_GINI))
  return(output_data)
}


box::use(`functions`/basics)
main()


