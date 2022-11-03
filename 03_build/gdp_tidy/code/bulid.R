main <- function(){
  my_folder <- "gdp"
  J_data <- read_J_raw_data(my_folder,
                            fime_name <- "raw_data_japan.csv")
  U_data <- read_US_raw_data(my_folder,
                            fime_name <- "raw_data_US.csv")

}

read_J_raw_data <- function(folder_name,file_name){
  file_path <- here::here("02_raw",folder_name,data,file_name)
  J_data <- readr::read_csv(file_path)
  return(J_data)
}

main()
