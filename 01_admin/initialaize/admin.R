main <- function(){
  prepare_packages()
}

prepare_packages <- function(){
  renv::restore()
  #devtools::update_packages(packages = TRUE)
  library(magrittr)
  #tinytex::install_tinytex()
  ggplot2::theme_set(ggplot2::theme_light())
  options(box.path = here::here("01_admin"))
}

main()
