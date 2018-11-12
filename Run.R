# required libraries - install.packages("PACKAGE_NAME_HERE")
# lubridate
# data.table
# ggplot2
# openxlsx


################
# Folder setup
################

if(.Platform$OS.type=="unix"){
  # richard's computer
  FOLDER_CODE <- file.path("/git",
                           "GD-Article-1")
  
  FOLDER_DATA <- file.path("/Volumes",
                           "crypt_data/")
  
  FOLDER_RESULTS <- file.path("/Filr",
                              "Shared with Me",
                              "Gender dysphoria -shared folder",
                              "results")
} else {
  # malin's computer
  FOLDER_CODE <- file.path("XX")
  
  FOLDER_DATA <- file.path("XX")
  
  FOLDER_RESULTS <- file.path("XX")
}

FOLDER_RESULTS_TODAY <- file.path(FOLDER_RESULTS,lubridate::today())
dir.create(FOLDER_RESULTS_TODAY)

################
# Load libraries
################

library(data.table)
library(ggplot2)

################
# Fake graph
################

q <- ggplot(mpg, aes(displ, hwy, colour = class))
q <- q + geom_point()

ggsave(file.path(FOLDER_RESULTS_TODAY,"graph.png"),
       plot = q,
       width = 297,
       height = 210, 
       units = "mm")

