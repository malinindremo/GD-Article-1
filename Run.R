# required libraries - install.packages("PACKAGE_NAME_HERE")
# lubridate
# data.table
# ggplot2
# openxlsx


################
# Folder setup
################

FOLDERS_SUGGEST <- list()
FOLDERS_SUGGEST$code <- c(
  file.path("/git","GD-Article-1"),
  file.path("/git","GD-Article-1")
)
FOLDERS_SUGGEST$data <- c(
  file.path("/Volumes","crypt_data"),
  file.path("/Users","malin976","Documents","Article-1-data")
)
FOLDERS_SUGGEST$results <- c(
  file.path("/Filr", "Shared with Me", "Gender dysphoria -shared folder", "results"),
  file.path("/Filr","Mina filer","Gender dysphoria -shared folder", "results")
)

FOLDERS <- list()
for(i in names(FOLDERS_SUGGEST)) for(j in FOLDERS_SUGGEST[[i]]){
  if(dir.exists(j)) FOLDERS[[i]] <- j 
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

