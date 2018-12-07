# required libraries - install.packages("PACKAGE_NAME_HERE")
# lubridate
# data.table
# ggplot2
# openxlsx
# readxl

################
# Folder setup
################


FOLDERS_SUGGEST <- list()
FOLDERS_SUGGEST$code <- c(
  file.path("/git","GD-Article-1"),
  file.path("/Users","malin976","Documents","GitHub","GD-Article-1")
)
FOLDERS_SUGGEST$data <- c(
  file.path("/Volumes","crypt_data","org","data_raw","code_minor","2018","GD-Article-1"),
  file.path("/Users","malin976","Documents","Article-1-data")
)
FOLDERS_SUGGEST$results <- c(
  file.path("/Filr", "Shared with Me", "Gender dysphoria -shared folder", "results"),
  file.path("/Users","malin976","Filr","Mina filer","Gender dysphoria -shared folder", "results")
)

FOLDERS <- list()
for(i in names(FOLDERS_SUGGEST)) for(j in FOLDERS_SUGGEST[[i]]){
  if(dir.exists(j)) FOLDERS[[i]] <- j 
}

setwd(FOLDERS$code)

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

FOLDERS$results_today <- file.path(FOLDERS$results,lubridate::today())
suppressWarnings({
  dir.create(FOLDERS$results_today)
  dir.create(file.path(FOLDERS$results_today,"descriptives"))
})

################
# Load libraries
################

library(data.table)
library(ggplot2)

d <- CleanData()

d[,incidentYear:=as.numeric(format.Date(dateFirst_F64_089, "%G")),by=LopNr]

agg <- d[category!="No diagnosis" & !is.na(incidentYear),
  .(
    N=.N
  ),keyby=.(
    incidentYear
  )]

pop <- data.table(readxl::read_excel(file.path(FOLDERS$data,"Other","be0101tab9utveng2017.xlsx"),skip=2))
pop

#a <- 

#Descriptives_1(d)









