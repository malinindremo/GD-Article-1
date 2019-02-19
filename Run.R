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
  file.path("/Users","malin976","Documents","GitHub","GD-Article-1"),
  file.path("/Users","Georgios","Documents","Research-Malin","GD-Article-1")
)
FOLDERS_SUGGEST$data <- c(
  file.path("/Volumes","crypt_data","org","data_raw","code_minor","2018","GD-Article-1"),
  file.path("/Users","malin976","Documents","Article-1-data"),
  file.path("/Volumes","KonsdysforiregisterKaramanis")
)
FOLDERS_SUGGEST$results <- c(
  file.path("/Filr", "Shared with Me", "Gender dysphoria -shared folder", "results"),
  file.path("/Users","malin976","Filr","Mina filer","Gender dysphoria -shared folder", "results"),
  file.path("/Users","Georgios","Filr","Shared with Me","Malin-results")
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
  dir.create(file.path(FOLDERS$results_today,"validation_1"))
  dir.create(file.path(FOLDERS$results_today,"validation_2"))
  dir.create(file.path(FOLDERS$results_today,"validation_3"))
  dir.create(file.path(FOLDERS$results_today,"analyses_1"))
})

################
# Load libraries
################

library(data.table)
library(ggplot2)

d <- CleanData()
dz <- d[!is.na(analysisCat_z)]

pop <- data.table(readxl::read_excel(file.path(FOLDERS$code,"structural_data","pop.xlsx"),skip=0))
pop <- melt.data.table(pop,id.vars=c("year","age"))
setnames(pop,c("year","age","sex","pop"))
pop[,year:=as.numeric(year)]
pop[,age:=stringr::str_extract(age,"[0-9]+")]
pop[,ageCat:=cut(as.numeric(age),breaks = c(0,18,30,50,200),include.lowest = T)]
pop[,ageCat:=as.character(ageCat)]
pop[,isBornMale:=sex=="men"]
pop[,bornSex:=ifelse(isBornMale,"Born Male","Born Female")]

pop0 <- pop[,.(
  pop=sum(pop)
),keyby=.(
  year,ageCat,bornSex
)]

pop1 <- pop[,.(
  pop=sum(pop)
),keyby=.(
  year,bornSex
)]
pop1[,ageCat:="All"]

pop2 <- pop[,.(
  pop=sum(pop)
),keyby=.(
  year,ageCat
)]
pop2[,bornSex:="All"]

pop3 <- pop[,.(
  pop=sum(pop)
),keyby=.(
  year
)]
pop3[,ageCat:="All"]
pop3[,bornSex:="All"]

pop <- rbind(pop0,pop1,pop2,pop3)

Descriptives_2(d)
Validate_1(d)
NumbersByYear_1(d)

Validate_2(d)
Validate_3(d)

Analyses_1(dz,pop)








