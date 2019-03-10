# required libraries - install.packages("PACKAGE_NAME_HERE")
# lubridate
# data.table
# ggplot2
# openxlsx
# readxl
# org
# fs

################
# Folder setup
################

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = c(
    fs::path("/git","GD-Article-1"),
    fs::path("/Users","malin976","Documents","GitHub","GD-Article-1"),
    fs::path("/Users","Georgios","Documents","Research-Malin","GD-Article-1")
  ),
  SHARED = c(
    fs::path("/Filr", "Shared with Me", "Gender dysphoria -shared folder", "results"),
    fs::path("/Users","malin976","Filr","Mina filer","Gender dysphoria -shared folder", "results"),
    fs::path("/Users","Georgios","Filr","Shared with Me","Malin-results")
  ),
  DATA_RAW = c(
    fs::path("/Volumes","crypt_data","org","data_raw","code_minor","2018","GD-Article-1"),
    fs::path("/Users","malin976","Documents","Article-1-data"),
    fs::path("/Volumes","KonsdysforiregisterKaramanis")
  )
)

fs::dir_create(file.path(org::PROJ$SHARED_TODAY,"descriptives"))
fs::dir_create(file.path(org::PROJ$SHARED_TODAY,"validation"))
fs::dir_create(file.path(org::PROJ$SHARED_TODAY,"analyses_diag"))
fs::dir_create(file.path(org::PROJ$SHARED_TODAY,"analyses_treatments"))

################
# Load libraries
################

library(data.table)
library(ggplot2)

d <- CleanData()
LossOfPeopleTreatments(d,type = "treatments")
LossOfPeopleTreatments(d,type = "diag")

Validate_1(d, byvar="c_analysisCat_F64_089_ge4")
Validate_1(d, byvar="c_analysisCat_F64_089_ge10")

dz <- d[!is.na(analysisCat_z)]

dz <- d[!is.na(c_analysisCat_treatments)]
dz[,analysisCat_z:=c_analysisCat_treatments]
dz[,analysisYear_z:=c_analysisYear_treatments]
Analyses_1(dz,pop=GetPop(), folder="analyses_treatments")

dz <- d[!is.na(c_analysisCat_diag)]
dz[,analysisCat_z:=c_analysisCat_diag]
dz[,analysisYear_z:=c_analysisYear_diag]
Analyses_1(dz,pop=GetPop(), folder="analyses_diag")


Descriptives_2(d)
Validate_1(d)
NumbersByYear_1(d)

Validate_2(d)
Validate_3(d)









