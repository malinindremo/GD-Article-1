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
fs::dir_create(file.path(org::PROJ$SHARED_TODAY,"hormones_surgeries_before_diagnosis"))

################
# Load libraries
################

library(data.table)
library(ggplot2)

d <- CleanData()
LossOfPeopleTreatments(d,type = "treatments")
LossOfPeopleTreatments(d,type = "diag")

time_to_diagnosis(d)



Validate_1(d, byvar="c_analysisCat_F64_089_ge4")
Validate_1(d, byvar="c_analysisCat_F64_089_ge10")

dz <- d[!is.na(c_analysisCat_treatments)]
nrow(dz)
dz[,analysisCat_z:=c_analysisCat_treatments]
dz[,analysisYear_z:=c_analysisYear_treatments]
dz[,analysisAgeCat_z:=c_analysisAgeCat_treatments]
Analyses_1(dz,pop=GetPop(), folder="analyses_treatments")

dz <- d[!is.na(c_analysisCat_diag)]
dz[,analysisCat_z:=c_analysisCat_diag]
dz[,analysisYear_z:=c_analysisYear_diag]
dz[,analysisAgeCat_z:=c_analysisAgeCat_diag]
Analyses_1(dz,pop=GetPop(), folder="analyses_diag")

# end?

ugly_table <- d[!is.na(c_analysisYear_treatments),.(
    N=.N,
    c_analysisAge_treatments=mean(c_analysisAge_treatments),
    prop_assigned_male=mean(isBornMale)
  ),
  keyby=.(
    c_analysisCat_treatments,
    c_analysisYear_treatments
  )]
ugly_table[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]",cat:="Hormones/surgery after diagnosis"]
ugly_table[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31], H/S BEFORE F64_089",cat:="Hormones/surgery before diagnosis"]

q <- ggplot(ugly_table,
            aes(x=c_analysisYear_treatments,y=N,group=cat,color=cat))
q <- q + geom_point()
q <- q + geom_line()
q <- q + scale_x_continuous("Year of first F64_089 diagnosis")
q <- q + scale_y_continuous("Number of people")
q <- q + labs(caption="\nFirst F64.0/8/9 diagnosis between 2006-01-01 and 2016-12-31")
SaveA4(
  q, 
  filename = fs::path(
    org::PROJ$SHARED_TODAY,
    "hormones_surgeries_before_diagnosis",
    "N_over_time.png"
  ))

q <- ggplot(ugly_table,
            aes(x=c_analysisYear_treatments,y=c_analysisAge_treatments,group=cat,color=cat))
q <- q + geom_point()
q <- q + geom_line()
q <- q + scale_x_continuous("Year of first F64_089 diagnosis")
q <- q + scale_y_continuous("Age at first F64_089 diagnosis")
q <- q + labs(caption="\nFirst F64.0/8/9 diagnosis between 2006-01-01 and 2016-12-31")
SaveA4(
  q, 
  filename = fs::path(
    org::PROJ$SHARED_TODAY,
    "hormones_surgeries_before_diagnosis",
    "age_over_time.png"
  ))




q <- ggplot(ugly_table,
            aes(x=c_analysisYear_treatments,y=prop_assigned_male*100,group=cat,color=cat))
q <- q + geom_point()
q <- q + geom_line()
q <- q + scale_x_continuous("Year of first F64_089 diagnosis")
q <- q + scale_y_continuous("Percentage assigned male at birth")
q <- q + labs(caption="\nFirst F64.0/8/9 diagnosis between 2006-01-01 and 2016-12-31")
SaveA4(
  q, 
  filename = fs::path(
    org::PROJ$SHARED_TODAY,
    "hormones_surgeries_before_diagnosis",
    "amale_over_time.png"
  ))



