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
    fs::path("/volumes", "data", "box", "Gender dysphoria", "results"),
    fs::path("/Users","malin976", "Box Sync", "Gender dysphoria", "results"),
    fs::path("/Users","Georgios", "Box Sync", "Gender dysphoria", "results")
  ),
  DATA_RAW = c(
    fs::path("/volumes","data","local","org","data_raw","code_minor","2018","GD-Article-1"),
    fs::path("/Users","malin976","Documents","Article-1-data"),
    fs::path("/Volumes","KonsdysforiregisterKaramanis")
  )
)

fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"descriptives"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"validation"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"analyses_diag"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"analyses_treatments"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"analyses_hybrid"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"analyses_together"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"hormones_surgeries_before_diagnosis"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"comorbidity"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"comorbidity_with_control_assigned"))
fs::dir_create(fs::path(org::PROJ$SHARED_TODAY,"comorbidity_with_control_opposite"))

################
# Load libraries
################

library(data.table)
library(ggplot2)

d <- CleanDataIncidentGD()

d[LopNr == 20842]
LossOfPeopleTreatments(d,type = "treatments")
LossOfPeopleTreatments(d,type = "diag")

time_to_diagnosis(d)

Validate_1(d, byvar="c_analysisCat_F64_089_ge4")
Validate_1(d, byvar="c_analysisCat_F64_089_ge10")

d_oneplusdiag <- d[!is.na(c_analysisCat_oneplusdiag) & excluded_oneplusdiag=="No"]
nrow(d_oneplusdiag)
d_oneplusdiag[,analysisCat_z:=c_analysisCat_oneplusdiag]
d_oneplusdiag[,analysisYear_z:=c_analysisYear_oneplusdiag]
d_oneplusdiag[,analysisAgeCat_z:=c_analysisAgeCat_oneplusdiag]

dz <- d[!is.na(c_analysisCat_treatments) & excluded_treatments=="No"]
nrow(dz)
dz[,analysisCat_z:=c_analysisCat_treatments]
dz[,analysisYear_z:=c_analysisYear_treatments]
dz[,analysisAgeCat_z:=c_analysisAgeCat_treatments]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag, pop=GetPop(), folder="analyses_treatments")

dz <- d[!is.na(c_analysisCat_diag) & excluded_diag=="No"]
dz[,analysisCat_z:=c_analysisCat_diag]
dz[,analysisYear_z:=c_analysisYear_diag]
dz[,analysisAgeCat_z:=c_analysisAgeCat_diag]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag,pop=GetPop(), folder="analyses_diag")

dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded_hybrid=="No"]
dz[,analysisCat_z:=c_analysisCat_hybrid]
dz[,analysisYear_z:=c_analysisYear_hybrid]
dz[,analysisAgeCat_z:=c_analysisAgeCat_hybrid]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag,pop=GetPop(), folder="analyses_hybrid")

dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded_hybrid=="No"]
analyses_together(dz,pop=GetPop(), folder="analyses_together")

# losing people?
dz <- d[c_analysisCat_hybrid=="Hybrid"]
sink(fs::path(org::PROJ$SHARED_TODAY,"lost_hybrid.txt"))
xtabs(~dz$excluded_hybrid)
sink()

# comorbidity
dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded_hybrid=="No"]
dz[,N:=1]
comorbidity(dz=dz, folder="comorbidity")

unique(d$c_analysisCat_hybrid)
dz <- d[c_analysisCat_hybrid %in% c("Hybrid","control_assigned") & excluded_hybrid=="No"]
dz[,N:=1]
comorbidity(dz=dz, folder="comorbidity_with_control_assigned")

unique(d$c_analysisCat_hybrid)
dz <- d[c_analysisCat_hybrid %in% c("Hybrid","control_opposite") & excluded_hybrid=="No"]
dz[,N:=1]
comorbidity(dz=dz, folder="comorbidity_with_control_opposite")


# end?

### STOP RUNNING CODE HERE




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



