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
    fs::path("/volumes", "data", "box", "Indremo", "results"),
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

fs::dir_create(fs::path(org::PROJ$DATA_RAW,"natasa"))

################
# Load libraries
################

library(data.table)
library(ggplot2)

prev <- CleanDataPrevalenceGD()
d <- CleanDataIncidentGD(apply_sex_age_cleaning=TRUE)
natasa(d)

d[LopNr == 20842]
#LossOfPeopleTreatments(d,type = "treatments")
#LossOfPeopleTreatments(d,type = "diag")

time_to_diagnosis(d)

Validate_1(d, byvar="c_analysisCat_F64_089_ge4")
Validate_1(d, byvar="c_analysisCat_F64_089_ge10")

d_oneplusdiag <- d[!is.na(c_analysisCat_oneplusdiag) & excluded=="No"]
nrow(d_oneplusdiag)
d_oneplusdiag[,analysisCat_z:=c_analysisCat_oneplusdiag]
d_oneplusdiag[,analysisYear_z:=c_analysisYear_oneplusdiag]
d_oneplusdiag[,analysisAgeCat_z:=c_analysisAgeCat_oneplusdiag]

dz <- d[!is.na(c_analysisCat_treatments) & excluded=="No"]
nrow(dz)
dz[,analysisCat_z:=c_analysisCat_treatments]
dz[,analysisYear_z:=c_analysisYear_treatments]
dz[,analysisAgeCat_z:=c_analysisAgeCat_treatments]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag, pop=GetPop(), folder="analyses_treatments")

dz <- d[!is.na(c_analysisCat_diag) & excluded=="No"]
dz[,analysisCat_z:=c_analysisCat_diag]
dz[,analysisYear_z:=c_analysisYear_diag]
dz[,analysisAgeCat_z:=c_analysisAgeCat_diag]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag,pop=GetPop(), folder="analyses_diag")

dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded=="No"]
dz[,analysisCat_z:=c_analysisCat_hybrid]
dz[,analysisYear_z:=c_analysisYear_hybrid]
dz[,analysisAgeCat_z:=c_analysisAgeCat_hybrid]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag,pop=GetPop(), folder="analyses_hybrid")

dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded=="No"]
analyses_together(
  dz,
  d_oneplusdiag=d_oneplusdiag,
  prev=prev,
  pop=GetPop(),
  folder="analyses_together")

# losing people?
dz <- d[c_analysisCat_hybrid=="Hybrid"]
sink(fs::path(org::PROJ$SHARED_TODAY,"lost_hybrid.txt"))
xtabs(~dz$excluded)
sink()

# comorbidity
dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded=="No"]
dz[,N:=1]
comorbidity(dz=dz, folder="comorbidity")

xtabs(~d$c_analysisCat_hybrid)
dz <- d[c_analysisCat_hybrid %in% c("Hybrid","control_assigned") & excluded=="No"]
dz[,N:=1]
comorbidity(dz=dz, folder="comorbidity_with_control_assigned")

unique(d$c_analysisCat_hybrid)
dz <- d[c_analysisCat_hybrid %in% c("Hybrid","control_opposite") & excluded=="No"]
dz[,N:=1]
comorbidity(dz=dz, folder="comorbidity_with_control_opposite")


# end?

# check some genders

# person who has had legal sex change
b <- d[lopnr_analysis_group==83054,c(
  "c_analysisCat_hybrid","bornSex","c_analysisSex_hybrid","dateSexChange"
)]
setorder(b,c_analysisCat_hybrid,bornSex)
b

# person who has NOT had legal sex change
b <- d[lopnr_analysis_group==64567,c(
  "c_analysisCat_hybrid","bornSex","c_analysisSex_hybrid","dateSexChange"
)]
setorder(b,c_analysisCat_hybrid,bornSex)
b


### STOP RUNNING CODE HERE
d[,c_dateFirstHormoneSurgery]
xtabs(~d[
  dateFirst_F64_089>="2001-01-01" & 
  dateFirst_F64_089<="2005-12-31" & 
  c_analysisCat_treatments_first_date_of_surgery_hormones >= "2006-01-01"
]$c_analysisCat_hybrid,addNA=T)

d[,yearFirst_F64_089:=lubridate::year(dateFirst_F64_089)]
d[,.(N=.N),keyby=.(yearFirst_F64_089)]

xtabs(~d$excluded_treatments)
xtabs(~d$excluded_diag)
xtabs(~d$excluded_hybrid)
xtabs(~d$excluded_oneplusdiag)
## checking




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



