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
#options(error = recover)

org::initialize_project(
  home = c(
    fs::path("/git","GD-Article-1"),
    fs::path("~/Documents/git/GD-Article-1"),
    fs::path("/Users","malin976","Documents","GitHub","GD-Article-1"),
    fs::path("/Users","Georgios","Documents","Research-Malin","GD-Article-1")
  ),
  results = c(
    fs::path("/volumes", "data", "box", "Indremo", "results"),
    fs::path("/Volumes", "data", "box", "Indremo", "results"),
    fs::path("/Users","malin976", "Box Sync", "Gender dysphoria", "results"),
    fs::path("/Users","Georgios", "Box Sync", "Gender dysphoria", "results")
  ),
  data_raw = c(
    fs::path("/volumes","data","local","org","data_raw","code_minor","2018","GD-Article-1"),
    fs::path("/Volumes","data","local","org","data_raw","code_minor","2018","GD-Article-1"),
    fs::path("/Users","malin976","Documents","Article-1-data"),
    fs::path("/Volumes","KonsdysforiregisterKaramanis")
  ),
  create_folders = TRUE
)

fs::dir_create(fs::path(org::project$data_raw,"clean"))
fs::dir_create(fs::path(org::project$results_today,"descriptives"))
fs::dir_create(fs::path(org::project$results_today,"validation"))
fs::dir_create(fs::path(org::project$results_today,"analyses_diag"))
fs::dir_create(fs::path(org::project$results_today,"analyses_treatments"))
fs::dir_create(fs::path(org::project$results_today,"analyses_hybrid"))
fs::dir_create(fs::path(org::project$results_today,"analyses_together"))
fs::dir_create(fs::path(org::project$results_today,"hormones_surgeries_before_diagnosis"))
fs::dir_create(fs::path(org::project$results_today,"comorbidity"))
fs::dir_create(fs::path(org::project$results_today,"comorbidity_with_control_assigned"))
fs::dir_create(fs::path(org::project$results_today,"comorbidity_with_control_opposite"))

fs::dir_create(fs::path(org::project$data_raw,"natasa"))

################
# Load libraries
################

library(data.table)
library(ggplot2)

prev <- CleanDataPrevalenceGD()
d <- CleanDataIncidentGD(apply_sex_age_cleaning=TRUE)
natasa(d)
saveRDS(d, file=fs::path(org::project$data_raw,"clean","dz.RDS"))
saveRDS(d, file=fs::path(org::project$data_raw,"natasa","dz.RDS"))
haven::write_sav(d, fs::path(org::project$data_raw, "natasa", "dz.sav"))

d <- readRDS(file=fs::path(org::project$data_raw,"clean","dz.RDS"))

d[
  numF64_089_2006_01_to_2016_12==0 & 
    excluded=="No" & 
    bornSex=="Assigned female" &
    c_isSurgicalPenisTestProsth_2006_01_to_2016_12==T
  ]

d[
  c_analysisCat_F64_089_ge4=="0 F64.0/8/9 diagnosis [2006-01-01, 2016-12-31], first between 2006-2014" & 
    excluded=="No" & 
    bornSex=="Assigned female" &
    c_isSurgicalPenisTestProsth_2006_01_to_2016_12==T
  ]$numF64_089_2006_01_to_2016_12

mean(!is.na(d[
  c_analysisCat_F64_089_ge4=="00 F64.0/8/9 diagnosis [2006-01-01, 2016-12-31], first between 2006-2014" & 
    excluded=="No" & 
    bornSex=="Assigned female" &
    c_isHormone_2006_01_to_2016_12==T
  ]$c_dateFirstHormoneFTM))

mean(!is.na(d[
  c_analysisCat_F64_089_ge4=="00 F64.0/8/9 diagnosis [2006-01-01, 2016-12-31], first between 2006-2014" & 
    excluded=="No" & 
    bornSex=="Assigned female" &
    c_isHormone_2006_01_to_2016_12==T
  ]$c_dateFirstHormonePubBlock))

mean(!is.na(d[
  c_analysisCat_F64_089_ge4=="00 F64.0/8/9 diagnosis [2006-01-01, 2016-12-31], first between 2006-2014" & 
    excluded=="No" & 
    bornSex=="Assigned male" &
    c_isHormone_2006_01_to_2016_12==T
  ]$c_dateFirstHormoneMTF))

mean(!is.na(d[
  c_analysisCat_F64_089_ge4=="00 F64.0/8/9 diagnosis [2006-01-01, 2016-12-31], first between 2006-2014" & 
    excluded=="No" & 
    bornSex=="Assigned male" &
    c_isHormone_2006_01_to_2016_12==T
  ]$c_dateFirstHormonePubBlock))


# numbers
sink(fs::path(org::project$results_today,"numbers.txt"))
print("numbers of people with an F64_089 diagnosis")
sum(d$numF64_089 >= 1,na.rm=T) # 4378
print("numbers of people with first F64_089 >= 2001-01-01")
sum(d$dateFirst_F64_089 >= "2001-01-01",na.rm=T) # 4378
print("looking at exclusions")
xtabs(~excluded, data=d[dateFirst_F64_089 >= "2001-01-01"])
print("excluded 100 due to ICD8/9")
print("excluded 22 due to legal sex change before F64/0/8/9 diag")
print("excluded 166 due to Hormones/surgery before F64.0/8/9 diag")
print("left with 4090 people")
print("looking at validation dataset - first diagnosis [2006-2014]")
print(nrow(d[dateFirst_F64_089 >= "2006-01-01" & dateFirst_F64_089 <= "2014-12-31" & excluded=="No"]))
print("looking at trend dataset [2001-2015]")
print(nrow(d[dateFirst_F64_089 >= "2001-01-01" & dateFirst_F64_089 <= "2015-12-31" & excluded=="No"]))
sink()


nrow(d[dateFirst_F64_089 >= "2006-01-01" & dateFirst_F64_089 <= "2014-12-31" & excluded=="No"])
sum(d[excluded=="No"]$c_analysisCat_F64_089_ge4!="0 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]")
sum(d[excluded=="No" & dateFirst_F64_089 >= "2006-01-01"]$c_analysisCat_F64_089_ge4!="0 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]")
nrow(d[dateFirst_F64_089 >= "2006-01-01" & dateFirst_F64_089 <= "2014-12-31" & excluded=="No"])

sum(!is.na(d[excluded=="No"]$c_analysisCat_oneplusdiag))

d[LopNr == 20842]
#LossOfPeopleTreatments(d,type = "treatments")
#LossOfPeopleTreatments(d,type = "diag")

time_to_diagnosis(d)

Validate_1(d, byvar="c_analysisCat_F64_089_ge4")
Validate_1(d, byvar="c_analysisCat_F64_089_ge10")
validate_hormones_and_no_diagnoses(d)

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

analyses_together_2(
  d,
  pop=GetPop(),
  folder="analyses_together")

# losing people?
dz <- d[c_analysisCat_hybrid=="Hybrid"]
sink(fs::path(org::project$results_today,"lost_hybrid.txt"))
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

