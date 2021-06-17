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
    fs::path("/volumes", "data", "box", "Indremo", "results", "P1"),
    fs::path("/Volumes", "data", "box", "Indremo", "results", "P1"),
    fs::path("/Users","malin976", "Box Sync", "Gender dysphoria", "results", "P1"),
    fs::path("/Users","Georgios", "Box Sync", "Gender dysphoria", "results", "P1")
  ),
  data_raw = c(
    fs::path("/volumes","data","local","org","data_raw","code_minor","2018","GD-Article-1"),
    fs::path("/Volumes","data","local","org","data_raw","code_minor","2018","GD-Article-1"),
    fs::path("/Users","malin976","Documents","Article-1-data"),
    fs::path("/Volumes","KonsdysforiregisterKaramanis")
  ),
  folders_to_be_sourced = "R_P1",
  create_folders = TRUE
)

fs::dir_create(fs::path(org::project$data_raw,"P1","clean"))
fs::dir_create(fs::path(org::project$data_raw,"P1","gunnar"))
fs::dir_create(fs::path(org::project$results_today,"descriptives"))
fs::dir_create(fs::path(org::project$results_today,"validation"))
fs::dir_create(fs::path(org::project$results_today,"analyses_diag_start_2001"))
fs::dir_create(fs::path(org::project$results_today,"analyses_diag_start_2001_weighted_by_coverage"))
fs::dir_create(fs::path(org::project$results_today,"analyses_diag_start_2004"))
fs::dir_create(fs::path(org::project$results_today,"analyses_diag_start_2004_weighted_by_coverage"))
fs::dir_create(fs::path(org::project$results_today,"analyses_treatments"))
fs::dir_create(fs::path(org::project$results_today,"analyses_hybrid"))
fs::dir_create(fs::path(org::project$results_today,"analyses_together"))
fs::dir_create(fs::path(org::project$results_today,"analyses_together_weighted_by_coverage"))
fs::dir_create(fs::path(org::project$results_today,"hormones_surgeries_before_diagnosis"))
fs::dir_create(fs::path(org::project$results_today,"comorbidity_2"))
#fs::dir_create(fs::path(org::project$results_today,"comorbidity_with_control_assigned"))
#fs::dir_create(fs::path(org::project$results_today,"comorbidity_with_control_opposite"))

fs::dir_create(fs::path(org::project$data_raw,"P1","natasa"))

################
# Load libraries
################

library(data.table)
library(ggplot2)
library(logbin)
# devtools::install_github("jackwasey/icd")

prev <- CleanDataPrevalenceGD()
d <- CleanDataIncidentGD(apply_sex_age_cleaning=TRUE)
xtabs(~d$excluded, addNA=T)
xtabs(~d$c_analysisCat_diag)
natasa(d)
saveRDS(d, file=fs::path(org::project$data_raw,"P1","clean","dz.RDS"))
saveRDS(d, file=fs::path(org::project$data_raw,"P1","natasa","dz.RDS"))
haven::write_sav(d, fs::path(org::project$data_raw,"P1", "natasa", "dz.sav"))
haven::write_dta(d, fs::path(org::project$data_raw,"P1", "gunnar", "d_richard_to_gunnar.sav"))

d <- readRDS(file=fs::path(org::project$data_raw,"P1","clean","dz.RDS"))

# coverage?/
coverage <- d[!is.na(lan),.(
  N=.N
), keyby=.(
  year = lubridate::year(dateFirst_F64_089),
  lan
)]
coverage <- dcast.data.table(
  coverage,
  lan ~ year,
  fill = 0
)
coverage[, `NA` := NULL]
writexl::write_xlsx(coverage, fs::path(org::project$results_today,"f64_089_coverage_by_lan.xlsx"))



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
xtabs(~d$excluded)
xtabs(~d$c_analysisCat_diag)
xtabs(~d[excluded=="No"]$c_analysisCat_diag)

sink(fs::path(org::project$results_today,"numbers.txt"))
print("numbers of people with an F64_089 diagnosis")
sum(d$numF64_089 >= 1,na.rm=T) # 4480
print("numbers of people with first F64_089 >= 2001-01-01")
sum(d$dateFirst_F64_089 >= "2001-01-01",na.rm=T) # 4378
print("looking at exclusions")
xtabs(~excluded, data=d[dateFirst_F64_089 >= "2001-01-01"])
print("excluded 100 due to ICD8/9")
print("excluded 22 due to legal sex change before F64/0/8/9 diag")
print("excluded 166 due to Hormones/surgery before F64.0/8/9 diag")
print("excluded 4 due to first F64.0/8/9 diag before 10 years old")
print("left with 4086 people")
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

# Validate_1(d, byvar="c_analysisCat_F64_089_ge4")

# figure 1 > validation/
# sup table 1 > validation/
Validate_1(d, byvar="c_analysisCat_F64_089_2006_01_to_2014_12_ge10", time_period_followup = "2006_01_to_2016_12")
Validate_1(d, byvar="c_analysisCat_F64_089_2006_01_to_2009_12_ge10", time_period_followup = "2006_01_to_2011_12")
Validate_1(d, byvar="c_analysisCat_F64_089_2010_01_to_2014_12_ge10", time_period_followup = "2010_01_to_2016_12")
validate_hormones_and_no_diagnoses(d)

d_oneplusdiag <- d[c_analysisCat_oneplusdiag=="numF64_089>=1, first diag: [2001-01-01, 2015-12-31]" & excluded=="No"]
nrow(d_oneplusdiag)
d_oneplusdiag[,analysisCat_z:=c_analysisCat_oneplusdiag]
d_oneplusdiag[,analysisYear_z:=c_analysisYear_oneplusdiag]
d_oneplusdiag[,analysisAgeCat_z:=c_analysisAgeCat_oneplusdiag]

dz <- d[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2001-01-01, 2015-12-31], first hormones/surgery>=2001-01-01" & excluded=="No"]
nrow(dz)
dz[,analysisCat_z:=c_analysisCat_treatments]
dz[,analysisYear_z:=c_analysisYear_treatments]
dz[,analysisAgeCat_z:=c_analysisAgeCat_treatments]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag, pop=GetPop(), folder="analyses_treatments")

# Figure 4 > analyses_diag_start_2001 > per_year_by_born_sex_age_incidence_no_caption
# Figure 4 > analyses_diag_start_2004 > per_year_by_born_sex_age_incidence_no_caption
dz <- d[c_analysisCat_diag=="numF64_089>=4, first diag: [2001-01-01, 2015-12-31]" & excluded=="No"]
dz[,analysisCat_z:=c_analysisCat_diag]
dz[,analysisYear_z:=c_analysisYear_diag]
dz[,analysisAgeCat_z:=c_analysisAgeCat_diag]
Analyses_1(dz=dz[analysisYear_z>=2001], d_oneplusdiag=d_oneplusdiag[c_analysisYear_diag>=2001],pop=GetPop(), folder="analyses_diag_start_2001")
Analyses_1(dz=dz[analysisYear_z>=2004], d_oneplusdiag=d_oneplusdiag[c_analysisYear_diag>=2004],pop=GetPop(), folder="analyses_diag_start_2004")

# Figure 4 > analyses_diag_start_2001_weighted_by_coverage > per_year_by_born_sex_age_incidence_no_caption
# Figure 4 > analyses_diag_start_2004_weighted_by_coverage > per_year_by_born_sex_age_incidence_no_caption
Analyses_1(dz=dz[analysisYear_z>=2001], d_oneplusdiag=d_oneplusdiag[c_analysisYear_diag>=2001],pop=GetPop(weighted = T), folder="analyses_diag_start_2001_weighted_by_coverage")
Analyses_1(dz=dz[analysisYear_z>=2004], d_oneplusdiag=d_oneplusdiag[c_analysisYear_diag>=2004],pop=GetPop(weighted = T), folder="analyses_diag_start_2004_weighted_by_coverage")


dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded=="No"]
dz[,analysisCat_z:=c_analysisCat_hybrid]
dz[,analysisYear_z:=c_analysisYear_hybrid]
dz[,analysisAgeCat_z:=c_analysisAgeCat_hybrid]
Analyses_1(dz=dz, d_oneplusdiag=d_oneplusdiag,pop=GetPop(), folder="analyses_hybrid")

# Figure 2 > analyses_together
# Figure 3 > analyses_together
analyses_together_2(
  d,
  pop_for_diagnosis = GetPop(), 
  pop_for_legal_sex_change = GetPop(), 
  folder="analyses_together")

# Figure 2 > analyses_together
# Figure 3 > analyses_together
analyses_together_2(
  d,
  pop_for_diagnosis = GetPop(weighted = T), 
  pop_for_legal_sex_change = GetPop(), 
  folder="analyses_together_weighted_by_coverage")

# losing people?
dz <- d[c_analysisCat_hybrid=="Hybrid"]
sink(fs::path(org::project$results_today,"lost_hybrid.txt"))
xtabs(~dz$excluded)
sink()

# comorbidity
dz <- d[
  excluded=="No" &
  c_analysisCat_diag %in% c(
    "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]",
    "control_assigned",
    "control_opposite"
  )
]
dz[,N:=1]
xtabs(~dz$c_analysisCat_diag)
comorbidity_2(dz=dz, folder="comorbidity_2")

# ### end here
# # comorbidity - old
# dz <- d[c_analysisCat_hybrid=="Hybrid" & excluded=="No"]
# dz[,N:=1]
# comorbidity(dz=dz, folder="comorbidity")
# 
# xtabs(~d$c_analysisCat_hybrid)
# dz <- d[c_analysisCat_hybrid %in% c("Hybrid","control_assigned") & excluded=="No"]
# dz[,N:=1]
# comorbidity(dz=dz, folder="comorbidity_with_control_assigned")
# 
# unique(d$c_analysisCat_hybrid)
# dz <- d[c_analysisCat_hybrid %in% c("Hybrid","control_opposite") & excluded=="No"]
# dz[,N:=1]
# comorbidity(dz=dz, folder="comorbidity_with_control_opposite")

