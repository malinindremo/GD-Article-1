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
    fs::path("/filr", "Shared with Me", "Gender dysphoria -shared folder", "results"),
    fs::path("/Users","malin976","Filr","Mina filer","Gender dysphoria -shared folder", "results"),
    fs::path("/Users","Georgios","Filr","Shared with Me","Malin-results")
  ),
  DATA_RAW = c(
    fs::path("/data","org","data_raw","code_minor","2018","GD-Article-1"),
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

dz <- d[!is.na(c_analysisCat_treatments) & excluded_treatments=="No"]
nrow(dz)
dz[,analysisCat_z:=c_analysisCat_treatments]
dz[,analysisYear_z:=c_analysisYear_treatments]
dz[,analysisAgeCat_z:=c_analysisAgeCat_treatments]
Analyses_1(dz,pop=GetPop(), folder="analyses_treatments")

dz <- d[!is.na(c_analysisCat_diag) & excluded_diag=="No"]
dz[,analysisCat_z:=c_analysisCat_diag]
dz[,analysisYear_z:=c_analysisYear_diag]
dz[,analysisAgeCat_z:=c_analysisAgeCat_diag]
Analyses_1(dz,pop=GetPop(), folder="analyses_diag")

dz <- d[!is.na(c_analysisCat_hybrid) & excluded_hybrid=="No"]
dz[,analysisCat_z:=c_analysisCat_hybrid]
dz[,analysisYear_z:=c_analysisYear_hybrid]
dz[,analysisAgeCat_z:=c_analysisAgeCat_hybrid]
Analyses_1(dz,pop=GetPop(), folder="analyses_hybrid")

dz <- d[!is.na(c_analysisCat_hybrid) & excluded_hybrid=="No"]
analyses_together(dz,pop=GetPop(), folder="analyses_together")

# comorbidity
dz <- d[!is.na(c_analysisCat_hybrid) & excluded_hybrid=="No"]
dz[,N:=1]
co <- stringr::str_subset(names(d),"^comorbid")

# by sex age
res <- dz[, lapply(.SD, sum), keyby = .(bornSex, c_analysisAgeCat_hybrid), .SDcols = c("N",co)]
openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,"comorbidity","by_sex_age.xlsx"))

long <- melt.data.table(res,id.vars=c("bornSex","c_analysisAgeCat_hybrid","N"))
long[,cat:=glue::glue("{bornSex} {c_analysisAgeCat_hybrid}",
                      bornSex=bornSex,
                      c_analysisAgeCat_hybrid=c_analysisAgeCat_hybrid)]
long[,cat:=factor(cat,levels=unique(long$cat))]
long[,prop:=value/N]
long[,lab:=glue::glue("{prop}%",prop=round(prop*100))]

q <- ggplot(long, aes(x=cat,y=prop))
q <- q + geom_col()
q <- q + geom_text(mapping=aes(label=lab,y=prop+0.01),vjust=0,size=3)
q <- q + facet_wrap(~variable)
q <- q + scale_y_continuous("Percentage",labels=scales::percent,limits=c(0,max(long$prop)*1.1))
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
q
SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,"comorbidity","by_sex_age.png"))

# by sex year
res <- dz[, lapply(.SD, sum), keyby = .(bornSex, c_analysisYear_hybrid), .SDcols = c("N",co)]
openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,"comorbidity","by_sex_year.xlsx"))

for(cx in co){
  to_plot <- res[,c("bornSex","c_analysisYear_hybrid","N",cx),with=F]
  to_plot[,n:=get(cx)]
  to_plot[,p:=-9]
  to_plot[,l_95:=-9]
  to_plot[,u_95:=-9]
  for(i in 1:nrow(to_plot)){
    fit <- stats::binom.test(to_plot[[cx]][i],to_plot$N[i])
    to_plot[i,p:=fit$estimate]
    to_plot[i,l_95:=fit$conf.int[1]]
    to_plot[i,u_95:=fit$conf.int[2]]
  }
  q <- ggplot(to_plot,aes(x=c_analysisYear_hybrid,y=p,ymin=l_95,ymax=u_95))
  q <- q + geom_pointrange()
  q <- q + geom_text(mapping=aes(label=glue::glue("{n}/{N}"),y=1.01),size=3,angle=90,hjust=0)
  q <- q + facet_wrap(~bornSex)
  q <- q + scale_y_continuous("Percentage",labels=scales::percent,breaks=seq(0,1,0.2))
  q <- q + expand_limits(y=c(0,1.05))
  q
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,"comorbidity",glue::glue("by_sex_year_{cx}.png")))
}

# by age sex year
res <- dz[, lapply(.SD, sum), keyby = .(bornSex, c_analysisYear_hybrid, c_analysisAgeCat_hybrid), .SDcols = c("N",co)]
openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,"comorbidity","by_age_sex_year.xlsx"))

restricted_co <- c("comorbid_F84","comorbid_F90")
for(cx in restricted_co){
  to_plot <- res[,c("bornSex","c_analysisYear_hybrid","c_analysisAgeCat_hybrid","N",cx),with=F]
  to_plot[,n:=get(cx)]
  to_plot[,p:=-9]
  to_plot[,l_95:=-9]
  to_plot[,u_95:=-9]
  for(i in 1:nrow(to_plot)){
    fit <- stats::binom.test(to_plot[[cx]][i],to_plot$N[i])
    to_plot[i,p:=fit$estimate]
    to_plot[i,l_95:=fit$conf.int[1]]
    to_plot[i,u_95:=fit$conf.int[2]]
  }
  q <- ggplot(to_plot,aes(x=c_analysisYear_hybrid,y=p,ymin=l_95,ymax=u_95))
  q <- q + geom_pointrange()
  q <- q + geom_text(mapping=aes(label=glue::glue("{n}/{N}"),y=1.01),size=3,angle=90,hjust=0)
  q <- q + facet_grid(bornSex~c_analysisAgeCat_hybrid)
  q <- q + scale_y_continuous("Percentage",labels=scales::percent,breaks=seq(0,1,0.2))
  q <- q + expand_limits(y=c(0,1.1))
  q
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,"comorbidity",glue::glue("by_age_sex_year_{cx}.png")))
}


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



