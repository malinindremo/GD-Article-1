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

ov <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/ov.sas7bdat")))
sv <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/sv.sas7bdat")))
demografi <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/demografi.sas7bdat")))
sex <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/kon.sas7bdat")))

demografi[,dob:=as.Date(sprintf(
  "%s-%s-%s",
  stringr::str_sub(fodelseman,1,4),
  stringr::str_sub(fodelseman,5,6),
  "15"))]
demografi[DodDatum!="",dod:=as.Date(sprintf(
  "%s-%s-%s",
  stringr::str_sub(DodDatum,1,4),
  stringr::str_sub(DodDatum,5,6),
  stringr::str_sub(DodDatum,7,8)
))]
demografi[,fodelseman:=NULL]
demografi[,DodDatum:=NULL]
setnames(demografi,"lopnr","LopNr")

ov[,type:="outpatient"]
sv[,type:="inpatient"]
patients <- rbind(ov,sv,fill=T)

setorder(patients,LopNr,INDATUM)

patients[,isF64_089:=FALSE]
patients[,isF64_0:=FALSE]
patients[,isF64_89:=FALSE]
patients[,isTranssexual_ICD_89:=FALSE]
for(i in stringr::str_subset(names(patients), "^DIA")){
  patients[stringr::str_detect(get(i),"^F640"), isF64_0:=TRUE]
  patients[stringr::str_detect(get(i),"^F648"), isF64_89:=TRUE]
  patients[stringr::str_detect(get(i),"^F649"), isF64_89:=TRUE]
  patients[get(i)=="302,31",isTranssexual_ICD_89:=TRUE]
  patients[get(i)=="302,99",isTranssexual_ICD_89:=TRUE]
  patients[get(i)=="302X",isTranssexual_ICD_89:=TRUE]
  #patients[stringr::str_detect(get(i),"^302"), isTranssexual_ICD_89:=TRUE]
}
patients[,isF64_089:=isF64_0 | isF64_89]

patients[,hadTranssexual_ICD_89:=as.logical(max(isTranssexual_ICD_89)),by=LopNr]

xtabs(~patients$isTranssexual_ICD_89)
xtabs(~patients$hadTranssexual_ICD_89+patients$isF64_089)

d <- patients[isF64_089==TRUE]
nrow(d)
d <- merge(d,demografi,by="LopNr")
nrow(d)

xtabs(~d$hadTranssexual_ICD_89)

stringr::str_subset(sort(unique(c(
  patients$DIA1,
  patients$DIA2,
  patients$DIA3,
  patients$DIA4,
  patients$DIA5,
  patients$DIA6,
  patients$DIA7,
  patients$DIA8,
  patients$DIA9,
  patients$DIA10,
  patients$DIA11,
  patients$DIA12,
  patients$DIA13,
  patients$DIA14,
  patients$DIA15,
  patients$DIA16,
  patients$DIA17
  ))),
  "^302")

d[,dateFirst:=min(INDATUM),by=LopNr]
d[,yearFirst:=RAWmisc::YearN(dateFirst),by=LopNr]
d[,ageFirst:=as.numeric(difftime(dateFirst,dob,units="days"))/365.25,by=LopNr]
d[,age:=as.numeric(difftime(INDATUM,dob,units="days"))/365.25,by=LopNr]
d[,ageFirstCat:=cut(ageFirst,breaks=c(0,12,15,20,30,100))]

d[,days:=as.numeric(difftime(INDATUM,dateFirst,units="days")),by=LopNr]

d[isF64_0==T,daysFirst_F64_0:=min(days),by=LopNr]
d[,daysFirst_F64_0:=min(daysFirst_F64_0,na.rm=T),by=LopNr]
d[is.infinite(daysFirst_F64_0),daysFirst_F64_0:=NA]

d[isF64_89==T,daysFirst_F64_89:=min(days),by=LopNr]
d[,daysFirst_F64_89:=min(daysFirst_F64_89,na.rm=T),by=LopNr]
d[is.infinite(daysFirst_F64_89),daysFirst_F64_89:=NA]

d[isF64_0==T,daysLast_F64_0:=max(days),by=LopNr]
d[,daysLast_F64_0:=min(daysLast_F64_0,na.rm=T),by=LopNr]
d[is.infinite(daysLast_F64_0),daysLast_F64_0:=NA]

d[isF64_89==T,daysLast_F64_89:=max(days),by=LopNr]
d[,daysLast_F64_89:=min(daysLast_F64_89,na.rm=T),by=LopNr]
d[is.infinite(daysLast_F64_89),daysLast_F64_89:=NA]

d[isF64_0==T,ageFirst_F64_0:=min(age),by=LopNr]
d[,ageFirst_F64_0:=min(ageFirst_F64_0,na.rm=T),by=LopNr]
d[is.infinite(ageFirst_F64_0),ageFirst_F64_0:=NA]

d[isF64_89==T,ageFirst_F64_89:=min(age),by=LopNr]
d[,ageFirst_F64_89:=min(ageFirst_F64_89,na.rm=T),by=LopNr]
d[is.infinite(ageFirst_F64_89),ageFirst_F64_89:=NA]

d[,category:=as.character(NA)]
d[is.na(daysFirst_F64_0) & !is.na(daysFirst_F64_89),category:="Only F64.8/9"]
d[!is.na(daysFirst_F64_0) & is.na(daysFirst_F64_89),category:="Only F64.0"]
d[daysFirst_F64_0==0 & daysFirst_F64_89==0,category:="F64.0 and F64.8/9 at first consult"]
d[daysLast_F64_0==daysLast_F64_89,category:="F64.0 and F64.8/9 at last consult"]
d[daysFirst_F64_0==0 & daysFirst_F64_89>0 & daysLast_F64_0>daysLast_F64_89,category:="F64.0 -> F64.8/9 -> F64.0"]
d[daysFirst_F64_0==0 & daysFirst_F64_89>0 & daysLast_F64_0<daysLast_F64_89,category:="F64.0 -> F64.8/9"]
d[daysFirst_F64_0>0 & daysFirst_F64_89==0 & daysLast_F64_89>daysLast_F64_0,category:="F64.8/9 -> F64.0 -> F64.8/9"]
d[daysFirst_F64_0>0 & daysFirst_F64_89==0 & daysLast_F64_89<daysLast_F64_0,category:="F64.8/9 -> F64.0"]
d[is.na(category)]
xtabs(~d$category,addNA=T)


toPlot <- d[,.(
  numF64_089=sum(isF64_089),
  numF64_0=sum(isF64_0),
  numF64_89=sum(isF64_89),
  daysFirst_F64_0=mean(daysFirst_F64_0),
  daysFirst_F64_89=mean(daysFirst_F64_89),
  daysLast_F64_0=mean(daysLast_F64_0),
  daysLast_F64_89=mean(daysLast_F64_89),
  ageFirst_F64_0=mean(ageFirst_F64_0),
  ageFirst_F64_89=mean(ageFirst_F64_89)
),by=.(LopNr,ageFirst,yearFirst,category,hadTranssexual_ICD_89)]
toPlot[,firstAgeCat:=cut(ageFirst,breaks=seq(0,100,2))]
toPlot[,firstF64_0_ageCat:=cut(ageFirst_F64_0,breaks=seq(0,100,1))]
toPlot[,firstF64_89_ageCat:=cut(ageFirst_F64_89,breaks=seq(0,100,1))]

toPlot[,hadTranssexual_ICD_89:=sprintf("hadTranssexual_ICD_89=%s",hadTranssexual_ICD_89)]

q <- ggplot(toPlot,aes(x=category))
q <- q + geom_bar(alpha=0.75)
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("Number of people")
q <- q + coord_flip()
#q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
#q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
SaveA4(q,
       file.path(FOLDERS$results_today,"descriptives","diagnosis_categories.png"),
       landscape=T)


q <- ggplot(toPlot,aes(x=category))
q <- q + geom_bar(alpha=0.75)
q <- q + facet_wrap(~hadTranssexual_ICD_89)
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("Number of people")
q <- q + coord_flip()
#q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
#q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
SaveA4(q,
       file.path(FOLDERS$results_today,"descriptives","diagnosis_categories_by_hadTranssexual_ICD_89.png"),
       landscape=T)

q <- ggplot(toPlot,aes(x=category))
q <- q + geom_bar(alpha=0.75)
q <- q + facet_wrap(~yearFirst)
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("Number of people")
q <- q + coord_flip()
q <- q + labs(title="People displayed by the year of first F68.0/8/9 diagnosis")
#q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
SaveA4(q,
       file.path(FOLDERS$results_today,"descriptives","diagnosis_categories_by_year.png"),
       landscape=T)

q <- ggplot(toPlot,aes(x=category,y=ageFirst))
q <- q + geom_boxplot(alpha=0.75)
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("Age at first consultation",breaks=seq(0,100,4))
q <- q + coord_flip()
#q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
SaveA4(q,
       file.path(FOLDERS$results_today,"descriptives","diagnosis_categories_age_first_consult.png"),
       landscape=T)


q <- ggplot(toPlot,aes(x=ageFirst,y=daysFirst_F64_0/365.25))
q <- q + geom_point(alpha=0.2)
q <- q + geom_smooth()
q <- q + facet_wrap(~category)
q <- q + scale_x_continuous("Age at first consultation",breaks=seq(0,100,4))
q <- q + scale_y_continuous("Years waiting until first F64.0 diagnosis",breaks=seq(0,20,2))
#q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
SaveA4(q,
       file.path(FOLDERS$results_today,"descriptives","time_until_first_F64_0.png"),
       landscape=T,
       scalev=1)

q <- ggplot(toPlot,aes(x=ageFirst,y=daysFirst_F64_89/365.25))
q <- q + geom_point(alpha=0.2)
q <- q + geom_smooth()
q <- q + facet_wrap(~category)
q <- q + scale_x_continuous("Age at first consultation",breaks=seq(0,100,4))
q <- q + scale_y_continuous("Years waiting until first F64.8/9 diagnosis",breaks=seq(0,20,2))
#q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
SaveA4(q,
       file.path(FOLDERS$results_today,"descriptives","time_until_first_F64_89.png"),
       landscape=T,
       scalev=1)


x <- d[,.(
  numF64_089=sum(isF64_089),
  numF64_0=sum(isF64_0),
  numF64_89=sum(isF64_89),
  daysFirst_F64_0=mean(daysFirst_F64_0),
  daysFirst_F64_89=mean(daysFirst_F64_89)
),by=.(LopNr,ageFirstCat,category,yearFirst)]

tab <- x[,.(
  N=.N,
  `Mean num of F64.0/8/9 diagnoses`=round(mean(numF64_089),1),
  `Mean num of F64.0 diagnoses`=round(mean(numF64_0),1),
  `Mean num of F64.8/9 diagnoses`=round(mean(numF64_89),1),
  `Percentage with 2 or more F64.0 diagnoses`=round(100*mean(numF64_0>=2)),
  `Percentage with 3 or more F64.0 diagnoses`=round(100*mean(numF64_0>=3)),
  `Percentage with 2 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=2)),
  `Percentage with 3 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=3)),
  `Percentage with 2 or more F64.0/8/9 diagnoses`=round(100*mean(numF64_089>=2)),
  `Percentage with 3 or more F64.0/8/9 diagnoses`=round(100*mean(numF64_089>=3)),
  `Percentage diagnosed with only F64.0`=round(100*mean(!is.na(daysFirst_F64_0) & is.na(daysFirst_F64_89))),
  `Percentage diagnosed with only F64.8/9`=round(100*mean(is.na(daysFirst_F64_0) & !is.na(daysFirst_F64_89))),
  `Percentage diagnosed with both F64.0 and F64.8/9`=round(100*mean(!is.na(daysFirst_F64_0) & !is.na(daysFirst_F64_89))),
  `For people diagnosed first with F64.8/9, mean days spent at F64.8/9 before F64.0`=round(mean(daysFirst_F64_0[daysFirst_F64_89==0],na.rm=T))
),keyby=.(category)]

openxlsx::write.xlsx(tab, file=
                       file.path(FOLDERS$results_today,"descriptives","categories.xlsx"))

tab <- x[,.(
  N=.N,
  `Mean num of F64.0/8/9 diagnoses`=round(mean(numF64_089),1),
  `Mean num of F64.0 diagnoses`=round(mean(numF64_0),1),
  `Mean num of F64.8/9 diagnoses`=round(mean(numF64_89),1),
  `Percentage with 2 or more F64.0 diagnoses`=round(100*mean(numF64_0>=2)),
  `Percentage with 3 or more F64.0 diagnoses`=round(100*mean(numF64_0>=3)),
  `Percentage with 2 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=2)),
  `Percentage with 3 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=3)),
  `Percentage diagnosed with only F64.0`=round(100*mean(!is.na(daysFirst_F64_0) & is.na(daysFirst_F64_89))),
  `Percentage diagnosed with only F64.8/9`=round(100*mean(is.na(daysFirst_F64_0) & !is.na(daysFirst_F64_89))),
  `Percentage diagnosed with both F64.0 and F64.8/9`=round(100*mean(!is.na(daysFirst_F64_0) & !is.na(daysFirst_F64_89))),
  `For people diagnosed first with F64.8/9, mean days spent at F64.8/9 before F64.0`=round(mean(daysFirst_F64_0[daysFirst_F64_89==0],na.rm=T))
),keyby=.(yearFirst,category)]

openxlsx::write.xlsx(tab, file=
                       file.path(FOLDERS$results_today,"descriptives","categories_by_year.xlsx"))

xtabs(~patients$identF64)
patients[identF64==T]
# Author:     Thomas Frisell
# Date:        20180803
# What/Why:     A program to calculate the incidence rate of transseuxalism in Sweden by year and sex, 
#               standardized to the age distribution in 2010.

# I will use the "tidyverse" family of packages, first they need to be read in:


# select picks only lopnr and indatum, while filter picks only visits with a code of F64
in_ov <- select(filter_at(ov, vars(HDIA, DIA1:DIA30), any_vars(substr(.,1,3) == "F64")), LopNr, INDATUM)
in_sv <- select(filter_at(sv, vars(HDIA, DIA1:DIA30), any_vars(substr(.,1,3) == "F64")), LopNr, INDATUM)

# Visists from in- and outpatient data is combined in one file, stacked on top of each other
F64 <- arrange(rbind(in_ov, in_sv), LopNr, INDATUM)

# Sort by indatum, keeping only the first ever diagnosis for every patient

firstF64 <- distinct(top_n(group_by(F64, LopNr), 1, desc(INDATUM)))

# Exclude patients who were incident before the analysis period
in_period<-filter(firstF64, INDATUM > as.Date('2000-12-31'))

#mutate birthdate
new<- mutate(dem, new1 =paste0(fodelseman, "15"), birthdate=ymd(new1))

# Add age and sex data 
info_sex <- left_join(in_period, kon, by="LopNr")
info_byear <- left_join(info_sex, select(new, lopnr, birthdate ), by=c("LopNr"="lopnr"))

# mutate is used to calculate age from the difference between birthdate and indatum
info_age = mutate(info_byear, age=floor((INDATUM-birthdate)/365.25), agecat=floor(age/5))

# Sum the number of incident cases by sex and age
n_by_year= summarize(group_by(info_age, year(INDATUM), kon, agecat), n=n())

# To fit with the data on the population size, I here transpose (turn) the table, rows into columns
transp <- spread(n_by_year, key='year(INDATUM)', value=n, fill=0)
# And I also sort the transposed table
transp <- arrange(transp, agecat, kon)

# Here I read in the population size, downloaded from Statistics Sweden
popsize <- as.tibble(read.csv("/Users/david/Documents/Malins/könsdysfori/popsize.csv", sep=";"))

# The incidence rates for every year, age, sex combination is calculated by dividing the corresponding
# cells of the table with the cases, and the table with the population size


# I'll re-add the columns which identified what age and sex the incidence rate was for
IRanot <- as.tibble(cbind(popsize$?lder, popsize$k?n, IR))

# If there are any missing values in the table, they should be zero
IRanot[is.na(IRanot)] <- 0

# Almost done, but now we need to standardize the individual incidence rates to the age distribution in 2010, 
# which will simplify the presentation a lot. For this we need to calculate the age-distribution among men and women in 2010
tot2010 <- summarize(group_by(popsize, k?n), tot2010=sum(X2010))
preweight <- left_join(popsize, tot2010, by="k?n")
preweight2 <- select(mutate(preweight, sexweight=X2010/tot2010), ?lder, k?n, sexweight)

toweight <- left_join(IRanot, preweight2, by=c(`popsize$?lder`='?lder',`popsize$k?n`='k?n'))
# Each IR is multiplied by how common that age-group was in 2010
weight <- mutate_at(toweight, vars(`2001`:`2016`), funs(. * sexweight))
# By now summing across age-groups, we end up with the age-standardized IR for men and women
sir <- summarize_at(group_by(weight, popsize$k?n),vars(`2001`:`2016`), sum)

# The data is done, but reformating the table helps with plotting
forplot <- gather(sir, key=year, value=IR, `2001`:`2016`)
forplot <- rename(forplot, sex=`popsize$k?n`)

# I'll save the incidence rates in a separate file, to perhaps use them in the future
write_csv(forplot, path="H:/Projects/Transsex/Incidence rates/R/Data/forplot.csv")

# A standard plot, can be changed in many ways
ggplot(data=forplot, aes(x=year, y=IR, group=sex))+geom_line(aes(color=sex))


