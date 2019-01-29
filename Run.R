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
  dir.create(file.path(FOLDERS$results_today,"validation"))
  dir.create(file.path(FOLDERS$results_today,"analyses"))
})

################
# Load libraries
################

library(data.table)
library(ggplot2)

d <- CleanData()
dx <- d[!is.na(analysisCat_x)]

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


# plot 1
# number of diagnoses
# all ages, by born sex
agg <- dx[,
  .(
    N=.N
  ),keyby=.(
    bornSex,
    analysisYear_x
  )][CJ(unique(dx$bornSex),
        unique(dx$analysisYear_x))
     ,allow.cartesian= TRUE]
agg[is.na(N), N:=0]

agg <- merge(agg,pop[ageCat=="All"],
             by.x=c("analysisYear_x","bornSex"),
             by.y=c("year","bornSex"))
openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_per_year_by_born_sex.xlsx"))

q <- ggplot(agg,aes(x=analysisYear_x,y=N,colour=bornSex))
q <- q + geom_line()
q <- q + geom_point()
q <- q + scale_color_brewer("",palette="Set1")
q <- q + scale_x_continuous("Year of first F64.0/8/9 diagnosis",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_per_year_by_born_sex.png"))

q <- ggplot(agg,aes(x=analysisYear_x,y=N/pop*10000,colour=bornSex))
q <- q + geom_line()
q <- q + geom_point()
q <- q + scale_color_brewer("",palette="Set1")
q <- q + scale_x_continuous("Year of first F64.0/8/9 diagnosis",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people/10,000 population")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_per_year_by_born_sex_incidence.png"))

# plot 2
# number of diagnoses
# age categories, by born sex
agg <- dx[,
          .(
            N=.N
          ),keyby=.(
            bornSex,
            analysisAgeCat_x,
            analysisYear_x
          )][CJ(unique(dx$bornSex),
                unique(dx$analysisAgeCat_x),
                unique(dx$analysisYear_x))
             ,allow.cartesian= TRUE]
agg[is.na(N), N:=0]

agg <- merge(agg,pop,
             by.x=c("analysisYear_x","bornSex","analysisAgeCat_x"),
             by.y=c("year","bornSex","ageCat"))
agg[,analysisAgeCat_x:=factor(analysisAgeCat_x,levels=c(
  "[0,18]",
  "(18,30]",
  "(30,50]",
  "(50,200]"
))]
openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_per_year_by_born_sex_age.xlsx"))

q <- ggplot(agg,aes(x=analysisYear_x,y=N,colour=analysisAgeCat_x))
q <- q + geom_line()
q <- q + geom_point()
q <- q + facet_grid(.~bornSex)
q <- q + scale_color_brewer("Age at first\ndiagnosis",palette="Set1")
q <- q + scale_x_continuous("Year of first F64.0/8/9 diagnosis",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_per_year_by_born_sex_age.png"))

q <- ggplot(agg,aes(x=analysisYear_x,y=N/pop*10000,colour=analysisAgeCat_x))
q <- q + geom_line()
q <- q + geom_point()
q <- q + facet_grid(.~bornSex)
q <- q + scale_color_brewer("Age at first\nF64.0/8/9 diagnosis",palette="Set1")
q <- q + scale_x_continuous("Year of first F64.0/8/9 diagnosis",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people/10,000 population")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_per_year_by_born_sex_age_incidence.png"))

# plot 3
# number of sex changes
# all ages, by born sex
agg <- dx[,
          .(
            N=.N
          ),keyby=.(
            bornSex,
            yearSexChange
          )][CJ(unique(dx$bornSex),
                2001:2015)
             ,allow.cartesian= TRUE]
agg[is.na(N), N:=0]

agg <- merge(agg,pop[ageCat=="All"],
             by.x=c("yearSexChange","bornSex"),
             by.y=c("year","bornSex"))
openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_sex_change_per_year_by_born_sex.xlsx"))

q <- ggplot(agg,aes(x=yearSexChange,y=N,colour=bornSex))
q <- q + geom_line()
q <- q + geom_point()
q <- q + scale_color_brewer("",palette="Set1")
q <- q + scale_x_continuous("Year of sex change",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_sex_change_per_year_by_born_sex.png"))

q <- ggplot(agg,aes(x=yearSexChange,y=N/pop*10000,colour=bornSex))
q <- q + geom_line()
q <- q + geom_point()
q <- q + scale_color_brewer("",palette="Set1")
q <- q + scale_x_continuous("Year of sex change",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people/10,000 population")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_sex_change_per_year_by_born_sex_incidence.png"))

# plot 4
# number of sex changes
# age categories, by born sex
agg <- dx[,
          .(
            N=.N
          ),keyby=.(
            bornSex,
            analysisAgeCat_x,
            yearSexChange
          )][CJ(unique(dx$bornSex),
                unique(dx$analysisAgeCat_x),
                2001:2015)
             ,allow.cartesian= TRUE]
agg[is.na(N), N:=0]

agg <- merge(agg,pop,
             by.x=c("yearSexChange","bornSex","analysisAgeCat_x"),
             by.y=c("year","bornSex","ageCat"))
agg[,analysisAgeCat_x:=factor(analysisAgeCat_x,levels=c(
  "[0,18]",
  "(18,30]",
  "(30,50]",
  "(50,200]"
))]
openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_sex_change_per_year_by_born_sex_age.xlsx"))

q <- ggplot(agg,aes(x=yearSexChange,y=N,colour=analysisAgeCat_x))
q <- q + geom_line()
q <- q + geom_point()
q <- q + facet_grid(.~bornSex)
q <- q + scale_color_brewer("Age at first\nF64.0/8/9 diagnosis",palette="Set1")
q <- q + scale_x_continuous("Year of sex change",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_sex_change_per_year_by_born_sex_age.png"))

q <- ggplot(agg,aes(x=yearSexChange,y=N/pop*10000,colour=analysisAgeCat_x))
q <- q + geom_line()
q <- q + geom_point()
q <- q + facet_grid(.~bornSex)
q <- q + scale_color_brewer("Age at first\nF64.0/8/9 diagnosis",palette="Set1")
q <- q + scale_x_continuous("Year of sex change",
                            breaks=seq(2001,2020,2))
q <- q + scale_y_continuous("Number of people/10,000 population")
q <- q + theme_gray(20)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
q
SaveA4(q, file.path(FOLDERS$results_today,"analyses","F64_089_ge_3_sex_change_per_year_by_born_sex_age_incidence.png"))











