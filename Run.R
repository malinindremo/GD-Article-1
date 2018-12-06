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

d <- CleanData()

Descriptives_1(d)


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


