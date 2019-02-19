Analyses_1 <- function(dz, pop){
  agg <- dz[,
            .(
              N=.N
            ),keyby=.(
              bornSex,
              analysisCat_z,
              analysisYear_z
            )][CJ(unique(dz$bornSex),
                  unique(dz$analysisCat_z),
                  unique(dz$analysisYear_z))
               ,allow.cartesian= TRUE]
  agg[is.na(N), N:=0]
  xlsx::write.xlsx(dcast.data.table(agg,bornSex+analysisCat_z~analysisYear_z),
                   file.path(FOLDERS$results_today,"analyses_1","raw_numbers.xlsx"))
  
  # plot 1
  # number of diagnoses
  # all ages, by born sex
  agg <- dz[,
            .(
              N=.N
            ),keyby=.(
              bornSex,
              analysisYear_z
            )][CJ(unique(dz$bornSex),
                  unique(dz$analysisYear_z))
               ,allow.cartesian= TRUE]
  agg[is.na(N), N:=0]
  
  agg <- merge(agg,pop[ageCat=="All"],
               by.x=c("analysisYear_z","bornSex"),
               by.y=c("year","bornSex"))
  openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex.xlsx"))
  
  q <- ggplot(agg,aes(x=analysisYear_z,y=N,colour=bornSex))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex.png"))
  
  q <- ggplot(agg,aes(x=analysisYear_z,y=N/pop*10000,colour=bornSex))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex_incidence.png"))
  
  # plot 2
  # number of diagnoses
  # age categories, by born sex
  agg <- dz[,
            .(
              N=.N
            ),keyby=.(
              bornSex,
              analysisAgeCat_z,
              analysisYear_z
            )][CJ(unique(dx$bornSex),
                  unique(dx$analysisAgeCat_z),
                  unique(dx$analysisYear_z))
               ,allow.cartesian= TRUE]
  agg[is.na(N), N:=0]
  
  agg <- merge(agg,pop,
               by.x=c("analysisYear_z","bornSex","analysisAgeCat_z"),
               by.y=c("year","bornSex","ageCat"))
  agg[,analysisAgeCat_z:=factor(analysisAgeCat_z,levels=c(
    "[0,18]",
    "(18,30]",
    "(30,50]",
    "(50,200]"
  ))]
  openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex_age.xlsx"))
  
  q <- ggplot(agg,aes(x=analysisYear_z,y=N,colour=analysisAgeCat_z))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(.~bornSex)
  q <- q + scale_color_brewer("Age at first\ndiagnosis",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex_age.png"))
  
  q <- ggplot(agg,aes(x=analysisYear_z,y=N/pop*10000,colour=analysisAgeCat_z))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(.~bornSex)
  #q <- q + scale_color_brewer("Age",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex_age_incidence.png"))
  
  q <- ggplot(agg[analysisAgeCat_z=="[0,18]"],aes(x=analysisYear_z,y=N))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(.~bornSex)
  q <- q + scale_color_brewer("Age at first\ndiagnosis",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex_0-18.png"))
  
  q <- ggplot(agg[analysisAgeCat_z=="[0,18]"],aes(x=analysisYear_z,y=N/pop*10000))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(.~bornSex)
  #q <- q + scale_color_brewer("Age",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","per_year_by_born_sex_0-18_incidence.png"))
  
  # plot 3
  # number of sex changes
  # all ages, by born sex
  agg <- dz[,
            .(
              N=.N
            ),keyby=.(
              bornSex,
              yearSexChange
            )][CJ(unique(dz$bornSex),
                  2001:2015)
               ,allow.cartesian= TRUE]
  agg[is.na(N), N:=0]
  
  agg <- merge(agg,pop[ageCat=="All"],
               by.x=c("yearSexChange","bornSex"),
               by.y=c("year","bornSex"))
  openxlsx::write.xlsx(agg, file.path(FOLDERS$results_today,"analyses_1","sex_change_per_year_by_born_sex.xlsx"))
  
  q <- ggplot(agg,aes(x=yearSexChange,y=N,colour=bornSex))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year of sex change",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","sex_change_per_year_by_born_sex.png"))
  
  q <- ggplot(agg,aes(x=yearSexChange,y=N/pop*10000,colour=bornSex))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year of sex change",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, file.path(FOLDERS$results_today,"analyses_1","sex_change_per_year_by_born_sex_incidence.png"))
  
  
}