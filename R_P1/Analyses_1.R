extract_poisson <- function(fit, var="analysisYear_z"){
  est <- coef(fit)[var]
  se <- sqrt(vcov(fit)[var,var])
  retval <- glue::glue(
    "{es} ({l}, {u})",
    es=formatC(exp(est), digits=2, format="f"),
    l=formatC(exp(est-1.96*se), digits=2, format="f"),
    u=formatC(exp(est+1.96*se), digits=2, format="f")
  )
  return(retval)
}

Analyses_1 <- function(dz, d_oneplusdiag, pop, folder){
  legal_sexchange_applications <- get_legal_sex_change()
  
  agg_oneplusdiag <- d_oneplusdiag[,
                                   .(
                                     N=.N
                                   ),keyby=.(
                                     bornSex,
                                     analysisCat_z,
                                     analysisYear_z
                                   )][CJ(unique(d_oneplusdiag$bornSex),
                                         unique(d_oneplusdiag$analysisCat_z),
                                         unique(d_oneplusdiag$analysisYear_z))
                                      ,allow.cartesian= TRUE]
  agg_oneplusdiag[is.na(N), N:=0]
  
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
  tosave <- rbind(
    dcast.data.table(agg,bornSex+analysisCat_z~analysisYear_z),
    dcast.data.table(agg_oneplusdiag,bornSex+analysisCat_z~analysisYear_z),
    fill=T
  )
  xlsx::write.xlsx(
    tosave,
    fs::path(org::project$results_today,folder,"raw_numbers.xlsx")
  )
  
  # plot 1
  # number of diagnoses
  # all ages, by born sex
  agg_oneplusdiag <- d_oneplusdiag[,
                                   .(
                                     N=.N
                                   ),keyby=.(
                                     bornSex,
                                     analysisYear_z
                                   )][CJ(unique(d_oneplusdiag$bornSex),
                                         unique(d_oneplusdiag$analysisYear_z))
                                      ,allow.cartesian= TRUE]
  agg_oneplusdiag[is.na(N), N:=0]
  
  agg_oneplusdiag <- merge(agg_oneplusdiag,pop[ageCat=="All"],
                           by.x=c("analysisYear_z","bornSex"),
                           by.y=c("year","bornSex"))
  agg_oneplusdiag[,definition:="1+ diagnoses"]
  
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
  agg[,definition:=folder]
  
  agg_together <- rbind(agg_oneplusdiag, agg)
  agg_together_total <- agg_together[,.(
    N=sum(N),
    pop=sum(pop)
  ),keyby=.(
    analysisYear_z,
    ageCat,
    definition
  )]
  agg_together_total[,bornSex:="Total"]
  agg_together <- rbind(agg_together, agg_together_total)
  agg_together <- rbind(agg_together,legal_sexchange_applications,fill=T)
  openxlsx::write.xlsx(agg_together, fs::path(org::project$results_today,folder,"per_year_by_born_sex.xlsx"))
  
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
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex.png"))
  
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
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_incidence.png"))
  
  q <- ggplot(agg_together,aes(x=analysisYear_z,y=N/pop*10000,colour=definition))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + facet_wrap(~bornSex)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_incidence_OVERLAYWITH1PLUSDIAG.png"))
  
  # plot 2
  # number of diagnoses
  # age categories, by born sex
  agg_oneplusdiag <- d_oneplusdiag[,
                                   .(
                                     N=.N
                                   ),keyby=.(
                                     bornSex,
                                     analysisAgeCat_z,
                                     analysisYear_z
                                   )][CJ(unique(d_oneplusdiag$bornSex),
                                         unique(d_oneplusdiag$analysisAgeCat_z),
                                         unique(d_oneplusdiag$analysisYear_z))
                                      ,allow.cartesian= TRUE]
  agg_oneplusdiag[is.na(N), N:=0]
  
  agg_oneplusdiag <- merge(agg_oneplusdiag,pop,
                           by.x=c("analysisYear_z","bornSex","analysisAgeCat_z"),
                           by.y=c("year","bornSex","ageCat"))
  agg_oneplusdiag[,analysisAgeCat_z:=factor(analysisAgeCat_z,levels=c(
    "10-17",
    "18-30",
    "31-50",
    "51+"
  ))]
  agg_oneplusdiag[,definition:="1+ diagnoses"]
  
  agg <- dz[,
            .(
              N=.N
            ),keyby=.(
              bornSex,
              analysisAgeCat_z,
              analysisYear_z
            )][CJ(unique(dz$bornSex),
                  unique(dz$analysisAgeCat_z),
                  unique(dz$analysisYear_z))
               ,allow.cartesian= TRUE]
  agg[is.na(N), N:=0]
  
  agg <- merge(agg,pop,
               by.x=c("analysisYear_z","bornSex","analysisAgeCat_z"),
               by.y=c("year","bornSex","ageCat"))
  agg[,analysisAgeCat_z:=factor(analysisAgeCat_z,levels=c(
    "10-17",
    "18-30",
    "31-50",
    "51+"
  ))]
  agg[,definition:=folder]
  
  agg_sexes_together <- agg[,.(
    N=sum(N),
    pop=sum(pop)
  ),keyby=.(
    analysisYear_z,
    analysisAgeCat_z,
    definition
  )]
  
  agg_together <- rbind(agg_oneplusdiag, agg)
  openxlsx::write.xlsx(agg_together, fs::path(org::project$results_today,folder,"per_year_by_born_sex_age.xlsx"))
  
  ## poisson regressions
  p <- list()
  est <- list()
  fit0 <- glm(
    N~analysisYear_z+analysisAgeCat_z+offset(log(pop)),
    data=agg[bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*analysisAgeCat_z+offset(log(pop)),
    data=agg[bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within assigned female, testing time*age"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+analysisAgeCat_z+offset(log(pop)),
    data=agg[bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*analysisAgeCat_z+offset(log(pop)),
    data=agg[bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within assigned male, testing time*age"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="10-17"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="10-17"],
    family="poisson"
  )
  p[["Within 10-17, testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="18-30"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="18-30"],
    family="poisson"
  )
  p[["Within 18-30, testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="31-50"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="31-50"],
    family="poisson"
  )
  p[["Within 31-50, testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="51+"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="51+"],
    family="poisson"
  )
  p[["Within 51+, testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  ##### 10-17 aM
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="10-17" & bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="10-17" & bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within 10-17 & assigned male, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 10-17 & assigned male, testing time"]] <- extract_poisson(fit1)
  
  # 10-17 aF
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="10-17" & bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="10-17" & bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within 10-17 & assigned female, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 10-17 & assigned female, testing time"]] <- extract_poisson(fit1)
  
  ##### 18-30 aM
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="18-30" & bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="18-30" & bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within 18-30 & assigned male, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 18-30 & assigned male, testing time"]] <- extract_poisson(fit1)
  
  # 18-30 aF
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="18-30" & bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="18-30" & bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within 18-30 & assigned female, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 18-30 & assigned female, testing time"]] <- extract_poisson(fit1)
  
  #####
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="31-50" & bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="31-50" & bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within 31-50 & assigned male, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 31-50 & assigned male, testing time"]] <- extract_poisson(fit1)
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="31-50" & bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="31-50" & bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within 31-50 & assigned female, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 31-50 & assigned female, testing time"]] <- extract_poisson(fit1)
  
  ##### 51+ aM
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="51+" & bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="51+" & bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within 51+ & assigned male, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 51+ & assigned male, testing time"]] <- extract_poisson(fit1)
  
  # 51+ aF
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="51+" & bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="51+" & bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within 51+ & assigned female, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 51+ & assigned female, testing time"]] <- extract_poisson(fit1)
  
  #####
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="18-30"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="18-30"],
    family="poisson"
  )
  p[["Within 18-30, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 18-30, testing time"]] <- extract_poisson(fit1)
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="51+"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="51+"],
    family="poisson"
  )
  p[["Within 51+, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within 51+, testing time"]] <- extract_poisson(fit1)
  
  p <- rbindlist(lapply(p, as.data.frame), idcol = "id")
  setnames(p, c("id","pval"))
  p[,pval:=formatC(round(pval,2),format="f", digits=2)]
  p[,pretty_val:=glue::glue("{id}: pvalue={pval}",id=id,pval=pval)]
  
  est <- rbindlist(lapply(est, as.data.frame), idcol = "id")
  setnames(est, c("id","es"))
  est[,pretty_val:=glue::glue("{id}: est={es}",id=id,es=es)]
  
  caption <- glue::glue_collapse(c(p$pretty_val, est$pretty_val), sep = "\n")
  
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
  q <- q + labs(caption=caption)
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_age.png"))
  
  q <- ggplot(agg,aes(x=analysisYear_z,y=N/pop*10000,colour=analysisAgeCat_z))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(.~bornSex)
  q <- q + scale_color_brewer("Age at first\ndiagnosis",palette="Set1")
  q <- q + scale_x_continuous("Year", breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + labs(caption=caption)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_age_incidence.png"))
  
  q <- q + labs(caption=NULL)
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_age_incidence_no_caption.png"))
  
  q <- ggplot(agg_together,aes(x=analysisYear_z,y=N/pop*10000,colour=definition))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(analysisAgeCat_z~bornSex)
  #q <- q + scale_color_brewer("Age",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_age_incidence_OVERLAYWITH1PLUSDIAG.png"))
  
  q <- ggplot(agg[analysisAgeCat_z=="10-17"],aes(x=analysisYear_z,y=N))
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
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_0-17.png"))
  
  q <- ggplot(agg[analysisAgeCat_z=="10-17"],aes(x=analysisYear_z,y=N/pop*10000))
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
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_0-17_incidence.png"))
  
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
  openxlsx::write.xlsx(agg, fs::path(org::project$results_today,folder,"sex_change_per_year_by_born_sex.xlsx"))
  
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
  SaveA4(q, fs::path(org::project$results_today,folder,"sex_change_per_year_by_born_sex.png"))
  
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
  SaveA4(q, fs::path(org::project$results_today,folder,"sex_change_per_year_by_born_sex_incidence.png"))
  
  
}



analyses_together_1 <- function(dz, d_oneplusdiag, prev, pop, folder){
  agg <- dz[,
            .(
              `Diagnoses (4+)`=sum(!is.na(c_analysisCat_diag)),
              `Diagnosis (1+) + treatment`=sum(!is.na(c_analysisCat_treatments)),
              Hybrid=sum(!is.na(c_analysisCat_hybrid))
            ),keyby=.(
              bornSex,
              c_analysisYear_hybrid
            )][CJ(unique(dz$bornSex),
                  unique(dz$c_analysisYear_hybrid))
               ,allow.cartesian= TRUE]
  
  agg_1p <- d_oneplusdiag[,
                          .(
                            `Diagnoses (1+)`=sum(!is.na(c_analysisAge_oneplusdiag))
                          ),keyby=.(
                            bornSex,
                            c_analysisYear_oneplusdiag
                          )][CJ(unique(d_oneplusdiag$bornSex),
                                unique(d_oneplusdiag$c_analysisYear_oneplusdiag))
                             ,allow.cartesian= TRUE]
  
  agg[agg_1p,on=c("bornSex==bornSex","c_analysisYear_hybrid==c_analysisYear_oneplusdiag"),`Diagnoses (1+)`:=`Diagnoses (1+)`]
  
  agg[prev,on=c("bornSex==bornSex","c_analysisYear_hybrid==year"),`Prevalence (1+ diag)`:=N]
  
  xlsx::write.xlsx(agg,
                   fs::path(org::project$results_today,folder,"raw_numbers.xlsx"))
  
  long <- melt.data.table(agg, id.vars=c("bornSex","c_analysisYear_hybrid"))
  
  q <- ggplot(long[variable!="Prevalence (1+ diag)"],aes(x=c_analysisYear_hybrid,y=value,colour=variable))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + expand_limits(y=0)
  q <- q + facet_wrap(~bornSex)
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex.png"))
  
  q <- ggplot(long,aes(x=c_analysisYear_hybrid,y=value,colour=variable))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + expand_limits(y=0)
  q <- q + facet_wrap(~bornSex)
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_born_sex_with_prevalence.png"))
  
  
}


analyses_together_2 <- function(d, pop_for_diagnosis, pop_for_legal_sex_change, folder){
  agg <- d[!is.na(c_analysisCat_oneplusdiag),
            .(
              `Diagnoses (1+)`=sum(!is.na(c_analysisCat_oneplusdiag)),
              `Diagnoses (4+)`=sum(!is.na(c_analysisCat_diag)),
              `Diagnosis (1+) + treatment`=sum(!is.na(c_analysisCat_treatments))
            ),keyby=.(
              bornSex,
              c_analysisYear_oneplusdiag
            )][CJ(unique(d$bornSex),
                  unique(d$c_analysisYear_oneplusdiag))
               ,allow.cartesian= TRUE]
  agg <- agg[!is.na(c_analysisYear_oneplusdiag)]
  aggx <- copy(agg)
  aggx[,bornSex:="All"]
  aggx <- aggx[,lapply(.SD,sum),keyby=.(bornSex,c_analysisYear_oneplusdiag)]
  
  agg <- rbind(agg,aggx)
  
  sexchange <- get_legal_sex_change()
  sexchange[,bornSex:="All"]
  setnames(sexchange,"analysisYear_z","c_analysisYear_oneplusdiag")
  agg[sexchange,on=c("bornSex","c_analysisYear_oneplusdiag"),`Legal sex change`:=N]
  
  xlsx::write.xlsx(agg,
                   fs::path(org::project$results_today,folder,"raw_numbers_by_sex.xlsx"))
  
  long <- melt.data.table(agg, id.vars=c("bornSex","c_analysisYear_oneplusdiag"))
  long[pop_for_diagnosis[ageCat=="All"],on=c("c_analysisYear_oneplusdiag==year","bornSex"),pop_for_diagnosis:=pop]
  long[pop_for_legal_sex_change[ageCat=="All"],on=c("c_analysisYear_oneplusdiag==year","bornSex"),pop_for_legal_sex_change:=pop]
  long[, pop := pop_for_diagnosis]
  long[variable=="Legal sex change", pop := pop_for_legal_sex_change]
  long[, pop_for_diagnosis := NULL]
  long[, pop_for_legal_sex_change := NULL]
  xlsx::write.xlsx(
    long,
    fs::path(org::project$results_today,folder,"raw_numbers_by_sex_long.xlsx")
  )
  
  q <- ggplot(long,aes(x=c_analysisYear_oneplusdiag,y=value,colour=variable))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + expand_limits(y=0)
  q <- q + facet_wrap(~bornSex)
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex.png"))
  
  q <- ggplot(long,aes(x=c_analysisYear_oneplusdiag,y=value/pop*10000,colour=variable))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year", breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + expand_limits(y=0)
  q <- q + facet_wrap(~bornSex)
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex_incidence_start_2001.png"), scalev=0.75)
  
  q <- q + scale_x_continuous("Year", breaks=seq(2001,2020,2), lim = c(2004, 2015))
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex_incidence_start_2004.png"), scalev=0.75)
  
  # include coverage
  d_coverage <- readxl::read_excel(fs::path(
    org::project$data_raw,
    "Coverage SKR_PAR_Psykiatri_Lakare.xlsx"
  ))
  setDT(d_coverage)
  setnames(d_coverage, c(1,6), c("year", "coverage"))
  d_coverage <- d_coverage[,.(
    year = as.numeric(year),
    coverage = coverage
  )]
  d_coverage <- na.omit(d_coverage)
  
  max_left <- max(long$value/long$pop*10000, na.rm=T)
  max_right <- max(d_coverage$coverage)
  
  d_coverage[, scaled_value_right := coverage / max_right * max_left]
  
  q <- q + geom_line(data = d_coverage, mapping = aes(x=year, y=scaled_value_right, color="Coverage"))
  q <- q + geom_point(data = d_coverage, mapping = aes(x=year, y=scaled_value_right, color="Coverage"))
  q <- q + scale_y_continuous("Number of people/10,000 population",
                              expand = expansion(mult = c(0, 0.1)),
                              sec.axis = sec_axis(
                                name = "Coverage (%)",
                                ~ . * max_right / max_left,
                                breaks = fhiplot::pretty_breaks(5),
                                labels = fhiplot::format_nor
                              )
  )
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex_incidence_start_2004_with_coverage.png"), scalev=0.75)
  
  
  agg <- d[!is.na(c_analysisCat_oneplusdiag),
           .(
             `Diagnoses (1+)`=sum(!is.na(c_analysisCat_oneplusdiag)),
             `Diagnoses (4+)`=sum(!is.na(c_analysisCat_diag)),
             `Diagnosis (1+) + treatment`=sum(!is.na(c_analysisCat_treatments))
           ),keyby=.(
             bornSex,
             c_analysisAgeCat_oneplusdiag,
             c_analysisYear_oneplusdiag
           )][CJ(unique(d$bornSex),
                 unique(d$c_analysisAgeCat_oneplusdiag),
                 unique(d$c_analysisYear_oneplusdiag))
              ,allow.cartesian= TRUE]
  agg <- agg[!is.na(c_analysisYear_oneplusdiag) & !is.na(c_analysisAgeCat_oneplusdiag)]
  for(i in names(agg)){
    agg[is.na(get(i)),(i):=0]
  }
  
  xlsx::write.xlsx(agg,
                   fs::path(org::project$results_today,folder,"raw_numbers_by_sex_age.xlsx"))
  
  long <- melt.data.table(agg, id.vars=c("bornSex","c_analysisYear_oneplusdiag","c_analysisAgeCat_oneplusdiag"))
  long[pop_for_diagnosis,on=c("c_analysisYear_oneplusdiag==year","bornSex","c_analysisAgeCat_oneplusdiag==ageCat"),pop:=pop]
  xlsx::write.xlsx(
    long,
    fs::path(org::project$results_today,folder,"raw_numbers_by_sex_age_long.xlsx")
  )
  
  q <- ggplot(long,aes(x=c_analysisYear_oneplusdiag,y=value,colour=variable))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people")
  q <- q + expand_limits(y=0)
  q <- q + facet_grid(c_analysisAgeCat_oneplusdiag~bornSex)
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + theme(legend.position="bottom")
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex_age.png"),landscape=F)
  
  q <- ggplot(long,aes(x=c_analysisYear_oneplusdiag,y=value/pop*10000,colour=variable))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + expand_limits(y=0)
  q <- q + facet_grid(c_analysisAgeCat_oneplusdiag~bornSex)
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + theme(legend.position="bottom")
  q
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex_age_incidence_2001.png"),landscape=F)
  
  q <- q + scale_x_continuous("Year", breaks=seq(2001,2020,2), lim = c(2004, 2015))
  SaveA4(q, fs::path(org::project$results_today,folder,"per_year_by_sex_age_incidence_2004.png"), landscape=F)
  
  # 3 time periods, compare
  
  agg <- d[!is.na(c_analysisYearCat_oneplusdiag),
           .(
             `Diagnoses (1+)`=sum(!is.na(c_analysisCat_oneplusdiag)),
             `Diagnoses (4+)`=sum(!is.na(c_analysisCat_diag)),
             `Diagnoses (1+) + treatment`=sum(!is.na(c_analysisCat_treatments))
           ),keyby=.(
             bornSex,
             c_analysisAgeCat_oneplusdiag,
             c_analysisYearCat_oneplusdiag
           )][CJ(unique(d$bornSex),
                 unique(d$c_analysisAgeCat_oneplusdiag),
                 unique(d$c_analysisYearCat_oneplusdiag))
              ,allow.cartesian= TRUE]
  agg <- agg[!is.na(c_analysisYearCat_oneplusdiag) & !is.na(c_analysisAgeCat_oneplusdiag)]
  for(i in names(agg)){
    agg[is.na(get(i)),(i):=0]
  }
  agg[,`Diagnoses (1+) (change from last period)`:= 
        paste0(round(100*`Diagnoses (1+)`/shift(`Diagnoses (1+)`)),"%"),
      by=.(bornSex,c_analysisAgeCat_oneplusdiag)]
  
  agg[,`Diagnoses (4+) (change from last period)`:= 
        paste0(round(100*`Diagnoses (4+)`/shift(`Diagnoses (4+)`)),"%"),
      by=.(bornSex,c_analysisAgeCat_oneplusdiag)]
  
  agg[,`Diagnoses (1+) + treatment (change from last period)`:= 
        paste0(round(100*`Diagnoses (1+) + treatment`/shift(`Diagnoses (1+) + treatment`)),"%"),
      by=.(bornSex,c_analysisAgeCat_oneplusdiag)]
  for(i in names(agg)){
    agg[get(i)=="NA%",(i):=""]
  }
  
  xlsx::write.xlsx(
    agg,
    fs::path(org::project$results_today,folder,"comparison_of_time_periods_richard.xlsx")
  )
  
  # 3 time periods, compare
  
  agg <- d[!is.na(c_analysisYearCat_oneplusdiag),
           .(
             `Diagnoses (1+)`=sum(!is.na(c_analysisCat_oneplusdiag)),
             `Diagnoses (4+)`=sum(!is.na(c_analysisCat_diag)),
             `Diagnoses (1+) + treatment`=sum(!is.na(c_analysisCat_treatments))
           ),keyby=.(
             bornSex,
             c_analysisAgeCat_oneplusdiag,
             c_analysisYearCat_oneplusdiag
           )][CJ(unique(d$bornSex),
                 unique(d$c_analysisAgeCat_oneplusdiag),
                 unique(d$c_analysisYearCat_oneplusdiag))
              ,allow.cartesian= TRUE]
  agg <- agg[!is.na(c_analysisYearCat_oneplusdiag) & !is.na(c_analysisAgeCat_oneplusdiag)]
  for(i in names(agg)){
    agg[is.na(get(i)),(i):=0]
  }
  # 
  # agg[,`Diagnoses (1+) (change from last period)`:= 
  #       paste0(round(100*`Diagnoses (1+)`/shift(`Diagnoses (1+)`)),"%"),
  #     by=.(bornSex,c_analysisAgeCat_oneplusdiag)]
  # 
  # agg[,`Diagnoses (4+) (change from last period)`:= 
  #       paste0(round(100*`Diagnoses (4+)`/shift(`Diagnoses (4+)`)),"%"),
  #     by=.(bornSex,c_analysisAgeCat_oneplusdiag)]
  # 
  # agg[,`Diagnoses (1+) + treatment (change from last period)`:= 
  #       paste0(round(100*`Diagnoses (1+) + treatment`/shift(`Diagnoses (1+) + treatment`)),"%"),
  #     by=.(bornSex,c_analysisAgeCat_oneplusdiag)]
  
  agg[`Diagnoses (4+)`!=0,`% difference Diagnosis 1+/Diagnosis 4+`:=
        paste0(round(100*`Diagnoses (1+)`/`Diagnoses (4+)`)-100,"%")
      ]
  agg[`Diagnoses (1+) + treatment`!=0,`% difference Diagnosis (1+)/Diagnosis (1+) + treatment`:=
        paste0(round(100*`Diagnoses (1+)`/`Diagnoses (1+) + treatment`)-100,"%")
      ]
  agg[]
  
  for(i in names(agg)){
    agg[is.na(get(i)),(i):=""]
  }
  
  xlsx::write.xlsx(
    agg,
    fs::path(org::project$results_today,folder,"comparison_of_time_periods_malin.xlsx")
  )
}










