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
  legal_sexchange_applications <- readxl::read_excel(fs::path(org::PROJ$HOME,"structural_data","legal_sex_change.xlsx"))
  setDT(legal_sexchange_applications)
  legal_sexchange_applications[pop[ageCat=="All" & bornSex=="All"],on="year",pop:=pop]
  setnames(legal_sexchange_applications, c("analysisYear_z","N","pop"))
  legal_sexchange_applications[,definition:="Legal change"]
  legal_sexchange_applications[,bornSex:="Total"]
  
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
    fs::path(org::PROJ$SHARED_TODAY,folder,"raw_numbers.xlsx")
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
  openxlsx::write.xlsx(agg_together, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex.xlsx"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_incidence.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_incidence_OVERLAYWITH1PLUSDIAG.png"))
  
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
    "[0,18]",
    "(18,30]",
    "(30,50]",
    "(50,200]"
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
    "[0,18]",
    "(18,30]",
    "(30,50]",
    "(50,200]"
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
  openxlsx::write.xlsx(agg_together, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_age.xlsx"))
  
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
    data=agg[analysisAgeCat_z=="[0,18]"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="[0,18]"],
    family="poisson"
  )
  p[["Within [0,18], testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(18,30]"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(18,30]"],
    family="poisson"
  )
  p[["Within (18,30], testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(30,50]"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(30,50]"],
    family="poisson"
  )
  p[["Within (30,50], testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  fit0 <- glm(
    N~analysisYear_z+bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(50,200]"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z*bornSex+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(50,200]"],
    family="poisson"
  )
  p[["Within (50,200], testing time*assigned sex"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  
  #####
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="[0,18]" & bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="[0,18]" & bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within [0,18] & assigned male, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within [0,18] & assigned male, testing time"]] <- extract_poisson(fit1)
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="[0,18]" & bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="[0,18]" & bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within [0,18] & assigned female, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within [0,18] & assigned female, testing time"]] <- extract_poisson(fit1)
  
  #####
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="(30,50]" & bornSex=="Assigned male"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(30,50]" & bornSex=="Assigned male"],
    family="poisson"
  )
  p[["Within (30,50] & assigned male, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within (30,50] & assigned male, testing time"]] <- extract_poisson(fit1)
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg[analysisAgeCat_z=="(30,50]" & bornSex=="Assigned female"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg[analysisAgeCat_z=="(30,50]" & bornSex=="Assigned female"],
    family="poisson"
  )
  p[["Within (30,50] & assigned female, testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within (30,50] & assigned female, testing time"]] <- extract_poisson(fit1)
  
  #####
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="(18,30]"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="(18,30]"],
    family="poisson"
  )
  p[["Within (18,30], testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within (18,30], testing time"]] <- extract_poisson(fit1)
  
  fit0 <- glm(
    N~offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="(50,200]"],
    family="poisson"
  )
  fit1 <- glm(
    N~analysisYear_z+offset(log(pop)),
    data=agg_sexes_together[analysisAgeCat_z=="(50,200]"],
    family="poisson"
  )
  p[["Within (50,200], testing time"]] <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
  est[["Within (50,200], testing time"]] <- extract_poisson(fit1)
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_age.png"))
  
  q <- ggplot(agg,aes(x=analysisYear_z,y=N/pop*10000,colour=analysisAgeCat_z))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + facet_grid(.~bornSex)
  #q <- q + scale_color_brewer("Age",palette="Set1")
  q <- q + scale_x_continuous("Year",
                              breaks=seq(2001,2020,2))
  q <- q + scale_y_continuous("Number of people/10,000 population")
  q <- q + theme_gray(16)
  q <- q + labs(caption=caption)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_age_incidence.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_age_incidence_OVERLAYWITH1PLUSDIAG.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_0-18.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_0-18_incidence.png"))
  
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
  openxlsx::write.xlsx(agg, fs::path(org::PROJ$SHARED_TODAY,folder,"sex_change_per_year_by_born_sex.xlsx"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"sex_change_per_year_by_born_sex.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"sex_change_per_year_by_born_sex_incidence.png"))
  
  
}



analyses_together <- function(dz, d_oneplusdiag, prev, pop, folder){
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
                   fs::path(org::PROJ$SHARED_TODAY,folder,"raw_numbers.xlsx"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex.png"))
  
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
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"per_year_by_born_sex_with_prevalence.png"))
  
  
}






