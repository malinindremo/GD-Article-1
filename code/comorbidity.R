comorbidity <- function(dz,folder="comorbidity"){
  
  co <- stringr::str_subset(names(d),"^comorbid")
  
  # by sex age
  res <- dz[, lapply(.SD, sum), keyby = .(c_analysisCat_hybrid, bornSex, c_analysisAgeCat_hybrid), .SDcols = c("N",co)]
  openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,folder,"by_sex_age.xlsx"))
  
  long <- melt.data.table(res,id.vars=c("c_analysisCat_hybrid","bornSex","c_analysisAgeCat_hybrid","N"))
  long[,cat:=glue::glue("{bornSex} {c_analysisAgeCat_hybrid}",
                        bornSex=bornSex,
                        c_analysisAgeCat_hybrid=c_analysisAgeCat_hybrid)]
  long[,cat:=factor(cat,levels=unique(long$cat))]
  long[,prop:=value/N]
  long[,lab:=glue::glue("{prop}%",prop=round(prop*100))]
  
  q <- ggplot(long, aes(x=cat,y=prop,fill=c_analysisCat_hybrid))
  q <- q + geom_col(position = position_dodge(width=0.5))
  q <- q + geom_text(mapping=aes(label=lab,y=prop+0.01),vjust=0,size=3)
  q <- q + facet_wrap(~variable)
  q <- q + scale_y_continuous("Percentage",labels=scales::percent,limits=c(0,max(long$prop)*1.1))
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  q
  SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,"by_sex_age.png"))
  
  # by sex year
  res <- dz[, lapply(.SD, sum), keyby = .(c_analysisCat_hybrid, bornSex, c_analysisYear_hybrid), .SDcols = c("N",co)]
  openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,folder,"by_sex_year.xlsx"))
  
  
  for(cx in co){
    to_plot <- res[,c("c_analysisCat_hybrid","bornSex","c_analysisYear_hybrid","N",cx),with=F]
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
    # check slopes
    to_plot[,id:=1:.N]
    expanded <- to_plot[rep(seq(1, nrow(to_plot)), to_plot$N)]
    expanded[,i:=1:.N,by=id]
    expanded[,outcome:= i<= get(cx) ]
    
    includes_controls <- FALSE
    if(nrow(to_plot[c_analysisCat_hybrid!="Hybrid"])>0) includes_controls <- TRUE
    
    if(includes_controls){
      fit0 <- lm(
        outcome~c_analysisYear_hybrid+c_analysisCat_hybrid, 
        data=expanded[bornSex=="Assigned female"]
      )
      fit1 <- lm(
        outcome~c_analysisYear_hybrid*c_analysisCat_hybrid, 
        data=expanded[bornSex=="Assigned female"]
      )
      summary(fit0)
      summary(fit1)
      p_assigned_female <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
      p_assigned_female <- formatC(round(p_assigned_female,2),format="f", digits=2)
      
      fit0 <- lm(
        outcome~c_analysisYear_hybrid+c_analysisCat_hybrid, 
        data=expanded[bornSex=="Assigned male"]
      )
      fit1 <- lm(
        outcome~c_analysisYear_hybrid*c_analysisCat_hybrid, 
        data=expanded[bornSex=="Assigned male"]
      )
      summary(fit0)
      summary(fit1)
      p_assigned_male <- lmtest::lrtest(fit1,fit0)$"Pr(>Chisq)"[2]
      p_assigned_male <- formatC(round(p_assigned_male,2),format="f", digits=2)
      
      caption <- glue::glue(
        "Within assigned female, testing time*is_control: pvalue={p_assigned_female}\n",
        "Within assigned male, testing time*is_control: pvalue={p_assigned_male}\n",
      )
    }
    
    q <- ggplot(to_plot,aes(x=c_analysisYear_hybrid,y=p,ymin=l_95,ymax=u_95,color=c_analysisCat_hybrid))
    q <- q + geom_pointrange(position=position_dodge(width = 0.5))
    q <- q + geom_text(data=to_plot[c_analysisCat_hybrid=="Hybrid"],mapping=aes(label=glue::glue("{n}/{N}"),y=1.01),size=3,angle=90,hjust=0)
    if(includes_controls) q <- q + geom_text(data=to_plot[c_analysisCat_hybrid!="Hybrid"],mapping=aes(label=glue::glue("{n}/{N}"),y=1.11),size=3,angle=90,hjust=0)
    q <- q + facet_wrap(~bornSex)
    q <- q + scale_y_continuous("Percentage",labels=scales::percent,breaks=seq(0,1,0.2))
    q <- q + expand_limits(y=c(0,1.05))
    if(includes_controls) q <- q + expand_limits(y=c(0,1.15))
    if(includes_controls) q <- q + labs(caption=caption)
    q
    SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,glue::glue("by_sex_year_{cx}.png")))
  }
  
  # by age sex year
  res <- dz[, lapply(.SD, sum), keyby = .(c_analysisCat_hybrid, bornSex, c_analysisYearCat_hybrid, c_analysisAgeCat_hybrid), .SDcols = c("N",co)]
  openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,folder,"by_age_sex_year.xlsx"))
  
  #restricted_co <- c("comorbid_F84","comorbid_F90")
  for(cx in co){
    to_plot <- res[,c("c_analysisCat_hybrid", "bornSex","c_analysisYearCat_hybrid","c_analysisAgeCat_hybrid","N",cx),with=F]
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
    q <- ggplot(to_plot,aes(x=c_analysisYearCat_hybrid,y=p,ymin=l_95,ymax=u_95,color=c_analysisCat_hybrid))
    q <- q + geom_pointrange(position=position_dodge(width = 0.5))
    q <- q + geom_text(data=to_plot[c_analysisCat_hybrid=="Hybrid"],mapping=aes(label=glue::glue("{n}/{N}"),y=1.01),size=3,angle=90,hjust=0)
    if(nrow(to_plot[c_analysisCat_hybrid!="Hybrid"])>0) q <- q + geom_text(data=to_plot[c_analysisCat_hybrid!="Hybrid"],mapping=aes(label=glue::glue("{n}/{N}"),y=1.15),size=3,angle=90,hjust=0)
    q <- q + facet_grid(bornSex~c_analysisAgeCat_hybrid)
    q <- q + scale_y_continuous("Percentage",labels=scales::percent,breaks=seq(0,1,0.2))
    q <- q + expand_limits(y=c(0,1.1))
    if(nrow(to_plot[c_analysisCat_hybrid!="Hybrid"])>0) q <- q + expand_limits(y=c(0,1.25))
    q
    SaveA4(q, fs::path(org::PROJ$SHARED_TODAY,folder,glue::glue("by_age_sex_year_{cx}.png")))
  }
}