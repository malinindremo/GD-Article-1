extract_surv_percentile <- function(fit, p, fitdata){
  res <- data.table(survminer::surv_summary(fit, data=fitdata))
  res[,surv_prev:=shift(surv)]
  retval <- res[surv_prev>p & surv<=p]
  retval[,surv_prev:=NULL]
  return(retval)
}

time_to_diagnosis <- function(d){
  pd <- d[
      excluded %in% c("No") &
      dateFirst_F64_089 >= "2006-01-01" & 
      dateFirst_F64_089 <= "2015-12-31" &
      (c_analysisCat_treatments_years_to_first_date_of_surgery_hormones>=0 | is.na(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones))
    ]
  pd[,event:=!is.na(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones)]
  pd[event==0,c_analysisCat_treatments_years_to_first_date_of_surgery_hormones:=difftime(as.Date("2016-12-31"),dateFirst_F64_089)/365.25]
  
  
  fit <- survival::survfit(survival::Surv(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones, event) ~ 1, data = pd)
  res <- survminer::surv_summary(fit)
  res
  
  q <- ggplot(res,aes(x=time,y=1-surv,ymin=1-lower,ymax=1-upper))
  q <- q + geom_step()
  q <- q + scale_y_continuous("P(receiving surgery/hormones)",lim=c(0,1),expand=c(0,0))
  q <- q + scale_x_continuous("Years from first F64.0/8/9 diagnosis",lim=c(0,5))
  q <- q + labs(caption="\nKaplan-Meier analysis restricted to first F64.0/8/9 diagnosis between 2006-01-01 and 2015-12-31. Follow-up ends 2016-12-31.")
  q <- q + theme_fhi_lines()
  SaveA4(
    q, 
    filename = fs::path(
      org::project$results_today,
      "descriptives",
      "time_to_hormones_surgery_survival.png"
    ))
  
  pd <- d[
    excluded %in% c("No","Hormones/surgery before F64.0/8/9 diag") &
    dateFirst_F64_089 >= "2006-01-01" & 
    dateFirst_F64_089 <= "2015-12-31"]
  pd[,cat:="No hormones/surgery (diagnosis between 2006-01-01 and 2015-12-31)"]
  pd[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2001-01-01, 2015-12-31], first hormones/surgery>=2001-01-01",cat:="Hormones/surgery after diagnosis (diagnosis between 2006-01-01 and 2015-12-31)"]
  pd[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2001-01-01, 2015-12-31], first hormones/surgery>=2001-01-01" & excluded=="Hormones/surgery before F64.0/8/9 diag",cat:="Hormones/surgery before diagnosis (diagnosis between 2006-01-01 and 2015-12-31)"]
  
  xtabs(~pd$cat)
  
  retval <- vector("list",length=10)
  for(y in 2:9){
    out <- glue::glue("years_to_F64_089_{y}")
    pd[,outcome:=get(out)]
    pd[,event:=!is.na(outcome)]
    pd[event==0,outcome:=difftime(as.Date("2016-12-31"),dateFirst_F64_089)/365.25]
    fit <- survival::survfit(survival::Surv(outcome, event) ~ cat, data = pd)
    p50 <- extract_surv_percentile(fit, p=0.5, pd)
    p25 <- extract_surv_percentile(fit, p=1-0.25, pd) # p25 = until there are 25 percent received it = 75% "alive" (not received it)
    p75 <- extract_surv_percentile(fit, p=1-0.75, pd) # p75 = until there are 75 percent received it = 25% "alive" (not received it)
    
    p50[,perc:=50]
    p25[,perc:=25]
    p75[,perc:=75]
    
    retval[[y]] <- rbind(p50,p25,p75, fill=T)
    retval[[y]]$outcome <- out
  }
  retval <- rbindlist(retval)
  retval <- retval[,c("cat","outcome","time","perc")]
  retval <- dcast.data.table(retval,cat+outcome~perc, value.var="time")
  retval[,obs:=stringr::str_extract(outcome,"[0-9]$")]
  retval[,outcome:=glue::glue("Years from 1st to {obs} F64_089 diag", obs=obs)]
  retval[,obs:=NULL]
  
  setnames(retval,c("25","50","75"),c("p25","p50","p75"))
  
  openxlsx::write.xlsx(
    retval, 
    fs::path(
      org::project$results_today,
      "descriptives",
      "time_to_X.xlsx"
    )
  )
  
  q <- ggplot(retval, aes(x=outcome, ymin=p25, y=p50, ymax=p75, color=cat))
  q <- q + geom_point(position=position_dodge(width=0.5),mapping=aes(y=p50))
  q <- q + geom_linerange(position=position_dodge(width=0.5),mapping=aes(ymin=p25,ymax=p50))
  q <- q + geom_linerange(position=position_dodge(width=0.5),mapping=aes(ymin=p50,ymax=p75))
  q <- q + scale_y_continuous("Years from first F64.0/8/9 diagnosis\n(median and 25th/75th percentiles)")
  q <- q + scale_x_discrete("Until Xth F64.0/8/9 diagnosis or Surgery/Hormones")
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + theme_fhi_lines()
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="\nKaplan-Meier analysis restricted to first F64.0/8/9 diagnosis between 2006-01-01 and 2015-12-31. Follow-up ends 2016-12-31.")
  SaveA4(
    q, 
    filename = fs::path(
      org::project$results_today,
      "descriptives",
      "time_to_X.png"
    ))
  
}