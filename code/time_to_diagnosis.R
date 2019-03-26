time_to_diagnosis <- function(d){
  pd <- d[
    dateFirst_F64_089 >= "2006-01-01" & 
      dateFirst_F64_089 <= "2014-12-31" &
      c_analysisCat_treatments_years_to_first_date_of_surgery_hormones>=0
    ]
  pd[,event:=1]
  
  fit <- survival::survfit(survival::Surv(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones, event) ~ 1, data = pd)
  res <- survminer::surv_summary(fit)
  res
  
  q <- ggplot(res,aes(x=time,y=1-surv,ymin=1-lower,ymax=1-upper))
  q <- q + geom_step()
  q <- q + scale_y_continuous("P(receiving surgery/hormones)",lim=c(0,1),expand=c(0,0))
  q <- q + scale_x_continuous("Years from first F64.0/8/9 diagnosis",lim=c(0,5))
  q <- q + labs(caption="\nAnalysis only shows people who received surgery/hormones after first F64.0/8/9 diagnosis.\nFirst F64.0/8/9 diagnosis between 2006-01-01 and 2014-12-31, first surgery/hormones between 2006-01-01 and 2016-12-31.")
  q <- q + theme_fhi_lines()
  SaveA4(
    q, 
    filename = fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_hormones_surgery_survival.png"
    ))
  
  pd <- d[dateFirst_F64_089 >= "2006-01-01" & dateFirst_F64_089 <= "2014-12-31"] 
  pd[,cat:="No hormones/surgery"]
  pd[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]",cat:="Hormones/surgery after diagnosis"]
  pd[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31], H/S BEFORE F64_089",cat:="Hormones/surgery before diagnosis"]
  q <- ggplot(pd,
              aes(x=c_analysisCat_treatments_years_to_first_date_of_surgery_hormones,
                  fill=cat))
  q <- q + geom_histogram(alpha=0.5,bins=50)
  q <- q + scale_fill_brewer("",palette="Set1")
  q <- q + scale_y_continuous("Number of people",expand=c(0,0))
  q <- q + scale_x_continuous("Years from first F64.0/8/9 diagnosis")
  q <- q + labs(caption="\nAnalysis only shows people who received surgery/hormones.\nFirst F64.0/8/9 diagnosis between 2006-01-01 and 2014-12-31, first surgery/hormones between 2006-01-01 and 2016-12-31.")
  q <- q + theme_fhi_lines()
  SaveA4(
    q, 
    filename = fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_hormones_surgery.png"
    ))
  
  ugly_table <- d[
    dateFirst_F64_089 >= "2006-01-01" & 
    dateFirst_F64_089 <= "2014-12-31"
    ,.(
    N_all_years=.N,
    
    
    x_2_N=sum(!is.na(years_to_F64_089_2)),
    x_3_N=sum(!is.na(years_to_F64_089_3)),
    x_4_N=sum(!is.na(years_to_F64_089_4)),
    x_5_N=sum(!is.na(years_to_F64_089_5)),
    x_6_N=sum(!is.na(years_to_F64_089_6)),
    x_7_N=sum(!is.na(years_to_F64_089_7)),
    x_8_N=sum(!is.na(years_to_F64_089_8)),
    x_9_N=sum(!is.na(years_to_F64_089_9)),
    x_10_N=sum(!is.na(years_to_F64_089_10)),
    x_N=sum(!is.na(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones)),
    
    years_to_F64_089_2_p50=quantile(years_to_F64_089_2,probs = 0.5, na.rm=T),
    years_to_F64_089_3_p50=quantile(years_to_F64_089_3,probs = 0.5,na.rm=T),
    years_to_F64_089_4_p50=quantile(years_to_F64_089_4,probs = 0.5,na.rm=T),
    years_to_F64_089_5_p50=quantile(years_to_F64_089_5,probs = 0.5,na.rm=T),
    years_to_F64_089_6_p50=quantile(years_to_F64_089_6,probs = 0.5,na.rm=T),
    years_to_F64_089_7_p50=quantile(years_to_F64_089_7,probs = 0.5,na.rm=T),
    years_to_F64_089_8_p50=quantile(years_to_F64_089_8,probs = 0.5,na.rm=T),
    years_to_F64_089_9_p50=quantile(years_to_F64_089_9,probs = 0.5,na.rm=T),
    years_to_F64_089_10_p50=quantile(years_to_F64_089_10,probs = 0.5,na.rm=T),
    years_to_surgery_hormones_p50=quantile(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones,probs = 0.5,na.rm=T),
    
    years_to_F64_089_2_p25=quantile(years_to_F64_089_2,probs = 0.25, na.rm=T),
    years_to_F64_089_3_p25=quantile(years_to_F64_089_3,probs = 0.25,na.rm=T),
    years_to_F64_089_4_p25=quantile(years_to_F64_089_4,probs = 0.25,na.rm=T),
    years_to_F64_089_5_p25=quantile(years_to_F64_089_5,probs = 0.25,na.rm=T),
    years_to_F64_089_6_p25=quantile(years_to_F64_089_6,probs = 0.25,na.rm=T),
    years_to_F64_089_7_p25=quantile(years_to_F64_089_7,probs = 0.25,na.rm=T),
    years_to_F64_089_8_p25=quantile(years_to_F64_089_8,probs = 0.25,na.rm=T),
    years_to_F64_089_9_p25=quantile(years_to_F64_089_9,probs = 0.25,na.rm=T),
    years_to_F64_089_10_p25=quantile(years_to_F64_089_10,probs = 0.25,na.rm=T),
    years_to_surgery_hormones_p25=quantile(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones,probs = 0.25,na.rm=T),
    
    years_to_F64_089_2_p75=quantile(years_to_F64_089_2,probs = 0.75, na.rm=T),
    years_to_F64_089_3_p75=quantile(years_to_F64_089_3,probs = 0.75,na.rm=T),
    years_to_F64_089_4_p75=quantile(years_to_F64_089_4,probs = 0.75,na.rm=T),
    years_to_F64_089_5_p75=quantile(years_to_F64_089_5,probs = 0.75,na.rm=T),
    years_to_F64_089_6_p75=quantile(years_to_F64_089_6,probs = 0.75,na.rm=T),
    years_to_F64_089_7_p75=quantile(years_to_F64_089_7,probs = 0.75,na.rm=T),
    years_to_F64_089_8_p75=quantile(years_to_F64_089_8,probs = 0.75,na.rm=T),
    years_to_F64_089_9_p75=quantile(years_to_F64_089_9,probs = 0.75,na.rm=T),
    years_to_F64_089_10_p75=quantile(years_to_F64_089_10,probs = 0.75,na.rm=T),
    years_to_surgery_hormones_p75=quantile(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones,probs = 0.75,na.rm=T)
    
  ),keyby=.(c_analysisCat_treatments)]
  
  ugly_table <- melt(
    ugly_table,
    measure = patterns( "_N$", "_p50$", "_p25$", "_p75$"),
    value.name = c("N", "p50", "p25", "p75"), 
    variable.factor = FALSE
  )
  
  ugly_table[,variable:=glue::glue("{X}th F64.0/8/9",X=as.numeric(variable)+1)]
  ugly_table[variable=="2th F64.0/8/9",variable:="2nd F64.0/8/9"]
  ugly_table[variable=="3th F64.0/8/9",variable:="3rd F64.0/8/9"]
  ugly_table[variable=="11th F64.0/8/9",variable:="Surgery/hormones"]
  
  ugly_table[,variable:=factor(variable,levels=unique(variable))]
  
  ugly_table[,cat:="No hormones/surgery"]
  ugly_table[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]",cat:="Hormones/surgery after diagnosis"]
  ugly_table[c_analysisCat_treatments=="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31], H/S BEFORE F64_089",cat:="Hormones/surgery before diagnosis"]
  
  openxlsx::write.xlsx(
    ugly_table, 
    fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_X.xlsx"
    )
  )
  
  q <- ggplot(ugly_table, aes(x=variable, ymin=p25, y=p50, ymax=p75, color=cat))
  q <- q + geom_pointrange(position=position_dodge(width=0.5))
  q <- q + scale_y_continuous("Years from first F64.0/8/9 diagnosis\n(median and 25th/75th percentiles)")
  q <- q + scale_x_discrete("Until Xth F64.0/8/9 diagnosis or Surgery/Hormones")
  q <- q + scale_color_brewer("",palette="Set1")
  q <- q + theme_fhi_lines()
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="\nFirst F64.0/8/9 diagnosis between 2006-01-01 and 2014-12-31")
  SaveA4(
    q, 
    filename = fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_X.png"
    ))
  
}