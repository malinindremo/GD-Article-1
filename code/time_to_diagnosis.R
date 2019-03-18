time_to_diagnosis <- function(d){
  ugly_table <- d[
    dateFirst_F64_089 >= "2005-07-01" & 
    dateFirst_F64_089 <= "2016-12-31"
    ,.(
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
    years_to_surgery_hormones_p25=quantile(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones,probs = 0.5,na.rm=T),
    
    years_to_F64_089_2_p75=quantile(years_to_F64_089_2,probs = 0.75, na.rm=T),
    years_to_F64_089_3_p75=quantile(years_to_F64_089_3,probs = 0.75,na.rm=T),
    years_to_F64_089_4_p75=quantile(years_to_F64_089_4,probs = 0.75,na.rm=T),
    years_to_F64_089_5_p75=quantile(years_to_F64_089_5,probs = 0.75,na.rm=T),
    years_to_F64_089_6_p75=quantile(years_to_F64_089_6,probs = 0.75,na.rm=T),
    years_to_F64_089_7_p75=quantile(years_to_F64_089_7,probs = 0.75,na.rm=T),
    years_to_F64_089_8_p75=quantile(years_to_F64_089_8,probs = 0.75,na.rm=T),
    years_to_F64_089_9_p75=quantile(years_to_F64_089_9,probs = 0.75,na.rm=T),
    years_to_F64_089_10_p75=quantile(years_to_F64_089_10,probs = 0.75,na.rm=T),
    years_to_surgery_hormones_p75=quantile(c_analysisCat_treatments_years_to_first_date_of_surgery_hormones,probs = 0.5,na.rm=T)
    
  )]
  
  ugly_table <- melt(
    ugly_table,
    measure = patterns("_p50$", "_p25$", "_p75$"),
    value.name = c("p50", "p25", "p75"), 
    variable.factor = FALSE
  )
  
  ugly_table[,variable:=glue::glue("{X}th F64.0/8/9",X=as.numeric(variable)+1)]
  ugly_table[variable=="2th F64.0/8/9",variable:="2nd F64.0/8/9"]
  ugly_table[variable=="3th F64.0/8/9",variable:="3rd F64.0/8/9"]
  ugly_table[variable=="11th F64.0/8/9",variable:="Surgery/hormones"]
  
  ugly_table[,variable:=factor(variable,levels=variable)]
  
  openxlsx::write.xlsx(
    ugly_table, 
    fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_X.xlsx"
    )
  )
  
  q <- ggplot(ugly_table, aes(x=variable, ymin=p25, y=p50, ymax=p75))
  q <- q + geom_pointrange()
  q <- q + scale_y_continuous("Years from first F64.0/8/9 diagnosis\n(median and 25th/75th percentiles)")
  q <- q + scale_x_discrete("Until Xth F64.0/8/9 diagnosis or Surgery/Hormones")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="\nFirst F64.0/8/9 diagnosis between 2005-07-01 and 2016-12-31")
  SaveA4(
    q, 
    filename = fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_X.png"
    ))
  
}