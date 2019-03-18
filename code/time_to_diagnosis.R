time_to_diagnosis <- function(d){
  ugly_table <- d[,.(
    years_to_F64_089_2_p50=quantile(years_to_F64_089_2,probs = 0.5, na.rm=T),
    years_to_F64_089_3_p50=quantile(years_to_F64_089_3,probs = 0.5,na.rm=T),
    years_to_F64_089_4_p50=quantile(years_to_F64_089_4,probs = 0.5,na.rm=T),
    years_to_F64_089_5_p50=quantile(years_to_F64_089_5,probs = 0.5,na.rm=T),
    years_to_F64_089_6_p50=quantile(years_to_F64_089_6,probs = 0.5,na.rm=T),
    years_to_F64_089_7_p50=quantile(years_to_F64_089_7,probs = 0.5,na.rm=T),
    years_to_F64_089_8_p50=quantile(years_to_F64_089_8,probs = 0.5,na.rm=T),
    years_to_F64_089_9_p50=quantile(years_to_F64_089_9,probs = 0.5,na.rm=T),
    years_to_F64_089_10_p50=quantile(years_to_F64_089_10,probs = 0.5,na.rm=T),
    
    years_to_F64_089_2_p25=quantile(years_to_F64_089_2,probs = 0.25, na.rm=T),
    years_to_F64_089_3_p25=quantile(years_to_F64_089_3,probs = 0.25,na.rm=T),
    years_to_F64_089_4_p25=quantile(years_to_F64_089_4,probs = 0.25,na.rm=T),
    years_to_F64_089_5_p25=quantile(years_to_F64_089_5,probs = 0.25,na.rm=T),
    years_to_F64_089_6_p25=quantile(years_to_F64_089_6,probs = 0.25,na.rm=T),
    years_to_F64_089_7_p25=quantile(years_to_F64_089_7,probs = 0.25,na.rm=T),
    years_to_F64_089_8_p25=quantile(years_to_F64_089_8,probs = 0.25,na.rm=T),
    years_to_F64_089_9_p25=quantile(years_to_F64_089_9,probs = 0.25,na.rm=T),
    years_to_F64_089_10_p25=quantile(years_to_F64_089_10,probs = 0.25,na.rm=T),
    
    years_to_F64_089_2_p75=quantile(years_to_F64_089_2,probs = 0.75, na.rm=T),
    years_to_F64_089_3_p75=quantile(years_to_F64_089_3,probs = 0.75,na.rm=T),
    years_to_F64_089_4_p75=quantile(years_to_F64_089_4,probs = 0.75,na.rm=T),
    years_to_F64_089_5_p75=quantile(years_to_F64_089_5,probs = 0.75,na.rm=T),
    years_to_F64_089_6_p75=quantile(years_to_F64_089_6,probs = 0.75,na.rm=T),
    years_to_F64_089_7_p75=quantile(years_to_F64_089_7,probs = 0.75,na.rm=T),
    years_to_F64_089_8_p75=quantile(years_to_F64_089_8,probs = 0.75,na.rm=T),
    years_to_F64_089_9_p75=quantile(years_to_F64_089_9,probs = 0.75,na.rm=T),
    years_to_F64_089_10_p75=quantile(years_to_F64_089_10,probs = 0.75,na.rm=T)
    
  )]

  ugly_table <- melt(
    ugly_table,
    measure = patterns("_p50$", "_p25$", "_p75$"),
    value.name = c("p50", "p25", "p75"), 
    variable.factor = FALSE
    )
  
  ugly_table[,variable:=as.numeric(variable)+1]
  
  ugly_table[,var:=glue::glue("Years to {X} F64 0/8/9 diagnoses",X=variable)]
  ugly_table[,var:=glue::glue("Years to {X} F64 0/8/9 diagnoses",X=variable)]
  
  openxlsx::write.xlsx(
    ugly_table, 
    fs::path(
     org::PROJ$SHARED_TODAY,
     "descriptives",
     "time_to_diagnosis.xlsx"
    )
  )
  
  q <- ggplot(ugly_table, aes(x=as.factor(variable), ymin=p25, y=p50, ymax=p75))
  q <- q + geom_pointrange()
  q <- q + scale_y_continuous("Years from first to Xth F64.0/8/9 diagnosis\n(median and 25th/75th percentiles)")
  q <- q + scale_x_discrete("Xth F64.0/8/9 diagnosis")
  q <- q + theme_gray(20)
  SaveA4(
    q, 
    filename = fs::path(
      org::PROJ$SHARED_TODAY,
      "descriptives",
      "time_to_diagnosis.png"
    ))
}