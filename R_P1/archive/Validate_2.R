Validate_2_int <- function(d,num){
  analysisData <- d[!is.na(get(sprintf("analysisCat_%s",num)))]
  analysisData[,incidentYear:=get(sprintf("analysisYear_%s",num))]
  analysisData[,analysisName:=get(sprintf("analysisCat_%s",num))]
  
  agg <- analysisData[!is.na(incidentYear),.(
    N=.N
  ),keyby=.(
    analysisName,
    isBornMale,
    isSurgicalMasectomy_2005_07_to_2016_12,
    isHormone_2005_07_to_2016_12
  )]
  agg[,perc:=round(100*N/sum(N))]
  
  return(agg)
}

Validate_2 <- function(d){
  
  
  xtabs(~d$analysisCat_y+d$analysisCat_1,addNA=T)
  
  pd <- d[!is.na(analysisCat_y),.(
    N=.N
  ),keyby=.(
    analysisYear_y,
    analysisCat_y,
    bornSex,
    hormoneAndSurgicalMasectomyCat_y
  )]
  pd[,denom:=sum(N),by=.(
    analysisYear_y,
    bornSex,
    analysisCat_y
  )]
  
  
  xlsx::write.xlsx2(pd, file=
                      file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_all_ages.xlsx"))
  
  pd[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y,"diag:","diag:\n")]
  
  q <- ggplot(pd,aes(x=analysisYear_y,y=N,fill=hormoneAndSurgicalMasectomyCat_y))
  q <- q + geom_col()
  q <- q + facet_grid(bornSex~analysisCat_y_pretty)
  q <- q + scale_fill_brewer("",palette="Set1")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_all_ages_numbers.png"))
  
  q <- ggplot(pd,aes(x=analysisYear_y,y=N/denom,fill=hormoneAndSurgicalMasectomyCat_y))
  q <- q + geom_col()
  q <- q + facet_grid(bornSex~analysisCat_y_pretty)
  q <- q + scale_y_continuous(labels=scales::percent)
  q <- q + scale_fill_brewer("",palette="Set1")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_all_ages_percentages.png"))
  
  # same as above, but collapsing by year
  
  pd <- d[!is.na(analysisCat_y) & analysisYear_y<=2015,.(
    N=.N
  ),keyby=.(
    analysisCat_y,
    bornSex,
    hormoneAndSurgicalMasectomyCat_y
  )]
  pd[,denom:=sum(N),by=.(
    bornSex,
    analysisCat_y
  )]
  
  
  xlsx::write.xlsx2(pd, file=
                      file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_collapse_all_ages.xlsx"))
  
  pd[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y,"diag:","diag:\n")]
  pd[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y_pretty,"2016","2015")]
  
  q <- ggplot(pd,aes(x=analysisCat_y_pretty,y=N,fill=hormoneAndSurgicalMasectomyCat_y))
  q <- q + geom_col()
  q <- q + facet_grid(.~bornSex)
  q <- q + scale_fill_brewer("",palette="Set1")
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="Restricted between 2001 and 2015")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_collapse_all_ages_numbers.png"))
  
  q <- ggplot(pd,aes(x=analysisCat_y_pretty,y=N/denom,fill=hormoneAndSurgicalMasectomyCat_y))
  q <- q + geom_col()
  q <- q + facet_grid(.~bornSex)
  q <- q + scale_y_continuous(labels=scales::percent)
  q <- q + scale_fill_brewer("",palette="Set1")
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="Restricted between 2001 and 2015")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_collapse_all_ages_percentages.png"))
  
  
  # same as above, but under 18
  pd <- d[!is.na(analysisCat_y) & analysisAge_y<18,.(
    N=.N
  ),keyby=.(
    analysisYear_y,
    analysisCat_y,
    bornSex,
    hormoneAndSurgicalMasectomyCat_y
  )]
  pd[,denom:=sum(N),by=.(
    analysisYear_y,
    bornSex,
    analysisCat_y
  )]
  
  
  xlsx::write.xlsx2(pd, file=
                      file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_under_18.xlsx"))
  
  pd[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y,"diag:","diag:\n")]
  
  q <- ggplot(pd,aes(x=analysisYear_y,y=N,fill=hormoneAndSurgicalMasectomyCat_y))
  q <- q + geom_col()
  q <- q + facet_grid(bornSex~analysisCat_y_pretty)
  q <- q + scale_fill_brewer("",palette="Set1")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_under_18_numbers.png"))
  
  q <- ggplot(pd,aes(x=analysisYear_y,y=N/denom,fill=hormoneAndSurgicalMasectomyCat_y))
  q <- q + geom_col()
  q <- q + facet_grid(bornSex~analysisCat_y_pretty)
  q <- q + scale_y_continuous(labels=scales::percent)
  q <- q + scale_fill_brewer("",palette="Set1")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_treatment_under_18_percentages.png"))
  
  # average number of F64_089 
  pd <- d[!is.na(analysisCat_y),.(
    numF64_089=mean(numF64_089)
  ),keyby=.(
    analysisYear_y,
    analysisCat_y
  )]
  
  
  xlsx::write.xlsx2(pd, file=
                      file.path(FOLDERS$results_today,"validation_2","Validate_2_numdiag_all_ages.xlsx"))
  
  pd[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y,"diag:","diag:\n")]
  
  q <- ggplot(pd,aes(x=analysisYear_y,y=numF64_089))
  q <- q + geom_col()
  q <- q + facet_grid(~analysisCat_y_pretty)
  q <- q + scale_fill_brewer("",palette="Set1")
  q <- q + scale_y_continuous("Average number numF64_089 diagnoses")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_numdiag_all_ages.png"))
  
  # average number of F64_089  under 18
  pd <- d[!is.na(analysisCat_y) & analysisAge_y<18,.(
    numF64_089=mean(numF64_089)
  ),keyby=.(
    analysisYear_y,
    analysisCat_y
  )]
  
  
  xlsx::write.xlsx2(pd, file=
                      file.path(FOLDERS$results_today,"validation_2","Validate_2_numdiag_under_18.xlsx"))
  
  pd[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y,"diag:","diag:\n")]
  
  q <- ggplot(pd,aes(x=analysisYear_y,y=numF64_089))
  q <- q + geom_col()
  q <- q + facet_grid(~analysisCat_y_pretty)
  q <- q + scale_fill_brewer("",palette="Set1")
  q <- q + scale_y_continuous("Average number numF64_089 diagnoses")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_2","Validate_2_numdiag_under_18.png"))
  
  
  
}