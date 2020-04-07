Validate_3 <- function(d){
  
  pd <- d[!is.na(analysisCat_y) & analysisYear_y<=2015,.(
    N=.N,
    hormone=sum(dateFirstHormone >= "2001-01-01" & dateFirstHormone<="2016-01-01",na.rm=T),
    masectomy=sum(dateFirst_SurgicalMasectomy >= "2001-01-01" & dateFirst_SurgicalMasectomy<="2016-01-01",na.rm=T),
    penisamp=sum(dateFirst_SurgicalPenisAmp >= "2001-01-01" & dateFirst_SurgicalPenisAmp<="2016-01-01",na.rm=T),
    reconstvag=sum(dateFirst_SurgicalReconstVag >= "2001-01-01" & dateFirst_SurgicalReconstVag<="2016-01-01",na.rm=T),
    penistestprost=sum(dateFirst_SurgicalPenisTestProsth >= "2001-01-01" & dateFirst_SurgicalPenisTestProsth<="2016-01-01",na.rm=T),
    any=sum(
      (dateFirstHormone >= "2001-01-01" & dateFirstHormone<="2016-01-01") |
      (dateFirst_SurgicalMasectomy >= "2001-01-01" & dateFirst_SurgicalMasectomy<="2016-01-01") |
      (dateFirst_SurgicalPenisAmp >= "2001-01-01" & dateFirst_SurgicalPenisAmp<="2016-01-01") |
      (dateFirst_SurgicalReconstVag >= "2001-01-01" & dateFirst_SurgicalReconstVag<="2016-01-01") |
      (dateFirst_SurgicalPenisTestProsth >= "2001-01-01" & dateFirst_SurgicalPenisTestProsth<="2016-01-01")
      ,na.rm=T)
  ),keyby=.(
    analysisCat_y,
    bornSex
  )]
  pd[,none:=N-any]
  pd[,analysisCat_y:=stringr::str_replace(analysisCat_y,"2016","2015")]
  pd[,denom:=N]
  

  xlsx::write.xlsx2(pd, file=
                      file.path(FOLDERS$results_today,"validation_3","Validate_3_treatment_all_ages.xlsx"))
  
  long <- melt.data.table(pd,id.vars=c("analysisCat_y","bornSex","denom"))
  long[,analysisCat_y_pretty:=stringr::str_replace(analysisCat_y,"diag:","diag:\n")]
  
  q <- ggplot(long,aes(x=analysisCat_y_pretty,y=value,fill=variable))
  q <- q + geom_col(position="dodge")
  q <- q + facet_grid(bornSex~analysisCat_y_pretty,scales="free_x")
  q <- q + scale_fill_brewer("",palette="Set1")
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="Restricted between 2001 and 2015")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_3","Validate_3_treatment_all_ages_numbers.png"))
  
  q <- ggplot(long,aes(x=analysisCat_y_pretty,y=value/denom,fill=variable))
  q <- q + geom_col(position="dodge")
  q <- q + facet_grid(bornSex~analysisCat_y_pretty,scales="free_x")
  q <- q + scale_fill_brewer("",palette="Set1")
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + labs(caption="Restricted between 2001 and 2015")
  q
  SaveA4(q,file.path(FOLDERS$results_today,"validation_3","Validate_3_treatment_all_ages_percentages.png"))
  
  
}