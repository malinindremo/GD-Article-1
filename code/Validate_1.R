Validate_1_int <- function(d,num){
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

Validate_1 <- function(d,byvar){
  res <- d[,
           .(
             N=.N,
             c_isHormone_2005_07_to_2016_12=sum(c_isHormone_2005_07_to_2016_12),
             c_isSurgicalMasectomy_2005_07_to_2016_12=sum(c_isSurgicalMasectomy_2005_07_to_2016_12),
             c_isSurgicalPenisTestProsth_2005_07_to_2016_12=sum(c_isSurgicalPenisTestProsth_2005_07_to_2016_12),
             c_isSurgicalReconstVag_2005_07_to_2016_12=sum(c_isSurgicalReconstVag_2005_07_to_2016_12),
             c_isSurgicalPenisAmp_2005_07_to_2016_12=sum(c_isSurgicalPenisAmp_2005_07_to_2016_12),
             
             num_people_c_isSurgical_2005_07_to_2016_12=sum(
                 c_isSurgicalMasectomy_2005_07_to_2016_12 |
                 c_isSurgicalPenisTestProsth_2005_07_to_2016_12 |
                 c_isSurgicalReconstVag_2005_07_to_2016_12 |
                 c_isSurgicalPenisAmp_2005_07_to_2016_12
             ),
             
             num_people_c_isSurgicalOrHormonal_2005_07_to_2016_12=sum(
               c_isHormone_2005_07_to_2016_12 |
                 c_isSurgicalMasectomy_2005_07_to_2016_12 |
                 c_isSurgicalPenisTestProsth_2005_07_to_2016_12 |
                 c_isSurgicalReconstVag_2005_07_to_2016_12 |
                 c_isSurgicalPenisAmp_2005_07_to_2016_12
             )
           ),
           keyby=.(
             bornSex,
             category=get(byvar)
           )]
  res[,perc_people_c_isSurgicalOrHormonal_2005_07_to_2016_12:=round(100*num_people_c_isSurgicalOrHormonal_2005_07_to_2016_12/N,1)]
  
  res <- melt.data.table(res, id.vars=c("bornSex","category"))
  res <- dcast.data.table(res, bornSex+variable~category, value.var = "value")
  
  openxlsx::write.xlsx(res, file=
                         fs::path(org::PROJ$SHARED_TODAY,"validation",glue::glue("Validate_{byvar}.xlsx")))
  
}