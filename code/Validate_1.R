
Validate_1 <- function(d,byvar){
  d[,xbyvar:=get(byvar)]
  res <- d[excluded=="No",
           .(
             N=.N,
             c_isHormone_2006_01_to_2016_12=sum(c_isHormone_2006_01_to_2016_12),
             c_isSurgicalMasectomy_2006_01_to_2016_12=sum(c_isSurgicalMasectomy_2006_01_to_2016_12),
             c_isSurgicalPenisTestProsth_2006_01_to_2016_12=sum(c_isSurgicalPenisTestProsth_2006_01_to_2016_12),
             c_isSurgicalReconstVag_2006_01_to_2016_12=sum(c_isSurgicalReconstVag_2006_01_to_2016_12),
             c_isSurgicalPenisAmp_2006_01_to_2016_12=sum(c_isSurgicalPenisAmp_2006_01_to_2016_12),
             
             num_people_c_isSurgical_2006_01_to_2016_12=sum(
                 c_isSurgicalMasectomy_2006_01_to_2016_12 |
                 c_isSurgicalPenisTestProsth_2006_01_to_2016_12 |
                 c_isSurgicalReconstVag_2006_01_to_2016_12 |
                 c_isSurgicalPenisAmp_2006_01_to_2016_12
             ),
             
             num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12=sum(
               c_isHormone_2006_01_to_2016_12 |
                 c_isSurgicalMasectomy_2006_01_to_2016_12 |
                 c_isSurgicalPenisTestProsth_2006_01_to_2016_12 |
                 c_isSurgicalReconstVag_2006_01_to_2016_12 |
                 c_isSurgicalPenisAmp_2006_01_to_2016_12
             )
           ),
           keyby=.(
             bornSex,
             category=xbyvar
           )]
  res[,perc_people_c_isSurgicalOrHormonal_2006_01_to_2016_12:=round(100*num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12/N,1)]
  
  res <- melt.data.table(res, id.vars=c("bornSex","category"))
  res <- dcast.data.table(res, bornSex+variable~category, value.var = "value")
  
  openxlsx::write.xlsx(res, file=
                         fs::path(org::PROJ$SHARED_TODAY,"validation",glue::glue("Validate_{byvar}.xlsx")))
  
  d[,xbyvar:=NULL]
  
}