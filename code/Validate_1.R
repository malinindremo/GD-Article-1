
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
  
  
  res <- d[excluded=="No",
           .(
             N=.N,
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
  setorder(res,bornSex,-category)
  res[,cum_N := cumsum(N),by=.(bornSex)]
  res[,cum_num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12 := cumsum(num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12),by=.(bornSex)]
  res[,perc_people_c_isSurgicalOrHormonal_2006_01_to_2016_12:=round(100*num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12/N,1)]
  res[,perc_cum_people_c_isSurgicalOrHormonal_2006_01_to_2016_12:=round(100*cum_num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12/cum_N,1)]
  res[,cum_category:=stringr::str_replace(category,"^([0-9][0-9]) ","\\1+ ")]
  setorder(res,bornSex,category)
  setcolorder(res,c(
    "bornSex",
    "category",
    "N",
    "num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "perc_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "cum_category",
    "cum_N",
    "cum_num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "perc_cum_people_c_isSurgicalOrHormonal_2006_01_to_2016_12"
  ))

  openxlsx::write.xlsx(res, file=
                         fs::path(org::PROJ$SHARED_TODAY,"validation",glue::glue("double_table_Validate_{byvar}.xlsx")))
  
  
  d[,xbyvar:=NULL]
  
}