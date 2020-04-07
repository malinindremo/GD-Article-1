LossOfPeopleTreatments <- function(d, type="treatments"){
  var <- glue::glue("c_analysisCat_{type}")
  
  if(type=="treatments"){
    d[,excluded:=excluded_treatments]
    #d[,var:=c_analysisCat_treatments]
    varSexChange <- "hadSexChange_le2006_01_01"
  } else {
    d[,excluded:=excluded_diag]
    #d[,var:=c_analysisCat_diag]
    varSexChange <- "hadSexChange_le2000_12_31"
  }
  
  if(type=="treatments"){

    res <- d[excluded=="No" &
               numF64_089>=1 & 
               dateFirst_F64_089>="2006-01-01" &
               dateFirst_F64_089<="2015-12-31",
      .(
        wrong_isHormone_2006_01_to_2016_12=sum(isHormone_2006_01_to_2016_12 & !c_isHormone_2006_01_to_2016_12),
        wrong_isSurgicalMasectomy_2006_01_to_2016_12=sum(isSurgicalMasectomy_2006_01_to_2016_12 & !c_isSurgicalMasectomy_2006_01_to_2016_12),
        wrong_isSurgicalPenisTestProsth_2006_01_to_2016_12=sum(isSurgicalPenisTestProsth_2006_01_to_2016_12 & !c_isSurgicalPenisTestProsth_2006_01_to_2016_12),
        wrong_isSurgicalReconstVag_2006_01_to_2016_12=sum(isSurgicalReconstVag_2006_01_to_2016_12 & !c_isSurgicalReconstVag_2006_01_to_2016_12),
        wrong_isSurgicalPenisAmp_2006_01_to_2016_12=sum(isSurgicalPenisAmp_2006_01_to_2016_12 & !c_isSurgicalPenisAmp_2006_01_to_2016_12)
      ),
      keyby=eval(
        var,
        "bornSex",
        "excluded"
      )]
    res
    
    openxlsx::write.xlsx(res, 
                         fs::path(
                           org::PROJ$SHARED_TODAY,
                           "descriptives",
                           glue::glue("data_cleaning_{type}_due_to_age_and_sex.xlsx", type=type)
                           )
                         )
  }
  
  res <- d[excluded!="No" & !is.na(var),
           .(
             N=.N
           ),
           keyby=.(
             bornSex,
             excluded
           )]
  res
  
  openxlsx::write.xlsx(res, 
                       fs::path(
                         org::PROJ$SHARED_TODAY,
                         "descriptives",
                         glue::glue("loss_of_people_{type}_due_to_sexchange_and_icd89.xlsx", type=type)
                       )
  )
  
}