LossOfPeopleTreatments <- function(d, type="treatments"){
  var_withicd89_sexchange <- glue::glue("c_analysisCat_{type}_withicd89_sexchange")
  var <- glue::glue("c_analysisCat_{type}")
  
  if(type=="treatments"){
    varSexChange <- "hadSexChange_le2006_01_01"
  } else {
    varSexChange <- "hadSexChange_le2000_12_31"
  }
  
  if(type=="treatments"){
    res <- d[is.na(get(var_withicd89_sexchange)) & 
      numF64_089>=1 & 
      dateFirst_F64_089>="2006-01-01" &
      dateFirst_F64_089<="2016-12-31",
      .(
        isHormone_2006_01_to_2016_12=sum(isHormone_2006_01_to_2016_12),
        isHormoneMTF_2006_01_to_2016_12=sum(isHormoneMTF_2006_01_to_2016_12),
        isHormoneFTM_2006_01_to_2016_12=sum(isHormoneFTM_2006_01_to_2016_12),
        isHormonePubBlock_2006_01_to_2016_12=sum(isHormonePubBlock_2006_01_to_2016_12),
        isSurgicalMasectomy_2006_01_to_2016_12=sum(isSurgicalMasectomy_2006_01_to_2016_12),
        isSurgicalPenisTestProsth_2006_01_to_2016_12=sum(isSurgicalPenisTestProsth_2006_01_to_2016_12),
        isSurgicalReconstVag_2006_01_to_2016_12=sum(isSurgicalReconstVag_2006_01_to_2016_12),
        isSurgicalPenisAmp_2006_01_to_2016_12=sum(isSurgicalPenisAmp_2006_01_to_2016_12),
        num_people_isSurgicalOrHormonal_2006_01_to_2016_12=sum(
          isHormone_2006_01_to_2016_12 |
          isSurgicalMasectomy_2006_01_to_2016_12 |
          isSurgicalPenisTestProsth_2006_01_to_2016_12 |
          isSurgicalReconstVag_2006_01_to_2016_12 |
          isSurgicalPenisAmp_2006_01_to_2016_12
        )
      ),
      keyby=.(
        bornSex
      )]
    
    openxlsx::write.xlsx(res, 
                         fs::path(
                           org::PROJ$SHARED_TODAY,
                           "descriptives",
                           glue::glue("loss_of_people_{type}_due_to_age_and_sex.xlsx", type=type)
                           )
                         )
  }
  
  res <- d[
    !is.na(get(var_withicd89_sexchange)) & 
    is.na(get(var)),
    .(
      hadLegalSexChange=sum(get(varSexChange)),
      hadTranssexual_ICD_89=sum(hadTranssexual_ICD_89),
      num_people_sexchange_icd89=sum(
        get(varSexChange) |
        hadTranssexual_ICD_89
      )
    ),
    keyby=.(
      bornSex
    )
  ]
  
  openxlsx::write.xlsx(res, 
                       fs::path(
                         org::PROJ$SHARED_TODAY,
                         "descriptives",
                         glue::glue("loss_of_people_{type}_due_to_sexchange_and_icd89.xlsx", type=type)
                       )
  )
  
}