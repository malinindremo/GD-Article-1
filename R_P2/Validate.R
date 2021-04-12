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
                         fs::path(org::project$results_today,"validation",glue::glue("Validate_{byvar}.xlsx")))
  
  
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
  res[,cum_category:=stringr::str_replace_all(category,"^([0-9]+) ","\\1+ ")]
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
                         fs::path(org::project$results_today,"validation",glue::glue("double_table_Validate_{byvar}.xlsx")))
  
  
  d[,xbyvar:=NULL]
  
  res <- res[!stringr::str_detect(category,"^\\.xx")]
  res <- res[!stringr::str_detect(category,"^00")]
  res <- res[,c(
    "bornSex",
    "category",
    "perc_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "N",
    "cum_category",
    "perc_cum_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "cum_num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "cum_N"
  )]
  res1 <- res[,c(
    "bornSex",
    "category",
    "perc_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "N"
  )]
  res2 <- res[,c(
    "bornSex",
    "cum_category",
    "perc_cum_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "cum_num_people_c_isSurgicalOrHormonal_2006_01_to_2016_12",
    "cum_N"
    )]
  res1[,type:="Percentage"]
  res2[,type:="Cumulative percentage"]
  res <- rbind(
    res1,
    res2,
    use.names=F
  )
  setnames(res,c(
    "bornSex",
    "category",
    "perc",
    "n",
    "N",
    "type"
  ))
  res[,lower:=Hmisc::binconf(n,N)[,"Lower"]*100]
  res[,upper:=Hmisc::binconf(n,N)[,"Upper"]*100]
  res[,type:=factor(type,levels = c(
    "Percentage",
    "Cumulative percentage"
  ))]
  
  res[,lab:=stringr::str_sub(category,1,12)]
  
  q <- ggplot(res, aes(x=lab,y=perc,ymin=lower,ymax=upper))
  q <- q + geom_pointrange(size=0.5)
  q <- q + facet_wrap(type~bornSex,scales="free_x")
  q <- q + scale_y_continuous("Percentage receiving GCMI",lim=c(0,100))
  q <- q + scale_x_discrete("Number of GD diagnoses")
  q <- q + theme_gray(16)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  SaveA4(q, fs::path(org::project$results_today,"validation",glue::glue("double_table_Validate_{byvar}.png")))
  
}

validate_hormones_and_no_diagnoses <- function(d){
  # people 
  diagnoses <- get_diagnoses()
  diagnoses <- diagnoses[HDIA!=""]
  hormones_no_diag <- d[numF64_089==0 & c_isHormone==T & excluded=="No",c(
    "LopNr",
    "bornSex",
    "c_dateFirstHormone",
    "c_ageFirstHormone",
    "isHormoneTestosterone_2006_01_to_2016_12",
    "isHormoneEstrogen_2006_01_to_2016_12"
  )]
  hormones_no_diag[,cat:=dplyr::case_when(
    bornSex=="Assigned female" & isHormoneTestosterone_2006_01_to_2016_12==0 ~ "Assigned female, no testosterone 2006-2016",
    bornSex=="Assigned female" & isHormoneTestosterone_2006_01_to_2016_12==1 ~ "Assigned female, yes testosterone 2006-2016",
    bornSex=="Assigned male" & isHormoneEstrogen_2006_01_to_2016_12==0 ~ "Assigned male, no estrogen 2006-2016",
    bornSex=="Assigned male" & isHormoneEstrogen_2006_01_to_2016_12==1 ~ "Assigned male, yes estrogen 2006-2016"
  )]
  hormones_no_diag[,bornSex:=NULL]
  hormones_no_diag[,isHormoneEstrogen_2006_01_to_2016_12:=NULL]
  hormones_no_diag[,isHormoneTestosterone_2006_01_to_2016_12:=NULL]
  diagnoses[hormones_no_diag,on="LopNr", c_dateFirstHormone:=c_dateFirstHormone]
  diagnoses <- diagnoses[!is.na(c_dateFirstHormone)]
  diagnoses[,years_dif := abs(as.numeric(difftime(c_dateFirstHormone,INDATUM,units="days"))/365.25)]
  diagnoses <- diagnoses[years_dif<=1]
  diagnoses <- unique(diagnoses[,c("LopNr","HDIA")])
  diagnoses[,value.var:=TRUE]
  diagnoses <- dcast.data.table(diagnoses, LopNr~HDIA,value.var = "value.var",fill = FALSE)
  
  hormones_no_diag <- merge(hormones_no_diag,diagnoses,by="LopNr",all.x=T)
  for(i in names(hormones_no_diag)) hormones_no_diag[is.na(get(i)),(i):=F]
  
  hormones_no_diag1 <- hormones_no_diag[ , lapply(.SD, mean),by=.(cat)]
  hormones_no_diag2 <- hormones_no_diag[ , .(denominator=.N),by=.(cat)]
  hormones_no_diag <- merge(
    hormones_no_diag1,
    hormones_no_diag2,
    by="cat"
  )
  hormones_no_diag[,LopNr:=NULL]
  hormones_no_diag[,c_dateFirstHormone:=NULL]
  hormones_no_diag <- melt.data.table(hormones_no_diag,id.vars=c("cat","denominator"))
  setnames(hormones_no_diag,"value","proportion")
  setorder(hormones_no_diag,cat,-proportion)
  hormones_no_diag[,long_desc:=icd::explain_table(x=variable)$long_desc]
  hormones_no_diag
  
  openxlsx::write.xlsx(
    x = hormones_no_diag, 
    file = fs::path(
      org::project$results_today,
      "validation",
      "people_who_have_hormones_but_no_diagnosis.xlsx"
    )
  )
}