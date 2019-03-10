YearN <- function(x) as.numeric(format.Date(x, "%G"))

OverwriteWithEarlist <- function(d,rows,resVarDate,resVarCat,valVarDate,valCat){
  d[rows,(resVarCat):=valCat]
  d[rows,(resVarDate):=get(valVarDate)]
}

CleanData <- function(){
  
  ov <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","ov.sas7bdat")))
  sv <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","sv.sas7bdat")))
  rx <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","ut_r_lmed_10218_2017.sas7bdat")))
  demografi <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","demografi.sas7bdat")))
  sex <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","kon.sas7bdat")))
  sex[,isBornMale:=kon==1]
  sex[,bornSex:=ifelse(isBornMale,"Born Male","Born Female")]
  sex[,kon:=NULL]
 
  ## sex change 
  sexChange <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","konsbyten.sas7bdat")))
  sexChange[,dateSexChange:=as.Date(sprintf(
    "%s-%s-%s",
    stringr::str_sub(konsbyte_datum,1,4),
    stringr::str_sub(konsbyte_datum,5,6),
    stringr::str_sub(konsbyte_datum,7,8)
  ))]
  sexChange[,yearSexChange:=YearN(dateSexChange)]
  sexChange[,konsbyte_datum:=NULL]
  
  ## DOB
  demografi[,dob:=as.Date(sprintf(
    "%s-%s-%s",
    stringr::str_sub(fodelseman,1,4),
    stringr::str_sub(fodelseman,5,6),
    "15"))]
  demografi[DodDatum!="",dod:=as.Date(sprintf(
    "%s-%s-%s",
    stringr::str_sub(DodDatum,1,4),
    stringr::str_sub(DodDatum,5,6),
    stringr::str_sub(DodDatum,7,8)
  ))]
  demografi[,fodelseman:=NULL]
  demografi[,DodDatum:=NULL]
  setnames(demografi,"lopnr","LopNr")
  
  ## hormone prescription
  rx[,isHormoneMTF:=FALSE]
  for(i in c("^L02", "^G04CB01", "^C03DA01", "^G03H", "^G03C")){
    rx[stringr::str_detect(atc,i),isHormoneMTF:=TRUE]
  }
  
  rx[,isHormoneFTM:=FALSE]
  for(i in c("^G03B", "^L02")){
    rx[stringr::str_detect(atc,i),isHormoneFTM:=TRUE]
  }
  
  rx[,isHormonePubBlock:=FALSE]
  for(i in c("^L02")){
    rx[stringr::str_detect(atc,i),isHormonePubBlock:=TRUE]
  }
  
  # merge in sex
  nrow(sex)
  rx <- merge(sex,rx,by.x="LopNr",by.y="lopnr",all.x=T)
  nrow(rx)
  # merge in demography
  rx <- merge(rx,demografi[,c("LopNr","dob")],by="LopNr")
  nrow(rx)
  rx[,age:=as.numeric(difftime(FDATUM,dob,units="days"))/365.25]
  rx[,dob:=NULL]
 
  # remove hormones that are going the wrong way with regards to gender
  rx[,c_isHormoneFTM:=isHormoneFTM]
  rx[,c_isHormoneMTF:=isHormoneMTF]
  rx[,c_isHormonePubBlock:=isHormonePubBlock]
  
  rx[isBornMale==TRUE,c_isHormoneFTM:=FALSE]
  rx[isBornMale==FALSE,c_isHormoneMTF:=FALSE]
  # puberty blockers for 19+ year olds are discarded
  rx[isHormonePubBlock==TRUE & age>=19,c_isHormonePubBlock:=FALSE]
  rx[,age:=NULL]
  
  rx[,isHormone:=isHormoneMTF | isHormoneFTM | isHormonePubBlock]
  rx[,c_isHormone:=c_isHormoneMTF | c_isHormoneFTM | c_isHormonePubBlock]
  
  rx[,isHormoneMTF_2005_07_to_2016_12:=isHormoneMTF]
  rx[FDATUM < "2005-07-01",isHormoneMTF_2005_07_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormoneMTF_2005_07_to_2016_12:=FALSE]
  
  rx[,isHormoneFTM_2005_07_to_2016_12:=isHormoneFTM]
  rx[FDATUM < "2005-07-01",isHormoneFTM_2005_07_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormoneFTM_2005_07_to_2016_12:=FALSE]
  
  rx[,isHormonePubBlock_2005_07_to_2016_12:=isHormonePubBlock]
  rx[FDATUM < "2005-07-01",isHormonePubBlock_2005_07_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormonePubBlock_2005_07_to_2016_12:=FALSE]
  
  rx[,isHormone_2005_07_to_2016_12:=isHormone]
  rx[FDATUM < "2005-07-01",isHormone_2005_07_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormone_2005_07_to_2016_12:=FALSE]
  
  rx[,c_isHormone_2005_07_to_2016_12:=c_isHormone]
  rx[FDATUM < "2005-07-01",c_isHormone_2005_07_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",c_isHormone_2005_07_to_2016_12:=FALSE]
  
  # collapse down to 1 row/person
  rx <- rx[,.(
    isHormone=as.logical(max(isHormone)),
    dateFirstHormone=min(FDATUM[isHormone==TRUE]),
    isHormone_2005_07_to_2016_12=as.logical(max(isHormone_2005_07_to_2016_12)),
    
    isHormoneMTF_2005_07_to_2016_12=as.logical(max(isHormoneMTF_2005_07_to_2016_12)),
    isHormoneFTM_2005_07_to_2016_12=as.logical(max(isHormoneFTM_2005_07_to_2016_12)),
    isHormonePubBlock_2005_07_to_2016_12=as.logical(max(isHormonePubBlock_2005_07_to_2016_12)),
    
    c_isHormone=as.logical(max(c_isHormone)),
    c_dateFirstHormone=min(FDATUM[c_isHormone==TRUE]),
    c_isHormone_2005_07_to_2016_12=as.logical(max(c_isHormone_2005_07_to_2016_12))
  ),by=.(LopNr)]
  nrow(rx)
  for(i in names(rx)){
    rx[is.infinite(get(i)),(i):=NA]
  }
  
  # diagnoses and surgeries
  ov[,type:="outpatient"]
  sv[,type:="inpatient"]
  patients <- rbind(ov,sv,fill=T)
  
  setorder(patients,LopNr,INDATUM)
  
  # merge with sex
  nrow(patients)
  patients <- merge(patients,sex,by="LopNr", all.x=T)
  nrow(patients)
  
  # diagnoses
  patients[,isF64_089:=FALSE]
  patients[,isF64_0:=FALSE]
  patients[,isF64_89:=FALSE]
  patients[,isTranssexual_ICD_89:=FALSE]
  patients[,isCodeUsedWithSurgery:=FALSE]
  for(i in c("HDIA",stringr::str_subset(names(patients), "^DIA"))){
    patients[stringr::str_detect(get(i),"^F640"), isF64_0:=TRUE]
    patients[stringr::str_detect(get(i),"^F648"), isF64_89:=TRUE]
    patients[stringr::str_detect(get(i),"^F649"), isF64_89:=TRUE]
    
    patients[get(i)=="302,31",isTranssexual_ICD_89:=TRUE]
    patients[get(i)=="302,99",isTranssexual_ICD_89:=TRUE]
    patients[get(i)=="302X",isTranssexual_ICD_89:=TRUE]
    
    patients[stringr::str_detect(get(i),"^F640"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^F648"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^F649"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^Q838"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^N629"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^Q319"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^Q555"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^Q550"), isCodeUsedWithSurgery:=TRUE]
    patients[stringr::str_detect(get(i),"^Q555"), isCodeUsedWithSurgery:=TRUE]
  }
  patients[,isF64_089:=isF64_0 | isF64_89]
  
  patients[,isF64_089_2005_07_to_2016_12:=isF64_089]
  patients[INDATUM < "2005-07-01",isF64_089_2005_07_to_2016_12:=FALSE]
  patients[INDATUM > "2016-12-31",isF64_089_2005_07_to_2016_12:=FALSE]
  
  # surgeries
  surgeries <- list()
  surgeries[["Masectomy"]] <- c(
    "HAC10",
    "HAC20",
    "HAC99")
  surgeries[["BreastReconst"]] <- c(
    "HAE00",
    "HAE99")
  surgeries[["BreastReconstAndOtherBreastOps"]] <- c(
    "HAD20",
    "HAD30",
    "HAD35",
    "HAD99")
  surgeries[["ReconstVag"]] <- c(
    "LEE40",
    "LFE96",
    "LEE96")
  surgeries[["PenisAmp"]] <- c(
    "KGC10")
  surgeries[["PenisTestProsth"]] <- c(
    "KFH50",
    "KGV30",
    "KGW96",
    "KGH96")
  surgeries[["InternalGenital"]] <- c(
    "LCD00",
    "LCD01",
    "LCD04",
    "LCD10",
    "LCD11",
    "LCD96",
    "LCD97")
  surgeries[["SterilizationAF"]] <- c(
    "^LGA",
    "LAF10")
  surgeries[["SterilizationAM"]] <- c(
    "KFC10",
    "KFD46")
  surgeries[["Larynx"]] <- c(
    "DQD40")
  
  for(i in seq_along(surgeries)){
    newVar <- sprintf("isSurgical%s",names(surgeries)[i])
    newVar_2005_07_to_2016_12 <- sprintf("%s_2005_07_to_2016_12",newVar)
    
    patients[,(newVar):=FALSE]
    for(code in surgeries[[i]]){
      patients[stringr::str_detect(OP,code),(newVar):=TRUE]
    }
    
    patients[,(newVar_2005_07_to_2016_12):=get(newVar)]
    patients[INDATUM < "2005-07-01",(newVar_2005_07_to_2016_12):=FALSE]
    patients[INDATUM > "2016-12-31",(newVar_2005_07_to_2016_12):=FALSE]
    
  }

  # merge diagnoses/surgeries with DOB
  nrow(patients)
  patients <- merge(patients,demografi[,c("LopNr","dob")],by="LopNr")
  nrow(patients)
  patients[,age:=as.numeric(difftime(INDATUM,dob,units="days"))/365.25,by=LopNr]
  patients[,dob:=NULL]
  patients[,dateFirst:=min(INDATUM),by=LopNr]
  patients[isF64_089==TRUE,days:=as.numeric(difftime(INDATUM,dateFirst,units="days")),by=LopNr]
  
  patients <- patients[,.(
    ageFirst_F64_089=min(age[isF64_089==TRUE]),
    ageFirst_F64_0=min(age[isF64_0==T]),
    ageFirst_F64_89=min(age[isF64_89==T]),
    
    dateFirst_F64_089=min(INDATUM[isF64_089==TRUE]),
    dateFirst_F64_0=min(INDATUM[isF64_0==T]),
    dateFirst_F64_89=min(INDATUM[isF64_89==T]),
    
    dateLast_F64_0=max(INDATUM[isF64_0==T]),
    dateLast_F64_89=max(INDATUM[isF64_89==T]),
    
    hadTranssexual_ICD_89=as.logical(max(isTranssexual_ICD_89)),
    
    numF64_089_2005_07_to_2016_12=sum(isF64_089_2005_07_to_2016_12),
    
    numF64_089=sum(isF64_089),
    numF64_0=sum(isF64_0),
    numF64_89=sum(isF64_89),
    
    isSurgicalMasectomy_2005_07_to_2016_12=as.logical(max(isSurgicalMasectomy_2005_07_to_2016_12)),
    dateFirst_SurgicalMasectomy=min(INDATUM[isSurgicalMasectomy==TRUE]),
    
    isSurgicalReconstVag_2005_07_to_2016_12=as.logical(max(isSurgicalReconstVag_2005_07_to_2016_12)),
    dateFirst_SurgicalReconstVag=min(INDATUM[isSurgicalReconstVag==TRUE]),
    
    isSurgicalPenisAmp_2005_07_to_2016_12=as.logical(max(isSurgicalPenisAmp_2005_07_to_2016_12)),
    dateFirst_SurgicalPenisAmp=min(INDATUM[isSurgicalPenisAmp==TRUE]),
    
    isSurgicalPenisTestProsth_2005_07_to_2016_12=as.logical(max(isSurgicalPenisTestProsth_2005_07_to_2016_12)),
    dateFirst_SurgicalPenisTestProsth=min(INDATUM[isSurgicalPenisTestProsth==TRUE])
  ), by=.(
    LopNr
  )]
  for(i in names(patients)){
    patients[is.infinite(get(i)),(i):=NA]
  }
  #patients[,ageFirstCat:=cut(ageFirst,breaks=c(0,12,15,20,30,100))]
  
  # merge sex with hormones with diagnoses/surgeries
  nrow(sex)
  d <- merge(sex,rx,by="LopNr",all.x=T)
  nrow(d)
  # merge sex/hormones with dob
  d <- merge(sex,demografi,by="LopNr",all.x=T)
  nrow(d)
  # merge sex/hormones/dob with sex change
  d <- merge(d,sexChange,by="LopNr",all.x=T)
  nrow(d)
  # merge sex/hormones/dob/sexchange with rx
  d <- merge(d,rx,by="LopNr",all.x=T)
  nrow(d)
  # merge sex/hormones/dob/sexchange/rx with diagnoses/surgeries
  d <- merge(d,patients,by="LopNr",all.x=T)
  nrow(d)
  
  # fix the NA numbers
  for(i in stringr::str_subset(names(d),"^num")){
    d[is.na(get(i)),(i):=0]
  }
  # fix the NA logicals
  for(i in stringr::str_subset(names(d),"^had")){
    d[is.na(get(i)),(i):=FALSE]
  }
  for(i in stringr::str_subset(names(d),"^is")){
    d[is.na(get(i)),(i):=FALSE]
  }
  for(i in stringr::str_subset(names(d),"^c_is")){
    d[is.na(get(i)),(i):=FALSE]
  }
  
  d[,hadSexChange_le2000_12_31:=FALSE]
  d[dateSexChange<"2001-01-01",hadSexChange_le2000_12_31:=TRUE]
  d[,hadSexChange_le2005_06_30:=FALSE]
  d[dateSexChange<"2005-07-01",hadSexChange_le2005_06_30:=TRUE]
  
  d[,c_isSurgicalMasectomy_2005_07_to_2016_12:=isSurgicalMasectomy_2005_07_to_2016_12]
  d[isBornMale==TRUE,c_isSurgicalMasectomy_2005_07_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalMasectomy:=dateFirst_SurgicalMasectomy]
  d[isBornMale==TRUE,c_dateFirst_SurgicalMasectomy:=NA]
  
  d[,c_isSurgicalPenisTestProsth_2005_07_to_2016_12:=isSurgicalPenisTestProsth_2005_07_to_2016_12]
  d[isBornMale==TRUE,c_isSurgicalPenisTestProsth_2005_07_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalPenisTestProsth:=dateFirst_SurgicalPenisTestProsth]
  d[isBornMale==TRUE,c_dateFirst_SurgicalPenisTestProsth:=NA]
  
  d[,c_isSurgicalReconstVag_2005_07_to_2016_12:=isSurgicalReconstVag_2005_07_to_2016_12]
  d[isBornMale==FALSE,c_isSurgicalReconstVag_2005_07_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalReconstVag:=dateFirst_SurgicalReconstVag]
  d[isBornMale==FALSE,c_dateFirst_SurgicalReconstVag:=NA]
  
  d[,c_isSurgicalPenisAmp_2005_07_to_2016_12:=isSurgicalPenisAmp_2005_07_to_2016_12]
  d[isBornMale==FALSE,c_isSurgicalPenisAmp_2005_07_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalPenisAmp:=dateFirst_SurgicalPenisAmp]
  d[isBornMale==FALSE,c_dateFirst_SurgicalPenisAmp:=NA]
  
  # create the categorioes
  
  ####
  # analysis cats
  # F64_089 + treatments
  d[,c_analysisCat_treatments_withicd89_sexchange:=as.character(NA)]
  d[,c_analysisDate_treatments:=dateFirst_F64_089]
  
  # hormones
  d[dateFirst_F64_089<c_analysisDate_treatments & 
      numF64_089>=1 & 
      dateFirst_F64_089>="2005-07-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirstHormone>="2005-07-01" & 
      c_dateFirstHormone<="2016-12-31",
    
    c_analysisCat_treatments_withicd89_sexchange:="numF64_089>=1 & hormones/surgery, first diag: [2005-07-01, 2016-12-31]"
  ]
  
  # masectomy
  d[dateFirst_F64_089<c_analysisDate_treatments & 
      numF64_089>=1 & 
      dateFirst_F64_089>="2005-07-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalMasectomy>="2005-07-01" & 
      c_dateFirst_SurgicalMasectomy<="2016-12-31",
    
    c_analysisCat_treatments_withicd89_sexchange:="numF64_089>=1 & hormones/surgery, first diag: [2005-07-01, 2016-12-31]"
    ]
  
  # penisamp
  d[dateFirst_F64_089<c_analysisDate_treatments & 
      numF64_089>=1 & 
      dateFirst_F64_089>="2005-07-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalPenisAmp>="2005-07-01" & 
      c_dateFirst_SurgicalPenisAmp<="2016-12-31",
    
    c_analysisCat_treatments_withicd89_sexchange:="numF64_089>=1 & hormones/surgery, first diag: [2005-07-01, 2016-12-31]"
    ]
  
  # reconstvag
  d[dateFirst_F64_089<c_analysisDate_treatments & 
      numF64_089>=1 & 
      dateFirst_F64_089>="2005-07-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalReconstVag>="2005-07-01" & 
      c_dateFirst_SurgicalReconstVag<="2016-12-31",
    
    c_analysisCat_treatments_withicd89_sexchange:="numF64_089>=1 & hormones/surgery, first diag: [2005-07-01, 2016-12-31]"
    ]
  
  # penistestprosth
  d[dateFirst_F64_089<c_analysisDate_treatments & 
      numF64_089>=1 & 
      dateFirst_F64_089>="2005-07-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalPenisTestProsth>="2005-07-01" & 
      c_dateFirst_SurgicalPenisTestProsth<="2016-12-31",
    
    c_analysisCat_treatments_withicd89_sexchange:="numF64_089>=1 & hormones/surgery, first diag: [2005-07-01, 2016-12-31]"
    ]
  
  d[,c_analysisCat_treatments:=c_analysisCat_treatments_withicd89_sexchange]
  d[hadTranssexual_ICD_89==TRUE,c_analysisCat_treatments:=NA]
  d[hadSexChange_le2005_06_30==TRUE,c_analysisCat_treatments:=NA]
  
  d[is.na(c_analysisCat_treatments), c_analysisDate_treatments:=NA]
  
  d[,c_analysisYear_treatments:=YearN(c_analysisDate_treatments)]
  d[,c_analysisAge_treatments:=as.numeric(difftime(c_analysisDate_treatments,dob,units="days"))/365.25]
  d[,c_analysisAgeCat_treatments:=cut(c_analysisAge_treatments,breaks = c(0,18,30,50,200),include.lowest = T)]
  xtabs(~d$c_analysisCat_treatments+d$c_analysisYear_treatments)
  xtabs(~d$c_analysisAgeCat_treatments)
  
  ####
  # analysis cats
  # F64_089
  d[,c_analysisCat_diag_withicd89_sexchange:=as.character(NA)]
  d[,c_analysisDate_diag:=dateFirst_F64_089]
  d[
    numF64_089>=4 & 
    dateFirst_F64_089>="2001-01-01" &
    dateFirst_F64_089<="2016-12-31",
    c_analysisCat_diag_withicd89_sexchange:="numF64_089>=4, first diag: [2001-01-01, 2016-12-31]"
   ]
  
  d[,c_analysisCat_diag:=c_analysisCat_diag_withicd89_sexchange]
  d[hadTranssexual_ICD_89==TRUE,c_analysisCat_diag:=NA]
  d[hadSexChange_le2000_12_31==TRUE,c_analysisCat_diag:=NA]
  
  d[is.na(c_analysisCat_diag), c_analysisDate_diag:=NA]
  
  d[,c_analysisYear_diag:=YearN(c_analysisDate_diag)]
  d[,c_analysisAge_diag:=as.numeric(difftime(c_analysisDate_diag,dob,units="days"))/365.25]
  d[,c_analysisAgeCat_diag:=cut(c_analysisAge_diag,breaks = c(0,18,30,50,200),include.lowest = T)]
  xtabs(~d$c_analysisCat_diag+d$c_analysisYear_diag)
  xtabs(~d$c_analysisAgeCat_diag)
  
  # dateFirst_F64_089
  xtabs(~d$numF64_089_2005_07_to_2016_12,addNA=T)
  d[,c_analysisCat_F64_089_ge10:="00 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==1,c_analysisCat_F64_089_ge10:="01 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==2,c_analysisCat_F64_089_ge10:="02 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==3,c_analysisCat_F64_089_ge10:="03 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==4,c_analysisCat_F64_089_ge10:="04 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==5,c_analysisCat_F64_089_ge10:="05 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==6,c_analysisCat_F64_089_ge10:="06 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==7,c_analysisCat_F64_089_ge10:="07 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==8,c_analysisCat_F64_089_ge10:="08 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==9,c_analysisCat_F64_089_ge10:="09 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12>=10,c_analysisCat_F64_089_ge10:="10+ F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  
  d[,c_analysisCat_F64_089_ge4:="0 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==1,c_analysisCat_F64_089_ge4:="1 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==2,c_analysisCat_F64_089_ge4:="2 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12==3,c_analysisCat_F64_089_ge4:="3 F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  d[numF64_089_2005_07_to_2016_12>=4,c_analysisCat_F64_089_ge4:="4+ F64.0/8/9 diagnosis [2005-07-01, 2016-12-31]"]
  
  xtabs(~d$c_analysisCat_F64_089_ge10,addNA=T)
  xtabs(~d$c_analysisCat_F64_089_ge4,addNA=T)
  
  return(d)
}




