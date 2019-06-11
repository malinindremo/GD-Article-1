YearN <- function(x) as.numeric(format.Date(x, "%G"))

OverwriteWithEarlist <- function(d,rows,resVarDate,resVarCat,valVarDate,valCat){
  d[rows,(resVarCat):=valCat]
  d[rows,(resVarDate):=get(valVarDate)]
}

CleanDataIncidentGD <- function(){
  
  ov <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","ov.sas7bdat")))
  sv <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","sv.sas7bdat")))
  rx <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","ut_r_lmed_10218_2017.sas7bdat")))
  demografi <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","demografi.sas7bdat")))
  sex <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","kon.sas7bdat")))
  sex[,isBornMale:=kon==1]
  sex[,bornSex:=ifelse(isBornMale,"Assigned male","Assigned female")]
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
  
  rx[,isHormoneMTF_2006_01_to_2016_12:=isHormoneMTF]
  rx[FDATUM < "2006-01-01",isHormoneMTF_2006_01_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormoneMTF_2006_01_to_2016_12:=FALSE]
  
  rx[,isHormoneFTM_2006_01_to_2016_12:=isHormoneFTM]
  rx[FDATUM < "2006-01-01",isHormoneFTM_2006_01_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormoneFTM_2006_01_to_2016_12:=FALSE]
  
  rx[,isHormonePubBlock_2006_01_to_2016_12:=isHormonePubBlock]
  rx[FDATUM < "2006-01-01",isHormonePubBlock_2006_01_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormonePubBlock_2006_01_to_2016_12:=FALSE]
  
  rx[,isHormone_2006_01_to_2016_12:=isHormone]
  rx[FDATUM < "2006-01-01",isHormone_2006_01_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormone_2006_01_to_2016_12:=FALSE]
  
  rx[,c_isHormone_2006_01_to_2016_12:=c_isHormone]
  rx[FDATUM < "2006-01-01",c_isHormone_2006_01_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",c_isHormone_2006_01_to_2016_12:=FALSE]
  
  # collapse down to 1 row/person
  rx <- rx[,.(
    isHormone=as.logical(max(isHormone)),
    dateFirstHormone=min(FDATUM[isHormone==TRUE]),
    isHormone_2006_01_to_2016_12=as.logical(max(isHormone_2006_01_to_2016_12)),
    
    isHormoneMTF_2006_01_to_2016_12=as.logical(max(isHormoneMTF_2006_01_to_2016_12)),
    isHormoneFTM_2006_01_to_2016_12=as.logical(max(isHormoneFTM_2006_01_to_2016_12)),
    isHormonePubBlock_2006_01_to_2016_12=as.logical(max(isHormonePubBlock_2006_01_to_2016_12)),
    
    c_isHormone=as.logical(max(c_isHormone)),
    c_dateFirstHormone=min(FDATUM[c_isHormone==TRUE]),
    c_isHormone_2006_01_to_2016_12=as.logical(max(c_isHormone_2006_01_to_2016_12))
  ),by=.(LopNr)]
  nrow(rx)
  for(i in names(rx)){
    rx[is.infinite(get(i)),(i):=NA]
  }
  
  # diagnoses and surgeries
  ov[,type:="outpatient"]
  sv[,type:="inpatient"]
  patients <- rbind(ov,sv,fill=T)
  
  # removing people who are missing INDATUM
  #nrow(patients)
  #patients <- patients[!is.na(INDATUM)]
  #nrow(patients)
  
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
  
  patients[, isF00_to_F99:=FALSE]
  patients[, isF70_to_F79:=FALSE]
  patients[, isF80_R47:=FALSE]
  patients[, isF20_to_F29:=FALSE]
  patients[, isF30_to_F31:=FALSE]
  patients[, isF32_to_F33:=FALSE]
  patients[, isF50:=FALSE]
  patients[, isF84:=FALSE]
  patients[, isF90:=FALSE]
  patients[, isF91_to_F98:=FALSE]
  patients[, isF40_to_F48:=FALSE]
  patients[, isF10_to_F16_F18_F19:=FALSE]
  patients[, isF60:=FALSE]
  patients[, isX60_to_X84:=FALSE]
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
   
    # any psychiatric disorder 
    #ICD10
    patients[stringr::str_detect(get(i),"^F"), isF00_to_F99:=TRUE]
    #ICD9: 290-319 
    patients[stringr::str_detect(get(i),"^29[0-9]"), isF00_to_F99:=TRUE]
    patients[stringr::str_detect(get(i),"^3[0-1][0-9]"), isF00_to_F99:=TRUE]
    #ICD8: 290-315
    
    patients[stringr::str_detect(get(i),"^F7"), isF70_to_F79:=TRUE]
    
    patients[stringr::str_detect(get(i),"^F80"), isF80_R47:=TRUE]
    patients[stringr::str_detect(get(i),"^R47"), isF80_R47:=TRUE]
    
    patients[stringr::str_detect(get(i),"^F2"), isF20_to_F29:=TRUE]
    patients[stringr::str_detect(get(i),"^F3[01]"), isF30_to_F31:=TRUE]
    patients[stringr::str_detect(get(i),"^F3[23]"), isF32_to_F33:=TRUE]
    patients[stringr::str_detect(get(i),"^F50"), isF50:=TRUE]
    patients[stringr::str_detect(get(i),"^F84"), isF84:=TRUE]
    patients[stringr::str_detect(get(i),"^F90"), isF90:=TRUE]
    patients[stringr::str_detect(get(i),"^F9[1-8]"), isF91_to_F98:=TRUE]
    patients[stringr::str_detect(get(i),"^F4[0-8]"), isF40_to_F48:=TRUE]
    patients[stringr::str_detect(get(i),"^F1[0-689]"), isF10_to_F16_F18_F19:=TRUE]
    patients[stringr::str_detect(get(i),"^F60"), isF60:=TRUE]
    
    patients[stringr::str_detect(get(i),"^X[67]"), isX60_to_X84:=TRUE]
    patients[stringr::str_detect(get(i),"^X8[0-4]"), isX60_to_X84:=TRUE]
    
  }
  patients[,isF64_089:=isF64_0 | isF64_89]
  
  patients[,isF64_089_2006_01_to_2014_12:=isF64_089]
  patients[INDATUM < "2006-01-01",isF64_089_2006_01_to_2014_12:=FALSE]
  patients[INDATUM > "2014-12-31",isF64_089_2006_01_to_2014_12:=FALSE]
  
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
    newVar_2006_01_to_2016_12 <- sprintf("%s_2006_01_to_2016_12",newVar)
    
    patients[,(newVar):=FALSE]
    for(code in surgeries[[i]]){
      patients[stringr::str_detect(OP,code),(newVar):=TRUE]
    }
    
    patients[,(newVar_2006_01_to_2016_12):=get(newVar)]
    patients[INDATUM < "2006-01-01",(newVar_2006_01_to_2016_12):=FALSE]
    patients[INDATUM > "2016-12-31",(newVar_2006_01_to_2016_12):=FALSE]
    
  }

  # merge diagnoses/surgeries with DOB
  nrow(patients)
  patients <- merge(patients,demografi[,c("LopNr","dob")],by="LopNr")
  nrow(patients)
  patients[,age:=as.numeric(difftime(INDATUM,dob,units="days"))/365.25,by=LopNr]
  patients[,dob:=NULL]
  patients[,dateFirst:=min(INDATUM),by=LopNr]
  patients[isF64_089==TRUE,days:=as.numeric(difftime(INDATUM,dateFirst,units="days")),by=LopNr]
  
  setorder(patients, LopNr, INDATUM)
  patients[isF64_089==TRUE,num_F64_089:=1:.N,by=LopNr]
  
  patients <- patients[,.(
    ageFirst_F64_089=min(age[isF64_089==TRUE]),
    ageFirst_F64_0=min(age[isF64_0==T]),
    ageFirst_F64_89=min(age[isF64_89==T]),
    
    dateFirst_F64_089=min(INDATUM[isF64_089==TRUE]),
    dateFirst_F64_0=min(INDATUM[isF64_0==T]),
    dateFirst_F64_89=min(INDATUM[isF64_89==T]),
    
    dateLast_F64_0=max(INDATUM[isF64_0==T]),
    dateLast_F64_89=max(INDATUM[isF64_89==T]),
    
    date_F64_089_2=min(INDATUM[num_F64_089==2]),
    date_F64_089_3=min(INDATUM[num_F64_089==3]),
    date_F64_089_4=min(INDATUM[num_F64_089==4]),
    date_F64_089_5=min(INDATUM[num_F64_089==5]),
    date_F64_089_6=min(INDATUM[num_F64_089==6]),
    date_F64_089_7=min(INDATUM[num_F64_089==7]),
    date_F64_089_8=min(INDATUM[num_F64_089==8]),
    date_F64_089_9=min(INDATUM[num_F64_089==9]),
    date_F64_089_10=min(INDATUM[num_F64_089==10]),
    
    hadTranssexual_ICD_89=as.logical(max(isTranssexual_ICD_89)),
    
    numF64_089_2006_01_to_2014_12=sum(isF64_089_2006_01_to_2014_12),
    
    numF64_089=sum(isF64_089),
    numF64_0=sum(isF64_0),
    numF64_89=sum(isF64_89),
    
    isSurgicalMasectomy_2006_01_to_2016_12=as.logical(max(isSurgicalMasectomy_2006_01_to_2016_12)),
    dateFirst_SurgicalMasectomy=min(INDATUM[isSurgicalMasectomy==TRUE]),
    
    isSurgicalReconstVag_2006_01_to_2016_12=as.logical(max(isSurgicalReconstVag_2006_01_to_2016_12)),
    dateFirst_SurgicalReconstVag=min(INDATUM[isSurgicalReconstVag==TRUE]),
    
    isSurgicalPenisAmp_2006_01_to_2016_12=as.logical(max(isSurgicalPenisAmp_2006_01_to_2016_12)),
    dateFirst_SurgicalPenisAmp=min(INDATUM[isSurgicalPenisAmp==TRUE]),
    
    isSurgicalPenisTestProsth_2006_01_to_2016_12=as.logical(max(isSurgicalPenisTestProsth_2006_01_to_2016_12)),
    dateFirst_SurgicalPenisTestProsth=min(INDATUM[isSurgicalPenisTestProsth==TRUE]),
    
    dateFirst_F00_to_F99=min(INDATUM[isF00_to_F99==T]),
    dateFirst_F70_to_F79=min(INDATUM[isF70_to_F79==T]),
    dateFirst_F80_R47=min(INDATUM[isF80_R47==T]),
    dateFirst_F20_to_F29=min(INDATUM[isF20_to_F29==T]),
    dateFirst_F30_to_F31=min(INDATUM[isF30_to_F31==T]),
    dateFirst_F32_to_F33=min(INDATUM[isF32_to_F33==T]),
    dateFirst_F50=min(INDATUM[isF50==T]),
    dateFirst_F84=min(INDATUM[isF84==T]),
    dateFirst_F90=min(INDATUM[isF90==T]),
    dateFirst_F91_to_F98=min(INDATUM[isF91_to_F98==T]),
    dateFirst_F40_to_F48=min(INDATUM[isF40_to_F48==T]),
    dateFirst_F10_to_F16_F18_F19=min(INDATUM[isF10_to_F16_F18_F19==T]),
    dateFirst_F60=min(INDATUM[isF60==T]),
    dateFirst_X60_to_X84=min(INDATUM[isX60_to_X84==T])
  ), by=.(
    LopNr
  )]
  for(i in names(patients)){
    patients[is.infinite(get(i)),(i):=NA]
  }
  
  # days to X diagnosis
  patients[,years_to_F64_089_2:=as.numeric(difftime(date_F64_089_2,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_3:=as.numeric(difftime(date_F64_089_3,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_4:=as.numeric(difftime(date_F64_089_4,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_5:=as.numeric(difftime(date_F64_089_5,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_6:=as.numeric(difftime(date_F64_089_6,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_7:=as.numeric(difftime(date_F64_089_7,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_8:=as.numeric(difftime(date_F64_089_8,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_9:=as.numeric(difftime(date_F64_089_9,dateFirst_F64_089,units="days"))/365.25]
  patients[,years_to_F64_089_10:=as.numeric(difftime(date_F64_089_10,dateFirst_F64_089,units="days"))/365.25]
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
  d[,hadSexChange_le2006_01_01:=FALSE]
  d[dateSexChange<"2006-01-01",hadSexChange_le2006_01_01:=TRUE]
  
  d[,c_isSurgicalMasectomy_2006_01_to_2016_12:=isSurgicalMasectomy_2006_01_to_2016_12]
  d[isBornMale==TRUE,c_isSurgicalMasectomy_2006_01_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalMasectomy:=dateFirst_SurgicalMasectomy]
  d[isBornMale==TRUE,c_dateFirst_SurgicalMasectomy:=NA]
  
  d[,c_isSurgicalPenisTestProsth_2006_01_to_2016_12:=isSurgicalPenisTestProsth_2006_01_to_2016_12]
  d[isBornMale==TRUE,c_isSurgicalPenisTestProsth_2006_01_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalPenisTestProsth:=dateFirst_SurgicalPenisTestProsth]
  d[isBornMale==TRUE,c_dateFirst_SurgicalPenisTestProsth:=NA]
  
  d[,c_isSurgicalReconstVag_2006_01_to_2016_12:=isSurgicalReconstVag_2006_01_to_2016_12]
  d[isBornMale==FALSE,c_isSurgicalReconstVag_2006_01_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalReconstVag:=dateFirst_SurgicalReconstVag]
  d[isBornMale==FALSE,c_dateFirst_SurgicalReconstVag:=NA]
  
  d[,c_isSurgicalPenisAmp_2006_01_to_2016_12:=isSurgicalPenisAmp_2006_01_to_2016_12]
  d[isBornMale==FALSE,c_isSurgicalPenisAmp_2006_01_to_2016_12:=FALSE]
  d[,c_dateFirst_SurgicalPenisAmp:=dateFirst_SurgicalPenisAmp]
  d[isBornMale==FALSE,c_dateFirst_SurgicalPenisAmp:=NA]
  
  # create the categorioes
  
  ####
  # analysis cats
  # F64_089 + treatments
  d[,c_analysisCat_treatments:=as.character(NA)]
  d[,c_analysisDate_treatments:=dateFirst_F64_089]
  
  # hormones
  d[numF64_089>=1 & 
      dateFirst_F64_089>="2006-01-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirstHormone>="2006-01-01" & 
      c_dateFirstHormone<="2016-12-31",
    
    c_analysisCat_treatments:="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]"
  ]
  
  # masectomy
  d[numF64_089>=1 & 
      dateFirst_F64_089>="2006-01-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalMasectomy>="2006-01-01" & 
      c_dateFirst_SurgicalMasectomy<="2016-12-31",
    
    c_analysisCat_treatments:="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]"
    ]
  
  # penisamp
  d[numF64_089>=1 & 
      dateFirst_F64_089>="2006-01-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalPenisAmp>="2006-01-01" & 
      c_dateFirst_SurgicalPenisAmp<="2016-12-31",
    
    c_analysisCat_treatments:="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]"
    ]
  
  # reconstvag
  d[numF64_089>=1 & 
      dateFirst_F64_089>="2006-01-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalReconstVag>="2006-01-01" & 
      c_dateFirst_SurgicalReconstVag<="2016-12-31",
    
    c_analysisCat_treatments:="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]"
    ]
  
  # penistestprosth
  d[numF64_089>=1 & 
      dateFirst_F64_089>="2006-01-01" &
      dateFirst_F64_089<="2016-12-31" & 
      c_dateFirst_SurgicalPenisTestProsth>="2006-01-01" & 
      c_dateFirst_SurgicalPenisTestProsth<="2016-12-31",
    
    c_analysisCat_treatments:="numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2016-12-31]"
    ]
  
  d[is.na(c_analysisCat_treatments), c_analysisDate_treatments:=NA]
  
  d[,c_analysisYear_treatments:=YearN(c_analysisDate_treatments)]
  d[,c_analysisAge_treatments:=as.numeric(difftime(c_analysisDate_treatments,dob,units="days"))/365.25]
  d[,c_analysisAgeCat_treatments:=cut(c_analysisAge_treatments,breaks = c(0,18,30,50,200),include.lowest = T)]
  xtabs(~d$c_analysisCat_treatments+d$c_analysisYear_treatments)
  xtabs(~d$c_analysisAgeCat_treatments)
  
  # date of first surgery or hormones
  d[
    !is.na(c_analysisCat_treatments) & 
      c_dateFirstHormone>="2006-01-01" & 
      c_dateFirstHormone<="2016-12-31", 
    c_analysisCat_treatments_first_date_of_surgery_hormones:=as.Date("2100-01-01")]
  
  d[
    !is.na(c_analysisCat_treatments) & 
      c_dateFirstHormone>="2006-01-01" & 
      c_dateFirstHormone<="2016-12-31" &
      c_dateFirstHormone <= c_analysisCat_treatments_first_date_of_surgery_hormones, 
    c_analysisCat_treatments_first_date_of_surgery_hormones:=c_dateFirstHormone]
  
  d[
    !is.na(c_analysisCat_treatments) & 
      c_dateFirst_SurgicalMasectomy>="2006-01-01" & 
      c_dateFirst_SurgicalMasectomy<="2016-12-31" &
      c_dateFirst_SurgicalMasectomy <= c_analysisCat_treatments_first_date_of_surgery_hormones, 
    c_analysisCat_treatments_first_date_of_surgery_hormones:=c_dateFirst_SurgicalMasectomy]
  
  d[
    !is.na(c_analysisCat_treatments) & 
      c_dateFirst_SurgicalPenisAmp>="2006-01-01" & 
      c_dateFirst_SurgicalPenisAmp<="2016-12-31" &
      c_dateFirst_SurgicalPenisAmp <= c_analysisCat_treatments_first_date_of_surgery_hormones, 
    c_analysisCat_treatments_first_date_of_surgery_hormones:=c_dateFirst_SurgicalPenisAmp]
  
  d[
    !is.na(c_analysisCat_treatments) & 
      c_dateFirst_SurgicalReconstVag>="2006-01-01" & 
      c_dateFirst_SurgicalReconstVag<="2016-12-31" &
      c_dateFirst_SurgicalReconstVag <= c_analysisCat_treatments_first_date_of_surgery_hormones, 
    c_analysisCat_treatments_first_date_of_surgery_hormones:=c_dateFirst_SurgicalReconstVag]
  
  d[
    !is.na(c_analysisCat_treatments) & 
      c_dateFirst_SurgicalPenisTestProsth>="2006-01-01" & 
      c_dateFirst_SurgicalPenisTestProsth<="2016-12-31" &
      c_dateFirst_SurgicalPenisTestProsth <= c_analysisCat_treatments_first_date_of_surgery_hormones, 
    c_analysisCat_treatments_first_date_of_surgery_hormones:=c_dateFirst_SurgicalPenisTestProsth]
  
  
  d[!is.na(c_analysisCat_treatments_first_date_of_surgery_hormones),c_analysisCat_treatments_years_to_first_date_of_surgery_hormones:=as.numeric(difftime(c_analysisCat_treatments_first_date_of_surgery_hormones,dateFirst_F64_089,units="days"))/365.25]
  
  
  ####
  # analysis cats
  # F64_089
  d[,c_analysisCat_diag:=as.character(NA)]
  d[,c_analysisDate_diag:=dateFirst_F64_089]
  d[
    numF64_089>=4 & 
    dateFirst_F64_089>="2001-01-01" &
    dateFirst_F64_089<="2016-12-31",
    c_analysisCat_diag:="numF64_089>=4, first diag: [2001-01-01, 2016-12-31]"
   ]
  
  d[is.na(c_analysisCat_diag), c_analysisDate_diag:=NA]
  
  d[,c_analysisYear_diag:=YearN(c_analysisDate_diag)]
  d[,c_analysisAge_diag:=as.numeric(difftime(c_analysisDate_diag,dob,units="days"))/365.25]
  d[,c_analysisAgeCat_diag:=cut(c_analysisAge_diag,breaks = c(0,18,30,50,200),include.lowest = T)]
  xtabs(~d$c_analysisCat_diag+d$c_analysisYear_diag)
  xtabs(~d$c_analysisAgeCat_diag)
  
  
  ## HYBRID
  d[,c_analysisCat_hybrid:=as.character(NA)]
  d[,c_analysisDate_hybrid:=dateFirst_F64_089]
  d[!is.na(c_analysisCat_diag) | !is.na(c_analysisCat_treatments),
    c_analysisCat_hybrid:="Hybrid"]
  d[is.na(c_analysisCat_hybrid), c_analysisDate_hybrid:=NA]
  
  d[,c_analysisYear_hybrid:=YearN(c_analysisDate_hybrid)]
  d[,c_analysisAge_hybrid:=as.numeric(difftime(c_analysisDate_hybrid,dob,units="days"))/365.25]
  d[,c_analysisAgeCat_hybrid:=cut(c_analysisAge_hybrid,breaks = c(0,18,30,50,200),include.lowest = T)]
  xtabs(~d$c_analysisCat_hybrid+d$c_analysisYear_hybrid)
  xtabs(~d$c_analysisAgeCat_hybrid)
  
  
  # #### exclusions
  d[,excluded_treatments:="No"]
  d[excluded_treatments=="No" & hadTranssexual_ICD_89==TRUE,excluded_treatments:="ICD8/9"]
  d[excluded_treatments=="No" & dateSexChange<dateFirst_F64_089,excluded_treatments:="Legal sex change"]
  d[excluded_treatments=="No" & is.na(dateFirst_F64_089) & !is.na(dateSexChange),excluded_treatments:="Legal sex change"]
  d[excluded_treatments=="No" & c_analysisCat_treatments_first_date_of_surgery_hormones<c_analysisDate_treatments,excluded_treatments:="Hormones/surgery before F64.0/8/9 diag"]
  d[,excluded_treatments:=factor(excluded_treatments,levels=c(
    "No",
    "ICD8/9",
    "Legal sex change",
    "Hormones/surgery before F64.0/8/9 diag"
  ))]
  xtabs(~d$excluded_treatments)
  xtabs(~d$excluded_treatments+d$c_analysisCat_treatments)
  
  d[,excluded_diag:="No"]
  d[excluded_diag=="No" & hadTranssexual_ICD_89==TRUE,excluded_diag:="ICD8/9"]
  d[excluded_diag=="No" & dateSexChange<dateFirst_F64_089,excluded_diag:="Legal sex change"]
  d[excluded_diag=="No" & is.na(dateFirst_F64_089) & !is.na(dateSexChange),excluded_diag:="Legal sex change"]
  d[,excluded_diag:=factor(excluded_diag,levels=c(
    "No",
    "ICD8/9",
    "Legal sex change"
  ))]
  xtabs(~d$excluded_diag)
  
  xtabs(~d$excluded_treatments+d$excluded_diag)
  d[,excluded_hybrid:=excluded_treatments]
  
  
  # dateFirst_F64_089
  xtabs(~d$numF64_089_2006_01_to_2014_12,addNA=T)
  d[,c_analysisCat_F64_089_ge10:="00 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==1,c_analysisCat_F64_089_ge10:="01 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==2,c_analysisCat_F64_089_ge10:="02 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==3,c_analysisCat_F64_089_ge10:="03 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==4,c_analysisCat_F64_089_ge10:="04 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==5,c_analysisCat_F64_089_ge10:="05 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==6,c_analysisCat_F64_089_ge10:="06 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==7,c_analysisCat_F64_089_ge10:="07 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==8,c_analysisCat_F64_089_ge10:="08 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==9,c_analysisCat_F64_089_ge10:="09 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12>=10,c_analysisCat_F64_089_ge10:="10+ F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  
  d[,c_analysisCat_F64_089_ge4:="0 F64.0/8/9 diagnosis [2006-01-01, 2016-12-31]"]
  d[numF64_089_2006_01_to_2014_12==1,c_analysisCat_F64_089_ge4:="1 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==2,c_analysisCat_F64_089_ge4:="2 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12==3,c_analysisCat_F64_089_ge4:="3 F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  d[numF64_089_2006_01_to_2014_12>=4,c_analysisCat_F64_089_ge4:="4+ F64.0/8/9 diagnosis [2006-01-01, 2014-12-31]"]
  
  xtabs(~d$c_analysisCat_F64_089_ge10,addNA=T)
  xtabs(~d$c_analysisCat_F64_089_ge4,addNA=T)
  
  # defining comorbidity
  #d[dateFirst_F00_to_F99
  d[,comorbid_F70_to_F79:=dateFirst_F70_to_F79<c_analysisDate_hybrid]
  d[is.na(comorbid_F70_to_F79),comorbid_F70_to_F79:=FALSE]
  
  d[,comorbid_F80_R47:=dateFirst_F80_R47<c_analysisDate_hybrid]
  d[is.na(comorbid_F80_R47),comorbid_F80_R47:=FALSE]
  
  d[,comorbid_F20_to_F29:=dateFirst_F20_to_F29<c_analysisDate_hybrid]
  d[is.na(comorbid_F20_to_F29),comorbid_F20_to_F29:=FALSE]
  
  d[,comorbid_F30_to_F31:=dateFirst_F30_to_F31<c_analysisDate_hybrid]
  d[is.na(comorbid_F30_to_F31),comorbid_F30_to_F31:=FALSE]
  
  d[,comorbid_F32_to_F33:=dateFirst_F32_to_F33<c_analysisDate_hybrid]
  d[is.na(comorbid_F32_to_F33),comorbid_F32_to_F33:=FALSE]
  
  d[,comorbid_F50:=dateFirst_F50<c_analysisDate_hybrid]
  d[is.na(comorbid_F50),comorbid_F50:=FALSE]
  
  d[,comorbid_F84:=dateFirst_F84<(c_analysisDate_hybrid+365*2)] # ASD
  d[is.na(comorbid_F84),comorbid_F84:=FALSE]
  
  d[,comorbid_F90:=dateFirst_F90<(c_analysisDate_hybrid+365*2)] #ADHD
  d[is.na(comorbid_F90),comorbid_F90:=FALSE]
  
  d[,comorbid_F91_to_F98:=dateFirst_F91_to_F98<c_analysisDate_hybrid]
  d[is.na(comorbid_F91_to_F98),comorbid_F91_to_F98:=FALSE]
  
  d[,comorbid_F40_to_F48:=dateFirst_F40_to_F48<c_analysisDate_hybrid]
  d[is.na(comorbid_F40_to_F48),comorbid_F40_to_F48:=FALSE]
  
  d[,comorbid_F10_to_F16_F18_F19:=dateFirst_F10_to_F16_F18_F19<c_analysisDate_hybrid]
  d[is.na(comorbid_F10_to_F16_F18_F19),comorbid_F10_to_F16_F18_F19:=FALSE]
  
  d[,comorbid_F60:=dateFirst_F60<c_analysisDate_hybrid]
  d[is.na(comorbid_F60),comorbid_F60:=FALSE]
  
  d[,comorbid_X60_to_X84:=dateFirst_X60_to_X84<c_analysisDate_hybrid]
  d[is.na(comorbid_X60_to_X84),comorbid_X60_to_X84:=FALSE]
  
  return(d)
}




