CleanData <- function(){
  
  ov <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/ov.sas7bdat")))
  sv <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/sv.sas7bdat")))
  rx <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/ut_r_lmed_10218_2017.sas7bdat")))
  demografi <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/demografi.sas7bdat")))
  sex <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/kon.sas7bdat")))
  sex[,isBornMale:=kon==1]
  sex[,kon:=NULL]
 
  ## sex change 
  sexChange <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/konsbyten.sas7bdat")))
  sexChange[,dateSexChange:=as.Date(sprintf(
    "%s-%s-%s",
    stringr::str_sub(konsbyte_datum,1,4),
    stringr::str_sub(konsbyte_datum,5,6),
    stringr::str_sub(konsbyte_datum,7,8)
  ))]
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
  
  rx <- rx[,.(
    dateFirstHormoneMTF=min(FDATUM[isHormoneMTF==TRUE]),
    dateFirstHormoneFTM=min(FDATUM[isHormoneFTM==TRUE]),
    dateFirstHormonePubBlock=min(FDATUM[isHormonePubBlock==TRUE])
  ),by=.(lopnr)]
  
  # merge in sex/hormones/dob
  nrow(sex)
  d <- merge(sex,rx,by.x="LopNr",by.y="lopnr",all.x=T)
  nrow(d)
  d <- merge(d,demografi,by="LopNr")
  nrow(d)
 
  d[isBornMale==TRUE,dateFirstHormoneFTM:=NA]
  d[isBornMale==FALSE,dateFirstHormoneMTF:=NA]

  d[,dateFirstHormone:=min(
    dateFirstHormoneFTM,
    dateFirstHormoneMTF,
    dateFirstHormonePubBlock,
    na.rm=T),
    by=.(
      LopNr
    )]
  d[is.infinite(dateFirstHormone),dateFirstHormone:=NA]
  d[,dateFirstHormoneFTM:=NULL]
  d[,dateFirstHormoneMTF:=NULL]
  d[,dateFirstHormonePubBlock:=NULL]
  
  # diagnoses and surgeries
  ov[,type:="outpatient"]
  sv[,type:="inpatient"]
  patients <- rbind(ov,sv,fill=T)
  
  setorder(patients,LopNr,INDATUM)
  
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
    #patients[stringr::str_detect(get(i),"^302"), isTranssexual_ICD_89:=TRUE]
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
  
  # surgeries
  patients[,isSurgical:=FALSE]
  for(i in c(
    "HAC10",
    "HAC20",
    "HAC99",
    "HAE00",
    "HAE99",
    "HAD20",
    "HAD30",
    "HAD35",
    "HAD99",
    "LEE40",
    "LFE96",
    "LEE96",
    "KGC10",
    "KFH50",
    "KGV30",
    "KGW96",
    "KGH96",
    "LCD00",
    "LCD01",
    "LCD04",
    "LCD10",
    "LCD11",
    "LCD96",
    "LCD97",
    "DQD40"
  )){
    patients[stringr::str_detect(OP,i),isSurgical:=TRUE]
  }
  for(i in c(
    "ZZS40",
    "LEE20"
  )){
    patients[stringr::str_detect(OP,i) & isCodeUsedWithSurgery==TRUE,isSurgical:=TRUE]
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
    ageFirst_Surgery=min(age[isSurgical==T]),
    
    dateFirst_F64_089=min(INDATUM[isF64_089==TRUE]),
    dateFirst_F64_0=min(INDATUM[isF64_0==T]),
    dateFirst_F64_89=min(INDATUM[isF64_89==T]),
    dateFirst_Surgery=min(INDATUM[isSurgical==T]),
    
    dateLast_F64_0=max(INDATUM[isF64_0==T]),
    dateLast_F64_89=max(INDATUM[isF64_89==T]),
    
    hadTranssexual_ICD_89=as.logical(max(isTranssexual_ICD_89))
  ), by=.(
    LopNr
  )]
  for(i in names(patients)){
    patients[is.infinite(get(i)),(i):=NA]
  }
  #patients[,ageFirstCat:=cut(ageFirst,breaks=c(0,12,15,20,30,100))]
  
  # merge sex/hormones/dob with diagnoses/surgeries
  nrow(patients)
  d <- merge(d,patients,by="LopNr",all.x=T)
  nrow(d)
  d <- merge(d,sexChange,by="LopNr",all.x=T)
  nrow(d)
  
  # create the categorioes
  d[,category:="No diagnosis"]
  d[is.na(dateFirst_F64_0) & !is.na(dateFirst_F64_89),category:="Only F64.8/9"]
  d[!is.na(dateFirst_F64_0) & is.na(dateFirst_F64_89),category:="Only F64.0"]
  d[dateFirst_F64_0==dateFirst_F64_089 & dateFirst_F64_89==dateFirst_F64_089,
    category:="F64.0 and F64.8/9 at first consult"]
  d[dateLast_F64_0==dateLast_F64_89,
    category:="F64.0 and F64.8/9 at last consult"]
  d[dateFirst_F64_0==dateFirst_F64_089 & dateFirst_F64_89>dateFirst_F64_089 & dateLast_F64_0>dateLast_F64_89,
    category:="F64.0 -> F64.8/9 -> F64.0"]
  d[dateFirst_F64_0==dateFirst_F64_089 & dateFirst_F64_89>dateFirst_F64_089 & dateLast_F64_0<dateLast_F64_89,
    category:="F64.0 -> F64.8/9"]
  d[dateFirst_F64_0>dateFirst_F64_089 & dateFirst_F64_89==dateFirst_F64_089 & dateLast_F64_89>dateLast_F64_0,
    category:="F64.8/9 -> F64.0 -> F64.8/9"]
  d[dateFirst_F64_0>dateFirst_F64_089 & dateFirst_F64_89==dateFirst_F64_089 & dateLast_F64_89<dateLast_F64_0,
    category:="F64.8/9 -> F64.0"]
  
  d[is.na(category)]
  xtabs(~d$category,addNA=T)
  
  # fixing hadTranssexual_ICD_89
  d[is.na(hadTranssexual_ICD_89), hadTranssexual_ICD_89:=FALSE]
  
  
  return(d)
}