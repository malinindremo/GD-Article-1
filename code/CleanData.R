YearN <- function(x) as.numeric(format.Date(x, "%G"))

CleanData <- function(){
  
  ov <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/ov.sas7bdat")))
  sv <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/sv.sas7bdat")))
  rx <- data.table(haven::read_sas(file.path(FOLDERS$data,"Sos/ut_r_lmed_10218_2017.sas7bdat")))
  demografi <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/demografi.sas7bdat")))
  sex <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/kon.sas7bdat")))
  sex[,isBornMale:=kon==1]
  sex[,bornSex:=ifelse(isBornMale,"Born Male","Born Female")]
  sex[,kon:=NULL]
 
  ## sex change 
  sexChange <- data.table(haven::read_sas(file.path(FOLDERS$data,"SCB/konsbyten.sas7bdat")))
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
  #rx <- merge(d,demografi,by="LopNr")
  #nrow(d)
 
  rx[isBornMale==TRUE,isHormoneFTM:=FALSE]
  rx[isBornMale==FALSE,isHormoneMTF:=FALSE]
  
  rx[,isHormone:=isHormoneMTF | isHormoneFTM | isHormonePubBlock]
  
  rx[,isHormone_2005_07_to_2016_12:=isHormone]
  rx[FDATUM < "2005-07-01",isHormone_2005_07_to_2016_12:=FALSE]
  rx[FDATUM > "2016-12-31",isHormone_2005_07_to_2016_12:=FALSE]
  
  # collapse down to 1 row/person
  rx <- rx[,.(
    isHormone=as.logical(max(isHormone)),
    dateFirstHormone=min(FDATUM[isHormone==TRUE]),
    isHormone_2005_07_to_2016_12=as.logical(max(isHormone_2005_07_to_2016_12))
  ),by=.(LopNr)]
  nrow(rx)
  
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
  # 
  # for(i in c(
  #   "ZZS40",
  #   "LEE20"
  # )){
  #   patients[stringr::str_detect(OP,i) & isCodeUsedWithSurgery==TRUE,isSurgical:=TRUE]
  # }
  # 
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
    
    numF64_089=sum(isF64_089),
    numF64_0=sum(isF64_0),
    numF64_89=sum(isF64_89),
    
    isSurgicalMasectomy_2005_07_to_2016_12=as.logical(max(isSurgicalMasectomy_2005_07_to_2016_12)),
    dateFirst_SurgicalMasectomy=min(INDATUM[isSurgicalMasectomy==TRUE])
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
  
  # gender confirming treatment
  d[,]
  
  # new variables
  d[,yearFirst_F64_089:=as.numeric(format.Date(dateFirst_F64_089, "%G"))]
  d[,daysFirst_F64_0:=as.numeric(difftime(dateFirst_F64_0,dateFirst_F64_089,units="days"))]
  d[,daysFirst_F64_89:=as.numeric(difftime(dateFirst_F64_89,dateFirst_F64_089,units="days"))]
  
  # analysis cats
  # F64_089
  # until 2013
  d[,analysisCat_x:=as.character(NA)]
  d[numF64_089>=3 & 
      dateFirst_F64_089>="2001-01-01" & 
      dateFirst_F64_089<="2016-12-31",
    analysisCat_x:="numF64_089>=3, first diag: [2001-01-01, 2016-12-31]"]
  d[hadTranssexual_ICD_89==TRUE,analysisCat_x:=NA]
  d[dateSexChange<"2001-01-01",analysisCat_x:=NA]
  d[!is.na(analysisCat_x),analysisYear_x:=YearN(dateFirst_F64_089)]
  d[!is.na(analysisCat_x),analysisAge_x:=ageFirst_F64_089]
  d[,analysisAgeCat_x:=cut(analysisAge_x,breaks = c(0,18,30,50,200),include.lowest = T)]
  d$analysisAgeCat_x
  #d[,analysisAgeCat_x:=as.character(analysisAgeCat_x)]
  
  
  # analysis cats
  # F64_089
  # until 2013
  d[,analysisCat_y:=as.character(NA)]
  d[numF64_089==1 & 
      dateFirst_F64_089>="2001-01-01" & 
      dateFirst_F64_089<="2016-12-31",
    analysisCat_y:="numF64_089==1, first diag: [2001-01-01, 2016-12-31]"]
  d[numF64_089==2 & 
      dateFirst_F64_089>="2001-01-01" & 
      dateFirst_F64_089<="2016-12-31",
    analysisCat_y:="numF64_089==2, first diag: [2001-01-01, 2016-12-31]"]
  d[numF64_089==3 & 
      dateFirst_F64_089>="2001-01-01" & 
      dateFirst_F64_089<="2016-12-31",
    analysisCat_y:="numF64_089==3, first diag: [2001-01-01, 2016-12-31]"]
  d[numF64_089>=4 & 
      dateFirst_F64_089>="2001-01-01" & 
      dateFirst_F64_089<="2016-12-31",
    analysisCat_y:="numF64_089>=4, first diag: [2001-01-01, 2016-12-31]"]
  d[hadTranssexual_ICD_89==TRUE,analysisCat_y:=NA]
  d[dateSexChange<"2001-01-01",analysisCat_y:=NA]
  d[!is.na(analysisCat_y),analysisYear_y:=YearN(dateFirst_F64_089)]
  d[!is.na(analysisCat_y),analysisAge_y:=ageFirst_F64_089]
  d[,analysisAgeCat_y:=cut(analysisAge_y,breaks = c(0,18,30,50,200),include.lowest = T)]
  d$analysisAgeCat_y
  #d[,analysisAgeCat_x:=as.character(analysisAgeCat_x)]
  
  d[,hormoneCat_y:=as.character(NA)]
  d[!is.na(analysisCat_y),hormoneCat_y:="No hormones"]
  d[!is.na(analysisCat_y) & dateFirstHormone<dateFirst_F64_089,hormoneCat_y:="Hormones before diag"]
  d[!is.na(analysisCat_y) & dateFirstHormone>=dateFirst_F64_089,hormoneCat_y:="Hormones after diag"]
  xtabs(~d$hormoneCat_y)
  
  d[,surgicalMasectomyCat_y:=as.character(NA)]
  d[!is.na(analysisCat_y),surgicalMasectomyCat_y:="No masectomy"]
  d[!is.na(analysisCat_y) & dateFirst_SurgicalMasectomy<dateFirst_F64_089,surgicalMasectomyCat_y:="Masectomy before diag"]
  d[!is.na(analysisCat_y) & dateFirst_SurgicalMasectomy>=dateFirst_F64_089,surgicalMasectomyCat_y:="Masectomy after diag"]
  xtabs(~d$surgicalMasectomyCat_y)
  
  d[,hormoneAndSurgicalMasectomyCat_y:=sprintf("%s/\n%s",hormoneCat_y,surgicalMasectomyCat_y)]
  xtabs(~d$hormoneAndSurgicalMasectomyCat_y)
  
  # analysis cats
  # F64_089
  # until 2013
  d[,analysisCat_1:=as.character(NA)]
  d[numF64_089==1 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2013-12-31",
    analysisCat_1:="numF64_089==1, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_1),analysisYear_1:=YearN(dateFirst_F64_089)]
  
  d[,analysisCat_2:=as.character(NA)]
  d[numF64_089==2 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2013-12-31",
    analysisCat_2:="numF64_089==2, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_2),analysisYear_2:=YearN(dateFirst_F64_089)]
  
  d[,analysisCat_3:=as.character(NA)]
  d[numF64_089>=3 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2013-12-31",
    analysisCat_3:="numF64_089>=3, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_3),analysisYear_3:=YearN(dateFirst_F64_089)]
  
  d[,analysisCat_4:=as.character(NA)]
  d[numF64_089>=4 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2013-12-31",
    analysisCat_4:="numF64_089>=4, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_4),analysisYear_4:=YearN(dateFirst_F64_089)]
  
  # until 2014
  d[,analysisCat_5:=as.character(NA)]
  d[numF64_089==1 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2014-12-31",
    analysisCat_5:="numF64_089==1, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_5),analysisYear_5:=YearN(dateFirst_F64_089)]
  
  d[,analysisCat_6:=as.character(NA)]
  d[numF64_089==2 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2014-12-31",
    analysisCat_6:="numF64_089==2, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_6),analysisYear_6:=YearN(dateFirst_F64_089)]
  
  d[,analysisCat_7:=as.character(NA)]
  d[numF64_089>=3 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2014-12-31",
    analysisCat_7:="numF64_089>=3, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_7),analysisYear_7:=YearN(dateFirst_F64_089)]
  
  d[,analysisCat_8:=as.character(NA)]
  d[numF64_089>=4 & 
      dateFirst_F64_089>="2005-07-01" & 
      dateFirst_F64_089<="2014-12-31",
    analysisCat_8:="numF64_089>=4, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_8),analysisYear_8:=YearN(dateFirst_F64_089)]
  
  # F64_89
  # until 2013
  d[,analysisCat_9:=as.character(NA)]
  d[numF64_89==1 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2013-12-31",
    analysisCat_9:="numF64_89==1, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_9),analysisYear_9:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_10:=as.character(NA)]
  d[numF64_89==2 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2013-12-31",
    analysisCat_10:="numF64_89==2, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_10),analysisYear_10:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_11:=as.character(NA)]
  d[numF64_89>=3 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2013-12-31",
    analysisCat_11:="numF64_89>=3, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_11),analysisYear_11:=YearN(dateFirst_F64_89)]
  
  # until 2014
  d[,analysisCat_12:=as.character(NA)]
  d[numF64_89==1 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2014-12-31",
    analysisCat_12:="numF64_89==1, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_12),analysisYear_12:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_13:=as.character(NA)]
  d[numF64_89==2 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2014-12-31",
    analysisCat_13:="numF64_89==2, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_13),analysisYear_13:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_14:=as.character(NA)]
  d[numF64_89>=3 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2014-12-31",
    analysisCat_14:="numF64_89>=3, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_14),analysisYear_14:=YearN(dateFirst_F64_89)]
  
  # F64_89 and F64_0==0
  # until 2013
  d[,analysisCat_15:=as.character(NA)]
  d[numF64_0==0 &
      numF64_89==1 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2013-12-31",
    analysisCat_15:="numF64_0==0 & numF64_89==1, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_15),analysisYear_15:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_16:=as.character(NA)]
  d[numF64_0==0 &
      numF64_89==2 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2013-12-31",
    analysisCat_16:="numF64_0==0 & numF64_89==2, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_16),analysisYear_16:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_17:=as.character(NA)]
  d[numF64_0==0 &
      numF64_89>=3 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2013-12-31",
    analysisCat_17:="numF64_0==0 & numF64_89>=3, first diag: [2005-07-01, 2013-12-31]"]
  d[!is.na(analysisCat_17),analysisYear_17:=YearN(dateFirst_F64_89)]
  
  # until 2014
  d[,analysisCat_18:=as.character(NA)]
  d[numF64_0==0 &
      numF64_89==1 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2014-12-31",
    analysisCat_18:="numF64_0==0 & numF64_89==1, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_18),analysisYear_18:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_19:=as.character(NA)]
  d[numF64_0==0 &
      numF64_89==2 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2014-12-31",
    analysisCat_19:="numF64_0==0 & numF64_89==2, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_19),analysisYear_19:=YearN(dateFirst_F64_89)]
  
  d[,analysisCat_20:=as.character(NA)]
  d[numF64_0==0 &
      numF64_89>=3 & 
      dateFirst_F64_89>="2005-07-01" & 
      dateFirst_F64_89<="2014-12-31",
    analysisCat_20:="numF64_0==0 & numF64_89>=3, first diag: [2005-07-01, 2014-12-31]"]
  d[!is.na(analysisCat_20),analysisYear_20:=YearN(dateFirst_F64_89)]
  
  return(d)
}




