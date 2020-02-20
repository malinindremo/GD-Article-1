links_assigned <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","fp_lev_fall_och_kontroller_1.sas7bdat")))
links_opposite <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","fp_lev_fall_och_kontroller_2.sas7bdat")))
ov <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","ov.sas7bdat")))
sv <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","sv.sas7bdat")))
rx <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","ut_r_lmed_10218_2017.sas7bdat")))
demografi <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","demografi.sas7bdat")))
sex <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","kon.sas7bdat")))

birth_reg1 <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"Sos","UT_MFR_BARN_10218_2017.sas7bdat"),encoding="UTF-8"))
birth_reg2 <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","fp_lev_fall_och_kontroller_1.sas7bdat"),encoding="UTF-8"))

cat("****** Line 21 /",number_lines,"\n")

## sex change 
sexChange <- data.table(haven::read_sas(fs::path(org::PROJ$DATA_RAW,"SCB","konsbyten.sas7bdat")))
sexChange[,dateSexChange:=as.Date(sprintf(
  "%s-%s-%s",
  stringr::str_sub(konsbyte_datum,1,4),
  stringr::str_sub(konsbyte_datum,5,6),
  stringr::str_sub(konsbyte_datum,7,8)
))]
sexChange[,yearSexChange:=lubridate::year(dateSexChange)]
sexChange[,konsbyte_datum:=NULL]

lopnrs <- sexChange$LopNr

sex <- sex[LopNr %in% lopnrs]


sex[unique(links_assigned[,c("lopnr_fall","fodelsekon")]),on="LopNr==lopnr_fall",assigned_fodelsekon:=fodelsekon]
sex[unique(links_opposite[,c("lopnr_fall","motsatt_fodelsekon")]),on="LopNr==lopnr_fall",opposite_fodelsekon:=motsatt_fodelsekon]

sex[birth_reg1,on="LopNr==LopNr",UT_MFR_BARN_10218_2017_kon:=KON]
sex[birth_reg2,on="LopNr==lopnr_fall",fp_lev_fall_och_kontroller_1_kon:=fodelsekon]

xtabs(~kon+assigned_fodelsekon,data=sex)
xtabs(~kon+opposite_fodelsekon,data=sex)

xtabs(~kon+UT_MFR_BARN_10218_2017_kon,data=sex)
xtabs(~kon+fp_lev_fall_och_kontroller_1_kon,data=sex)


sex[kon!=UT_MFR_BARN_10218_2017_kon]
