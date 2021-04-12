get_legal_sex_change <- function(){
  pop <- GetPop()
  legal_sexchange_applications <- readxl::read_excel(fs::path(org::project$home,"structural_data","legal_sex_change.xlsx"))
  setDT(legal_sexchange_applications)
  legal_sexchange_applications[pop[ageCat=="All" & bornSex=="All"],on="year",pop:=pop]
  setnames(legal_sexchange_applications, c("analysisYear_z","N","pop"))
  legal_sexchange_applications[,definition:="Legal change"]
  legal_sexchange_applications[,bornSex:="Total"]
  return(legal_sexchange_applications)
}