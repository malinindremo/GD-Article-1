GetPop <- function(){
  pop <- data.table(readxl::read_excel(fs::path(org::PROJ$HOME,"structural_data","pop.xlsx"),skip=0))
  pop <- melt.data.table(pop,id.vars=c("year","age"))
  setnames(pop,c("year","age","sex","pop"))
  pop[,year:=as.numeric(year)]
  pop[,age:=stringr::str_extract(age,"[0-9]+")]
  pop[,ageCat:=cut(as.numeric(age),breaks = c(0,18,30,50,200),include.lowest = T)]
  pop[,ageCat:=as.character(ageCat)]
  pop[,isBornMale:=sex=="men"]
  pop[,bornSex:=ifelse(isBornMale,"Assigned male","Assigned female")]
  
  pop0 <- pop[,.(
    pop=sum(pop)
  ),keyby=.(
    year,ageCat,bornSex
  )]
  
  pop1 <- pop[,.(
    pop=sum(pop)
  ),keyby=.(
    year,bornSex
  )]
  pop1[,ageCat:="All"]
  
  pop2 <- pop[,.(
    pop=sum(pop)
  ),keyby=.(
    year,ageCat
  )]
  pop2[,bornSex:="All"]
  
  pop3 <- pop[,.(
    pop=sum(pop)
  ),keyby=.(
    year
  )]
  pop3[,ageCat:="All"]
  pop3[,bornSex:="All"]
  
  pop <- rbind(pop0,pop1,pop2,pop3)
}