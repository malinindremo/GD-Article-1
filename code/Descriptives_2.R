Descriptives_2 <- function(d){
  
  toPlot <- d[category!="No diagnosis"]
  
  q <- ggplot(toPlot,aes(x=category))
  q <- q + geom_bar(alpha=0.75)
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("Number of people")
  q <- q + coord_flip()
  #q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  SaveA4(q,
         file.path(FOLDERS$results_today,"descriptives","diagnosis_categories.png"),
         landscape=T)
  
  q <- ggplot(toPlot,aes(x=category))
  q <- q + geom_bar(alpha=0.75)
  q <- q + facet_wrap(~hadTranssexual_ICD_89)
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("Number of people")
  q <- q + coord_flip()
  #q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  SaveA4(q,
         file.path(FOLDERS$results_today,"descriptives","diagnosis_categories_by_hadTranssexual_ICD_89.png"),
         landscape=T)
  
  q <- ggplot(toPlot,aes(x=category))
  q <- q + geom_bar(alpha=0.75)
  q <- q + facet_wrap(~yearFirst_F64_089)
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("Number of people")
  q <- q + coord_flip()
  q <- q + labs(title="People displayed by the year of first F68.0/8/9 diagnosis")
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  SaveA4(q,
         file.path(FOLDERS$results_today,"descriptives","diagnosis_categories_by_year.png"),
         landscape=T)
  
  q <- ggplot(toPlot,aes(x=category,y=ageFirst_F64_089))
  q <- q + geom_boxplot(alpha=0.75)
  q <- q + scale_x_discrete("")
  q <- q + scale_y_continuous("Age at first F64.0/8/9 consultation",breaks=seq(0,100,4))
  q <- q + coord_flip()
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  SaveA4(q,
         file.path(FOLDERS$results_today,"descriptives","diagnosis_categories_age_first_consult.png"),
         landscape=T)
  
  
  q <- ggplot(toPlot,aes(x=ageFirst_F64_089,y=daysFirst_F64_0/365.25))
  q <- q + geom_point(alpha=0.2)
  q <- q + geom_smooth()
  q <- q + facet_wrap(~category)
  q <- q + scale_x_continuous("Age at first consultation",breaks=seq(0,100,4))
  q <- q + scale_y_continuous("Years waiting until first F64.0 diagnosis",breaks=seq(0,20,2))
  #q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
  SaveA4(q,
         file.path(FOLDERS$results_today,"descriptives","time_until_first_F64_0.png"),
         landscape=T,
         scalev=1)
  
  q <- ggplot(toPlot,aes(x=ageFirst_F64_089,y=daysFirst_F64_89/365.25))
  q <- q + geom_point(alpha=0.2)
  q <- q + geom_smooth()
  q <- q + facet_wrap(~category)
  q <- q + scale_x_continuous("Age at first consultation",breaks=seq(0,100,4))
  q <- q + scale_y_continuous("Years waiting until first F64.8/9 diagnosis",breaks=seq(0,20,2))
  #q <- q + labs(title="People who initially had a F64.8/9 diagnosis and subsequently F64.0")
  SaveA4(q,
         file.path(FOLDERS$results_today,"descriptives","time_until_first_F64_89.png"),
         landscape=T,
         scalev=1)
  
  
  x <- d[,.(
    numF64_089=sum(isF64_089),
    numF64_0=sum(isF64_0),
    numF64_89=sum(isF64_89),
    daysFirst_F64_0=mean(daysFirst_F64_0),
    daysFirst_F64_89=mean(daysFirst_F64_89)
  ),by=.(LopNr,ageFirstCat,category,yearFirst)]
  
  tab <- d[,.(
    N=.N,
    `Mean num of F64.0/8/9 diagnoses`=round(mean(numF64_089),1),
    `Mean num of F64.0 diagnoses`=round(mean(numF64_0),1),
    `Mean num of F64.8/9 diagnoses`=round(mean(numF64_89),1),
    `Percentage with 2 or more F64.0 diagnoses`=round(100*mean(numF64_0>=2)),
    `Percentage with 3 or more F64.0 diagnoses`=round(100*mean(numF64_0>=3)),
    `Percentage with 2 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=2)),
    `Percentage with 3 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=3)),
    `Percentage with 2 or more F64.0/8/9 diagnoses`=round(100*mean(numF64_089>=2)),
    `Percentage with 3 or more F64.0/8/9 diagnoses`=round(100*mean(numF64_089>=3)),
    `Percentage diagnosed with only F64.0`=round(100*mean(numF64_0>0 & numF64_89==0)),
    `Percentage diagnosed with only F64.8/9`=round(100*mean(numF64_0==0 & numF64_89>0)),
    `Percentage diagnosed with both F64.0 and F64.8/9`=round(100*mean(numF64_0>0 & numF64_89>0)),
    `For people diagnosed first with F64.8/9, mean days spent at F64.8/9 before F64.0`=round(mean(daysFirst_F64_0[daysFirst_F64_89==0],na.rm=T))
  ),keyby=.(category)]
  
  openxlsx::write.xlsx(tab, file=
                         file.path(FOLDERS$results_today,"descriptives","categories.xlsx"))
  
  tab <- d[,.(
    N=.N,
    `Mean num of F64.0/8/9 diagnoses`=round(mean(numF64_089),1),
    `Mean num of F64.0 diagnoses`=round(mean(numF64_0),1),
    `Mean num of F64.8/9 diagnoses`=round(mean(numF64_89),1),
    `Percentage with 2 or more F64.0 diagnoses`=round(100*mean(numF64_0>=2)),
    `Percentage with 3 or more F64.0 diagnoses`=round(100*mean(numF64_0>=3)),
    `Percentage with 2 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=2)),
    `Percentage with 3 or more F64.8/9 diagnoses`=round(100*mean(numF64_89>=3)),
    `Percentage with 2 or more F64.0/8/9 diagnoses`=round(100*mean(numF64_089>=2)),
    `Percentage with 3 or more F64.0/8/9 diagnoses`=round(100*mean(numF64_089>=3)),
    `Percentage diagnosed with only F64.0`=round(100*mean(numF64_0>0 & numF64_89==0)),
    `Percentage diagnosed with only F64.8/9`=round(100*mean(numF64_0==0 & numF64_89>0)),
    `Percentage diagnosed with both F64.0 and F64.8/9`=round(100*mean(numF64_0>0 & numF64_89>0)),
    `For people diagnosed first with F64.8/9, mean days spent at F64.8/9 before F64.0`=round(mean(daysFirst_F64_0[daysFirst_F64_89==0],na.rm=T))
  ),keyby=.(yearFirst_F64_089,category)]
  
  openxlsx::write.xlsx(tab, file=
                         file.path(FOLDERS$results_today,"descriptives","categories_by_year.xlsx"))
  
 
  
  
  
}