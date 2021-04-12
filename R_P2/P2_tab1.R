P2_tab1 <- function(d){
  dz <- d[
    excluded=="No" &
      c_analysisCat_diag %in% c(
        "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]",
        "control_assigned",
        "control_opposite"
      )
  ]
  dz[,N:=1]
  
  co <- stringr::str_subset(names(dz),"^comorbid")
  
  dz[, label_sex := c_analysisSex_diag]
  dz[, label_age := c_analysisAgeCat_diag]
  dz[, label_analysis := c_analysisCat_diag]
  
  # by sex age
  res <- dz[
    , 
    lapply(.SD, sum),
    keyby = .(label_analysis, label_sex), 
    .SDcols = c("N",co)
  ]
  
  res <- melt.data.table(
    res,
    id.vars = c(
      "label_analysis",
      "label_sex",
      "N"
    )
  )
  res[label_analysis=="numF64_089>=4, first diag: [2001-01-01, 2015-12-31]", case_N := N]
  res[,case_N := mean(case_N, na.rm=T),by=.(label_sex, variable)]
  res[label_analysis=="numF64_089>=4, first diag: [2001-01-01, 2015-12-31]", case_value := value]
  res[,case_value := mean(case_value, na.rm=T),by=.(label_sex, variable)]
  res[,row:=1:.N]
  res[,pval := round(chisq.test(cbind(c(N,case_N),c(value,case_value)))$p.value,3),by=.(row)]
  res
  
  res[,formatted := glue::glue(
    "{formatC(100*value/N, digits=1, format='f')}% ({value}/{N}) p={formatC(pval, digits=3, format='f')}",
    value = value,
    N=N,
    pval=pval
  )]
  res[label_analysis=="numF64_089>=4, first diag: [2001-01-01, 2015-12-31]",formatted := glue::glue(
    "{formatC(100*value/N, digits=1, format='f')}% ({value}/{N})",
    value = value,
    N=N
  )]
  res
  res[, label_sex := factor(
    label_sex,
    levels = c(
      "Total",
      "Assigned female",
      "Assigned male"
    )
  )]
  
  res[, label_analysis := factor(
    label_analysis,
    levels = c(
      "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]",
      "control_assigned",
      "control_opposite",
      "control"
    )
  )]
  
  (tab1 <- dcast.data.table(
    res,
    variable ~ label_sex+label_analysis,
    value.var = c("formatted")
  ))
  openxlsx::write.xlsx(tab1, fs::path(org::project$results_today,"tab1.xlsx"))
  
}