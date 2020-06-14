comorbidity_2 <- function(dz,folder="comorbidity_2"){
  co <- stringr::str_subset(names(dz),"^comorbid")
  
  # duplicate
  dz_1 <- copy(dz)
  dz_1[, c_analysisSex_diag := "Total"]
  dz <- rbind(dz, dz_1)
  
  dz_1 <- copy(dz)
  dz_1[, c_analysisAgeCat_diag := "Total"]
  dz <- rbind(dz, dz_1)
  
  dz_1 <- copy(dz)
  dz_1[, c_analysisYearCat_diag := "Total"]
  dz <- rbind(dz, dz_1)
  
  
  
  # by sex age
  res <- dz[
    , 
    lapply(.SD, sum),
    keyby = .(c_analysisCat_diag, c_analysisSex_diag, c_analysisAgeCat_diag, c_analysisYearCat_diag), 
    .SDcols = c("N",co)
  ]
  
  res <- melt.data.table(
    res,
    id.vars = c(
      "c_analysisCat_diag",
      "c_analysisSex_diag",
      "c_analysisAgeCat_diag",
      "c_analysisYearCat_diag",
      "N"
    )
  )
  res
  res[,formatted := glue::glue(
    "{formatC(100*value/N, digits=1, format='f')}% ({value}/{N})",
    value = value,
    N=N
  )]
  res
  res[, c_analysisSex_diag := factor(
    c_analysisSex_diag,
    levels = c(
      "Total",
      "Assigned female",
      "Assigned male"
    )
  )]
  
  res[, c_analysisAgeCat_diag := factor(
    c_analysisAgeCat_diag,
    levels = c(
      "Total",
      "10-17",
      "18-30",
      "31-50",
      "51+"
    )
  )]
  
  res[, c_analysisCat_diag := factor(
    c_analysisCat_diag,
    levels = c(
      "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]",
      "control_assigned",
      "control_opposite"
    )
  )]
  
  res <- dcast.data.table(
    res,
    variable + c_analysisSex_diag + c_analysisAgeCat_diag + c_analysisCat_diag ~  c_analysisYearCat_diag,
    value.var = c("formatted")
  )
  res[, `Annual_Increase_%`:=""]
  res[, `Annual_Increase_pval_bernoulli`:=""]
  res[, `Annual_Increase_pval`:=""]
  res[, `Annual_Increase_interaction_pval`:=""]
  for(i in 1:nrow(res)){
    cat(i, "/", nrow(res),"\n")
    x_outcome <- res$variable[i]
    x_c_analysisSex_diag <- res$c_analysisSex_diag[i]
    x_c_analysisAgeCat_diag <- res$c_analysisAgeCat_diag[i]
    x_c_analysisCat_diag <- res$c_analysisCat_diag[i]
    d_analysis <- dz[
      c_analysisSex_diag == x_c_analysisSex_diag &
      c_analysisAgeCat_diag == x_c_analysisAgeCat_diag &
      c_analysisCat_diag == x_c_analysisCat_diag & 
      c_analysisYearCat_diag == "Total"
    ]
    
    if(sum(d_analysis[[as.character(x_outcome)]])<5) next
    formula <- glue::glue("{x_outcome} ~ c_analysisYear_diag")
    fit <- logbin::logbin(as.formula(formula), data = d_analysis, method = "em")
    x_est <- coef(summary(fit))[2,"Estimate"]
    x_p <- coef(summary(fit))[2,"Pr(>|z|)"]
    x_p_bernoulli <- x_p * nrow(res)
    if(x_p_bernoulli>1) x_p_bernoulli <- 1
    
    if(!is.nan(x_p)){
      res[i,`Annual_Increase_%`:= paste0(formatC(100*exp(x_est)-100, digits=2, format='f'),"%")]

      res[i,`Annual_Increase_pval_bernoulli`:= formatC(x_p_bernoulli, digits=3, format='f')]
      if(x_p_bernoulli < 0.05) res[i,`Annual_Increase_pval_bernoulli`:= paste0(`Annual_Increase_pval_bernoulli`,"*")]
      
      res[i,`Annual_Increase_pval`:= formatC(x_p, digits=3, format='f')]
      if(x_p < 0.05) res[i,`Annual_Increase_pval`:= paste0(`Annual_Increase_pval`,"*")]
    }
    
    try({
      # run interaction term
      if(x_c_analysisCat_diag %in% c("control_assigned","control_opposite")){
        d_analysis <- dz[
          c_analysisSex_diag == x_c_analysisSex_diag &
          c_analysisAgeCat_diag == x_c_analysisAgeCat_diag &
          c_analysisCat_diag %in% c(
            as.character(x_c_analysisCat_diag), 
            "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]"
          ) & 
          c_analysisYearCat_diag == "Total"
          ]
        
        formula0 <- glue::glue("{x_outcome} ~ c_analysisYear_diag+c_analysisCat_diag")
        fit0 <- glm(as.formula(formula0), data = d_analysis, family=binomial)
        
        formula1 <- glue::glue("{x_outcome} ~ c_analysisYear_diag*c_analysisCat_diag")
        fit1 <- glm(as.formula(formula1), data = d_analysis, family=binomial)
        
        x_p_int <- lmtest::lrtest(fit0, fit1)$`Pr(>Chisq)`[2]
        res[i,`Annual_Increase_interaction_pval`:= formatC(x_p_int, digits=3, format='f')]
        if(x_p_int < 0.05) res[i,`Annual_Increase_interaction_pval`:= paste0(`Annual_Increase_interaction_pval`,"*")]
      }
    },TRUE)
  }
  openxlsx::write.xlsx(res, fs::path(org::project$results_today,folder,"by_sex_age_year.xlsx"))
  
}