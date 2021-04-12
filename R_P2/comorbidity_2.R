comorbidity_2 <- function(dz,folder="comorbidity_2"){
  co <- stringr::str_subset(names(dz),"^comorbid")
  
  dz[, label_sex := c_analysisSex_diag]
  dz[, label_age := c_analysisAgeCat_diag]
  dz[, label_analysis := c_analysisCat_diag]
  # duplicate
  dz_1 <- copy(dz)
  dz_1[, label_sex := "Total"]
  dz <- rbind(dz, dz_1)
  
  dz_1 <- copy(dz)
  dz_1[, label_age := "Total"]
  dz <- rbind(dz, dz_1)
  
  dz_1 <- copy(dz)
  dz_1[, c_analysisYearCat_diag := "Total"]
  dz <- rbind(dz, dz_1)
  
  # when analysing the sex groups together, make c_analysisCat_diag="control"
  dz[
    label_sex=="Total" & 
      label_analysis %in% c(
      "control_assigned",
      "control_opposite"
    ),
    label_analysis := "control"
  ]
  
  
  # by sex age
  res <- dz[
    , 
    lapply(.SD, sum),
    keyby = .(label_analysis, label_sex, label_age, c_analysisYearCat_diag), 
    .SDcols = c("N",co)
  ]
  
  res <- melt.data.table(
    res,
    id.vars = c(
      "label_analysis",
      "label_sex",
      "label_age",
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
  res[, label_sex := factor(
    label_sex,
    levels = c(
      "Total",
      "Assigned female",
      "Assigned male"
    )
  )]
  
  res[, label_age := factor(
    label_age,
    levels = c(
      "Total",
      "10-17",
      "18-30",
      "31-50",
      "51+"
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
    res[c_analysisYearCat_diag=="Total"],
    variable + label_sex + label_analysis ~ label_age,
    value.var = c("formatted")
  ))
  openxlsx::write.xlsx(tab1, fs::path(org::project$results_today,folder,"tab1.xlsx"))
  
  res <- dcast.data.table(
    res[c_analysisYearCat_diag!="Total"],
    variable + label_sex + label_age + label_analysis ~  c_analysisYearCat_diag,
    value.var = c("formatted")
  )
  res[, `Annual_Increase_RR`:=""]
  res[, `Annual_Increase_pval_bernoulli`:=""]
  res[, `Annual_Increase_pval`:=""]
  res[, `Annual_Increase_interaction_pval`:=""]
  for(i in 1:nrow(res)){
    cat(i, "/", nrow(res),"\n")
    x_outcome <- res$variable[i]
    x_c_analysisSex_diag <- res$label_sex[i]
    x_c_analysisAgeCat_diag <- res$label_age[i]
    x_c_analysisCat_diag <- res$label_analysis[i]
    
    extras <- "+region_3"
    if(x_c_analysisSex_diag == "Total") extras <- paste0(extras, "+c_analysisSex_diag")
    if(x_c_analysisAgeCat_diag == "Total") extras <- paste0(extras, "+c_analysisAgeCat_diag")
  
    d_analysis <- dz[
      label_sex == x_c_analysisSex_diag &
      label_age == x_c_analysisAgeCat_diag &
      label_analysis == x_c_analysisCat_diag & 
      c_analysisYearCat_diag == "Total"
    ]
    
    if(sum(d_analysis[[as.character(x_outcome)]])<5) next
    formula <- glue::glue("{x_outcome} ~ c_analysisYear_diag")
    fit <- logbin::logbin(as.formula(formula), data = d_analysis, method = "em")
    x_est <- coef(summary(fit))[2,"Estimate"]
    x_se <- coef(summary(fit))[2,"Std. Error"]
    x_p <- coef(summary(fit))[2,"Pr(>|z|)"]
    x_p_bernoulli <- x_p * nrow(res)
    if(x_p_bernoulli>1) x_p_bernoulli <- 1
    
    if(!is.nan(x_p)){
      res[i,`Annual_Increase_RR`:= glue::glue(
        "{b0} ({b1}, {b2})",
        b0 = formatC(exp(x_est), digits=2, format='f'),
        b1 = formatC(exp(x_est-1.96*x_se), digits=2, format='f'),
        b2 = formatC(exp(x_est+1.96*x_se), digits=2, format='f')
      )
    ]

      res[i,`Annual_Increase_pval_bernoulli`:= formatC(x_p_bernoulli, digits=3, format='f')]
      if(x_p_bernoulli < 0.05) res[i,`Annual_Increase_pval_bernoulli`:= paste0(`Annual_Increase_pval_bernoulli`,"*")]
      
      res[i,`Annual_Increase_pval`:= formatC(x_p, digits=3, format='f')]
      if(x_p < 0.05) res[i,`Annual_Increase_pval`:= paste0(`Annual_Increase_pval`,"*")]
    }
    
    try({
      # run interaction term
      if(x_c_analysisCat_diag %in% c("control_assigned","control_opposite","control")){
        d_analysis <- dz[
          label_sex == x_c_analysisSex_diag &
          label_age == x_c_analysisAgeCat_diag &
          label_analysis %in% c(
            as.character(x_c_analysisCat_diag), 
            "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]"
          ) & 
          c_analysisYearCat_diag == "Total"
          ]
        
        formula0 <- glue::glue("{x_outcome} ~ c_analysisYear_diag+label_analysis {extras}")
        #fit0 <- lme4::glmer(as.formula(formula0), data = d_analysis, family="binomial")
        fit0 <- glm(as.formula(formula0), data = d_analysis, family="binomial")
        
        formula1 <- glue::glue("{x_outcome} ~ c_analysisYear_diag*label_analysis {extras}")
        #fit1 <- lme4::glmer(as.formula(formula1), data = d_analysis, family="binomial")
        fit1 <- glm(as.formula(formula1), data = d_analysis, family="binomial")
        
        x_p_int <- lmtest::lrtest(fit0, fit1)$`Pr(>Chisq)`[2]
        res[i,`Annual_Increase_interaction_pval`:= formatC(x_p_int, digits=3, format='f')]
        if(x_p_int < 0.05) res[i,`Annual_Increase_interaction_pval`:= paste0(`Annual_Increase_interaction_pval`,"*")]
      }
    },TRUE)
  }
  openxlsx::write.xlsx(res, fs::path(org::project$results_today,folder,"tab2.xlsx"))
  
}