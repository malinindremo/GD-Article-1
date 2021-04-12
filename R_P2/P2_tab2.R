P2_tab2_analysis_fn <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p$get_data()
    argset <- p$get_argset(index_analysis = 27)
    argset
  }
  
  #return(1)
  
  dx_including_cases <- data$data[
    label_analysis %in% c(
      argset$label_analysis,
      "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]: >=2006"
    ) &
    label_sex == argset$label_sex & 
    label_age == argset$label_age
  ]
  setnames(dx_including_cases, argset$co, "outcome")
  
  dx_including_cases[,interaction_yr_gd := (as.numeric(as.factor(label_analysis))-1)*c_analysisYear_diag]
  xtabs(~dx_including_cases$interaction_yr_gd)
  xtabs(~dx_including_cases$c_analysisYear_diag, addNA=T)
  
  dx <- dx_including_cases[label_analysis == argset$label_analysis]

  
  extras <- "+region_3"
  if(argset$label_age == "Total") extras <- paste0(extras, "+c_analysisAgeCat_diag")

  # formulas
  irr_formula <- glue::glue("outcome ~ c_analysisYear_diag {extras}")
  
  int_yr_gd_formula0 <- glue::glue("outcome ~ label_analysis + c_analysisYear_diag {extras}")
  int_yr_gd_formula1 <- glue::glue("outcome ~ interaction_yr_gd + label_analysis + c_analysisYear_diag {extras}")
  
  int_yr_gd_age_formula0 <- glue::glue("outcome ~ label_analysis*c_analysisAge_diag + label_analysis*c_analysisYear_diag + c_analysisAge_diag*c_analysisYear_diag {extras}")
  int_yr_gd_age_formula1 <- glue::glue("outcome ~ label_analysis*c_analysisAge_diag*c_analysisYear_diag {extras}")
  
  # irr start
  irr_num <- sum(dx$outcome)
  irr_denom <- nrow(dx)
  
  if(sum(dx$outcome)<20){
    irr_res <- "-"
    irr_pval <- "-"
    int_yr_gd_pval <- "-"
    int_yr_gd_age_pval <- "-"
  } else {
    set.seed(4)
    fit <- tryCatch({
        logbin::logbin(as.formula(irr_formula), data = dx, method = "glm2")
      },
      error = function(e){
        logbin::logbin(as.formula(irr_formula), data = dx, method = "cem")
      }
    )
    x_est <- coef(summary(fit))[2,"Estimate"]
    x_se <- coef(summary(fit))[2,"Std. Error"]
    x_p <- coef(summary(fit))[2,"Pr(>|z|)"]
    
    irr_res <- glue::glue(
      "{est} ({l95}, {u95})",
      est = formatC(exp(x_est), digits = 2, format="f"),
      l95 = formatC(exp(x_est) - 1.96*x_se, digits = 2, format="f"),
      u95 = formatC(exp(x_est) + 1.96*x_se, digits = 2, format="f")
    )
    irr_res <- as.character(irr_res)
    irr_pval <- round(x_p, digits=4)
    
    # interaction year*gd
    if(argset$label_analysis=="numF64_089>=4, first diag: [2001-01-01, 2015-12-31]: >=2006"){
      int_yr_gd_pval <- "-"
    } else {
      int_yr_gd_pval <- try({
        fit0 <- logbin::logbin(as.formula(int_yr_gd_formula0), data = dx_including_cases, method = "glm2")
        fit1 <- logbin::logbin(as.formula(int_yr_gd_formula1), data = dx_including_cases, method = "glm2")
        
        round(lmtest::lrtest(fit0, fit1)$`Pr(>Chisq)`[2], digits=4)
      },TRUE)
      if(!is.numeric(int_yr_gd_pval)) int_yr_gd_pval <- "failed"
    }
    
    # interaction year*gd*age
    if(argset$label_analysis=="numF64_089>=4, first diag: [2001-01-01, 2015-12-31]: >=2006" | argset$label_age!="Total"){
      int_yr_gd_age_pval <- "-"
    } else {
      int_yr_gd_age_pval <- try({
        fit0 <- logbin::logbin(as.formula(int_yr_gd_age_formula0), data = dx_including_cases, method = "glm2")
        fit1 <- logbin::logbin(as.formula(int_yr_gd_age_formula1), data = dx_including_cases, method = "glm2")
        
        summary(fit0)
        summary(fit1)
        round(lmtest::lrtest(fit0, fit1)$`Pr(>Chisq)`[2], digits=4)
      },TRUE)
      if(!is.numeric(int_yr_gd_age_pval)) int_yr_gd_age_pval <- "failed"
    }
  }
  
  retval <- data.frame(
    icd10 = argset$co,
    asex = argset$label_sex,
    age = argset$label_age,
    case = argset$label_analysis,
    irr_num = irr_num,
    irr_denom = irr_denom,
    irr_res = irr_res,
    irr_pval = irr_pval,
    int_yr_gd_pval = int_yr_gd_pval,
    int_yr_gd_age_pval = int_yr_gd_age_pval,
    stringsAsFactors = FALSE
  )
  
  return(retval)
}

P2_tab2 <- function(d){
  dz <- d[
    excluded=="No" &
    c_analysisCat_diag %in% c(
      "numF64_089>=4, first diag: [2001-01-01, 2015-12-31]",
      "control_assigned",
      "control_opposite"
    ) & 
    c_analysisYear_diag >= 2006
  ]
  dz[,N:=1]
  
  co <- stringr::str_subset(names(dz),"^comorbid")
  
  dz[, label_sex := c_analysisSex_diag]
  dz[, label_age := c_analysisAgeCat_diag]
  dz[, label_analysis := paste0(c_analysisCat_diag,": >=2006")]
  
  # duplicate
  dz_1 <- copy(dz)
  dz_1[, label_age := "Total"]
  dz <- rbind(dz, dz_1)

  # end duplicate
  
  # plan start
  analyses <- expand.grid(
    label_analysis = unique(dz$label_analysis),
    label_sex = unique(dz$label_sex),
    label_age = unique(dz$label_age),
    co = co,
    stringsAsFactors = FALSE
  )
  
  p <- plnr::Plan$new()
  #p$add_data(name="x", direct=3)
  p$add_data(name = "data", direct = dz)
  
  p$add_argset_from_df(analyses)
  p$apply_analysis_fn_to_all(fn = P2_tab2_analysis_fn)
  
  x_seq_along <- p$x_seq_along()
  res <- vector("list", length=length(x_seq_along))
  for(i in x_seq_along){
    print(i)
    res[[i]] <- p$run_one(i)
  }
  res
  # plan end
  
  res <- rbindlist(res)
  res
  res[,
      age := factor(
        age,
        levels = c(
          "Total",
          "10-17",
          "18-30",
          "31-50",
          "51+"
        )
      )
  ]
  
  setorder(
    res,
    icd10,
    asex,
    age,
    -case
  )
  res
  
  openxlsx::write.xlsx(res, fs::path(org::project$results_today,"tab2.xlsx"))
  
}