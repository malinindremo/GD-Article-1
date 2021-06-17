natasa <- function(d){
  dn <- d[,
          c(
            "LopNr",
            "dob",
            
            "isBornMale",
            "bornSex",
            "c_analysisSex_diag",
            
            "dateFirst_F64_089",
            "numF64_089",
            "numF64_0",
            "numF64_89",
            
            "c_dateFirstHormone",
            "c_dateFirstHormoneFTM",
            "c_dateFirstHormoneMTF",
            "c_dateFirstHormonePubBlock",
            
            "c_dateFirst_SurgicalMasectomy",
            "c_dateFirst_SurgicalBreastReconstAndOtherBreastOpsFTM",
            "c_dateFirst_SurgicalPenisTestProsth",
            "c_dateFirst_SurgicalInternalGenital",
            "c_dateFirst_SurgicalKolpectomy",
            "c_dateFirst_SurgicalBreastReconstAndOtherBreastOpsMTF",
            "c_dateFirst_SurgicalReconstVag",
            "c_dateFirst_SurgicalPenisAmp",
            "c_dateFirst_SurgicalLarynx",
            
            "c_dateFirst_surgery_hormones",
            
            "excluded", 
            
            "lopnr_analysis_group",
            
            "c_analysisCat_treatments",
            "c_analysisCat_hybrid",
            "c_analysisCat_oneplusdiag",
            "c_analysisCat_diag",
            
            "c_analysisDate_diag",
            "c_analysisAge_diag",
            "c_analysisAgeCat_diag",
            "c_analysisYear_diag",
            "c_analysisYearCat_diag",
            
            "comorbid_F70_to_F79",
            "comorbid_F80_R47",
            "comorbid_F20_to_F29",
            "comorbid_F30_to_F31",
            "comorbid_F32_to_F33",
            "comorbid_F50",
            "comorbid_F84",
            "comorbid_F90",
            "comorbid_F84_F90",
            "comorbid_F91_to_F98",
            "comorbid_F40_to_F48",
            "comorbid_F10_to_F16_F18_F19",
            "comorbid_F60",
            "comorbid_X60_to_X84"
          )
        ]
  
  # descriptions
  
  labs <- data.table(var=names(dn))
  
  labs[var=="dob", description:="Date of birth"]
  
  xtabs(~dn$isBornMale, addNA=T)
  labs[var=="isBornMale", description:="(Independent of time/diag): 0/FALSE = Female; 1/TRUE = Male"]
  labs[var=="bornSex", description:="(Independent of time/diag): Assigned sex at birth"]
  labs[var=="c_analysisSex_diag", description:="Only applicable for 'diag' when comparing 'diag' with 'controls_assigned' and 'controls_opposite': Assigned sex at birth for cases and controls_assigned, switched sex for controls_opposite"]
  
  unique(dn$dateFirst_F64_089)[1:10]
  labs[var=="dateFirst_F64_089", description:="(Independent of time/diag): The first observed date for a F64_089 diagnosis"]
  labs[var=="numF64_089", description:="(Independent of time/diag): The number of F64_089 diagnoses for any time period"]
  labs[var=="numF64_0", description:="(Independent of time/diag): The number of F64_0 diagnoses for any time period"]
  labs[var=="numF64_89", description:="(Independent of time/diag): The number of F64_89 diagnoses for any time period"]
  
  labs[var=="c_dateFirstHormone", description:="(Independent of time/diag): The first observed date of any hormone (cleaning applied for appropriate sex/age)"]
  labs[var=="c_dateFirstHormoneFTM", description:="(Independent of time/diag): The first observed date of M->F hormones (cleaning applied for appropriate sex)"]
  labs[var=="c_dateFirstHormoneMTF", description:="(Independent of time/diag): The first observed date of F->M hormones (cleaning applied for appropriate sex)"]
  labs[var=="c_dateFirstHormonePubBlock", description:="(Independent of time/diag): The first observed date of puberty blocker hormones (cleaning applied for appropriate age)"]
  
  labs[var=="c_dateFirst_Surgical*", description:="(Independent of time/diag): First observed surgical date of a [surgery]"]
  
  labs[var=="c_dateFirst_surgery_hormones", description:="(Independent of time/diag): First observed date of hormones or surgery"]
  
  xtabs(~dn$excluded, addNA=T)
  labs[var=="excluded", description:="IMPORTANT! When analysing 'c_analysis*' or 'comorbid*' this variable should be set to 'No'"]
  
  labs[var=="lopnr_analysis_group", description:="Only applicable for 'diag'. This is a group identifier identifying which controls are matched to which cases"]
  
  xtabs(~dn$c_analysisCat_treatments, addNA=T)
  labs[var=="c_analysisCat_treatments", description:="cases for 'treatments' (numF64_089>=1 & hormones/surgery, first diag: [2001-01-01, 2015-12-31])"]
  xtabs(~dn$c_analysisCat_diag, addNA=T)
  labs[var=="c_analysisCat_diag", description:="case/control for 'diag' (numF64_089>=4, first diag: [2001-01-01, 2015-12-31])"]
  xtabs(~dn$c_analysisCat_oneplusdiag, addNA=T)
  labs[var=="c_analysisCat_oneplusdiag", description:="cases for 'oneplusdiag' (numF64_089>=1, first diag: [2001-01-01, 2015-12-31])"]
  xtabs(~dn$c_analysisCat_hybrid, addNA=T)
  labs[var=="c_analysisCat_hybrid", description:="cases for 'hybrid' = 'treatments' (numF64_089>=1 & hormones/surgery, first diag: [2001-01-01, 2015-12-31]) or 'diag' (numF64_089>=4, first diag: [2001-01-01, 2015-12-31])"]
  sum(!is.na(dn$c_analysisCat_hybrid))
  
  labs[var=="c_analysisDate_diag", description:="Diag only (cases and matched controls): Date of first F64_089 diagnosis"]
  labs[var=="c_analysisAge_diag", description:="Diag only (cases and matched controls): Age at of first F64_089 diagnosis"]
  labs[var=="c_analysisAgeCat_diag", description:="Diag only (cases and matched controls): Age at of first F64_089 diagnosis"]
  labs[var=="c_analysisYear_diag", description:="Diag only (cases and matched controls): Year of first F64_089 diagnosis"]
  labs[var=="c_analysisYearCat_diag", description:="Diag only (cases and matched controls): Year of first F64_089 diagnosis"]
  
  xtabs(~dn$comorbid_F70_to_F79, addNA=T)
  labs[var=="comorbid_F70_to_F79", description:="Diag only (cases and matched controls): comorbid 'Intellectual disability/Mental retardation'"]
  labs[var=="comorbid_F80_R47", description:="Diag only (cases and matched controls): comorbid 'Speech and language disorders'"]
  labs[var=="comorbid_F20_to_F29", description:="Diag only (cases and matched controls): comorbid 'Psychotic disorders'"]
  labs[var=="comorbid_F30_to_F31", description:="Diag only (cases and matched controls): comorbid 'bipolar disorder'"]
  labs[var=="comorbid_F32_to_F33", description:="Diag only (cases and matched controls): comorbid 'depression'"]
  labs[var=="comorbid_F50", description:="Diag only (cases and matched controls): comorbid 'eating disorders'"]
  labs[var=="comorbid_F84", description:="Diag only (cases and matched controls): comorbid 'asd'"]
  labs[var=="comorbid_F90", description:="Diag only (cases and matched controls): comorbid 'adhd'"]
  labs[var=="comorbid_F84_F90", description:="Diag only (cases and matched controls): comorbid 'asd OR adhd'"]
  labs[var=="comorbid_F91_to_F98", description:="Diag only (cases and matched controls): comorbid 'Other behavioral/emotional disorders with onset in childhood'"]
  labs[var=="comorbid_F40_to_F48", description:="Diag only (cases and matched controls): comorbid 'Neurotic, stress related or somatoform disorder'"]
  labs[var=="comorbid_F10_to_F16_F18_F19", description:="Diag only (cases and matched controls): comorbid 'Alcohol and substance use disorder'"]
  labs[var=="comorbid_F60", description:="Diag only (cases and matched controls): comorbid 'Personality disorder'"]
  labs[var=="comorbid_X60_to_X84", description:="Diag only (cases and matched controls): comorbid 'Suicide attempt'"]
  
  labs[is.na(description)]
  
  sum(is.na(dn[c_analysisCat_hybrid=="control_assigned"]$dateFirst_F64_089))
  
  important_info <- glue::glue(
    "UPDATED 2021-05-28\r\n",
    "These definitions are used when analysing people:\r\n",
    "- 'diag' (variable=c_analysisCat_diag)\r\n",
    "- c_analysisCat_diag=='numF64_089>=4, first diag: [2001-01-01, 2015-12-31]' means 'this person has gender dysmorphia'\r\n",
    "- c_analysisCat_diag=='control_assigned' means 'this person is a control for a person with gender dysmorphia, matched on the person's assigned gender at birth\r\n",
    "- c_analysisCat_diag=='control_opposite' means 'this person is a control for a person with gender dysmorphia, matched on the person's OPPOSITE assigned gender at birth\r\n",
    "- c_analysisCat_diag==MISSING means 'this person does not have gender dysphoria and was not matched as a control\r\n\r\n",
    
    "VERY IMPORTANT: *All* analyses must use the variables to subset:\r\n",
    "- excluded is a variable that determines if a person should be excluded from the analysis (only analyse people where this equals 'No'!!!!!!)\r\n",
    "- c_analysisCat_diag (choose the people who are relevant)\r\n\r\n",
    
    "Matched analyses:\r\n",
    "- lopnr_analysis_group identifies the groups for matching cases to controls (e.g. for lopnr_analysis_group==3 there should be 1 case and approx 10 matched controls)\r\n",
    "- use 'c_analysisSex_diag' as the sex variable\r\n",
    
    "\n\n\nUPDATED 2020-02-01\r\n",
    "There are definitions of people who are useful to know/understand, but not analysed:\r\n",
    "- 'oneplusdiag' (variable=c_analysisCat_oneplusdiag; numF64_089>=1, first diag: [2001-01-01, 2015-12-31])\r\n",
    "- 'treatments' (variable=c_analysisCat_treatments; numF64_089>=1 & hormones/surgery, first diag: [2006-01-01, 2015-12-31])\r\n",
    "- 'diag' (variable=c_analysisCat_diag; numF64_089>=4, first diag: [2001-01-01, 2015-12-31])\r\n\r\n",
    
    "These definitions are used when analysing people:\r\n",
    "- 'hybrid' (variable=c_analysisCat_hybrid; either 'treatments' OR 'diag')\r\n",
    "- c_analysisCat_hybrid=='Hybrid' means 'this person has gender dysmorphia'\r\n",
    "- c_analysisCat_hybrid=='control_assigned' means 'this person is a control for a person with gender dysmorphia, matched on the person's assigned gender at birth\r\n",
    "- c_analysisCat_hybrid=='control_opposite' means 'this person is a control for a person with gender dysmorphia, matched on the person's OPPOSITE assigned gender at birth\r\n",
    "- c_analysisCat_hybrid==MISSING means 'this person does not have gender dysphoria and was not matched as a control\r\n\r\n",
    
    "VERY IMPORTANT: *All* analyses must use the variables to subset:\r\n",
    "- excluded is a variable that determines if a person should be excluded from the analysis (only analyse people where this equals 'No'!!!!!!)\r\n",
    "- c_analysisCat_hybrid (choose the people who are relevant)\r\n\r\n",
    
    "Matched analyses:\r\n",
    "- lopnr_analysis_group identifies the groups for matching cases to controls (e.g. for lopnr_analysis_group==3 there should be 1 case and approx 10 matched controls)\r\n",
    "- use 'c_analysisSex_hybrid' as the sex variable\r\n"
  )
  
  writeLines(important_info, fs::path(org::project$data_raw, "P1", "natasa", "important_info.txt"))
  writexl::write_xlsx(labs, fs::path(org::project$data_raw, "P1","natasa", "data_descriptions.xlsx"))
  haven::write_sav(dn, fs::path(org::project$data_raw, "P1","natasa", "data.sav"))
  
}
