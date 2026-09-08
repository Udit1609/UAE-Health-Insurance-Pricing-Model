health_data_2025 <- health_data_2025 %>%
  mutate(
    age_effect_freq = case_when(age < 18 ~ 0.05, age < 30 ~ -0.05, age < 45 ~ 0.10, age <= 60 ~ 0.25, TRUE ~ 0.25),
    condition_effect_freq = case_when(condition_group == "None" ~ 0.00, condition_group == "Minor" ~ 0.15, condition_group == "Moderate" ~ 0.45, condition_group == "Severe" ~ 0.75, condition_group == "Multiple" ~ 1.00, TRUE ~ 0.00),
    diagnosis_effect_freq = case_when(
      diagnosis_group == "Wellness" ~ -0.30,
      diagnosis_group == "Minor Acute" ~ 0.10,
      diagnosis_group == "OPD Routine" ~ 0.20,
      diagnosis_group == "Diagnostics" ~ 0.30,
      diagnosis_group == "Chronic OPD" ~ 0.55,
      diagnosis_group == "OPD Major" ~ 0.35,
      diagnosis_group == "Maternity" ~ 0.25,
      diagnosis_group == "Mental Health" ~ 0.30,
      diagnosis_group == "IP Medical" ~ -0.40,
      diagnosis_group == "IP Surgical" ~ -0.55,
      diagnosis_group == "High Cost" ~ -0.65,
      TRUE ~ 0.00
    ),
    plan_effect_freq = case_when(plan_type == "EBP" ~ 0.00, plan_type == "Standard" ~ 0.08, plan_type == "Premium" ~ 0.17, TRUE ~ 0.00),
    network_effect_freq = case_when(network == "DHA_Network_A" ~ 0.15, network == "DHA_Network_B" ~ 0.08, TRUE~ 0),
    prior_effect_freq = case_when(prior_claims_band == "0" ~ 0.00, prior_claims_band == "1-2" ~ 0.30, prior_claims_band == "3+" ~ 0.60, TRUE ~ 0.00),
    group_effect_freq = case_when(group_type == "Individual" ~ 0.00, group_type == "Family" ~ 0.05, group_type == "SME" ~ 0.10, group_type == "Corporate" ~ 0.15, TRUE ~ 0.00),
    region_effect_freq = case_when(
      region == "Dubai" ~ 0.15,
      region == "Abu Dhabi" ~ 0.10,
      region == "Sharjah" ~ 0.00,
      region == "North Emirates" ~ -0.05,
      TRUE ~ 0
    ),
    nationality_effect_freq = case_when(
      nationality_group == "UAE_National"  ~  0.223,
      nationality_group == "Arab_Expat"    ~  0.140,
      nationality_group == "South_Asian"   ~ -0.105,
      nationality_group == "Western_Expat" ~  0.182,
      nationality_group == "Other_Asian"   ~  0.000,
      TRUE                                 ~  0.000
    ),
    claim_lambda = exposure * exp(
      -2.20 + age_effect_freq + condition_effect_freq + diagnosis_effect_freq + #expected claim frequency
        plan_effect_freq + network_effect_freq + prior_effect_freq + group_effect_freq + policy_risk_factor 
      + region_effect_freq +  nationality_effect_freq
    ),
    claim_count = rpois(n(), lambda = claim_lambda) #to get the actual claim count for my synthetic data
  )

health_data_2025
