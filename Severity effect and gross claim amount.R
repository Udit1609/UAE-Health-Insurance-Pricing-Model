#Giving severity affect to all the variable

health_data_2025 <- health_data_2025 %>%
  mutate(
    age_effect_sev = case_when(age < 18 ~ -0.10, age < 30 ~ 0.00, age < 45 ~ 0.12, age <= 60 ~ 0.30, TRUE ~ 0.30),
    condition_effect_sev = case_when(condition_group == "None" ~ 0.00, condition_group == "Minor" ~ 0.10, condition_group == "Moderate" ~ 0.35, condition_group == "Severe" ~ 0.70, condition_group == "Multiple" ~ 1.00, TRUE ~ 0.00),
    diagnosis_effect_sev = case_when(
      diagnosis_group == "Wellness" ~ -0.50,
      diagnosis_group == "Minor Acute" ~ -0.20,
      diagnosis_group == "OPD Routine" ~ 0.00,
      diagnosis_group == "Diagnostics" ~ 0.25,
      diagnosis_group == "Chronic OPD" ~ 0.40,
      diagnosis_group == "OPD Major" ~ 0.55,
      diagnosis_group == "Maternity" ~ 1.00,
      diagnosis_group == "Mental Health" ~ 0.35,
      diagnosis_group == "IP Medical" ~ 1.10,
      diagnosis_group == "IP Surgical" ~ 1.35,
      diagnosis_group == "High Cost" ~ 1.80,
      TRUE ~ 0.00
    ),
    plan_effect_sev = case_when(plan_type == "EBP" ~ 0.00, plan_type == "Standard" ~ 0.15, plan_type == "Premium" ~ 0.35, TRUE ~ 0.00),
    network_effect_sev = case_when(network == "DHA_Network_A" ~ 0.25, network == "DHA_Network_B" ~ 0.12, TRUE ~ 0),
    prior_effect_sev = case_when(prior_claims_band == "0" ~ 0.00, prior_claims_band == "1-2" ~ 0.10, prior_claims_band == "3+" ~ 0.25, TRUE ~ 0.00),
    region_effect_sev = case_when(
      region == "Dubai" ~ 0.15,
      region == "Abu Dhabi" ~ 0.10,
      region == "Sharjah" ~ 0.00,
      region == "North Emirates" ~ -0.08,
      TRUE ~ 0
    ),nationality_effect_sev = case_when(
      nationality_group == "UAE_National"  ~  0.262,
      nationality_group == "Arab_Expat"    ~  0.095,
      nationality_group == "South_Asian"   ~ -0.163,
      nationality_group == "Western_Expat" ~  0.336,
      nationality_group == "Other_Asian"   ~  0.000,
      TRUE                                 ~  0.000
    ),
    gross_mean_severity = exp(
      7.02 + age_effect_sev + condition_effect_sev + diagnosis_effect_sev +
        plan_effect_sev + network_effect_sev + region_effect_sev + prior_effect_sev + 0.50 * policy_risk_factor + nationality_effect_sev
    ),
    avg_claim_cost_gross = if_else(
      claim_count > 0,
      rgamma(n(), shape = 3.2, scale = gross_mean_severity/3.2),
      NA_real_
    ),
    high_cost_shock = if_else(
      claim_count > 0 & runif(n()) < 0.015,
      rgamma(n(), shape = 2, scale = 25000),
      0
    ),
    csection_shock = if_else(claim_count>0 & diagnosis_group=="Maternity" & runif(n())<csection_rate_base            ,csection_cost_aed - normal_delivery_cost_aed, 0),
    avg_claim_cost_gross = if_else(claim_count > 0, avg_claim_cost_gross + high_cost_shock + csection_shock, NA_real_),
    total_claim_cost_gross = if_else(claim_count > 0, claim_count * avg_claim_cost_gross, 0)
  )

health_data_2025

cat("Section 4-6 complete: Diagnosis, frequency, severity simulated\n")
