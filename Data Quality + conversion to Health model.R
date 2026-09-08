# ============================================================
# 8. DATA QUALITY + PREPARE 2025 MODEL DATA
# ============================================================
health_model_2025 <- health_data_2025 %>%
  filter(!is.na(age),!is.na(exposure),exposure>0,exposure<=1,
         claim_count>=0,total_claim_cost_gross>=0) %>%
  mutate(
    age_band          = cut(age, breaks = c(0,18,26,35,45,55,60), right = FALSE, include.lowest = TRUE),
    sex               = relevel(factor(sex),              ref="Female"),
    relation          = relevel(factor(relation),         ref="Employee"),
    plan_type         = relevel(factor(plan_type),        ref="EBP"),
    region            = relevel(factor(region),           ref="Sharjah"),
    network           = relevel(factor(network),          ref="DHA_Network_C"),
    group_type        = relevel(factor(group_type),       ref="Individual"),
    nationality_group = relevel(factor(nationality_group),ref="Other_Asian"),
    condition_group   = relevel(factor(condition_group),  ref="None"),
    diagnosis_group   = relevel(factor(diagnosis_group),  ref="OPD Routine"),
    benefit_type      = relevel(factor(benefit_type),     ref="Outpatient"),
    prior_claims_band = relevel(factor(prior_claims_band),ref="0")
  )

health_model_2025

cat("Section 8 complete: 2025 model data |", nrow(health_model_2025), "members\n")
