# ============================================================
# 4-6. DIAGNOSIS, FREQUENCY AND SEVERITY — 2025
# ============================================================

health_data_2025 <- member_data_2025 %>%
  mutate(
    age_band_simple = case_when(
      age < 18 ~ "Child",
      age < 30 ~ "Young Adult",
      age < 45 ~ "Adult",
      age < 55 ~ "Older Adult",
      TRUE ~ "Senior"
    ),
    condition_group = case_when(
      age < 18 ~ sample(c("None", "Minor", "Moderate"), n(), replace = TRUE, prob = c(0.75, 0.20, 0.05)),
      age < 40 ~ sample(c("None", "Minor", "Moderate", "Severe"), n(), replace = TRUE, prob = c(0.62, 0.23, 0.12, 0.03)),
      age < 60 ~ sample(c("None", "Minor", "Moderate", "Severe", "Multiple"), n(), replace = TRUE, prob = c(0.45, 0.25, 0.18, 0.08, 0.04)),
      TRUE ~ sample(c("None", "Minor", "Moderate", "Severe", "Multiple"), n(), replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.15, 0.10))
    )
  )
health_data_2025

health_data_2025 <- health_data_2025 %>%
  mutate(
    chronic_flag = if_else(condition_group %in% c("Moderate", "Severe", "Multiple"), "Yes", "No"),
    diagnosis_group = case_when(
      condition_group == "None" ~ sample(c("Wellness", "Minor Acute", "OPD Routine"), n(), replace = TRUE, prob = c(0.35, 0.40, 0.25)), 
      condition_group == "Minor" ~ sample(c("OPD Routine", "Minor Acute", "Diagnostics", "Mental Health"), n(), replace = TRUE, prob = c(0.40, 0.35, 0.18, 0.07)),
      condition_group == "Moderate" & sex == "Female" & age >= 23 & age <= 40 ~  sample(c("Chronic OPD", "Diagnostics", "OPD Major", "Maternity"),n(),replace = TRUE,
                                                                                        prob = c(0.45, 0.25, 0.20, 0.10)),
      condition_group == "Moderate" ~ sample(c("Chronic OPD", "Diagnostics", "OPD Major"),n(),replace = TRUE,prob = c(0.50, 0.30, 0.20)),
      condition_group == "Severe" ~ sample(c("Chronic OPD", "IP Medical", "IP Surgical", "Mental Health"), n(), replace = TRUE, prob = c(0.35, 0.35, 0.20, 0.10)),
      TRUE ~ sample(c("Chronic OPD", "IP Medical", "IP Surgical", "High Cost"), n(), replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15))
    ),
    benefit_type = case_when(
      diagnosis_group %in% c("IP Medical", "IP Surgical", "High Cost") ~ "Inpatient",
      diagnosis_group == "Maternity" ~ "Maternity",
      diagnosis_group == "Mental Health" ~ "Mental Health",
      diagnosis_group == "Wellness" ~ "Wellness",
      TRUE ~ "Outpatient"
    ),
    prior_claims_band = case_when(
      condition_group %in% c("Severe", "Multiple") ~ sample(c("0", "1-2", "3+"), n(), replace = TRUE, prob = c(0.20, 0.35, 0.45)),
      condition_group == "Moderate" ~ sample(c("0", "1-2", "3+"), n(), replace = TRUE, prob = c(0.40, 0.40, 0.20)),
      TRUE ~ sample(c("0", "1-2", "3+"), n(), replace = TRUE, prob = c(0.65, 0.25, 0.10))
    ),
    maternity_risk_flag = sex == "Female" & age >= 23 & age <= 40
  )  

health_data_2025

