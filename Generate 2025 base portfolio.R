# ============================================================
# 2. GENERATE 2025 BASE PORTFOLIO
# ============================================================
policy_data_2025 <- tibble(
  policy_id  = 1:n_policies,
  group_type = sample(c("Individual","Family","SME","Corporate"), n_policies,
                      replace=TRUE, prob=c(0.25,0.45,0.20,0.10)),
  plan_type  = sample(c("EBP","Standard","Premium"), n_policies,
                      replace=TRUE, prob=c(0.45,0.35,0.20)),
  region     = sample(c("Dubai","Abu Dhabi","Sharjah","North Emirates"), n_policies,
                      replace=TRUE, prob=c(0.42,0.24,0.16,0.18)),
  network    = sample(c("DHA_Network_A","DHA_Network_B","DHA_Network_C"), n_policies,
                      replace=TRUE, prob=c(0.20,0.42,0.38))
) %>%
  mutate(
    family_size = case_when(
      group_type=="Individual" ~ 1L,
      group_type=="Family"     ~ sample(2:5,n(),replace=TRUE,prob=c(0.35,0.30,0.22,0.13)),
      group_type=="SME"        ~ sample(5:25,n(),replace=TRUE),
      group_type=="Corporate"  ~ sample(25:100,n(),replace=TRUE),
      TRUE ~ 1L
    ),
    ebp_flag           = plan_type=="EBP",
    policy_risk_factor = rnorm(n(),mean=0,sd=0.30),
    accident_year      = 2025L
  )
  
policy_data_2025  

cat("Section 2 complete : 2025 base portfolio |", n_policies, "policies\n")
