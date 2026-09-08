# ============================================================
# 7. BENEFIT DESIGN AND REINSURANCE
# ============================================================
health_data_2025 <- health_data_2025 %>%
  mutate(
    deductible  = case_when(plan_type=="EBP"~500,plan_type=="Standard"~250,plan_type=="Premium"~100,TRUE~500),
    coinsurance = case_when(plan_type=="EBP"~0.20,plan_type=="Standard"~0.10,plan_type=="Premium"~0.05,TRUE~0.20),
    claim_cap   = case_when(benefit_type=="Inpatient"~40000,benefit_type=="Outpatient"~12000,
                            benefit_type=="Wellness"~3000,benefit_type=="Maternity"~20000,
                            benefit_type=="Mental Health"~8000,TRUE~10000),
    avg_claim_cost_net_pre_cap = if_else(claim_count>0,
                                         pmax(avg_claim_cost_gross-deductible,0)*(1-coinsurance),NA_real_),
    avg_claim_cost_net   = if_else(claim_count>0,pmin(avg_claim_cost_net_pre_cap,claim_cap),NA_real_),
    total_claim_cost_net = if_else(claim_count>0,claim_count*avg_claim_cost_net,0),
    ceded_reinsurance    = pmax(total_claim_cost_net-reinsurance_retention,0)*reinsurance_ceded_share,
    retained_claim_cost  = total_claim_cost_net - ceded_reinsurance,
    premium_floor_current = case_when(
      region %in% c("Dubai","Abu Dhabi") & plan_type=="EBP" ~ ebp_minimum_premium_2025*exposure,
      region %in% c("Sharjah","North Emirates")             ~ federal_basic_premium_2025*exposure,
      TRUE ~ 0
    ),  premium_floor_future = case_when(
      region %in% c("Dubai","Abu Dhabi") & plan_type == "EBP" ~
        ebp_minimum_premium_2026 * exposure,
      region %in% c("Sharjah","North Emirates") ~
        federal_basic_premium_2026 * exposure,
      TRUE ~ 0
    )
  )

sev_cap_2025     <- quantile(health_data_2025$avg_claim_cost_net, 0.995, na.rm=TRUE)
health_data_2025 <- health_data_2025 %>%
  mutate(avg_claim_cost_net_wins = if_else(claim_count>0,
                                           pmin(avg_claim_cost_net,sev_cap_2025),NA_real_))

health_data_2025
cat("Section 7 complete: Benefit design and reinsurance\n")
