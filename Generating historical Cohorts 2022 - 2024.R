# ============================================================
# 9. GENERATE HISTORICAL COHORTS (2022-2024)
# ============================================================
# Each historical year = subsample of 2025 base
# with deflated claim costs (claims were lower in earlier years due to trend)

generate_cohort <- function(base_data, yr, vol_factor, trend_rate) {
  yrs_before <- 2025 - yr
  trend_defl <- (1/(1+trend_rate))^yrs_before
  set.seed(12345 + yr)
  base_data %>%
    slice_sample(prop= vol_factor) %>%
    mutate(
      accident_year          = as.integer(yr),
      total_claim_cost_gross = total_claim_cost_gross * trend_defl * (1+rnorm(n(),0,year_claim_volatility)),
      total_claim_cost_net   = total_claim_cost_net   * trend_defl * (1+rnorm(n(),0,year_claim_volatility)),
      retained_claim_cost    = pmax(retained_claim_cost * trend_defl * (1+rnorm(n(),0,year_claim_volatility)),0),
      avg_claim_cost_gross   = if_else(!is.na(avg_claim_cost_gross), avg_claim_cost_gross*trend_defl, NA_real_),
      avg_claim_cost_net     = if_else(!is.na(avg_claim_cost_net),   avg_claim_cost_net*trend_defl,   NA_real_),
      avg_claim_cost_net_wins = if_else(!is.na(avg_claim_cost_net_wins), avg_claim_cost_net_wins*trend_defl, NA_real_)
    )
}

cohort_2022 <- generate_cohort(health_model_2025, 2022, historical_volume_factors["2022"], medical_trend)
cohort_2023 <- generate_cohort(health_model_2025, 2023, historical_volume_factors["2023"], medical_trend)
cohort_2024 <- generate_cohort(health_model_2025, 2024, historical_volume_factors["2024"], medical_trend)

health_model_2025 <- health_model_2025 %>% mutate(accident_year = 2025L)

all_years <- bind_rows(cohort_2022, cohort_2023, cohort_2024, health_model_2025)

all_years

annual_claim_totals <- all_years %>%
  group_by(accident_year) %>%
  summarise(total_retained_claims=sum(retained_claim_cost,na.rm=TRUE),
            total_members=n(), total_exposure=sum(exposure), .groups="drop")

annual_claim_totals

write_csv(annual_claim_totals, file.path(output_folder,"09_annual_claim_totals.csv"))

cat("Section 9 complete: Historical cohorts\n")
cat("| 2022:",nrow(cohort_2022),
    "| 2023:",nrow(cohort_2023),"| 2024:",nrow(cohort_2024),
    "| 2025:",nrow(health_model_2025),"\n")

