# ============================================================
# 13. APPLY PRICING TO 2025 PORTFOLIO
# ============================================================
# CURRENT and FUTURE gross premium clearly separated
#
# CURRENT gross premium = pure_premium x pricing_load
#   - No trend applied
#   - Reflects what the policyholder is paying THIS year (2025)
#   - Used for: actual loss ratio, expected loss ratio, profitability
#
# FUTURE gross premium = pure_premium x (1+trend) x pricing_load
#   - Trend applied to project next year cost
#   - Reflects what the policyholder SHOULD pay at 2026 renewal
#   - Used for: renewal indication, stress testing, IFRS17


health_model <- price_data %>%
  mutate(
    # GLM predictions
    pred_sev  = predict(severity_glm,  newdata=., type="response"),
    
    # Technical pure premium — this year's cost estimate, no trend
    technical_pure_premium = pred_freq * pred_sev,
    
    # CURRENT gross premium — what policyholder pays in 2025
    # No trend — uses 2025 regulatory floors
    current_gross_premium = pmax(
      technical_pure_premium * pricing_load_multiplier,
      premium_floor_current
    ),
    
    # FUTURE gross premium — what policyholder should pay at 2026 renewal
    # Trend applied — uses 2026 regulatory floors
    technical_pure_premium_trended = technical_pure_premium * (1 + medical_trend),
    future_gross_premium = pmax(
      technical_pure_premium_trended * pricing_load_multiplier,
      premium_floor_future
    ),
    
    # Loss ratios — always current premium basis
    actual_loss_ratio = if_else(
      current_gross_premium > 0,
      retained_claim_cost / current_gross_premium,
      NA_real_
    ),
    expected_loss_ratio = if_else(
      current_gross_premium > 0,
      technical_pure_premium / current_gross_premium,
      NA_real_
    ),
    
    # Required future premium — trended cost at target loss ratio
    required_future_premium = technical_pure_premium_trended / target_loss_ratio,
    
    # Pricing gap and underpriced flag — vs future premium
    pricing_gap      = required_future_premium - future_gross_premium,
    underpriced_flag = pricing_gap > 0,
    
    # EBP compliance — current year uses 2025 floors
    ebp_compliant_current = case_when(
      region %in% c("Dubai","Abu Dhabi") & plan_type == "EBP" ~
        current_gross_premium >= ebp_minimum_premium_2025 * exposure,
      region %in% c("Sharjah","North Emirates") ~
        current_gross_premium >= federal_basic_premium_2025 * exposure,
      TRUE ~ TRUE
    ),
    
    # EBP compliance — renewal year uses 2026 floors
    ebp_compliant_future = case_when(
      region %in% c("Dubai","Abu Dhabi") & plan_type == "EBP" ~
        future_gross_premium >= ebp_minimum_premium_2026 * exposure,
      region %in% c("Sharjah","North Emirates") ~
        future_gross_premium >= federal_basic_premium_2026 * exposure,
      TRUE ~ TRUE
    )
  )

health_model

cat("Section 13 complete: Pricing applied to 2025 portfolio\n")
cat("  Current premium:      AED",
    format(round(sum(health_model$current_gross_premium), 0), big.mark=","), "\n")
cat("  Future premium:       AED",
    format(round(sum(health_model$future_gross_premium), 0), big.mark=","), "\n")
cat("  Implied rate change: +",
    round((sum(health_model$future_gross_premium) /
             sum(health_model$current_gross_premium) - 1) * 100, 1), "%\n")
cat("  Underpriced members: ",
    sum(health_model$underpriced_flag, na.rm=TRUE), "\n")
cat("  EBP non-compliant current:",
    sum(!health_model$ebp_compliant_current, na.rm=TRUE), "\n")
cat("  EBP non-compliant future: ",
    sum(!health_model$ebp_compliant_future, na.rm=TRUE), "\n")

write_csv(health_model, file.path(output_folder, "17_member_level_priced_portfolio.csv"))

