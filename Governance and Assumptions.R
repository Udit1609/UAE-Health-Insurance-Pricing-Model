# ============================================================
# 1. GOVERNANCE AND ASSUMPTIONS
# ============================================================
n_policies     <- 25000
current_year   <- 2025
valuation_date <- as.Date("2025-12-31")

#Portfolio was smaller in earlier years
historical_volume_factors <- c("2022"=0.70,"2023"=0.82,"2024"=0.91,"2025"=1.00)
year_claim_volatility     <- 0.05

medical_trend              <- 0.113   # WTW 2026 Middle East Survey
admin_expense_loading      <- 0.10
commission_loading         <- 0.02
risk_margin                <- 0.03
profit_loading             <- 0.05
target_loss_ratio          <- 0.85
# Pricing load multiplier used for BOTH current and future premium
pricing_load_multiplier    <- 1 + admin_expense_loading + commission_loading +
  risk_margin + profit_loading


ebp_minimum_premium_2025 <- 620    # approx DHA 2025
ebp_minimum_premium_2026 <- 635    # approx DHA 2026 (Jan increase)
federal_basic_premium_2025 <- 310  # UAE federal 2025
federal_basic_premium_2026 <- 320  # UAE federal 2026 (Jan increase)

#Csection
csection_rate_base       <- 0.38
normal_delivery_cost_aed <- 12000
csection_cost_aed        <- 18000

#Floor rate
rate_change_floor    <- -0.10
rate_change_ceiling  <-  0.35
full_credibility_exposure <- 100

#Reinsurance design
reinsurance_retention    <- 35000
reinsurance_ceded_share  <- 0.90



