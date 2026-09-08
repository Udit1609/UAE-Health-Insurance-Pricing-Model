# ============================================================
# 10. TRAIN / VALIDATE / PRICE SPLIT (TIME-BASED)
# ============================================================
# Train=2022-2023 | Validate=2024 | Price=2025
train_data    <- all_years %>% filter(accident_year <= 2023)
validate_data <- all_years %>% filter(accident_year == 2024)
price_data    <- all_years %>% filter(accident_year == 2025)

sev_train    <- train_data    %>% filter(claim_count>0,!is.na(avg_claim_cost_net_wins),avg_claim_cost_net_wins>0)
sev_validate <- validate_data %>% filter(claim_count>0,!is.na(avg_claim_cost_net_wins),avg_claim_cost_net_wins>0)
sev_price <- price_data %>% filter(claim_count>0,!is.na(avg_claim_cost_net_wins), avg_claim_cost_net_wins>0)
cat("Section 10 complete: Train",nrow(train_data),"| Validate",nrow(validate_data),"| Price",nrow(price_data),"\n")
