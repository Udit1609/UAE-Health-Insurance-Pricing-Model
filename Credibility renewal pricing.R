# ============================================================
# 15. CREDIBILITY RENEWAL PRICING
# ============================================================
# Uses 2024 cohort as prior year experience
# Rate change = future required / current paid - 1
# lr_interpretation added
# renewal_ebp_compliant check against 2026 floors

# Prior year 2024 experience at policy level
prior_year_exp <- cohort_2024 %>%
  group_by(policy_id) %>%
  summarise(
    prior_retained = sum(retained_claim_cost, na.rm=TRUE),
    prior_exposure = sum(exposure),
    .groups = "drop"
  )

prior_year_exp

# Current year 2025 at policy level
policy_profitability <- health_model %>%
  group_by(policy_id, group_type, plan_type, region, network) %>%
  summarise(
    members               = n(),
    exposure              = sum(exposure),
    claims                = sum(claim_count),
    retained_claim_cost   = sum(retained_claim_cost),
    model_pure_premium    = sum(technical_pure_premium),
    current_gross_premium = sum(current_gross_premium),
    future_gross_premium  = sum(future_gross_premium),
    policy_loss_ratio     = retained_claim_cost / current_gross_premium,
    .groups = "drop"
  ) %>%
  left_join(prior_year_exp, by="policy_id") %>%
  mutate(
    prior_retained   = replace_na(prior_retained, 0),
    prior_exposure   = replace_na(prior_exposure, 0),
    combined_claims  = retained_claim_cost + prior_retained,
    combined_exposure = exposure + prior_exposure
  )

policy_profitability

credibility_renewal_pricing <- policy_profitability %>%
  mutate(
    # Credibility weight from combined 2024+2025 exposure
    credibility_weight     = pmin(sqrt(combined_exposure / full_credibility_exposure), 1),
    
    # Experience and model cost per exposure
    exp_pp_per_exp         = if_else(combined_exposure > 0,
                                     combined_claims / combined_exposure, NA_real_),
    mod_pp_per_exp         = if_else(exposure > 0,
                                     model_pure_premium / exposure, NA_real_),
    
    # Credibility blended cost — weighted average of experience and model
    cred_blended           = credibility_weight * exp_pp_per_exp +
      (1 - credibility_weight) * mod_pp_per_exp,
    
    # Trend forward to next year
    cred_trended           = cred_blended * (1 + medical_trend),
    
    # Indicated renewal premium per exposure
    indicated_prem_per_exp = cred_trended / target_loss_ratio,
    
    # Current premium per exposure
    current_prem_per_exp   = current_gross_premium / exposure,
    
    # Rate change = future required vs current paid
    indicated_rate_change  = indicated_prem_per_exp / current_prem_per_exp - 1,
    
    # Cap rate change using named constants
    capped_rate_change     = pmin(pmax(indicated_rate_change,
                                       rate_change_floor),
                                  rate_change_ceiling),
    rate_cap_applied       = capped_rate_change != indicated_rate_change,
    
    # Renewal action
    renewal_action = case_when(
      policy_loss_ratio >= 1.10 ~ "Significant increase / underwriting review",
      policy_loss_ratio >= 1.00 ~ "Moderate increase",
      policy_loss_ratio >= 0.90 ~ "Small increase or maintain",
      TRUE                      ~ "Maintain / competitive pricing"
    ),
    
    # Loss ratio interpretation
    lr_interpretation = case_when(
      policy_loss_ratio >= 1.10 ~ "Loss making — significant increase needed",
      policy_loss_ratio >= 1.00 ~ "Above target — moderate increase needed",
      policy_loss_ratio >= 0.90 ~ "Moderate — review at renewal",
      policy_loss_ratio >= 0.80 ~ "At or below target — maintain",
      TRUE                      ~ "Well below target — competitive opportunity"
    ),
    
    # EBP compliance check on renewal premium vs 2026 floor
    renewal_ebp_compliant = case_when(
      region %in% c("Dubai","Abu Dhabi") & plan_type == "EBP" ~
        (current_gross_premium * (1 + capped_rate_change)) >=
        ebp_minimum_premium_2026 * exposure,
      region %in% c("Sharjah","North Emirates") ~
        (current_gross_premium * (1 + capped_rate_change)) >=
        federal_basic_premium_2026 * exposure,
      TRUE ~ TRUE
    )
  )

credibility_renewal_pricing

write_csv(policy_profitability,
          file.path(output_folder, "21_policy_profitability.csv"))
write_csv(credibility_renewal_pricing,
          file.path(output_folder, "22_credibility_renewal_pricing.csv"))

cat("Section 15 complete: Credibility renewal pricing\n")
cat("  Policies priced:     ", scales::comma(nrow(credibility_renewal_pricing)), "\n")
cat("  Rate cap applied:    ", sum(credibility_renewal_pricing$rate_cap_applied), "\n")
cat("  Renewal non-compliant:", sum(!credibility_renewal_pricing$renewal_ebp_compliant), "\n")

# GRAPH 03: Renewal rate change distribution
# Plot
p_renewal <- ggplot(credibility_renewal_pricing,
                    aes(x=capped_rate_change*100, fill=renewal_action)) +
  geom_histogram(binwidth=2, colour="white") +
  geom_vline(xintercept=0, linetype="dashed", linewidth=1) +
  scale_fill_manual(values=c(
    "Significant increase / underwriting review" = "#993C1D",
    "Moderate increase"                          = "#BA7517",
    "Small increase or maintain"                 = "#185FA5",
    "Maintain / competitive pricing"             = "#1D9E75"
  )) +
  labs(title="Renewal rate change distribution",
       x="Rate change (%)", y="Policies", fill="Action") +
  theme_minimal() +
  theme(legend.position="bottom")

# Save
ggsave(file.path(output_folder, "graph_03_renewal_distribution.png"),
       p_renewal, width=11, height=6, dpi=150)

#Credibility curve 
# Plot
p_cred <- ggplot(credibility_renewal_pricing,
                 aes(x=combined_exposure, y=credibility_weight, colour=group_type)) +
  geom_point(alpha=0.3, size=1) +
  stat_function(fun=function(x) pmin(sqrt(x/full_credibility_exposure), 1),
                colour="black", linewidth=1.2) +
  geom_vline(xintercept=full_credibility_exposure,
             linetype="dashed", colour="grey40") +
  scale_colour_brewer(palette="Set1") +
  labs(title="Credibility Z by combined exposure (2024+2025)",
       subtitle="Black curve = theoretical formula",
       x="Combined member years", y="Credibility Z",
       colour="Group type") +
  theme_minimal()

# Save
ggsave(file.path(output_folder, "graph_04_credibility_curve.png"),
       p_cred, width=10, height=6, dpi=150)

