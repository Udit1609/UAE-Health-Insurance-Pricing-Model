# ============================================================
# 14. PORTFOLIO AND SEGMENT PROFITABILITY
# ============================================================

portfolio_profitability <- health_model %>%
  summarise(
    members                     = n(),
    policies                    = n_distinct(policy_id),
    exposure                    = sum(exposure),
    claims                      = sum(claim_count),
    retained_claim_cost         = sum(retained_claim_cost),
    technical_pure_premium      = sum(technical_pure_premium),
    current_gross_premium       = sum(current_gross_premium),
    future_gross_premium        = sum(future_gross_premium),
    actual_loss_ratio           = retained_claim_cost / current_gross_premium,
    expected_loss_ratio         = technical_pure_premium / current_gross_premium,
    implied_renewal_rate_change = future_gross_premium / current_gross_premium - 1,
    underwriting_margin         = current_gross_premium - retained_claim_cost,
    underpriced_members         = sum(underpriced_flag,        na.rm=TRUE),
    # EBP compliance split into current and future year
    ebp_non_compliant_current   = sum(!ebp_compliant_current,  na.rm=TRUE),
    ebp_non_compliant_future    = sum(!ebp_compliant_future,   na.rm=TRUE)
  )

portfolio_profitability

segment_profitability <- health_model %>%
  group_by(plan_type, region, network, diagnosis_group) %>%
  summarise(
    members               = n(),
    exposure              = sum(exposure),
    claims                = sum(claim_count),
    retained_claim_cost   = sum(retained_claim_cost),
    current_gross_premium = sum(current_gross_premium),
    future_gross_premium  = sum(future_gross_premium),
    loss_ratio            = retained_claim_cost / current_gross_premium,
    avg_current_premium   = current_gross_premium / exposure,
    avg_future_premium    = future_gross_premium  / exposure,
    implied_rate_change   = future_gross_premium / current_gross_premium - 1,
    underpriced_members   = sum(underpriced_flag, na.rm=TRUE),
    # Underpriced as percentage alongside count
    pct_underpriced       = underpriced_members / n(),
    .groups = "drop"
  ) %>%
  # Only show segments with at least 50 members — below this loss ratios are
  # too volatile to be meaningful for pricing decisions
  filter(members >= 50) %>%
  mutate(
    loss_ratio_flag = case_when(
      loss_ratio >= 1.10 ~ "Loss making — immediate action needed",
      loss_ratio >= 1.00 ~ "Above target — significant increase needed",
      loss_ratio >= 0.90 ~ "Moderate — review at renewal",
      loss_ratio >= 0.80 ~ "At or below target — maintain",
      TRUE               ~ "Well below target — competitive opportunity"
    ),
    credibility_note = case_when(
      members >= 500  ~ "High credibility",
      members >= 200  ~ "Moderate credibility",
      members >= 50   ~ "Low credibility — treat with caution",
      TRUE            ~ "Insufficient — exclude from analysis"
    )
  ) %>%
  arrange(desc(loss_ratio))

segment_profitability

cat("Section 14 complete: Portfolio and segment profitability\n")
cat("  Members:                ", scales::comma(portfolio_profitability$members), "\n")
cat("  Actual loss ratio:      ", scales::percent(portfolio_profitability$actual_loss_ratio, 0.1), "\n")
cat("  Current premium:    AED ", scales::comma(round(portfolio_profitability$current_gross_premium, 0)), "\n")
cat("  Future premium:     AED ", scales::comma(round(portfolio_profitability$future_gross_premium, 0)), "\n")
cat("  Implied rate chg:      +", round(portfolio_profitability$implied_renewal_rate_change * 100, 1), "%\n")
cat("  Underpriced members:   ", portfolio_profitability$underpriced_members, "\n")
cat("  EBP non-compliant 2025:", portfolio_profitability$ebp_non_compliant_current, "\n")
cat("  EBP non-compliant 2026:", portfolio_profitability$ebp_non_compliant_future, "\n")

write_csv(portfolio_profitability, file.path(output_folder, "18_portfolio_profitability.csv"))
write_csv(segment_profitability,   file.path(output_folder, "19_segment_profitability.csv"))


# GRAPH 01: Loss ratio heatmap — current premium basis
lr_data <- health_model %>%
  group_by(plan_type, region) %>%
  summarise(
    loss_ratio = sum(retained_claim_cost) / sum(current_gross_premium),
    .groups = "drop"
  )

# Plot
p_lr_heatmap <- ggplot(lr_data, aes(x=region, y=plan_type, fill=loss_ratio)) +
  geom_tile(colour="white") +
  geom_text(aes(label=scales::percent(loss_ratio, 1)), 
            colour="white", fontface="bold", size=4.5) +
  scale_fill_gradient2(low="#1D9E75", mid="#F9CB42", high="#993C1D",
                       midpoint=0.85, labels=scales::percent) +
  labs(title="Loss ratio heatmap",
       x="Region", y="Plan type", fill="Loss ratio") +
  theme_minimal()
ggsave(file.path(output_folder,"graph_01_lr_heatmap.png"),p_lr_heatmap,width=9,height=5,dpi=150)

# Data
prem_data <- health_model %>%
  group_by(plan_type) %>%
  summarise(
    current = sum(current_gross_premium) / 1e6,
    future  = sum(future_gross_premium)  / 1e6,
    .groups = "drop"
  ) %>%
  pivot_longer(c(current, future), names_to="type", values_to="premium")

prem_data
# Plot
p_prem_compare <- ggplot(prem_data, aes(x=plan_type, y=premium, fill=type)) +
  geom_col(position="dodge", width=0.6) +
  scale_fill_manual(values=c(current="#185FA5", future="#993C1D"),
                    labels=c("Current 2025", "Future 2026")) +
  labs(title="Current vs future premium by plan type",
       x="Plan type", y="Total premium (AED millions)", fill="") +
  theme_minimal()

p_prem_compare

ggsave(file.path(output_folder, "graph_02_current_vs_future_premium.png"),
       p_prem_compare, width=9, height=6, dpi=150)



