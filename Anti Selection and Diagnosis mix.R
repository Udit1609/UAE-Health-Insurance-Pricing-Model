# ============================================================
# 16. ANTI-SELECTION AND DIAGNOSIS MIX
# ============================================================

anti_selection_review <- health_model %>%
  group_by(plan_type, network, condition_group, diagnosis_group) %>%
  summarise(
    members               = n(),
    exposure              = sum(exposure),
    retained_claim_cost   = sum(retained_claim_cost),
    current_gross_premium = sum(current_gross_premium),
    loss_ratio            = retained_claim_cost / current_gross_premium,
    .groups = "drop"
  ) %>%
  arrange(desc(loss_ratio))

anti_selection_review

diagnosis_mix_review <- health_model %>%
  group_by(diagnosis_group, benefit_type) %>%
  summarise(
    members               = n(),
    claims                = sum(claim_count),
    retained_claim_cost   = sum(retained_claim_cost),
    current_gross_premium = sum(current_gross_premium),
    loss_ratio            = retained_claim_cost / current_gross_premium,
    claim_share           = retained_claim_cost / sum(health_model$retained_claim_cost),
    .groups = "drop"
  ) %>%
  arrange(desc(claim_share))

diagnosis_mix_review

write_csv(anti_selection_review, file.path(output_folder, "23_anti_selection_review.csv"))
write_csv(diagnosis_mix_review,  file.path(output_folder, "24_diagnosis_mix_review.csv"))

cat("Section 16 complete: Anti-selection and diagnosis mix\n")
cat("  Anti-selection segments:", nrow(anti_selection_review), "\n")
cat("  Diagnosis groups:       ", nrow(diagnosis_mix_review), "\n")
