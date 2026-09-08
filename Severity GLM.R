# ============================================================
# 12. SEVERITY GLM
# ============================================================
severity_glm <- glm(
  avg_claim_cost_net_wins ~ age_band*sex + relation + plan_type + region + network +
    group_type + nationality_group + condition_group + diagnosis_group +
    prior_claims_band,
  family=Gamma(link="log"), data=sev_train
)

severity_glm

# Gamma dispersion check
# Expected value is 1 — same logic as Poisson dispersion check
gamma_dispersion <- sum(residuals(severity_glm, type="pearson")^2) /
  severity_glm$df.residual

cat("Gamma dispersion:", round(gamma_dispersion, 3), "\n")

if (gamma_dispersion > 2) {
  cat("Warning: Gamma dispersion elevated — check for influential claims\n")
} else {
  cat("Gamma dispersion acceptable\n")
}

sev_train    <- sev_train    %>% mutate(pred_sev=predict(severity_glm,newdata=.,type="response"))
sev_validate <- sev_validate %>% mutate(pred_sev=predict(severity_glm,newdata=.,type="response"))
sev_price <- sev_price %>% mutate(pred_sev=predict(severity_glm,newdata=.,type="response"))

severity_validation <- tibble(
  dataset  = c("Train 2022-23","Validate 2024", "Applied 2025"),
  rmse     = c(sqrt(mean((sev_train$avg_claim_cost_net_wins    - sev_train$pred_sev)^2,    na.rm=TRUE)),
               sqrt(mean((sev_validate$avg_claim_cost_net_wins - sev_validate$pred_sev)^2, na.rm=TRUE)),
               sqrt(mean((sev_price$avg_claim_cost_net_wins    - sev_price$pred_sev)^2,    na.rm=TRUE))),
  mae      = c(
    mean(abs(sev_train$avg_claim_cost_net_wins    - sev_train$pred_sev),    na.rm=TRUE),
    mean(abs(sev_validate$avg_claim_cost_net_wins - sev_validate$pred_sev), na.rm=TRUE),
    mean(abs(sev_price$avg_claim_cost_net_wins    - sev_price$pred_sev),    na.rm=TRUE)
  ),
  ae_ratio = c(sum(sev_train$avg_claim_cost_net_wins)/sum(sev_train$pred_sev),
               sum(sev_validate$avg_claim_cost_net_wins)/sum(sev_validate$pred_sev),
               sum(sev_price$avg_claim_cost_net_wins)    / sum(sev_price$pred_sev))
)

severity_validation

severity_validation <- severity_validation %>%
  mutate(
    ae_interpretation = case_when(
      ae_ratio < 1.05 ~ "Good — model close to actual",
      ae_ratio < 1.20 ~ "Moderate — medical trend effect expected",
      TRUE            ~ "Elevated — medical trend driving gap from training years"
    )
  )

severity_diagnosis_check <- sev_validate %>%
  group_by(diagnosis_group) %>%
  summarise(
    actual_severity    = mean(avg_claim_cost_net_wins, na.rm=TRUE),
    predicted_severity = mean(pred_sev, na.rm=TRUE),
    ae_ratio           = actual_severity / predicted_severity
  ) %>%
  arrange(desc(actual_severity))

severity_validation
severity_diagnosis_check

write_csv(severity_validation,   file.path(output_folder,"15_severity_validation.csv"))
write_csv(severity_diagnosis_check, file.path(output_folder, "16b_severity_diagnosis_check.csv"))
cat("Section 12 complete: Severity GLM\n")

