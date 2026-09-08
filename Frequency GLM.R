# ============================================================
# 11. FREQUENCY GLM
# ============================================================
frequency_glm <- glm(
  claim_count ~ age_band*sex + relation + plan_type + region + network +
    group_type + nationality_group + condition_group + diagnosis_group +
    prior_claims_band + offset(log(exposure)),
  family=poisson(link="log"), data=train_data
)

frequency_glm

dispersion_ratio <- sum(residuals(frequency_glm,type="pearson")^2)/frequency_glm$df.residual

dispersion_ratio
cat("Dispersion ratio:",round(dispersion_ratio,3),"\n")
if (dispersion_ratio > 2) {
  freq_nb <- glm.nb(
    claim_count ~ age_band*sex + relation + plan_type + region + network +
      group_type + nationality_group + condition_group + diagnosis_group + prior_claims_band + 
      offset(log(exposure)),
    data=train_data)
  if (AIC(freq_nb) < AIC(frequency_glm)) { frequency_glm <- freq_nb; cat("Switched to NegBin\n") }
} else cat("Poisson retained\n")

train_data    <- train_data    %>% mutate(pred_freq=predict(frequency_glm,newdata=.,type="response"))
validate_data <- validate_data %>% mutate(pred_freq=predict(frequency_glm,newdata=.,type="response"))
price_data    <- price_data    %>% mutate(pred_freq=predict(frequency_glm,newdata=.,type="response"))

train_data

frequency_validation <- tibble(
  dataset  = c("Train 2022-23","Validate 2024","Applied 2025"),
  rmse     = c(sqrt(mean((train_data$claim_count    - train_data$pred_freq)^2,    na.rm=TRUE)),
               sqrt(mean((validate_data$claim_count - validate_data$pred_freq)^2, na.rm=TRUE)),
               sqrt(mean((price_data$claim_count    - price_data$pred_freq)^2,    na.rm=TRUE))),
  mae      = c(mean(abs(train_data$claim_count    - train_data$pred_freq),    na.rm=TRUE),
               mean(abs(validate_data$claim_count - validate_data$pred_freq), na.rm=TRUE),
               mean(abs(price_data$claim_count    - price_data$pred_freq),    na.rm=TRUE)),
  ae_ratio = c(sum(train_data$claim_count)/sum(train_data$pred_freq),
               sum(validate_data$claim_count)/sum(validate_data$pred_freq),
               sum(price_data$claim_count)/sum(price_data$pred_freq))
)

frequency_validation

frequency_validation <- frequency_validation %>%
  mutate(
    ae_interpretation = case_when(
      ae_ratio < 0.95 ~ "Under-predicting — model overstates claims",
      ae_ratio < 1.05 ~ "Good — model close to actual",
      ae_ratio < 1.20 ~ "Moderate — review for trend or mix shift",
      TRUE            ~ "Elevated — significant deviation from actual"
    )
  )

frequency_validation


write_csv(frequency_validation,   file.path(output_folder,"13_frequency_validation.csv"))
cat("Section 11 complete: Frequency GLM | 2024 A/E:",round(frequency_validation$ae_ratio[2],4),"\n")


