# ============================================================
# UAE HEALTH INSURANCE PRICING MODEL — MAIN SCRIPT
# Run this file to execute the complete model in order
# ============================================================

source("Package setup.R")
source("Governance and Assumptions.R")
source("Generate 2025 base portfolio.R")
source("Member expansion - 2025 portfolio.R")
source("Diagnosis and Prior claim.R")
source("Frequency effect's and claim count.R")
source("Severity effect and gross claim amount.R")
source("Benefit Design and Reinsurance.R")
source("Data Quality + conversion to Health model.R")
source("Generating historical Cohorts 2022 - 2024.R")
source("Train & Validate & Price split.R")
source("Frequency GLM.R")
source("Severity GLM.R")
source("Apply pricing to 2025 portfolio.R")
source("Portfolio and Segment profitability.R")
source("Credibility renewal pricing.R")
source("Anti Selection and Diagnosis mix.R")

cat("\nModel complete — outputs saved to", output_folder, "\n")