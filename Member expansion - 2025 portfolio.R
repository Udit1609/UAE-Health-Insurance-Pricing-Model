# ============================================================
# 3. MEMBER EXPANSION — 2025 PORTFOLIO
# ============================================================

nat_freq_rel <- c(UAE_National=1.25,Arab_Expat=1.15,South_Asian=0.90,
                  Western_Expat=1.20,Other_Asian=1.00)
nat_sev_rel  <- c(UAE_National=1.30,Arab_Expat=1.10,South_Asian=0.85,
                  Western_Expat=1.40,Other_Asian=1.00)

member_data_2025 <- policy_data_2025 %>%
  uncount(weights = family_size, .id = "member_number") %>%
  mutate(
    member_id = row_number(),
    relation = case_when(
      group_type == "Individual" ~ "Employee",
      member_number == 1 ~ "Employee",
      member_number == 2 & group_type == "Family" ~ "Spouse",
      group_type == "Family" ~ "Child",
      group_type %in% c("SME", "Corporate") ~ "Employee",
      TRUE ~ "Employee"
    ),
    age = case_when(
      relation == "Employee" ~ sample(25:60, n(), replace = TRUE),
      relation == "Spouse" ~ sample(22:58, n(), replace = TRUE),
      relation == "Child" ~ sample(1:24, n(), replace = TRUE),
      TRUE ~ sample(18:60, n(), replace = TRUE)
    ),
    employee_sex = sample(c("Female", "Male"), n(), replace = TRUE, prob = c(0.48, 0.52)),
    member_months = sample(1:12, n(), replace = TRUE),
    exposure = member_months / 12
  )%>%
  group_by(policy_id) %>%
  mutate(
    sex = case_when(
      relation == "Employee" ~ employee_sex,
      relation == "Spouse" & employee_sex == "Male" ~ "Female",
      relation == "Spouse" & employee_sex == "Female" ~ "Male",
      TRUE ~ employee_sex
    )
  ) %>% 
  ungroup() %>%
  select(-employee_sex)

member_data_2025 <-member_data_2025 %>%
  mutate(
    #give everyone their own random nationality first
    own_nationality = sample(
      c("UAE_National","Arab_Expat","South_Asian",
        "Western_Expat","Other_Asian"),
      n(), replace = TRUE,
      prob = c(0.11, 0.16, 0.42, 0.12, 0.19)
    )
  ) %>%
  group_by(policy_id) %>%
  mutate(
    # find the male parent nationality in this policy
    # male parent = male employee OR male spouse, whichever exists
    male_parent_nat = case_when(
      any(relation == "Employee" & sex == "Male") ~
        first(own_nationality[relation == "Employee" & sex == "Male"]),
      any(relation == "Spouse"   & sex == "Male") ~
        first(own_nationality[relation == "Spouse"   & sex == "Male"]),
      TRUE ~
        first(own_nationality[relation == "Employee"])  # fallback if no male
    ),
    # assigning nationality by relation
    nationality_group = case_when(
      relation == "Child" ~ male_parent_nat,   # child follows father
      TRUE                ~ own_nationality     # everyone else keeps their own
    )
  ) %>%
  ungroup() %>%
  select(-own_nationality, -male_parent_nat)  # cleaning up

member_data_2025

cat("Section 3 complete : Members expanded | Min Spouse age:",
    min(member_data_2025$age[member_data_2025$relation == "Spouse"], na.rm = TRUE), "\n")

