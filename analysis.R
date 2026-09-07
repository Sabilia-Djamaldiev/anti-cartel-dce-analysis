# ==========================================================
# MASTER'S DISSERTATION – M1 BDEEM
# Discrete Choice Experiment Analysis
# Author: Sabilia Djamaldiev
# ==========================================================

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)

# Set the working directory to the folder containing this script,
# limesurvey.xlsx and prolific.csv before running the analysis.
# Example:
# setwd("C:/Users/YourName/Documents/Memoire")

dir.create("Tables", showWarnings = FALSE)
dir.create("Figures", showWarnings = FALSE)

# ----------------------------------------------------------
# Data import
# ----------------------------------------------------------

survey <- read_excel("limesurvey.xlsx")
prolific <- read_csv("prolific.csv")

dim(survey)
dim(prolific)

# ----------------------------------------------------------
# Experimental design
# ----------------------------------------------------------

design <- data.frame(
  task = rep(1:12, each = 2),
  alt = rep(c("A", "B"), times = 12),
  
  fine_high = c(
    0,1, 0,0, 1,0, 0,0, 1,0, 0,1,
    0,1, 1,0, 0,1, 1,0, 1,1, 1,1
  ),
  
  proof = c(
    0,1, 0,1, 0,0, 0,1, 1,0, 1,0,
    1,1, 0,1, 1,0, 0,1, 1,1, 1,1
  ),
  
  transfer = c(
    0,0, 1,0, 0,1, 0,0, 1,0, 0,1,
    0,0, 1,1, 0,0, 0,0, 0,1, 1,0
  ),
  
  fairfunds = c(
    0,0, 0,0, 0,0, 1,0, 0,1, 0,0,
    0,1, 0,0, 1,0, 1,1, 0,0, 0,1
  )
)

# ----------------------------------------------------------
# Data cleaning
# ----------------------------------------------------------

choice_cols <- names(survey)[13:24]

survey_clean <- survey %>%
  mutate(
    resp_id = `ID de la réponse`,
    prolific_id = .[[10]],
    consent = .[[9]],
    age = as.numeric(.[[35]]),
    time_vignette = as.numeric(.[[49]]),
    time_instructions = as.numeric(.[[51]]),
    time_reading = time_vignette + time_instructions
  ) %>%
  filter(
    consent == "Oui, j’accepte",
    if_all(all_of(choice_cols), ~ !is.na(.)),
    time_reading > 60,
    age >= 18,
    age <= 100
  )

n_initial <- nrow(survey)
n_final <- nrow(survey_clean)

n_initial
n_final
round(100 * n_final / n_initial, 1)

# ----------------------------------------------------------
# Data cleaning summary
# ----------------------------------------------------------

cleaning_steps <- data.frame(
  Step = c(
    "Raw LimeSurvey export",
    "Informed consent provided",
    "All twelve DCE choice tasks completed",
    "Reading time above 60 seconds",
    "Age between 18 and 100 years",
    "Final analytical sample"
  ),
  Respondents = c(
    nrow(survey),
    
    survey %>%
      mutate(consent = .[[9]]) %>%
      filter(consent == "Oui, j’accepte") %>%
      nrow(),
    
    survey %>%
      mutate(consent = .[[9]]) %>%
      filter(
        consent == "Oui, j’accepte",
        if_all(all_of(choice_cols), ~ !is.na(.))
      ) %>%
      nrow(),
    
    survey %>%
      mutate(
        consent = .[[9]],
        time_vignette = as.numeric(.[[49]]),
        time_instructions = as.numeric(.[[51]]),
        time_reading = time_vignette + time_instructions
      ) %>%
      filter(
        consent == "Oui, j’accepte",
        if_all(all_of(choice_cols), ~ !is.na(.)),
        time_reading > 60
      ) %>%
      nrow(),
    
    survey %>%
      mutate(
        consent = .[[9]],
        age = as.numeric(.[[35]]),
        time_vignette = as.numeric(.[[49]]),
        time_instructions = as.numeric(.[[51]]),
        time_reading = time_vignette + time_instructions
      ) %>%
      filter(
        consent == "Oui, j’accepte",
        if_all(all_of(choice_cols), ~ !is.na(.)),
        time_reading > 60,
        age >= 18,
        age <= 100
      ) %>%
      nrow(),
    
    nrow(survey_clean)
  )
)

write.csv2(cleaning_steps, "Tables/table_cleaning_steps.csv", row.names = FALSE)

# ----------------------------------------------------------
# Long format for conditional logit
# ----------------------------------------------------------

choices_long <- survey_clean %>%
  select(resp_id, prolific_id, all_of(choice_cols)) %>%
  pivot_longer(
    cols = all_of(choice_cols),
    names_to = "question",
    values_to = "chosen_alt"
  ) %>%
  group_by(resp_id) %>%
  mutate(task = row_number()) %>%
  ungroup()

dce_long <- choices_long %>%
  left_join(design, by = "task") %>%
  mutate(
    chosen = ifelse(chosen_alt == alt, 1, 0),
    choice_set = paste(resp_id, task, sep = "_"),
    proof_transfer = proof * transfer,
    proof_fairfunds = proof * fairfunds,
    fine_transfer = fine_high * transfer,
    fine_fairfunds = fine_high * fairfunds
  )

nrow(dce_long)
length(unique(dce_long$resp_id))
table(dce_long$chosen)

check_choices <- dce_long %>%
  group_by(choice_set) %>%
  summarise(number_chosen = sum(chosen), .groups = "drop")

table(check_choices$number_chosen)

# ----------------------------------------------------------
# Conditional logit models
# ----------------------------------------------------------

model_basic <- clogit(
  chosen ~ fine_high + proof + transfer + fairfunds +
    strata(choice_set) + cluster(resp_id),
  data = dce_long,
  method = "efron"
)

model_full <- clogit(
  chosen ~ fine_high + proof + transfer + fairfunds +
    proof_transfer + proof_fairfunds +
    fine_transfer + fine_fairfunds +
    strata(choice_set) + cluster(resp_id),
  data = dce_long,
  method = "efron"
)

summary(model_basic)
summary(model_full)

# ----------------------------------------------------------
# Conditional logit results table
# ----------------------------------------------------------

coef_raw <- summary(model_full)$coefficients
conf_raw <- summary(model_full)$conf.int

results_table <- data.frame(
  Variable = rownames(coef_raw),
  Coefficient = round(coef_raw[, "coef"], 3),
  Robust_SE = round(coef_raw[, "robust se"], 3),
  Z = round(coef_raw[, "z"], 3),
  P_value = signif(coef_raw[, "Pr(>|z|)"], 3),
  Odds_Ratio = round(conf_raw[, "exp(coef)"], 3),
  CI_95 = paste0(
    "[", round(conf_raw[, "lower .95"], 3),
    " ; ", round(conf_raw[, "upper .95"], 3), "]"
  )
)

results_table$Variable_label <- c(
  "High administrative fine",
  "Evidence transmission",
  "Transfer to remaining cartel members",
  "Fair Funds",
  "Evidence transmission × Transfer",
  "Evidence transmission × Fair Funds",
  "High administrative fine × Transfer",
  "High administrative fine × Fair Funds"
)

results_table <- results_table %>%
  select(Variable_label, Coefficient, Robust_SE, Z, P_value, Odds_Ratio, CI_95)

write.csv2(results_table, "Tables/table_conditional_logit.csv", row.names = FALSE)

# ----------------------------------------------------------
# Sample description
# ----------------------------------------------------------

age_table <- data.frame(
  Variable = "Age",
  Mean = round(mean(survey_clean$age, na.rm = TRUE), 2),
  Median = median(survey_clean$age, na.rm = TRUE),
  Standard_deviation = round(sd(survey_clean$age, na.rm = TRUE), 2),
  Minimum = min(survey_clean$age, na.rm = TRUE),
  Maximum = max(survey_clean$age, na.rm = TRUE)
)

write.csv2(age_table, "Tables/table_age.csv", row.names = FALSE)

freq_table <- function(x, variable_name) {
  data.frame(x = x) %>%
    filter(!is.na(x)) %>%
    count(x) %>%
    mutate(
      Percentage = round(100 * n / sum(n), 1),
      Variable = variable_name
    ) %>%
    rename(Category = x, Frequency = n) %>%
    select(Variable, Category, Frequency, Percentage)
}

gender_table <- freq_table(survey_clean[[36]], "Gender")
education_table <- freq_table(survey_clean[[37]], "Education level")
status_table <- freq_table(survey_clean[[38]], "Main activity status")
field_table <- freq_table(survey_clean[[39]], "Field of study or activity")

sample_table <- bind_rows(
  gender_table,
  education_table,
  status_table,
  field_table
)

write.csv2(sample_table, "Tables/table_sample.csv", row.names = FALSE)

# ----------------------------------------------------------
# Knowledge questions
# ----------------------------------------------------------

knowledge_table <- bind_rows(
  freq_table(survey_clean[[33]], "Cartel agreements are legal"),
  freq_table(survey_clean[[34]], "Leniency may reduce administrative fines")
)

write.csv2(knowledge_table, "Tables/table_knowledge.csv", row.names = FALSE)

# ----------------------------------------------------------
# Respondent attitudes
# ----------------------------------------------------------

attitude_questions <- names(survey_clean)[25:32]

attitudes_summary <- data.frame(
  Variable = attitude_questions,
  Mean = sapply(
    survey_clean[attitude_questions],
    function(x) mean(as.numeric(x), na.rm = TRUE)
  ),
  Median = sapply(
    survey_clean[attitude_questions],
    function(x) median(as.numeric(x), na.rm = TRUE)
  ),
  Standard_deviation = sapply(
    survey_clean[attitude_questions],
    function(x) sd(as.numeric(x), na.rm = TRUE)
  ),
  N = sapply(
    survey_clean[attitude_questions],
    function(x) sum(!is.na(x))
  )
)

attitudes_summary$Statement <- c(
  "Leniency protection is too generous",
  "Companies should pay for wrongdoing",
  "Favourable treatment if cooperation improves enforcement",
  "Importance of fair procedures",
  "Support for severe sanctions",
  "Consistency of sanctions",
  "Clear and fair rules",
  "Leniency exemption is acceptable if it improves enforcement"
)

attitudes_summary <- attitudes_summary %>%
  mutate(
    Mean = round(Mean, 2),
    Standard_deviation = round(Standard_deviation, 2)
  ) %>%
  select(Statement, Mean, Median, Standard_deviation, N)

write.csv2(attitudes_summary, "Tables/table_attitudes.csv", row.names = FALSE)

# ----------------------------------------------------------
# Model comparison
# ----------------------------------------------------------

model_comparison <- data.frame(
  Model = c(
    "Main effects",
    "Main effects and interactions"
  ),
  Log_likelihood = c(
    round(as.numeric(logLik(model_basic)), 3),
    round(as.numeric(logLik(model_full)), 3)
  ),
  AIC = c(
    round(AIC(model_basic), 3),
    round(AIC(model_full), 3)
  )
)

write.csv2(model_comparison, "Tables/table_model_comparison.csv", row.names = FALSE)

# ----------------------------------------------------------
# Experimental design balance
# ----------------------------------------------------------

design_balance <- design %>%
  summarise(
    High_fine = sum(fine_high),
    Evidence_transmission = sum(proof),
    Transfer = sum(transfer),
    FairFunds = sum(fairfunds),
    Total_alternatives = n()
  )

write.csv2(design_balance, "Tables/table_design_balance.csv", row.names = FALSE)

# ----------------------------------------------------------
# Choice distribution
# ----------------------------------------------------------

choice_distribution <- data.frame(
  Alternative = names(table(dce_long$chosen_alt)),
  Frequency = as.numeric(table(dce_long$chosen_alt)),
  Percentage = round(100 * as.numeric(prop.table(table(dce_long$chosen_alt))), 1)
)

write.csv2(choice_distribution, "Tables/table_choice_distribution.csv", row.names = FALSE)

# ----------------------------------------------------------
# Correlation between estimated policy variables
# ----------------------------------------------------------

design_vars <- dce_long %>%
  select(
    fine_high,
    proof,
    transfer,
    fairfunds,
    proof_transfer,
    proof_fairfunds,
    fine_transfer,
    fine_fairfunds
  )

round(cor(design_vars), 2)

# ----------------------------------------------------------
# Figure: education level
# ----------------------------------------------------------

education_plot_data <- education_table %>%
  arrange(desc(Percentage))

education_plot <- ggplot(
  education_plot_data,
  aes(x = reorder(Category, Percentage), y = Percentage)
) +
  geom_col() +
  coord_flip() +
  labs(
    x = "",
    y = "Percentage of respondents",
    title = "Distribution of respondents by education level"
  ) +
  theme_minimal()

ggsave(
  "Figures/figure_education_level.png",
  plot = education_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ----------------------------------------------------------
# Figure: mean respondent attitudes
# ----------------------------------------------------------

figure_attitudes <- attitudes_summary %>%
  arrange(Mean)

attitude_plot <- ggplot(
  figure_attitudes,
  aes(x = Mean, y = reorder(Statement, Mean))
) +
  geom_col() +
  labs(
    title = "Mean respondent attitudes",
    x = "Mean score on a 1–7 scale",
    y = NULL
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "Figures/figure_attitudes.png",
  plot = attitude_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ----------------------------------------------------------
# Figure: odds ratios
# ----------------------------------------------------------

forest_data <- results_table %>%
  mutate(
    lower = as.numeric(gsub("\\[(.*) ; (.*)\\]", "\\1", CI_95)),
    upper = as.numeric(gsub("\\[(.*) ; (.*)\\]", "\\2", CI_95))
  )

forest_plot <- ggplot(
  forest_data,
  aes(x = Odds_Ratio, y = reorder(Variable_label, Odds_Ratio))
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2
  ) +
  geom_point(size = 3) +
  labs(
    x = "Odds Ratio (95% CI)",
    y = "",
    title = "Estimated effects of policy variables on choice probability"
  ) +
  theme_minimal()

ggsave(
  "Figures/figure_odds_ratios.png",
  plot = forest_plot,
  width = 8,
  height = 5,
  dpi = 300
)
