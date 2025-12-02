library(dplyr)
library(lsr)
library(stringr)
library(tidyverse)



setwd("/Users/harleyungar/Desktop/INFO_610_Stats/Kidney Data")
data <- read.csv("Donating_Cleaned.csv")
data <- data %>% mutate (Willing = as.numeric(Willing)) # Change "Willing" from integer to numeric

#Vector of belief variable names
belief_vars <- c(
  "Belief_body",
  "Belief_Minorities",
  "Belief_Costs",
  "Belief_Need",
  "Belief_Doctors",
  "Belief_Undeserving",
  "Belief_Fair"
)

# Create a factor version of Willing for nicer tables and summaries:
# This will make "Willing" numeric (0/1) for models & tests
# Makes "Willing-f" a factor labeled "Willing" and "Unwilling" for charts
data <- data %>% 
  mutate(
    Willing_f = factor(Willing,
                       levels = c(0, 1),
                       labels = c("Not willing", "Willing"))
  )

# Make sure belief responses are numeric:
data <- data %>%
  mutate(across(all_of(belief_vars), ~ as.numeric(.)))

#Get rid of all the responses that are NOT 1, 2, 3, or 4 (e.g. 5 or 6).
data <- data %>%
  mutate(across(all_of(belief_vars), ~ as.numeric(.))) %>%
  mutate(across(
    all_of(belief_vars),
    ~ ifelse(. %in% 5:6, NA_real_, .)
  ))

# Create a table of descriptives
belief_descriptives <- data %>%
  select(Willing_f, all_of(belief_vars)) %>%
  tidyr::pivot_longer(
    cols = all_of(belief_vars),
    names_to = "Belief",
    values_to = "Score"
  ) %>%
  group_by(Belief, Willing_f) %>%
  summarise(
    n      = n(),
    mean   = mean(Score, na.rm = TRUE),
    sd     = sd(Score, na.rm = TRUE),
    median = median(Score, na.rm = TRUE),
    .groups = "drop"
  )

belief_descriptives

# Run a Mann-Whitney Test.
mw_results <- map_dfr(
  belief_vars,
  ~ {
    formula <- as.formula(paste(.x, "~ Willing"))
    test    <- wilcox.test(formula, data = data, exact = FALSE)
    broom::tidy(test) %>%
      mutate(Belief = .x)
  }
)

mw_results

# Create a consolidated table
belief_desc_wide <- belief_descriptives %>%
  select(Belief, Willing_f, mean, median) %>%
  tidyr::pivot_wider(
    names_from = Willing_f,
    values_from = c(mean, median),
    names_glue = "{Willing_f}_{.value}"
  )

belief_summary <- belief_desc_wide %>%
  left_join(mw_results, by = "Belief")

belief_summary

model_beliefs <- glm(
  Willing ~ Belief_body + Belief_Minorities + Belief_Costs +
    Belief_Need + Belief_Doctors + Belief_Undeserving + Belief_Fair,
  data   = data,
  family = binomial
)

summary(model_beliefs)

OR_table <- broom::tidy(
  model_beliefs,
  exponentiate = TRUE,  # exp(coef) = odds ratios
  conf.int    = TRUE    # 95% CI
)

OR_table

# ---- Plot: Mean belief scores for Willing vs Unwilling ----

# Use the means from belief_descriptives
belief_means <- belief_descriptives %>%
  select(Belief, Willing_f, mean) %>%
  # Order the beliefs so they appear in the same vertical order as your example
  mutate(
    Belief = factor(
      Belief,
      levels = c(
        "Belief_Undeserving",
        "Belief_Need",
        "Belief_Minorities",
        "Belief_Fair",
        "Belief_Doctors",
        "Belief_Costs",
        "Belief_body"
      )
    )
  )

ggplot(belief_means, aes(x = mean, y = Belief, color = Willing_f)) +
  # grey line connecting the two points (dumbbell)
  geom_line(aes(group = Belief), linewidth = 0.5, color = "grey80") +
  geom_point(size = 3) +
  scale_x_continuous(
    limits = c(1, 4),
    breaks = 1:4
  ) +
  labs(
    title = "Mean Belief Scores for Willing vs Unwilling Donors",
    x = "Average Response (1 = strongly agree, 4 = strongly disagree)",
    y = "Belief Statement",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank()
  )


