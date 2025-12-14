library(dplyr)
library(stringr)
library(tidyverse)
library(ggeffects)
library(broom)    
install.packages("showtext")
library(showtext)
font_add_google("IBM Plex Sans", "plex")
showtext_auto()

setwd("/Users/harleyungar/Desktop/INFO_610_Stats/Kidney Data")
data <- read.csv("Donating_Cleaned.csv")

# CLEAN UP THE DATASET

## Drop off all the responses that are "refused", "don't know", etc. NOTE: I lose about 50% of data 
## becasue of limiting to these responses. That should not make a difference statistically.
data_clean <- data %>%
  mutate(Willing = as.numeric(Willing)) %>% # Make sure Willing is numeric
  filter(
    !is.na(Willing),
    RaceCat %in% 1:4,
    AgeCat %in% 1:4,
    EduCat %in% 1:4,
    Income %in% 1:9,
    Coverage %in% 1:2,
    Sex %in% 1:2,
    Urban %in% 0:1,
  )

## Convert demographic variables into categories (factors) with labels
data_clean <- data_clean %>%
  mutate(
    RaceCat = factor(RaceCat,
                     levels = c(1, 2, 3, 4),
                     labels = c("White", "Black", "Asian", "Native American")),
    AgeCat = factor(AgeCat,
                    levels = 1:4,
                    labels = c("18–34", "35–49", "50–64", "65+")),
    EduCat = factor(EduCat,
                    levels = 1:4,
                    labels = c("HS or less",
                               "Some college/tech/voc",
                               "College grad",
                               "Postgraduate")),
    Income = factor(Income,
                    levels = 1:9,
                    labels = c("<$20K", "$20–30K", "$30–40K", "$40–50K",
                               "$50–60K", "$60–75K", "$75–100K",
                               "$100–150K", "$150K+")),
    Coverage = factor(Coverage,
                      levels = c(1, 2),
                      labels = c("Has coverage", "No coverage")),
    Sex = factor(Sex,
                 levels = c(1, 2),
                 labels = c("Female", "Male")),
    Urban = factor(Urban,
                   levels = c(0, 1),
                   labels = c("Suburban/Rural", "Urban")),
  )

summary(data_clean$RaceCat) #Note: Now there are only 4,765 observvations in the full dataset now, not 10K. 

## DESCRIPTIVE CHECKS

prop.table(table(data_clean$RaceCat, data_clean$Willing), 1) #Race
prop.table(table(data_clean$AgeCat, data_clean$Willing), 1) # Age
prop.table(table(data_clean$EduCat, data_clean$Willing), 1) # Education
prop.table(table(data_clean$Income, data_clean$Willing), 1) #Income
prop.table(table(data_clean$Coverage, data_clean$Willing), 1) #Coverage
prop.table(table(data_clean$Sex, data_clean$Willing), 1)  #Sex
prop.table(table(data_clean$Urban, data_clean$Willing), 1) #Urban

## Bar charts of % for each category

ggplot(data_clean, aes(RaceCat, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Race",
    y = "Percent Willing",
    title = "Willingness to Donate by Race"
  )

ggplot(data_clean, aes(AgeCat, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Age",
    y = "Percent Willing",
    title = "Willingness to Donate by Age"
  )

ggplot(data_clean, aes(EduCat, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Education",
    y = "Percent Willing",
    title = "Willingness to Donate by Education"
  )

ggplot(data_clean, aes(Income, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Income",
    y = "Percent Willing",
    title = "Willingness to Donate by Income"
  )

ggplot(data_clean, aes(Coverage, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Insurance Coverage",
    y = "Percent Willing",
    title = "Willingness to Donate by Insurance Coverage"
  )

ggplot(data_clean, aes(Sex, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Sex",
    y = "Percent Willing",
    title = "Willingness to Donate by Sex"
  )

ggplot(data_clean, aes(Urban, Willing)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Sex",
    y = "Percent Willing",
    title = "Willingness to Donate by Sex"
  )


# ANALYSIS

##Run multivariate regression model
model_demo <- glm(
  Willing ~ RaceCat + AgeCat + EduCat + Income +
    Coverage + Sex + Urban,
  data = data_clean,
  family = binomial
)

summary(model_demo)

# Likelihood-ratio tests for variable importance
lr_table <- drop1(model_demo, test = "Chisq")
lr_table


#Odds Ratios

or_table <- tidy(model_demo, conf.int = TRUE, exponentiate = TRUE)
View(or_table)

or_ranked <- or_table %>%
  filter(term != "(Intercept)") %>%
  mutate(
    dist_from_1 = abs(estimate - 1),
    direction = ifelse(estimate > 1, "Higher odds of willing", "Lower odds of willing")
  ) %>%
  arrange(desc(dist_from_1))

View(or_ranked)

# Likelihood ratio. Which variables overall matter the most?

lrt_table <- drop1(model_demo, test = "Chisq")
lrt_table

########To visualize averages ###########

## RACE
plot(ggpredict(model_demo, terms = "RaceCat"))  %>%
  plot() +
  ylim(0, 1)

p_race <- ggpredict(model_demo, terms = "RaceCat")

plot(p_race) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_text(family = "plex", size = 16, margin = margin(b = 10)),
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
  )

##AGE

plot(ggpredict(model_demo, terms = "AgeCat"))  %>%
  plot() +
  ylim(0, 1)

p_age <- ggpredict(model_demo, terms = "AgeCat")

plot(p_age) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_text(family = "plex", size = 16, margin = margin(b = 10)),
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
)

## EDUCATION

plot(ggpredict(model_demo, terms = "EduCat"))  %>%
  plot() +
  ylim(0, 1)

p_education <- ggpredict(model_demo, terms = "EduCat")

plot(p_education) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_blank(), 
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
  )

## INCOME

plot(ggpredict(model_demo, terms = "Income"))  %>%
  plot() +
  ylim(0, 1)

p_income <- ggpredict(model_demo, terms = "Income")

plot(p_income) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_blank(), 
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
  )

## COVERAGE

plot(ggpredict(model_demo, terms = "Coverage"))  %>%
  plot() +
  ylim(0, 1)

p_coverage <- ggpredict(model_demo, terms = "Coverage")

plot(p_coverage) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_text(family = "plex", size = 16, margin = margin(b = 10)),
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
  )

## SEX

plot(ggpredict(model_demo, terms = "Sex"))  %>%
  plot() +
  ylim(0, 1)

p_sex <- ggpredict(model_demo, terms = "Sex")

plot(p_sex) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_text(family = "plex", size = 16, margin = margin(b = 10)),
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
  )

## URBAN

plot(ggpredict(model_demo, terms = "Urban"))  %>%
  plot() +
  ylim(0, 1)

p_urban <- ggpredict(model_demo, terms = "Urban")

plot(p_urban) +
  geom_point(size = 4, color = "#F19230") +
  labs(title = NULL) +
  scale_y_continuous(limits = c(0.5, 1), labels = scales::percent_format()) +
  theme_minimal(base_family = "plex") +
  theme(
    
    plot.margin = margin(t = 40, r = 40, b = 50, l = 40),
    
    # ⭐ KEEP horizontal gridlines
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
    
    # (Optional) Keep vertical gridlines
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # ⭐ Transparent background so box shows cleanly
    panel.background = element_rect(fill = NA),
    
    # ⭐ Add box around data region ONLY
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    
    # Axes, ticks, labels, fonts
    axis.title.x = element_text(family = "plex", size = 18, margin = margin(t = 30)),
    axis.title.y = element_text(family = "plex", size = 18, margin = margin(r = 15)),
    
    axis.text.x  = element_text(family = "plex", size = 16, margin = margin(b = 10)),
    axis.text.y  = element_text(family = "plex", size = 16, margin = margin(r = 10)),
    
    axis.ticks.x = element_line(size = 1.2),
    axis.ticks.y = element_line(size = 1.2),
    
    plot.title   = element_text(family = "plex", size = 20, face = "bold")
  )








## Do an analysis to see if any of the variables are unhelpful in the model,
## in that don't help predict. URBAN ends up being removed 

model_step_back <- step(
  model_demo,
  direction = "backward"
)

summary(model_step_back)

