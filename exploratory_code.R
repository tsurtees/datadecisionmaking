# exploratory analysis

library(haven)
library(dplyr)

# Load IBM teleco data
teleco_data_raw <- read.csv("data/telco-customer-churn.csv")

churn_yes <- teleco_data_raw %>% filter(Churn == "Yes")
churn_no  <- teleco_data_raw %>% filter(Churn == "No")

# gender
prop.table(table(churn_no$gender)) * 100
prop.table(table(churn_yes$gender)) * 100

# Overall, pretty even split, more females in churn than men, but small amounts

# SeniorCitizen
prop.table(table(churn_no$SeniorCitizen)) * 100
prop.table(table(churn_yes$SeniorCitizen)) * 100

# In churn, more senior citizens (25% vs 13% for not churn)

# Partner
prop.table(table(churn_no$Partner)) * 100
prop.table(table(churn_yes$Partner)) * 100

# In churn, majority with no partner (64% vs 47% for not churn)

# Dependents
prop.table(table(churn_no$Dependents)) * 100
prop.table(table(churn_yes$Dependents)) * 100

# In churn, more customers had no dependents (83% vs 66% for not churn)


library(ggplot2)
library(tidyr)

# ---- Prepare data ----
# Add Churn label to full dataset and calculate proportions for each variable

plot_data <- teleco_data_raw %>%
  mutate(
    SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No")
  ) %>%
  select(Churn, gender, SeniorCitizen, Partner, Dependents) %>%
  pivot_longer(cols = -Churn, names_to = "Variable", values_to = "Value") %>%
  group_by(Churn, Variable, Value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Churn, Variable) %>%
  mutate(pct = n / sum(n) * 100)

# ---- Plot ----
ggplot(plot_data, aes(x = Value, y = pct, fill = Churn)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3) +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_fill_manual(values = c("No" = "#95B8D1", "Yes" = "#E07A5F")) +
  labs(
    title = "Customer Characteristics: Churn vs No Churn",
    x = NULL,
    y = "Percentage (%)",
    fill = "Churn"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )






# Data on service they bought (can split this in same way)





# Data on payment
# Can look at how much money each month, therefore how much money lost if churn








