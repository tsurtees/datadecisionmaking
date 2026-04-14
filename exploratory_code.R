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



# Data on service they bought (can split this in same way)





# Data on payment
# Can look at how much money each month, therefore how much money lost if churn













