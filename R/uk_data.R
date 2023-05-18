# Uses UK OCDS open contracting data download from https://data.open-contracting.org/en/search/
# CSV zip extracted into individual csvs
# This file extracts tenders from main.csvs 2023

library(sqldf)
library(dplyr)
library(ggplot2)
library(naniar)
library(tidyr)


# load main.csv if not already parsed tenders
#main <- read.csv("../data/main.csv")

#head(main)
#dim(main)

# cols
#colnames(main)

# remove some unneeded cols
#main <- select(main, id, date, ocid, tender_id, tender_title, tender_status, tender_description, tender_value_amount, tender_value_currency, tender_suitability_sme)
  
# save out to tenders
#write.csv(main, "../data/uk_tenders_cleaned.csv")

# otherwise load tenders data
tenders <- read.csv("../data/uk_tenders_cleaned.csv")  

# check colnames
colnames(tenders)

# OK, accessible
# look at missing values
miss_var_summary(tenders)

# many missing in tender_value_amount, maybe due to status?
empty <- filter(tenders, is.na(tender_value_amount))
empty %>%
  group_by(tender_status) %>%
  summarize(count = n())
# not just status, so caution required 

# but otherwise a handful with descr and id
cleaned_data <- drop_na(tenders, c("tender_description", "tender_id"))

cleaned_data_no_missing_t_vals <- drop_na(tenders, c("tender_value_amount"))

# summary stats
cleaned_data %>%
  select(where(is.numeric)) %>%
  summary()

# spread of tender values
plot(cleaned_data_no_missing_t_vals$tender_value_amount)

# outliers! 
cleaned_data_no_missing_t_vals %>%
  ggplot(aes(x=tender_value_amount, y=tender_suitability_sme)) + geom_col()

# look at some groups
library(dplyr)
cleaned_data %>% 
  group_by(tender_suitability_sme) %>%
  summarise(n_rows = length(tender_suitability_sme))

# 8 blanks

# also unscuccessful tenders?
cleaned_data %>% 
  group_by(tender_status) %>%
  summarise(n_rows = length(tender_status))

ggplot(data = cleaned_data) +
  geom_bar(mapping = aes(x=tender_status))

ggplot(data = cleaned_data) +
  geom_bar(mapping = aes(x=tender_suitability_sme))

t_complete_unsuc = cleaned_data %>%
  filter(tender_status == "complete" | tender_status == "unsuccessful")

# check the tender currency too
t_complete_unsuc %>%
  group_by(tender_value_currency) %>%
  summarise(n_rows = length(tender_value_currency))
#mostly GBP but missing 2792 so caution needed for dealing with amounts

# value by sme
sqldf("select sum(tender_value_amount), tender_suitability_sme from t_complete_unsuc group by tender_suitability_sme")
sqldf("select max(tender_value_amount) as max, min(tender_value_amount) as min, tender_suitability_sme from t_complete_unsuc group by tender_suitability_sme")

# some implausible vals

# export the cleaned data
write.csv(cleaned_data, "../data/uk_tenders_cleaned.csv")
