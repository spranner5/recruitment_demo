library(sqldf)
library(dplyr)
library(rjson)
library(ggplot2)

# load tenders data
tenders <- read.csv("data/tenders.csv")

# check colnames
colnames(tenders)

# OK, accessible
# blanks
tenders[tenders = ""] <- NA

# count of tenders suitable/not suitable / not stated
rowSums(is.na(tenders))

cleaned_data <- tenders[rowSums(is.na(tenders)) != ncol(tenders), ] # drop empty

library(dplyr)
tenders %>% 
  group_by(issuitableforsme) %>%
  summarise(n_rows = length(issuitableforsme))

# summary stats
cleaned_data %>%
  select(where(is.numeric)) %>%
  summary()

# spread of tender values
plot(cleaned_data$tender_value_amount)

# outliers! barplot
cleaned_data[cleaned_data$issuitableforsme] %>%
  ggplot(aes(x=tender_value_amount, y=issuitableforsme)) + geom_col



cleaned_data %>% 
  group_by(issuitableforsme) %>%
  summarise(n_rows = length(issuitableforsme))

# 997 blanks

# also unscuccessful tenders
cleaned_data %>% 
  group_by(tender_status) %>%
  summarise(n_rows = length(tender_status))

ggplot(data = cleaned_data) +
  geom_bar(mapping = aes(x=tender_status))

ggplot(data = cleaned_data) +
  geom_bar(mapping = aes(x=issuitableforsme))

t_complete_unsuc = cleaned_data %>%
  filter(tender_status == "complete" | tender_status == "unsuccessful")

# check the tender currency too
t_complete_unsuc %>%
  group_by(tender_value_currency) %>%
  summarise(n_rows = length(tender_value_currency))
# all GBP 

ggplot(data = t_complete_unsuc) +
  geom_bar(mapping = aes(x=tender_status, color=tender_value_amount))

ggplot(data=t_complete_unsuc, mapping=aes(y=tender_value_amount, x=tender_status)) + 
  geom_boxplot()

lower_val <- t_complete_unsuc %>%
  filter(tender_value_amount < 10000000)

ggplot(data=lower_val, mapping=aes(y=tender_value_amount, x=tender_status)) + 
  geom_boxplot()




# value by sme
ggplot(data = t_complete_unsuc, mapping = aes(x=tender_value_amount)) + 
  geom_freqpoly(mapping = aes(colour = issuitableforsme))

ggplot(data = t_complete_unsuc, mapping=aes(x=tender_value_amount))

# export the cleaned data
write.csv(cleaned_data, "../data/uk_tenders_cleaned.csv")
