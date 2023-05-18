# Prep / explore Uganda OCDS data
library(sf)
library(dplyr)
library(glue)
library(parallel)
library(lubridate)
library(jsonlite)
library(ggplot2)
library(tidyr)
install.packages('DT')
library(sqldf)

# extract bids, awards, buyers info from data 2022 Uganda OCDS contracts
# data from https://data.open-contracting.org/en/search/

#jsonl format

#test <-read.csv("../Demonstrator-main/data/test.csv")

lines <- readLines("../data/uganda_releases_2022.jsonl/2022.jsonl")
lines <- lapply(lines, fromJSON)
lines <- lapply(lines, unlist)
uganda_data <- bind_rows(lines)
lines <- NULL

# rows, cols
dim(uganda_data)

# TODO sort out the mess of extra data columns
colnames(uganda_data)

# reduce columns for now by removing any colnames with a number
pattern_n = "^.+?\\d$"
uganda_data <- uganda_data %>% select(!(matches(pattern_n)))

# explore
unique(uganda_data$tag) #all same anyway
unique(uganda_data$awards.value.currency)
unique(uganda_data$initiationType)
unique(uganda_data$contracts.value.currency)
unique(uganda_data$contracts.status)
unique(uganda_data$tender.status)
unique(uganda_data$tender.procurementMethod)

uganda_data <- uganda_data %>% mutate(across(any_of(ends_with("currency")),as.factor))
uganda_data <- uganda_data %>% mutate(across(any_of(ends_with("amount")),as.numeric))
uganda_data <- uganda_data %>% mutate(across(any_of(ends_with(".id")),as.numeric))
uganda_data <- uganda_data %>% mutate(across(any_of(ends_with("status")),as.factor))
uganda_data <- uganda_data %>% mutate(across(any_of(ends_with("date")),as.Date, format="%Y-%m-%d"))
uganda_data$tender.procurementMethod <- as.factor(uganda_data$tender.procurementMethod)

# look at numbers
summary(uganda_data %>% select(where(is.numeric)))
#lots of NAs, unclear if zeroes mean zero
# bids_details_value_amount=bids.details.value.amount,
uganda_data <- rename(uganda_data, bids_details_value_amount=bids.details.value.amount)
sqldf("select min(bids_details_value_amount) from uganda_data")
sqldf("select count(*), bids_details_value_amount from uganda_data where bids_details_value_amount < 100 group by bids_details_value_amount")

hist(uganda_data$bids_details_value_amount[uganda_data$bids_details_value_amount < 50000])

# add months to explore patterns
uganda_data <- uganda_data %>% mutate(month = month.name[month(bids.details.date)])
uganda_data <- uganda_data %>% mutate(year = year(bids.details.date))
# only Jan though in 23 so use '22 or other

# factorise months
uganda_data$month = factor(uganda_data$month, levels = month.name)

uganda_data_22 <- sqldf("select month, bids_details_value_amount from uganda_data where
                        year = 2022")

uganda_data_22$month = factor(uganda_data_22$month, levels = month.name)

#uganda_data %>% group_by(month) %>% summarise_each(funs(sum))
by_month <- uganda_data_22 %>%
  group_by(month) %>%
  filter(!is.na(bids_details_value_amount)) %>%
  summarize(count=n(), amount = sum(bids_details_value_amount))
#!caution - not all dates are 22!
by_month
 
# bids
#tibble(x=1:8, y=c('ocid', 'date', 'tag', 'bids.details.id', 'bids.details.date', 'bids_details_value_amount', 'bids.details.value.currency', 'bids.details.status'))

bids <- uganda_data[ , c('ocid', 'date', 'tag', 'bids.details.id', 'bids.details.date', 'bids_details_value_amount', 'bids.details.value.currency', 'bids.details.status')]

# rename
bids <- rename(bids, bids_details_id=bids.details.id,
               bids_details_date=bids.details.date,
               #bids_details_value_amount=bids.details.value.amount,
               bids_details_value_currency=bids.details.value.currency,
               bids_details_status=bids.details.status)

# drop nas?
sum(!complete.cases(bids[-1]))
#bids_complete <- bids %>% drop_na() 

# save bid details by month
write.csv(bids, "../data/uganda_22_bids.csv")

# awards
awards <- uganda_data[, c('ocid', 'date', 'tag', 'awards.id', 'awards.date',
                          'awards.value.amount', 'awards.value.currency', 'awards.suppliers.id',
                          'awards.suppliers.name')]

sum(!complete.cases(awards[-1]))
awards <- awards %>% drop_na() 
# rename #TODO function for this
awards <- rename(awards, awards_id=awards.id, awards_date=awards.date, awards_v_amount=awards.value.amount,
                 awards_v_currency=awards.value.currency, awards_suppliers_id=awards.suppliers.id, awards_suppliers_name=awards.suppliers.name)
write.csv(awards, "../data/uganda_22_awards.csv")

# buyers
buyers <- uganda_data[, c('ocid', 'date', 'buyer.id', 'buyer.name')]

sum(!complete.cases(buyers[-1]))
buyers <- buyers %>% drop_na() 

buyers <- rename(buyers, buyer_id=buyer.id,buyer_name=buyer.name)
# TO DO refactor auto rename from dot notation

write.csv(buyers, "../data/uganda_22_buyers.csv")
