# Prep Data
library(sf)
library(dplyr)
library(glue)
library(parallel)
library(lubridate)
library(jsonlite)
library(ggplot2)
library(tidyr)
install.packages('DT')

# extract bids, awards, buyers info from data 2022 Uganda OCDS contracts
# data from https://data.open-contracting.org/en/search/

#jsonl format

#test <-read.csv("../data/test.csv")

lines <- readLines("../data/uganda_releases_2022.jsonl/2022.jsonl")
lines <- lapply(lines, fromJSON)
lines <- lapply(lines, unlist)
x <- bind_rows(lines)

# better name
uganda_data <- x

# fix numeric values TODO put in col names variables
uganda_data$bids.details.value.amount <- as.numeric(uganda_data$bids.details.value.amount)

# TODO sort out the mess of extra data columns
colnames(uganda_data)


# what's in the tags
uganda_data[1,]$tag

#compiled only




uganda_data %>%
  slice_head(n=10) %>%
  #View()
  filter(!is.na(bids.details.value.amount)) %>%
  DT::datatable()




# add months
uganda_data <- uganda_data %>% mutate(month = month.name[month(date)])
# only Jan though in 23 so use '22 or other
uganda_data <- uganda_data %>% mutate(day = day.name[day(date)])

# factorise months
uganda_data$month = factor(uganda_data$month, levels = month.name)



#uganda_data %>% group_by(month) %>% summarise_each(funs(sum))

by_month <- uganda_data %>%
  group_by(month) %>%
  filter(!is.na(bids.details.value.amount)) %>%
  summarize(count=n(), amount = sum(bids.details.value.amount))
#!caution - not all dates are 22!

# all cols
colnames(uganda_data)


# bids
tibble(x=1:8, y=c('ocid', 'date', 'tag', 'bids.details.id', 'bids.details.date', 'bids.details.value.amount', 'bids.details.value.currency', 'bids.details.status'))

#by_month
bids <- uganda_data[ , c('ocid', 'date', 'tag', 'bids.details.id', 'bids.details.date', 'bids.details.value.amount', 'bids.details.value.currency', 'bids.details.status')]
# type conversions
bids$bids.details.id <- as.numeric(bids$bids.details.id)
bids$bids.details.date <- as.Date(bids$bids.details.date, format="%Y-%m-%d")

# drop nas
sum(!complete.cases(bids[-1]))
bids_complete <- bids %>% drop_na() 

# save bid details by month
write.csv(bids, "../data/uganda_22_bids.csv")

# awards
awards <- uganda_data[, c('ocid', 'date', 'tag', 'awards.id', 'awards.date',
                          'awards.value.amount', 'awards.value.currency', 'awards.suppliers.id',
                          'awards.suppliers.name')]

sum(!complete.cases(awards[-1]))
awards <- awards %>% drop_na() 
write.csv(awards, "../data/uganda_22_awards.csv")

# buyers
buyers <- uganda_data[, c('ocid', 'date', 'buyer.id', 'buyer.name')]

sum(!complete.cases(buyers[-1]))
buyers <- buyers %>% drop_na() 

# save data back out
write.csv(buyers, "../data/uganda_22_buyers.csv")
 

 
