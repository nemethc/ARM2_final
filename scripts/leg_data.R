library(dplyr)
library(readr)
library(tidyr)
library(here)

bill_data <- read_csv(here("data/session_data.csv"))
party_data <- read_csv(here("data/leg_rosters.csv"))


bill_data <- bill_data %>%
  separate(Bill, c("prefix", "bill_number"), sep = " ") %>% #seperate e.g. HB 1065 into components
  mutate(billpassed = if_else(Status == "Del to Gov" | startsWith(Status, "C"), 1 , 0)) %>% #filter for bills that passed, 1 = passed
  rename(original_sponsor = `Original Sponsor`) %>% 
  distinct() %>% #removes house of origin duplicates
  left_join(party_data, by = c("original_sponsor" = "Member Name")) %>% #join by party affiliation
  select(bill_number, Status, Title,original_sponsor, billpassed, Party) %>% 
  filter(`original_sponsor` != "People of the State of Washington")  #not including initiatives, sorry Tim Eyman
  

sponsor_record <- bill_data %>% #Get bill counts by legislator
  count(original_sponsor, billpassed, Party) %>% 
  spread(billpassed, n) %>% 
  mutate(`1` = replace_na(`1`, 0),
         `0` = replace_na(`0`, 0),
         tot_bills = `1` + `0`,
         pct_passed = `1` / (tot_bills))

