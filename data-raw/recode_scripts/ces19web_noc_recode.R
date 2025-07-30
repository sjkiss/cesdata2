library(readxl)
library(tidyverse)
library(here)
#Read in Kristin's NOC 2016 unique occupations file
read_excel(path=here("data-raw/2016-unique-occupations-updated.xls")) %>%
  rename("title"=2) %>%
  select(2,4) ->noc2016
names(noc2016)
 #take Kristin's file
lookfor(ces19web, "occupation")
ces19web %>%
  mutate(pes19_occ_text_lower=str_to_lower(pes19_occ_text)) %>%
  left_join(., noc2016, by=c("pes19_occ_text_lower"="title"))->ces19web

#Read in Rafael's unique occupations file
#read in the data file
read_excel(path=here("data-raw/2021_occupations_coded.xlsx")) %>%
  #Select only the columns with the unique job title and the NOC codes
  select("title", "NOC21_4", "NOC21_5") %>%
  #Set 0s to be missing in the two columns containing the NOC code
  mutate(across(2:3, function(x) car::Recode(x, "0=NA", as.numeric=T)))->
  # Store as object noc
  noc21

#This is a check
noc21 %>%
  #Convert each NOC variable to character, then count the characters in each entry
  mutate(across(2:3, ~nchar(as.character(.)))) %>%
  #check that each has a maximum of 4 and 5 respectively
  summarize(across(2:3, ~max(., na.rm=T)))

#| label: merge-2019-web
ces19web %>%
  #mutate(pes19_occ_text_lower=str_to_lower(pes19_occ_text)) %>%
  left_join(., noc21, by=c("pes19_occ_text_lower"="title"))->ces19web

# Get job titles
# library(openxlsx)
# noc_2021_4_titles<-read.xlsx(xlsxFile = "data-raw/NOC_2021_4_job_titles.xlsx")
# noc_2021_5_titles<-read.xlsx(xlsxFile = "data-raw/NOC_2021_5_job_titles.xlsx")
#
#
# ces19web %>%
#   left_join(., noc_2021_5_titles, by=c("NOC21_5")) %>%
#   left_join(., noc_2021_4_titles) %>%
#   # select(contains("NOC21")) %>%
#   select(-contains('Description')) ->ces19web
