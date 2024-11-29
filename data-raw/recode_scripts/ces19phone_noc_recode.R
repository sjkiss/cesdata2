library(readxl)
noc<-read_excel(path=here("data-raw/2016-unique-occupations-updated.xls"), col_names=T)
head(noc)
ces19phone$p52<-tolower(ces19phone$p52)
#
#
noc %>%
 # select(p52) %>%
  slice(1782)

noc %>%
  select(p52, NOC) %>%
  right_join(.,ces19phone, keep=F)->out

#Provide Check
#Jobs uNique to Government program
out %>%
  filter(NOC==4168) %>%
  select(p52)
#Teachers
out %>%
  filter(NOC==4031) %>%
  select(p52) %>%
  print(n=100)
#Carpenters
out %>%
  filter(NOC==7271) %>%
  select(p52) %>%
  print(n=100)
#Check that weird librarian
out[3059,'NOC']
#replace ces19phone with out
ces19phone<-out
ces19phone[3059,'NOC']
names(ces19phone)
#
# library(labelled)
# library(tidyverse)
#

#Read in Rafael's unique occupations file
#read in the data file
read_excel(path=here("data-raw/2021_occupations_coded.xlsx")) %>%
  #Select only the columns with the unique job title and the NOC codes
  select("title", "NOC21_4", "NOC21_5") %>%
  #Set 0s to be missing in the two columns containing the NOC code
  mutate(across(2:3, function(x) car::Recode(x, "0=NA", as.factor=T)))->
  # Store as object noc
  noc21

ces19phone %>%
  left_join(., noc21,by=c("p52"="title"))->ces19phone
library(openxlsx)
noc_2021_4_titles<-read.xlsx(xlsxFile = "data-raw/NOC_2021_4_job_titles.xlsx")
noc_2021_5_titles<-read.xlsx(xlsxFile = "data-raw/NOC_2021_5_job_titles.xlsx")


ces19phone %>%
  left_join(., noc_2021_5_titles, by=c("NOC21_5")) %>%
  left_join(., noc_2021_4_titles) %>%
  # select(contains("NOC21")) %>%
  select(-contains('Description')) ->ces19phone
