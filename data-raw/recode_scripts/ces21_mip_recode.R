library(tidyverse)
library(here)
data("ces21")
lookfor(ces21, "important issue")
#Convert cps21_imp_iss to lower
ces21 %>%
  mutate(mip_lower=str_trim(str_to_lower(cps21_imp_iss)))->ces21

#This shows the unique responses to most important issue with counts
ces21 %>%
  group_by(mip_lower) %>%
  count() %>%
  arrange(desc(n))

#This puts that out into a csv file for inspection in excel
ces21 %>%
  group_by(mip_lower) %>%
  count() %>%
  arrange(desc(n)) %>%
  write_csv(here("data-raw/2021_mip_unique.csv"))

#We need to code these responses as best we can
# I would like a series of dichotomous variables for each issue in this model in this list
#Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9,
                              #Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16)
# At the start =here are 8949 responses
# In the space of about 45 minutes of work I have gotten that down to about 4500
# So it can go pretty quick.
# The Environment
ces21 %>%
  mutate(enviro_mip=case_when(
    str_detect(mip_lower, "environ")~1
  ))->ces21
# Crime
ces21 %>%
  mutate(crime_mip=case_when(
    str_detect(mip_lower, "crime|crimin")~1
  ))->ces21
#Ethics
# Education
# Energy
# Jobs
# Economy
ces21 %>%
  mutate(economy_mip=case_when(
    str_detect(mip_lower, "economy")~1,
    str_detect(mip_lower, "économie")~1,
  ))->ces21
# Health and Health Care
ces21 %>%
  mutate(health_mip=case_when(
    str_detect(mip_lower, "health")~1,
    str_detect(mip_lower, "santé")~1
  ))->ces21
# Socio-Cultural
ces21 %>%
  mutate(socio_cultural_mip=case_when(
    str_detect(mip_lower, "trans")~1,
    str_detect(mip_lower, "identity")~1,
    str_detect(mip_lower, "values")~1
  ))->ces21
# Social Programs
# Brokerage (Canada, Quebec Fed- Prov)
# Inflation
ces21 %>%
  mutate(inflation_mip=case_when(
    str_detect(mip_lower, "inflation")~1,
    str_detect(mip_lower, "prices")~1,
    str_detect(mip_lower, "cost of living")~1
  ))->ces21
# Housing, rent
ces21 %>%
  mutate(housing_mip=case_when(
    str_detect(mip_lower, "logement")~1,
    str_detect(mip_lower, "housing")~1,
    str_detect(mip_lower, "rent")~1,
  ))->ces21
# COVID
ces21 %>%
  mutate(covid_mip=case_when(
    str_detect(mip_lower, "covid")~1,
    str_detect(mip_lower, "pandémie")~1,
  ))->ces21
# Taxes
ces21 %>%
  mutate(taxes_mip=case_when(
    str_detect(mip_lower, "tax|tax*")~1
  ))->ces21
# Debt and Deficit
ces21 %>%
  mutate(debt_mip=case_when(
    str_detect(mip_lower, "debt|deficit")~1
  ))->ces21
# Democracy
ces21 %>%
  mutate(democracy_mip=case_when(
    str_detect(mip_lower, "democr*")~1
  ))->ces21
# Foreign Affairs
ces21 %>%
  mutate(foreign_mip=case_when(
    str_detect(mip_lower, "peace|war")~1
  ))->ces21
# Immigration
ces21 %>%
  mutate(immigration_mip=case_when(
    str_detect(mip_lower, "immigr*")~1
  ))->ces21

tail(names(ces21))
#This code calculates the sum of issues that have been coded
ces21 %>%
  mutate(mip_total=rowSums(across(ends_with("_mip")), na.rm=T))->ces21
### This code will show the responses that have no mip codes that have been assigned
# This should be run repeatedly to iteratively capture as many topics as possible
ces21 %>%
  filter(mip_total<1) %>%
  group_by(mip_lower) %>%
  count() %>%
  arrange(desc(n))
### This will go quicker than you think.
### 1) For example, adding str_detect(mip_lower, "santé") combine multiple responses.
### As will housing.
### 2) str_detect() takes what is called a regular expression. See here https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
### So for example, * is a wildcard. "democr*" will capture democracy and democratic
### immigr* will capture immigrants and immigration
### 3) French here will obviously be a particular challenge, but basically just google stuff as it comes up.
summary(ces21$mip_total)
ces21 %>%
  select(ends_with("_mip")) %>%
  View()
