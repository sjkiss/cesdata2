library(haven)
library(here)
library(tidyverse)
library(srvyr)

library(dplyr)
library(labelled)
# Read in CIS file
cis<-read_sav(file=here("data-raw/cis_2022.sav"))

#Set missing values
cis %>%
  set_na_values(cfttinc=1000000000000)->cis

cis %>%
  #Weight the dataset
  as_survey_design(weights = fweight) ->cis_des
cis_des
  #PRoduce terciles
  summarize(tertiles=survey_quantile(as.numeric(cfttinc), quantiles=c(0.33, 0.66))) ->cuts
cuts
update(cis_des,
       income_tercile=cut(as.numeric(cfttinc), breaks=c(-Inf, 69275, 137000, max(cis$cfttinc)), labels=c("Low", "Middle", "High")))->cis_des
cis_des %>%
  group_by(income_tercile) %>%
  summarize(out=survey_total())


