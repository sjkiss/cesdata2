#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
if (!file.exists(here("data/ces93.rda"))) {
  #If it does not exist read in the original raw data file
  ces93<-read_sav(file=here("data-raw/CES1993"))
} else {
  load("data/ces93.rda")
}

# Save the file
save(ces93, file=here("data/ces93.rda"))
