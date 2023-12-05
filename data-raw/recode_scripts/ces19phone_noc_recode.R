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
