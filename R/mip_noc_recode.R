# library(readxl)
# mip<-read_excel(path="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/CES_Folder/Data/mip.xlsx", col_names=T)
# names(mip)
# look_for(ces19phone, "important")
# library(tidyverse)
# ces19phone$q7_lower<-str_to_lower(ces19phone$q7)
# names(mip)
# mip %>%
#   select(q7_lower, q7_out) %>%
#   full_join(ces19phone, ., by="q7_lower")->out
#
# #use names in mip$mip15 for the numbers in CPS15_1.
# #What I'm doing here is taking the same value labels from 2015 and applying them to 2019.
# mip %>%
#   #I'm dumping out all the excess rows of the spreadsheet that do not have a value for CPS15_1
#   #S'o I'm just keeping the rows that have the values and the value labels for the issues from 2015
#   filter(!is.na(CPS15_1)) %>%
#   #Now I'm just selecting those two variables and naming them label and value
#   select(label=mip15, CPS15_1) %>%
#   #This turns the CPS15_1 variable into a numeric variable
#   mutate(value=as.numeric(CPS15_1))->mip_labels
# #This takes the value labels in the value column and it makes them the names of the label variable
# names(mip_labels$value)<-mip_labels$label
# #This then turns out$mip into a labelled variable using the named mip_labels$value label that was justr created
# out$q7_out<-labelled(out$q7_out, labels=mip_labels$value)
#
# #This performs a check
# table(as_factor(out$q7_out))
#
# #Overwrite ces19phone with out
# ces19phone<-out
# #### Add Occupations to 2019 phone####
# # data("ces19phone")
#
# noc<-read_excel(path="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/CES_Folder/Data/unique-occupations-updated.xls", col_names=T)
# head(noc)
# ces19phone$p52<-tolower(ces19phone$p52)
# #
# #
# noc %>%
#   select(p52, NOC) %>%
#   full_join(ces19phone, ., by="p52")->out
# out$NOC
# #Provide Check
# #Jobs uNique to Government program
# out %>%
#   filter(NOC==4168) %>%
#   select(p52)
# #Teachers
# out %>%
#   filter(NOC==4031) %>%
#   select(p52) %>%
#   print(n=100)
# #Carpenters
# out %>%
#   filter(NOC==7271) %>%
#   select(p52) %>%
#   print(n=100)
# #Check that weird librarian
# out[3059,'NOC']
# #replace ces19phone with out
# ces19phone<-out
# ces19phone[3059,'NOC']
# names(ces19phone)
#
# library(labelled)
# library(tidyverse)
#
#
# # #### Save ces19phone after NOC and MIP###
# names(out)
#
#
# use_data(ces19phone, overwrite=T, version=2)
# names(ces19phone)
