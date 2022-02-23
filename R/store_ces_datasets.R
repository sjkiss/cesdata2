#
# library(usethis)
#
# library(tidyverse)
# ##load data files
# library(haven)
# library(labelled)
# ces65<-read_dta(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1965.dta")
# ces68<-read_dta(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1968.dta")
# ces72_jul<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1972/CES-E-1972-jun-july_F1.sav")
# ces72_sept<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1972/CES-E-1972-sept_F1.sav")
# ces72_nov<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1972/CES-E-1972-nov_F1.sav")
# ces74<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-1974/CES-E-1974_F1.sav")
# ces7980<-read_sav(file="/Users/skiss/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1974_1979_1980.sav")
# ces84<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1984.sav")
# ces88<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES1988.sav")
# ces93<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1993/CES-E-1993_F1.sav", user_na=T)
# ces97<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-1997/CES-E-1997_F1.sav',user_na=T)
# ces00<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-2000/CES-E-2000_F1.sav', user_na=T)
# ces0411<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces.merged/CES_04060811_ISR_revised.sav')
# ces15phone<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/CES2015-phone-release/CES2015_CPS-PES-MBS_complete.sav')
# ces15web<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/web/CES15_CPS+PES_Web_SSI Full.SAV')
# ces19phone<-read_dta(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES19/2019 Canadian Election Study - Phone Survey v1.0.dta', encoding="utf-8")
#ces19web<-read_sav(file='/Users/skiss/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES19/CES-E-2019-online/CES-E-2019-online_F1.sav', encoding='latin1')
#read_dta(file="/Users/skiss/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES19/CES-E-2019-online_F1.dta")
# ces19_kiss<-read_dta(file='/Users/skiss/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES19/CES2019 Campaign Period Survey Kiss module data 2020-01-28.dta', encoding="")
#ces21<-read_dta(file='/Users/skiss/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces21.dta')

# library(usethis)
# data('ces19web')

# library(labelled)
# lookfor(ces19web, "federal")
# View(var_label(ces19web))
# val_labels(ces19web)
# str(ces19web)
# #rm(ces19web)
# library(tidyverse)
# ces19web %>%
#   select(starts_with('cps19_spend')) %>%
#   lookfor(., 'federal')
# ncol(ces19web)
# ces19web %>%
#   select(275:310) %>%
#   lookfor(., 'federal')
# ?select
# ####Combine 1972 files####
# #Rename respondent idea variables for continuity
# names(ces72_jul)[1]
# names(ces72_sept)[1]
# names(ces72_nov)[1]
# ces72_jul %>%
#   rename("CASEID"="RES_ID1A") ->ces72_jul
# names(ces72_jul)[1]
#
# names(ces72_sept)[1]
# ces72_sept %>%
#   rename("CASEID"="RESPID1A")->ces72_sept
# names(ces72_sept)[1]
# names(ces72_nov)[1]

# # #### Add Molly's Most Important Problem to 2019 Phone ####
# library(readxl)
# mip<-read_excel(path="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/CES_Folder/Data/mip.xlsx", col_names=T)
#
# library(stringr)
# ces19phone$q7_lower<-str_to_lower(ces19phone$q7)
# ces19phone$q7_lower
# names(mip)
# mip %>%
#   select(q7_lower, q7_out) %>%
#   full_join(ces19phone, ., by="q7_lower")->out
# names(out)
# #use names in mip$mip15 for the numbers in CPS15_1.
# #What I'm doing here is taking the same value labels from 2015 and applying them to 2019.
#
# mip %>%
#   #I'm dumping out all the excess rows of the spreadsheet that do not have a value for CPS15_1
#   #S'o I'm just keeping the rows that have the values and the value labels for the issues from 2015
#   filter(!is.na(CPS15_1)) %>%
#   #Now I'm just selecting those two variables and naming them label and value
#   select(label=mip15, CPS15_1) %>%
#   #This turns the CPS15_1 variable into a numeric variable
#   mutate(value=as.numeric(CPS15_1))->mip_labels
# mip_labels
# #This takes the value labels in the value column and it makes them the names of the label variable
# names(mip_labels$value)<-mip_labels$label
# mip_labels$value
# #This then turns out$mip into a labelled variable using the named mip_labels$value label that was justr created
# names(out)
# names(out)
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
# #
# # library(labelled)
# # library(tidyverse)
# #
# #
# # # #### Save ces19phone after NOC and MIP###
# # names(out)
# #
# #
# #
# #
# #
# #### Add Occupations to 2019 Web####
#
# data("ces19web")
# library(readxl)
# #Load the unique occupation codes Kristin did
# noc<-read_excel(path="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/CES_Folder/Data/unique-occupations-updated.xls", col_names=T)
# #check
# head(noc)
#
#
#
# table(is.na(ces19web$pes19_occ_cat_28_TEXT))
# table(is.na(ces19web$pes19_occ_text))
# look_for(ces19web, "occupation")
#
# #Combine the text occupation responses
#  ces19web %>%
#    #create the occ_text_joint variable
#    mutate(pes19_occ_text_joint=case_when(
#      #because pes19_occ_text came first in the survey, when nchar > 0 return it
#           nchar(ces19web$pes19_occ_text)>0 ~ tolower(ces19web$pes19_occ_text),
#      #when pes19_occ_text is empty (nchar==0), return the follow-up question
#      nchar(ces19web$pes19_occ_text)==0 ~ tolower(ces19web$pes19_occ_cat_28_TEXT)
#
#    ))->ces19web
#  ces19web$pes19_occ_text_joint
#  names(noc)
#  #take Kristin's file
#  noc %>%
#    #rename the p52 variable which comes from the phone survey to match what we just made above
#    rename(pes19_occ_text_joint=p52) %>%
#    #only select those two variables to get rid of superfluous stuff
#   select(pes19_occ_text_joint, NOC) %>%
#    #join ces19web to noc by the joint text variable
#   full_join(ces19web, ., by="pes19_occ_text_joint")->out
# names(out)
#  #Check how many are missing
# table(is.na(out$NOC))
# # #Provide Check
# #Jobs uNique to Government program
# out %>%
#   filter(NOC==4168) %>%
#   select(pes19_occ_text)
# #Teachers
# out %>%
#   filter(NOC==4031) %>%
#     select(pes19_occ_text)%>%
#   print(n=100)
# #Carpenters
# out %>%
#   filter(NOC==7271) %>%
#     select(pes19_occ_text)%>%
#   print(n=100)
# names(out)
# #keyword search
# out %>%
#   filter(str_detect(pes19_occ_text_joint, "customer service")) %>%
#   select(pes19_occ_text_joint, NOC)
#
# out %>%
#   select(-occ_text_joint, -noc_code) -> out
# names(out)
# ces19web<-out

#### Check out CES21 ####
#names(ces21)
#lookfor(ces21, "occupation")

# #### Complete save command ###

#library(usethis)
#use_data(ces65, ces68, ces74, ces7980, ces84, ces88, ces93, ces97, ces00, ces0411, ces15phone, ces15web, ces19phone, ces19web,overwrite=T)
#use_data(ces93,ces97, ces00, overwrite=T)
# #tail(names(ces7980))
# names(ces19phone)
#use_data(ces19web, overwrite=T)
