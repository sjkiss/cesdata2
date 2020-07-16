#
# library(usethis)


##load data files
#1993
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
# ces93<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1993/CES-E-1993_F1.sav")
# ces97<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-1997/CES-E-1997_F1.sav')
# ces00<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-2000/CES-E-2000_F1.sav')
# ces0411<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces.merged/CES_04060811_ISR_revised.sav')
# ces15phone<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/CES2015-phone-release/CES2015_CPS-PES-MBS_complete.sav')
# ces15web<-read_sav(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/web/CES15_CPS+PES_Web_SSI Full.SAV')
# ces19phone<-read_dta(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES19/2019 Canadian Election Study - Phone Survey v1.0.dta', encoding="utf-8")
# ces19web<-read_dta(file='~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES19/2019 Canadian Election Study - Online Survey v1.0.dta', encoding="")

#### Add Occupations to 2019 phone####
# data("ces19phone")
# library(readxl)
# noc<-read_excel(path="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/CES_Folder/Data/unique-occupations-updated.xls", col_names=T)
# head(noc)
# ces19phone$p52<-tolower(ces19phone$p52)
# library(tidyverse)
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
# #replace ces19phone with out
# ces19phone<-out
# names(ces19phone)
# tail(names(ces0411))
# library(labelled)
# library(tidyverse)

#Combine 1972 files
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


#use_data(ces65, ces68, ces74, ces7980, ces84, ces88, ces93, ces97, ces00, ces0411, ces15phone, ces15web, ces19phone, ces19web, overwrite=T)
#tail(names(ces7980))

#save(ces19phone, file="data/ces19phone.rda")
#### Add Occupations to 2019 Web####
# data("ces19web")
# library(readxl)
# noc<-read_excel(path="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/CES_Folder/Data/unique-occupations-updated.xls", col_names=T)
# head(noc)
# data("ces19web")
# ces19web$pes19_occ_text
#  noc %>%
#    rename(pes19_occ_text=p52) %>%
#   select(pes19_occ_text, NOC) %>%
#   full_join(ces19web, ., by="pes19_occ_text")->out
# table(out$NOC, useNA = "ifany")
#
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
# save(ces19web, file="data/ces19web.rda")
