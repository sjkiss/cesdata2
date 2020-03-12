## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")
library(usethis)


##load data files
#1993
library(haven)
library(labelled)
ces65<-read_dta(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1965.dta")
ces68<-read_dta(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1968.dta")
ces72_jul<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1972/CES-E-1972-jun-july_F1.sav")
ces72_sept<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1972/CES-E-1972-sept_F1.sav")
ces72_nov<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces1972/CES-E-1972-nov_F1.sav")
ces79<-read_sav(file="/Users/skiss/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1974_1979_1980.sav")
ces74b<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-1974/CES-E-1974_F1.sav")
ces84<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1984.sav")
ces88<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES1988.sav")
ces93<-read_sav(file="~/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/1993/CES-E-1993_F1.sav")
ces97<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-1997/CES-E-1997_F1.sav')
ces00<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-2000/CES-E-2000_F1.sav')
ces0411<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces.merged/CES_04060811_ISR_revised.sav')
ces15phone<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/CES2015-phone-release/CES2015_CPS-PES-MBS_complete.sav')
ces15web<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/web/CES15_CPS+PES_Web_SSI Full.SAV')

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

#use_data(ces65, ces68,  overwrite=T)
use_data(ces74b, overwrite=T)
use_data(ces79, overwrite=T)

