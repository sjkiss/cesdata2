## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")
library(usethis)


##load data files
#1993
library(haven)

ces97<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-1997/CES-E-1997_F1.sav')
ces00<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES-E-2000/CES-E-2000_F1.sav')
ces0411<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/ces.merged/CES_04060811_ISR_revised.sav')
ces0411$survey<-as_factor(ces0411$Survey_Type04060811)
ces15phone<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/CES2015-phone-release/CES2015_CPS-PES-MBS_complete.sav')
ces15web<-read_sav(file='/Users/Simon/OneDrive - Wilfrid Laurier University/canadian_politics/canadian_election_studies/CES15/web/CES15_CPS+PES_Web_SSI Full.SAV')
#use_data(ces97, ces00, ces0411,overwrite=T)

use_data(ces0411, overwrite=T)
use_data(ces15phone, ces15web, overwrite=T)
