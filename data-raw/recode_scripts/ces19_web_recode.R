#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces19web <- read_sav(file=here("data-raw/CES-E-2019-online_F1.sav"), encoding="latin1")
#there is a problem with French accented characters int he original data file of the 2019
# web survey.
#This script repairs those.
source("data-raw/recode_scripts/ces19_web_problem_with_encodings.R")

ces19web %>%
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>%
  select(cps19_ResponseId, pes19_occ_text)
lookfor(ces19web, "occupation")
#Add 2016 NOC and 2021 NOC
source("data-raw/recode_scripts/ces19web_noc_recode.R")
#Note this creates the occupation variable FROM THE 2016 NOC CODES

ces19web$occupation<-Recode(as.numeric(ces19web$NOC), "0:1099=2;
1100:1199=1;
2100:2199=1;
 3000:3199=1;
 4000:4099=1;
 4100:4199=1;
 5100:5199=1;
 1200:1599=3;
 2200:2299=3;
 3200:3299=3;
 3400:3500=3;
 4200:4499=3;
 5200:5299=3;
 6200:6399=3;
 6400:6799=3; 7200:7399=4;
                              7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
val_labels(ces19web$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
var_label(ces19web$occupation)<-c("5 category class no self-employed from actual NOC codes")
#This code checks if we missed anything
ces19web %>%
  filter(is.na(NOC)==F&is.na(occupation)==T) %>%
  select(NOC, occupation)

table(ces19web$occupation, useNA = "ifany")
ces19web %>%
  mutate(occupation2=case_when(
    #If occupation is not missing then return the value for occupation
    is.na(occupation)==F ~ occupation,
    #If occupoatuib is missing and categorical occupation is 2 then return managers
    is.na(occupation)==T & pes19_occ_cat==18 ~ 2,
    is.na(occupation)==T & pes19_occ_cat==19 ~ 1,
    #This is questionable; Here we assign technician or associate professional to routine non-manual; could revisit
    is.na(occupation)==T & pes19_occ_cat==20 ~ 3,
    #Clerical Support Worker
    is.na(occupation)==T & pes19_occ_cat==21 ~ 3,
    #Service or Sales Workers
    is.na(occupation)==T & pes19_occ_cat==22 ~ 3,
    #Skilled agricultural, forestry or fishery
    is.na(occupation)==T & pes19_occ_cat==23 ~ 4,
    #Craft or related trades worker
    is.na(occupation)==T & pes19_occ_cat==24 ~ 4,
    #Plant Machine Operator
    is.na(occupation)==T & pes19_occ_cat==25 ~ 5,
    #Cleaner Labourer
    is.na(occupation)==T & pes19_occ_cat==26 ~ 5,
    TRUE ~ NA_real_
  ))->ces19web
table(is.na(ces19web$occupation))
table(is.na(ces19web$occupation2))
table(ces19web$occupation)
table(ces19web$occupation2)
table(is.na(ces19web$NOC))

table(is.na(ces19web$NOC21_4))
var_label(ces19web$occupation2)<-c("5 category class no self-employed from pre-existing categories provided to R")

look_for(ces19web, "ethnic")
ces19web$cps19_ethnicity_41_TEXT
ces19web$cps19_ethnicity_23
ces19web %>%
  select(contains("_ethnicity_")) %>%
  val_labels()

ces19web %>%
  mutate(vismin=case_when(
    cps19_ethnicity_23==1~1,
    cps19_ethnicity_25==1~1,
    cps19_ethnicity_32==1~1,
    TRUE~0
  ))->ces19web
ces19web$cps19_ethnicity_41_TEXT
prop.table(table(ces19web$vismin))


#### recode political efficacy ####
#recode No Say (cps19_govt_say)
look_for(ces19web, "have any say")
ces19web$efficacy_internal<-Recode(as.numeric(ces19web$cps19_govt_say), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#val_labels(ces19web$efficacy_internal)<-c(low=0, high=1)
#checks
val_labels(ces19web$efficacy_internal)
table(ces19web$efficacy_internal)
table(ces19web$efficacy_internal, ces19web$cps19_govt_say , useNA = "ifany" )

#recode MPs lose touch (pes19_losetouch)
look_for(ces19web, "lose touch")
ces19web$efficacy_external<-Recode(as.numeric(ces19web$pes19_losetouch), "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces19web$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces19web$efficacy_external)
table(ces19web$efficacy_external)
table(ces19web$efficacy_external, ces19web$pes19_losetouch , useNA = "ifany" )

#recode Official Don't Care (pes19_govtcare)
look_for(ces19web, "care much")
ces19web$efficacy_external2<-Recode(as.numeric(ces19web$pes19_govtcare), "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces19web$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces19web$efficacy_external2)
#table(ces19web$efficacy_external2)
#table(ces19web$efficacy_external2, ces19web$pes19_govtcare , useNA = "ifany" )

ces19web %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces19web

ces19web %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
#qplot(ces19web$political_efficacy, geom="histogram")
table(ces19web$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces19web %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  psych::alpha(.)
#Check correlation
ces19web %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  cor(., use="complete.obs")

#recode efficacy rich (pes19_populism_8)
look_for(ces19web, "rich")
ces19web$efficacy_rich<-Recode(as.numeric(ces19web$pes19_populism_8), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; else=NA")
#checks
#table(ces19web$efficacy_rich)
table(ces19web$efficacy_rich, ces19web$pes19_populism_8)

#recode Break Promise (pes19_keepromises)
look_for(ces19web, "promise")
ces19web$promise<-Recode(as.numeric(ces19web$pes19_keepromises), "1=0; 2=0.5; 3=1; 4=1; 5=0.5; else=NA", as.numeric=T)
#val_labels(ces19web$promise)<-c(low=0, high=1)
#checks
#val_labels(ces19web$promise)
#table(ces19web$promise)
#table(ces19web$promise, ces19web$pes19_keepromises , useNA = "ifany" )

#recode Trust (pes19_trust)
look_for(ces19web, "trust")
ces19web$trust<-Recode(ces19web$pes19_trust, "1=1; 2=0; else=NA", as.numeric=T)
val_labels(ces19web$trust)<-c(no=0, yes=1)
#checks
#val_labels(ces19web$trust)
# table(ces19web$trust)
# table(ces19web$trust, ces19web$pes19_trust , useNA = "ifany" )

#recode duty (cps19_duty_choice )
look_for(ces19web, "duty")
ces19web$duty<-Recode(ces19web$cps19_duty_choice , "1=1; 2=0; else=NA")
val_labels(ces19web$duty)<-c(No=0, Yes=1)
#checks
val_labels(ces19web$duty)
table(ces19web$duty, ces19web$cps19_duty_choice, useNA="ifany")
ces19web$mode<-rep("Web", nrow(ces19web))
ces19web$election<-rep(2019, nrow(ces19web))

# table(ces19web$cps19_sector, ces19web$sector)
lookfor(ces19web, "vote")

ces19web$vote<-Recode(ces19web$pes19_votechoice2019, "1=1; 2=2; 3=3; 4=4; 5=5; 6=2; 7=0; 8:9=NA")
val_labels(ces19web$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)

lookfor(ces19web, "degree")
ces19web$degree<-Recode(ces19web$cps19_education, "1:8=0; 9:11=0; 12=NA")
val_labels(ces19web$degree)<-c(`nodegree`=0, `degree`=1)

ces19web$male<-Recode(ces19web$cps19_gender, "1=1; 2=0; 3=0")
val_labels(ces19web$male)<-c(`Male`=1, `Female`=0)

#glimpse(ces19web)
ces19web$degree
ces19web$vote
ces19web$male
lookfor(ces19web, "sector")
lookfor(ces19web, "employ")
ces19web$sector<-Recode(ces19web$cps19_sector, "1=0; 4=0; 2=1; 5=NA")
ces19web %>%
  mutate(sector=case_when(
    cps19_sector==1~0,
    cps19_sector==4~0,
    cps19_sector==2~1,
    cps19_sector==5~NA,
    cps19_employment>3&cps19_employment<13~ 0
  ))->ces19web
with(ces19web, table(cps19_sector, sector, useNA = "ifany"))
val_labels(ces19web$sector)<-c(`Public`=1, `Private`=0)
#### Income
lookfor(ces19web, "income")
ces19web$cps19_income_number

#Recode Income2 # Quintles
ces19web$income2<-Recode(ces19web$cps19_income_number, "0:57500=1;
57501:87500=2; 87501:125000=3;
       125001:187500=4; 187501:9999999999=5; else=NA")

val_labels(ces19web$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
ces19web$income2

#$55,000 to $59,999
#$85,000 to $89,999
#$120,000 to $129,999
#$175,000 to $199,999

# Tertiles
# $75,000 to $79,999
# $130,000 to $139,999
ces19web$income_tertile<-car::Recode(ces19web$cps19_income_number, "0:77500=1;
77501:135000=2; 135001:99999999=3;else=NA")

val_labels(ces19web$income_tertile)<-c(Lowest=1,  Middle=2, Highest=3)
with(ces19web, table(ces19web$cps19_sector, ces19web$sector, useNA = "ifany"))

#recode Quebec Accommodation (pes19_doneqc ) (Left=more accom)
look_for(ces19web, "quebec")
ces19web$quebec_accom<-Recode(as.numeric(ces19web$pes19_doneqc), "2=0.25; 1=0; 3=0.5; 4=0.75; 5=1; 6=0.5; else=NA")
#checks
table(ces19web$quebec_accom)

#### recode Quebec Sovereignty (cps19_quebec_sov) (Right=more sovereignty)
# look_for(ces19web, "sovereignty")
ces19web$quebec_sov<-Recode(as.numeric(ces19web$cps19_quebec_sov), "1=1; 2=0.75; 5=0.5; 3=0.25; 4=0; else=NA")
#checks
# table(ces19web$quebec_sov, ces19web$cps19_quebec_sov, useNA = "ifany" )
#val_labels(ces19web$quebec_sov)<-NULL

#### recode Environment vs Jobs
look_for(ces19web, "env")
ces19web$enviro<-Recode(as.numeric(ces19web$cps19_pos_jobs), "5=1; 4=0.75; 3=0.5; 2=0.25; 1=0; 6=0.5; else=NA")
#checks
table(ces19web$enviro , ces19web$cps19_pos_jobs , useNA = "ifany" )

#recode Previous Vote (cps19_vote_2015)
# look_for(ces19web, "vote")
ces19web$previous_vote<-Recode(ces19web$cps19_vote_2015, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; else=NA")
val_labels(ces19web$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$previous_vote)
table(ces19web$previous_vote)

# Save the file
save(ces19web, file=here("data/ces19web.rda"))
