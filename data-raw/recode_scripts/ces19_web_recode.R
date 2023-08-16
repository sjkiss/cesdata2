#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
#load data
if (!file.exists(here("data/ces19web.rda"))) {
  #If it does not exist read in the original raw data file
  ces19web<-read_sav(file=here("data-raw/CES-E-2019-online_F1_character_encodings_fixed.sav"))
  #Note, if this is the case, the entire Recode script must be uncommented and run
  return(ces19web)
} else {
  load("data/ces19web.rda")
}

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
table(is.na(ces19web$NOC))



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
ces19web$efficacy_internal<-Recode(ces19web$cps19_govt_say, "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
val_labels(ces19web$efficacy_internal)<-c(low=0, high=1)
#checks
val_labels(ces19web$efficacy_internal)
table(ces19web$efficacy_internal)
table(ces19web$efficacy_internal, ces19web$cps19_govt_say , useNA = "ifany" )

#recode MPs lose touch (pes19_losetouch)
look_for(ces19web, "lose touch")
ces19web$efficacy_external<-Recode(ces19web$pes19_losetouch, "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
val_labels(ces19web$efficacy_external)<-c(low=0, high=1)
#checks
val_labels(ces19web$efficacy_external)
table(ces19web$efficacy_external)
table(ces19web$efficacy_external, ces19web$pes19_losetouch , useNA = "ifany" )

#recode Official Don't Care (pes19_govtcare)
look_for(ces19web, "care much")
ces19web$efficacy_external2<-Recode(ces19web$pes19_govtcare, "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
val_labels(ces19web$efficacy_external2)<-c(low=0, high=1)
#checks
val_labels(ces19web$efficacy_external2)
table(ces19web$efficacy_external2)
table(ces19web$efficacy_external2, ces19web$pes19_govtcare , useNA = "ifany" )

ces19web %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces19web

ces19web %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
qplot(ces19web$political_efficacy, geom="histogram")
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
ces19web$efficacy_rich<-Recode(ces19web$pes19_populism_8, "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; else=NA")
#checks
table(ces19web$efficacy_rich)
table(ces19web$efficacy_rich, ces19web$pes19_populism_8)

#recode Break Promise (pes19_keepromises)
look_for(ces19web, "promise")
ces19web$promise<-Recode(ces19web$pes19_keepromises, "1=0; 2=0.5; 3=1; 4=1; 5=0.5; else=NA", as.numeric=T)
val_labels(ces19web$promise)<-c(low=0, high=1)
#checks
val_labels(ces19web$promise)
table(ces19web$promise)
table(ces19web$promise, ces19web$pes19_keepromises , useNA = "ifany" )

#recode Trust (pes19_trust)
look_for(ces19web, "trust")
ces19web$trust<-Recode(ces19web$pes19_trust, "1=1; 2=0; else=NA", as.numeric=T)
val_labels(ces19web$trust)<-c(no=0, yes=1)
#checks
val_labels(ces19web$trust)
# table(ces19web$trust)
# table(ces19web$trust, ces19web$pes19_trust , useNA = "ifany" )

# Save the file
save(ces0411, file=here("data/ces0411.rda"))
