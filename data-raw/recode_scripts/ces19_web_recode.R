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

#### recode occupation ####
ces19web %>%
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>%
  select(cps19_ResponseId, pes19_occ_text)
lookfor(ces19web, "occupation")
#Add 2016 NOC and 2021 NOC
source("data-raw/recode_scripts/ces19web_noc_recode.R")
#Note this creates the occupation variable FROM THE 2016 NOC CODES
# This code misses some responents
# ces19web$occupation<-Recode(as.numeric(ces19web$NOC), "0:1099=2;
# 1100:1199=1;
# 2100:2199=1;
#  3000:3199=1;
#  4000:4099=1;
#  4100:4199=1;
#  5100:5199=1;
#  1200:1599=3;
#  2200:2299=3;
#  3200:3299=3;
#  3400:3500=3;
#  4200:4499=3;
#  5200:5299=3;
#  6200:6399=3;
#  6400:6799=3; 7200:7399=4;
#                               7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
# val_labels(ces19web$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#var_label(ces19web$occupation)<-c("5 category class no self-employed from actual NOC codes")

# This code assigns respondents to a class category using the NOC21_4

ces19web$occupation<-Recode(ces19web$NOC21_4, as.numeric=T,"9500:9599=5;
       8500:8599=5;
       7500:7599=5;
       6500:6599=5;
       5500:6599=5;
       4500:4599=5;
       9400:9499=4;
       8400:8499=4;
       7400:7499=4;
       6400:6499=3;
       5400:5499=3;
       4400:4499=3;
       1400:1499=3;
       9300:9399=4;
       8300:8399=4;
       7300:7399=4;
       6300:6399=3;
       5300:5399=3;
       4300:4399=3;
       3300:3399=3;
       1300:1399=3;
       9200:9299=4;
       8200:8299=4;
       7200:7299=4;
       6200:6299=3;
       5200:5299=3;
       4200:4299=3;
       3200:3299=3;
       2200:2999=3;
       1200:1299=3;
       9000:9099=2;
       8000:8099=2;
       7000:7099=2;
       6000:6099=2;
       5000:5099=2;
       4000:4099=2;
       3000:3099=2;
       2000:2099=2;
       1000:1099=2;
       0000:0001=2;
       1100:1199=1;
       2100:2199=1;
       3100:3199=1;
       4100:4199=1;
       5100:5199=1;
       9999=NA;
       else=NA")

# ADd value labels
val_labels(ces19web$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)

# Add in self-employed
library(labelled)
lookfor(ces19web, "employed")

ces19web$occupation3<-ifelse(ces19web$cps19_employment==3, 6, ces19web$occupation)

# ADd value labels for occupation3
val_labels(ces19web$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)

#This code checks if we missed anything
# ces19web %>%
#   filter(is.na(NOC)==F&is.na(occupation)==T) %>%
#   select(NOC, occupation)
#
# table(ces19web$occupation, useNA = "ifany")
# ces19web %>%
#   mutate(occupation2=case_when(
#     #If occupation is not missing then return the value for occupation
#     is.na(occupation)==F ~ occupation,
#     #If occupoatuib is missing and categorical occupation is 2 then return managers
#     is.na(occupation)==T & pes19_occ_cat==18 ~ 2,
#     is.na(occupation)==T & pes19_occ_cat==19 ~ 1,
#     #This is questionable; Here we assign technician or associate professional to routine non-manual; could revisit
#     is.na(occupation)==T & pes19_occ_cat==20 ~ 3,
#     #Clerical Support Worker
#     is.na(occupation)==T & pes19_occ_cat==21 ~ 3,
#     #Service or Sales Workers
#     is.na(occupation)==T & pes19_occ_cat==22 ~ 3,
#     #Skilled agricultural, forestry or fishery
#     is.na(occupation)==T & pes19_occ_cat==23 ~ 4,
#     #Craft or related trades worker
#     is.na(occupation)==T & pes19_occ_cat==24 ~ 4,
#     #Plant Machine Operator
#     is.na(occupation)==T & pes19_occ_cat==25 ~ 5,
#     #Cleaner Labourer
#     is.na(occupation)==T & pes19_occ_cat==26 ~ 5,
#     TRUE ~ NA_real_
#   ))->ces19web



#var_label(ces19web$occupation2)<-c("5 category class no self-employed from pre-existing categories provided to R")

#### recode visible minority ####
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

#### recode Gender ####
ces19web$male<-Recode(ces19web$cps19_gender, "1=1; 2=0; 3=0")
val_labels(ces19web$male)<-c(`Male`=1, `Female`=0)

# recode Union Household (cps19web_union)
look_for(ces19web, "union")
ces19web$union<-Recode(ces19web$cps19_union, "1=1; 2=0; else=NA")
val_labels(ces19web$union)<-c(None=0, Union=1)
# Checks
val_labels(ces19web$union)
table(ces19web$union , useNA = "ifany" )

#Union Combined variable (identical copy of union) ### Respondent only
ces19web$union_both<-ces19web$union
#checks
val_labels(ces19web$union_both)
table(ces19web$union_both , useNA = "ifany" )

#### recode Education (cps19_education) ####
lookfor(ces19web, "degree")
ces19web$degree<-Recode(ces19web$cps19_education, "1:8=0; 9:11=1; 12=NA")
val_labels(ces19web$degree)<-c(`nodegree`=0, `degree`=1)

#recode Region (cps19_province)
look_for(ces19web, "province")
ces19web$region<-Recode(ces19web$cps19_province, "14:16=3; 17:18=1; 20=1; 23=1; 22=2; 25=3; else=NA")
val_labels(ces19web$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces19web$region)
table(ces19web$region , ces19web$cps19_province , useNA = "ifany" )

#recode Province (cps19_province)
# look_for(ces19web, "province")
ces19web$prov<-Recode(ces19web$cps19_province, "18=1; 23=2; 20=3; 17=4; 24=5; 22=6; 16=7; 25=8; 14=9; 15=10; else=NA")
val_labels(ces19web$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces19web$prov)
table(ces19web$prov)

#recode Quebec (cps19_province)
look_for(ces19web, "province")
ces19web$quebec<-Recode(ces19web$cps19_province, "14:18=0; 24=1; 20=0; 22:23=0; 26=0; else=NA")
val_labels(ces19web$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces19web$quebec)
table(ces19web$quebec, ces19web$cps19_province , useNA = "ifany" )

#recode Age (cps19_age)
look_for(ces19web, "age")
ces19web$age<-ces19web$cps19_age
#checks
table(ces19web$age)

#recode Religion (cps19_religion)
look_for(ces19web, "relig")
ces19web$religion<-Recode(ces19web$cps19_religion, "1:2=0; 3:7=3; 8:9=2; 10:11=1; 12:21=2; 22=3; else=NA")
val_labels(ces19web$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces19web$religion)
table(ces19web$religion , ces19web$cps19_religion , useNA = "ifany" )

#recode Language (cps19_Q_Language)
look_for(ces19web, "language")
ces19web$language<-Recode(ces19web$cps19_Q_Language, "'FR-CA'=0; 'EN'=1; else=NA")
val_labels(ces19web$language)<-c(French=0, English=1)
#checks
val_labels(ces19web$language)
table(ces19web$language)

#recode Non-charter Language (pes19_lang)
look_for(ces19web, "language")
ces19web$non_charter_language<-Recode(ces19web$pes19_lang, "68:69=1; 70:84=0; else=NA")
val_labels(ces19web$non_charter_language)<-c(Charter=0, Non_Charter=1)
table(as_factor(ces19web$pes19_lang),ces19web$non_charter_language , useNA = "ifany" )
#checks
val_labels(ces19web$non_charter_language)
table(ces19web$non_charter_language)

#recode Employment (cps19_employment)
look_for(ces19web, "employment")
ces19web$employment<-Recode(ces19web$cps19_employment, "4:8=0; 1:3=1; 9:11=1; else=NA")
val_labels(ces19web$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces19web$employment)
table(ces19web$employment , ces19web$cps19_employment , useNA = "ifany" )

#### recode Sector ####
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


#### recode Sector Health and Welfare ####
ces19web$NOC21_4
ces19web$cps19_sector
ces19web %>%
  mutate(sector_welfare=case_when(
    cps19_sector==1~0,
    cps19_sector==4~0,
    #Health
    cps19_sector==2&(NOC21_4>3000&NOC21_4<3311)~1,
    #Education, welfare social services  Managers
    cps19_sector==2&(NOC21_4>4001&NOC21_4<4004)~1,
    cps19_sector==2&(NOC21_4>3000&NOC21_4<3312)~1,
    #Teachers
    cps19_sector==2&(NOC21_4>4119&NOC21_4<4123)~1,
    #Teachers assistants
    cps19_sector==2&NOC21_4==4310~1,
    #Social Services
    cps19_sector==2&(NOC21_4>4129&NOC<4131)~1,
    #employment counsellors
    cps19_sector==2&NOC21_4==4132~1,
    #home workers
    cps19_sector==2&(NOC21_4==4410)~1,
    cps19_sector==5~NA,
    cps19_employment>3&cps19_employment<13~ 0
  ))->ces19web
with(ces19web, table(cps19_sector, sector_welfare, useNA = "ifany"))
val_labels(ces19web$sector)<-c(`Public`=1, `Private`=0)

#### recode SectorSecurity ####
ces19web %>%
  mutate(sector_security=case_when(
    cps19_sector==1~0,
    cps19_sector==4~0,
    #Police
    cps19_sector==2&(NOC>41309&NOC<41322)~1,
    #Sherrifs and correctional services
    cps19_sector==2&(NOC>43199&NOC<43202)~1,
    #Forces
    cps19_sector==2&(NOC==42102)~1,
    #Forces
    cps19_sector==2&(NOC==43204)~1,
    #Forces
    cps19_sector==2&(NOC==44200)~1,
    NOC==64410~1,
    cps19_sector==5~NA,
    cps19_employment>3&cps19_employment<13~ 0
  ))->ces19web

#### recode Income ####
lookfor(ces19web, "income")
ces19web$cps19_income_number

#recode Income (cps19_income_cat, cps19_income_number)
look_for(ces19web, "income")
ces19web %>%
  mutate(income=case_when(
    cps19_income_cat==1 | cps19_income_number> -1 & cps19_income_number < 30001 ~ 1,
    cps19_income_cat==2 | cps19_income_number> -1 & cps19_income_number < 30001 ~ 1,
    cps19_income_cat==3 | cps19_income_number> 30000 & cps19_income_number < 60001 ~ 2,
    cps19_income_cat==4 | cps19_income_number> 60000 & cps19_income_number < 90001 ~ 3,
    cps19_income_cat==5 | cps19_income_number> 90000 & cps19_income_number < 150001 ~ 4,
    cps19_income_cat==6 | cps19_income_number> 90000 & cps19_income_number < 150001 ~ 4,
    cps19_income_cat==7 | cps19_income_number> 150000 & cps19_income_number < 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ~ 5,
    cps19_income_cat==8 | cps19_income_number> 150000 & cps19_income_number < 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ~ 5,
  ))->ces19web

val_labels(ces19web$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces19web$income)
table(ces19web$income)

#Recode Income2 # Quintles
ces19web$income2<-Recode(ces19web$cps19_income_number, "0:57500=1;
57501:87500=2; 87501:125000=3;
       125001:187500=4; 187501:9999999999=5; else=NA")

val_labels(ces19web$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
ces19web$income2

# #$55,000 to $59,999
# #$85,000 to $89,999
# #$120,000 to $129,999
# #$175,000 to $199,999
#
# # Tertiles
# # $75,000 to $79,999
# # $130,000 to $139,999
# ces19web$income_tertile<-car::Recode(ces19web$cps19_income_number, "0:60000=1;
# 60001:115000=2; 115001:99999999=3;else=NA")

ces19web %>%
  mutate(income_tertile=case_when(
    cps19_income_cat==1 | cps19_income_number> -1 & cps19_income_number < 30001 ~ 1,
    cps19_income_cat==2 | cps19_income_number> -1 & cps19_income_number < 30001 ~ 1,
    cps19_income_cat==3 | cps19_income_number> 30000 & cps19_income_number < 60001 ~ 1,
    cps19_income_cat==4 | cps19_income_number> 60000 & cps19_income_number < 90001 ~ 2,
    cps19_income_cat==5 | cps19_income_number> 90000 & cps19_income_number < 110001 ~ 2,
    cps19_income_cat==6 | cps19_income_number> 110000 & cps19_income_number < 150001 ~ 3,
    cps19_income_cat==7 | cps19_income_number> 150000 & cps19_income_number < 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ~ 3,
    cps19_income_cat==8 | cps19_income_number> 150000 & cps19_income_number < 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ~ 3,
  ))->ces19web

val_labels(ces19web$income_tertile)<-c(Lowest=1,  Middle=2, Highest=3)
table(ces19web$income_tertile)
with(ces19web, table(ces19web$cps19_sector, ces19web$sector, useNA = "ifany"))

#### recode Household size (cps19_household)####
# look_for(ces19web, "household")
ces19web$household<-Recode(ces19web$cps19_household, "1=0.5; 2=1; 3=1.5; 4=2; 5=2.5; 6=3; 7=3.5; 8=4; 9=4.5; 10=5; 11=5.5; 12=6; 13=6.5; 15:501=7.5; else=NA")
#checks
# table(ces19web$household, useNA = "ifany" )

#recode Party ID (cps19_fed_id)
look_for(ces19web, "fed_id")
ces19web$party_id<-Recode(ces19web$cps19_fed_id, "1=1; 2=2; 3=3; 4=4; 5=5; 6=2; 7=0; 8:9=NA")
val_labels(ces19web$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$party_id)
table(ces19web$party_id, ces19web$cps19_fed_id , useNA = "ifany" )

#recode Party ID 2 (cps19_fed_id)
look_for(ces19web, "fed_id")
ces19web$party_id2<-Recode(ces19web$cps19_fed_id, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=0; 8:9=NA")
val_labels(ces19web$party_id2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$party_id2)
table(ces19web$party_id2, ces19web$cps19_fed_id , useNA = "ifany" )

#### recode Vote (pes21_votechoice2019) ####
lookfor(ces19web, "vote")
ces19web$vote<-Recode(ces19web$pes19_votechoice2019, "1=1; 2=2; 3=3; 4=4; 5=5; 6=2; 7=0; 8:9=NA")
val_labels(ces19web$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(ces19web$vote , ces19web$pes19_votechoice2019 , useNA = "ifany" )

#### recode Vote splitting Conservatives ####
ces19web$vote3<-Recode(ces19web$pes19_votechoice2019, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=0; 8:9=NA")
val_labels(ces19web$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PPC=6)
table(ces19web$vote3 , ces19web$pes19_votechoice2019 , useNA = "ifany" )

#recode Religiosity (cps19_rel_imp)
look_for(ces19web, "relig")
ces19web$religiosity<-Recode(ces19web$cps19_rel_imp, "1=5; 2=4; 5=3; 3=2; 4=1; else=NA")
val_labels(ces19web$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces19web$religiosity)
table(ces19web$religiosity, ces19web$cps19_rel_imp , useNA = "ifany")

#recode Community Size (pes19_rural_urban)
look_for(ces19web, "urban")
ces19web$size<-Recode(ces19web$pes19_rural_urban, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
val_labels(ces19web$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces19web$size)
table(ces19web$size, ces19web$pes19_rural_urban , useNA = "ifany" )

#recode Native-born (cps19_bornin_canada)
ces19web$native<-Recode(ces19web$cps19_bornin_canada, "1=1; 2=0; else=NA")
val_labels(ces19web$native)<-c(Foreign=0, Native=1)
#checks
val_labels(ces19web$native)
table(ces19web$native, ces19web$cps19_bornin_canada , useNA = "ifany" )

#recode Liberal leader
ces19web$liberal_leader<-Recode(as.numeric(ces19web$cps19_lead_rating_23), "-99=NA")
#checks
#ces19web$liberal_leader<-(ces19web$liberal_leader2 /100)
table(ces19web$liberal_leader)

#recode Conservative leader
ces19web$conservative_leader<-Recode(as.numeric(ces19web$cps19_lead_rating_24), "-99=NA")
#checks
#ces19web$conservative_leader<-(ces19web$conservative_leader2 /100)
table(ces19web$conservative_leader)

#recode NDP leader
ces19web$ndp_leader<-Recode(as.numeric(ces19web$cps19_lead_rating_25), "-99=NA")
#checks
#ces19web$ndp_leader<-(ces19web$ndp_leader2 /100)
table(ces19web$ndp_leader)

#recode Bloc leader
ces19web$bloc_leader<-Recode(as.numeric(ces19web$cps19_lead_rating_26), "-99=NA")
#checks
#ces19web$bloc_leader<-(ces19web$bloc_leader2 /100)
table(ces19web$bloc_leader)

#recode Green leader
ces19web$green_leader<-Recode(as.numeric(ces19web$cps19_lead_rating_27), "-99=NA")
#checks
#ces19web$green_leader<-(ces19web$green_leader2 /100)
table(ces19web$green_leader)

#recode PPC leader
ces19web$ppc_leader<-Recode(as.numeric(ces19web$cps19_lead_rating_28), "-99=NA")
#checks
#ces19web$ppc_leader<-(ces19web$ppc_leader2 /100)
table(ces19web$ppc_leader)

#recode Manage economy (cps19_issue_handle_8)
# look_for(ces19web, "economy")
ces19web$cps19_issue_handle_8
ces19web$manage_economy<-Recode(ces19web$cps19_issue_handle_8, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19web$manage_economy)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$manage_economy)
table(ces19web$manage_economy)

#recode Manage environment (cps19_issue_handle_3)
# look_for(ces19web, "environment")
ces19web$manage_environment<-Recode(ces19web$cps19_issue_handle_3, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19web$manage_environment)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$manage_environment)
table(ces19web$manage_environment)

#recode Manage immigration (cps19_issue_handle_7)
# look_for(ces19web, "immigration")
ces19web$cps19_issue_handle_7
ces19web$manage_immigration<-Recode(ces19web$cps19_issue_handle_7, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19web$manage_immigration)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$manage_immigration)
table(ces19web$manage_immigration)

#recode Addressing Main Issue (cps19_imp_iss_party)
# look_for(ces19web, "issue")
ces19web$address_issue<-Recode(ces19web$cps19_imp_iss_party, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19web$address_issue)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$address_issue)
table(ces19web$address_issue)

#recode Immigration (cps19_imm)
look_for(ces19web, "admit")
ces19web$immigration_rates<-Recode(as.numeric(ces19web$cps19_imm), "1=0; 2=1; 3=0.5; 4=0.5; else=NA", as.numeric=T)
#checks
table(ces19web$immigration_rates, ces19web$cps19_imm , useNA = "ifany" )

# recode Immigration sentiment (pes19_immigjobs)
look_for(ces19web, "immigr")
ces19web$immigration_job<-Recode(as.numeric(ces19web$pes19_immigjobs), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 6=0.5; else=NA", as.numeric=T)
#checks
table(ces19web$immigration_job, ces19web$pes19_immigjobs, useNA = "ifany" )

# #recode Environment (cps19_spend_env)
# look_for(ces19web, "enviro")
# ces19web$environment<-Recode(as.numeric(ces19web$cps19_spend_env), "3=0.5; 1=1; 2=0; else=NA")
# #val_labels(ces19web$environment)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
# #checks
# #val_labels(ces19web$environment)
# table(ces19web$environment, ces19web$cps19_spend_env , useNA = "ifany" )

#recode Redistribution (pes19_gap)
look_for(ces19web, "rich")
ces19web$redistribution<-Recode(as.numeric(ces19web$pes19_gap), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; else=NA", as.numeric=T)
#val_labels(ces19web$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
table(ces19web$redistribution)

#recode Pro-Redistribution
ces19web$pro_redistribution<-Recode(ces19web$pes19_gap, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces19web$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces19web$pro_redistribution)
table(ces19web$pro_redistribution)

#Calculate Cronbach's alpha
library(psych)

#recode Market Liberalism (pes21_privjobs and pes21_blame)
look_for(ces19web, "leave")
look_for(ces19web, "blame")

ces19web$market1<-Recode(as.numeric(ces19web$pes19_privjobs), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
ces19web$market2<-Recode(as.numeric(ces19web$pes19_blame), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#checks
table(ces19web$market1, ces19web$pes19_privjobs , useNA = "ifany" )
table(ces19web$market2, ces19web$pes19_blame , useNA = "ifany" )
ces19web$market1
ces19web %>%
  mutate(market_liberalism=rowMeans(select(., num_range("market", 1:2)), na.rm=T))->ces19web

ces19web %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
#qplot(ces19web$market_liberalism, geom="histogram")
table(ces19web$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19web %>%
  select(market1, market2) %>%
  alpha(.)
#For some reason the cronbach's alpha doesn't work here.
#Check correlation
# ces19web %>%
#   select(market1, market2) %>%
#   cor(., use="complete.obs")

#recode Capital Punishment (Missing)

#recode Crime (cps21_spend_just_law) (spending question)
look_for(ces19web, "crime")
ces19web$crime<-Recode(as.numeric(ces19web$cps19_spend_just_law), "1=0; 2=0; 3=1; else=NA")
#checks
table(ces19web$crime, ces19web$cps19_spend_just_law , useNA = "ifany" )

#recode Gay Rights (pes21_donegl) (should do more question)
look_for(ces19web, "gays")
ces19web$gay_rights<-Recode(as.numeric(ces19web$pes19_donegl), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19web$gay_rights, ces19web$pes19_donegl , useNA = "ifany" )

#recode Abortion (pes21_abort2)
look_for(ces19web, "abortion")
ces19web$abortion<-Recode(as.numeric(ces19web$pes19_abort2), "1=1; 2=0.5; 3=0; else=NA")
#checks
table(ces19web$abortion, ces19web$pes19_abort2 , useNA = "ifany" )

#recode Lifestyle (pes21_newerlife)
look_for(ces19web, "lifestyle")
ces19web$lifestyles<-Recode(as.numeric(ces19web$pes19_newerlife), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19web$lifestyles, ces19web$pes19_newerlife, useNA = "ifany")

#recode Women (pes19_womenhome)
look_for(ces19web, "women")
ces19web$stay_home<-Recode(as.numeric(ces19web$pes19_womenhome), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19web$stay_home, ces19web$pes19_womenhome, useNA = "ifany")

#recode Moral Trad (abortion, lifestyles, stay home, values, marriage, childen, morals)
ces19web$trad1<-ces19web$stay_home
ces19web$trad2<-ces19web$gay_rights
ces19web$trad3<-ces19web$abortion
table(ces19web$trad1)
table(ces19web$trad2)
table(ces19web$trad3)
ces19web %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", 1:3)), na.rm=T))->ces19web
#Check distribution of traditionalism
#qplot(ces19web$traditionalism, geom="histogram")
table(ces19web$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19web %>%
  select(trad1, trad2, trad3) %>%
  psych::alpha(.)
#Check correlation
ces19web %>%
  select(trad1, trad2, trad3) %>%
  cor(., use="complete.obs")

#recode Moral Traditionalism 2 (women & gay rights) (Left-Right)
ces19web %>%
  mutate(traditionalism2=rowMeans(select(., num_range("trad", 1:2)), na.rm=T))->ces19web

#Check distribution of traditionalism2
#qplot(ces19web$traditionalism2, geom="histogram")
table(ces19web$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
# ces19web %>%
#   select(trad1, trad2) %>%
#   psych::alpha(.)
# #Check correlation
# ces19web %>%
#   select(trad1, trad2) %>%
#   cor(., use="complete.obs")

#recode 2nd Dimension (lifestyles, immigration, gay rights, crime)
ces19web$author1<-ces19web$lifestyles
ces19web$author2<-ces19web$immigration_rates
ces19web$author3<-ces19web$gay_rights
ces19web$author4<-ces19web$crime
table(ces19web$author1)
table(ces19web$author2)
table(ces19web$author3)
table(ces19web$author4)

ces19web %>%
  mutate(authoritarianism=rowMeans(select(. ,num_range("author", 1:4)), na.rm=T))->ces19web

ces19web %>%
  select(starts_with("author")) %>%
  summary()
tail(names(ces19web))
#Check distribution of traditionalism
#qplot(ces19web$authoritarianism, geom="histogram")

#Calculate Cronbach's alpha
ces19web %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces19web %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")


#recode Personal Retrospective (cps19_own_fin_retro)
look_for(ces19web, "situation")
ces19web$personal_retrospective<-Recode(as.numeric(ces19web$cps19_own_fin_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces19web$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces19web$personal_retrospective)
table(ces19web$personal_retrospective , ces19web$cps19_own_fin_retro, useNA = "ifany" )

#recode National Retrospective (cps19_econ_retro)
look_for(ces19web, "economy")
ces19web$national_retrospective<-Recode(as.numeric(ces19web$cps19_econ_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces19web$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces19web$national_retrospective)
table(ces19web$national_retrospective, ces19web$cps19_econ_retro, useNA = "ifany" )

#recode Education (cps19_spend_educ)
look_for(ces19web, "education")
ces19web$education_spend<-Recode(as.numeric(ces19web$cps19_spend_educ), "1=0; 3=1; 2=0.5; 4=0.5; else=NA")
#val_labels(ces19web$education)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
#val_labels(ces19web$education)
table(ces19web$education_spend, ces19web$cps19_spend_educ , useNA = "ifany" )

#recode Ideology (cps19_lr_scale_bef_1)
look_for(ces19web, "scale")
ces19web$ideology<-Recode(as.numeric(ces19web$cps19_lr_scale_bef_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; -99=NA; else=NA")
#val_labels(ces19web$ideology)<-c(Left=0, Right=1)
#checks
#val_labels(ces19web$ideology)
table(ces19web$ideology, ces19web$cps19_lr_scale_bef_1 , useNA = "ifany" )

# recode turnout (pes19_turnout2019 & pes19_turnout2019_v2)
look_for(ces19web, "turnout")
ces19web %>%
  mutate(turnout=case_when(
    pes19_turnout2019==1 ~1,
    pes19_turnout2019==2 ~0,
    pes19_turnout2019==3 ~0,
    pes19_turnout2019==4 ~0,
    pes19_turnout2019==5 ~0,
    pes19_turnout2019==6 ~0,
    pes19_turnout2019==8 ~NA_real_ ,
    pes19_turnout2019_v2==1 ~1,
    pes19_turnout2019_v2==2 ~0,
    pes19_turnout2019_v2==3 ~0,
    pes19_turnout2019_v2==4 ~NA_real_ ,
  ))->ces19web
val_labels(ces19web$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces19web$turnout)
table(ces19web$turnout)
table(ces19web$turnout, ces19web$vote)

# recode satisfaction with democracy (cps19_demsat & pes19_dem_sat)
look_for(ces19web, "dem")
ces19web$satdem<-Recode(as.numeric(ces19web$pes19_dem_sat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces19web$satdem, ces19web$pes19_dem_sat, useNA = "ifany" )

ces19web$satdem2<-Recode(as.numeric(ces19web$cps19_demsat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces19web$satdem2, ces19web$cps19_demsat, useNA = "ifany" )

#recode Postgrad (cps19_education)
look_for(ces19web, "education")
ces19web$postgrad<-Recode(as.numeric(ces19web$cps19_education), "10:11=1; 1:9=0; else=NA")
#checks
table(ces19web$postgrad)

#recode Perceive Inequality (pes19_inequal)
look_for(ces19web, "ineq")
ces19web$inequality<-Recode(as.numeric(ces19web$pes19_inequal), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; else=NA")
#checks
table(ces19web$inequality)
table(ces19web$inequality, ces19web$pes19_inequal)

# recode political interest (cps19_interest_gen_1)
look_for(ces19web, "interest")
ces19web$pol_interest<-Recode(as.numeric(ces19web$cps19_interest_gen_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
table(ces19web$pol_interest, ces19web$cps19_interest_gen_1, useNA = "ifany" )

# recode foreign born (cps19_bornin_canada)
look_for(ces19web, "born")
ces19web$foreign<-Recode(ces19web$cps19_bornin_canada, "1=0; 2=1; else=NA")
val_labels(ces19web$foreign)<-c(No=0, Yes=1)
#checks
#val_labels(ces19web$foreign)
table(ces19web$foreign, ces19web$cps19_bornin_canada, useNA="ifany")

# recode Environment Spend (cps19_spend_env)
look_for(ces19web, "env")
ces19web$enviro_spend<-Recode(as.numeric(ces19web$cps19_spend_env), "1=0; 2=0.5; 3=1; else=NA")
#checks
table(ces19web$enviro_spend , ces19web$cps19_spend_env , useNA = "ifany" )

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
#table(ces19web$efficacy_externxal2)
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

#### recode Women - how much should be done (pes19_donew) #### (coded above for moral trad)
look_for(ces19web, "women")
table(ces19web$pes19_donew)
ces19web$women<-Recode(as.numeric(ces19web$pes19_donew), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19web$women,  useNA = "ifany")

#### recode Race - how much should be done (pes19_donerm) ####
look_for(ces19web, "racial")
table(ces19web$pes19_donerm)
ces19web$race<-Recode(as.numeric(ces19web$pes19_donerm), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19web$race,  useNA = "ifany")

#recode Previous Vote (cps19_vote_2015)
# look_for(ces19web, "vote")
ces19web$previous_vote<-Recode(ces19web$cps19_vote_2015, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; else=NA")
val_labels(ces19web$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19web$previous_vote)
table(ces19web$previous_vote)

#recode Provincial Vote (pes19_provvote)
# look_for(ces19web, "vote")
ces19web$prov_vote<-car::Recode(as.numeric(ces19web$pes19_provvote), "281=1; 292=2; 290=2; 288=2; 282=3; 285=4; 293=0; 284=10; 291=7; 289=0; 286=11; 283=5; 298=0; 295=0; else=NA")
val_labels(ces19web$prov_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, PQ=4, Green=5, Reform=6, Sask=7,
                                    ADQ=8, Wildrose=9, CAQ=10, QS=11)
#checks
val_labels(ces19web$prov_vote)
table(ces19web$prov_vote)

#### recode Homeowner(cps19_property_1) ####
look_for(ces19web, "home")
ces19web %>%
  mutate(homeowner=case_when(
    cps19_property_1==1 ~1,
    cps19_property_5==1 ~0,
    cps19_property_6==1 ~NA_real_ ,
    cps19_property_2==1 ~0,
    cps19_property_3==1 ~0,
    cps19_property_4==1 ~0,
  ))->ces19web
#checks
table(ces19web$homeowner, ces19web$cps19_property_1, useNA = "ifany")

#### Business tax (pes19_taxes_2) ####
look_for(ces19web, "tax")
table(ces19web$pes19_taxes_2)
ces19web$business_tax<-Recode(as.numeric(ces19web$pes19_taxes_2), "1:2:=0; 4:5=1; 3=0.5; 6=0.5; else=NA")
#checks
table(ces19web$business_tax,  ces19web$pes19_taxes_2, useNA = "ifany")

#### recode feminism (cps19_groups_therm_4)
ces19web$feminism_rating<-Recode(as.numeric(ces19web$cps19_groups_therm_4 /100), "-99=NA")
table(ces19web$feminism_rating)

# Save the file
save(ces19web, file=here("data/ces19web.rda"))
