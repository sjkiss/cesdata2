# This script loads and recodes CES25 Kiss module
library(haven)
library(here)
library(tidyverse)
library(srvyr)
library(survey)

#Load data
ces25b<-read_dta(here("data-raw/CES 25 Kiss Module Final (with occupation & Additional Qs).dta"))
library(labelled)
library(car)
look_for(ces25b, "class")
look_for(ces25b, "vote")
look_for(ces25b, "vote")

#### recode Gender ####
ces25b$male<-Recode(ces25b$cps25_genderid, "1=1; 2=0; else=NA")
val_labels(ces25b$male)<-c(Female=0, Male=1)
#checks
val_labels(ces25b$male)
table(ces25b$male)

# recode Union Household (cps25_union)
ces25b$union<-Recode(ces25b$cps25_union, "1=1; 2=0; else=NA")
val_labels(ces25b$union)<-c(None=0, Union=1)
# Checks
val_labels(ces25b$union)
table(ces25b$union , useNA = "ifany" )

#Union Combined variable (identical copy of union) ### Respondent only
ces25b$union_both<-ces25b$union
#checks
val_labels(ces25b$union_both)
table(ces25b$union_both , useNA = "ifany" )

#recode Education (cps25_education)
ces25b$degree<-Recode(ces25b$cps25_education, "9:11=1; 1:8=0; else=NA")
val_labels(ces25b$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces25b$degree)
table(ces25b$degree, ces25b$cps25_education , useNA = "ifany" )

#recode Region (cps25_province)
look_for(ces25b, "province")
ces25b$region<-Recode(ces25b$cps25_province, "1:3=3; 4:5=1; 7=1; 9=2; 10=1; 12=3; else=NA")
val_labels(ces25b$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces25b$region)
table(ces25b$region , ces25b$cps25_province , useNA = "ifany" )

#### Create province and quebec variable ####
ces25b$cps25_province
ces25b$prov<-Recode(ces25b$cps25_province, "5=1; 10=2; 7=3; 4=4; 11=5; 9=6; 3=7; 12=8; 1=9; 2=10; else=NA")
val_labels(ces25b$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
table(ces25b$cps25_province)
table(ces25b$prov)
val_labels(ces25b$cps25_province)
ces25b$quebec<-Recode(ces25b$cps25_province, "1:5=0; 11=1; 7=0;9:10=0;12=0; else=NA")
val_labels(ces25b$quebec)<-c(Other=0, Quebec=1)
table(ces25b$quebec)

#### recode Age (cps25_age_in_years) ####
look_for(ces25b, "age")
ces25b$age<-ces25b$cps25_age_in_years
#checks
table(ces25b$age)

#recode Religion (cps25_religion)
look_for(ces25b, "relig")
ces25b$religion<-Recode(ces25b$cps25_religion, "1:2=0; 3:7=3; 8:9=2; 10:11=1; 12:21=2; 22=3; else=NA")
val_labels(ces25b$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces25b$religion)
table(ces25b$religion , ces25b$cps25_religion , useNA = "ifany" )

#recode Language (cps25_UserLanguage)
look_for(ces25b, "language")
ces25b$language<-Recode(ces25b$cps25_UserLanguage, "'FR-CA'=0; 'EN'=1; else=NA")
val_labels(ces25b$language)<-c(French=0, English=1)
#checks
val_labels(ces25b$language)
table(ces25b$language)

# #recode Non-charter Language (pes21_lang)
# look_for(ces21, "language")
# ces21$non_charter_language<-Recode(ces21$pes21_lang, "1:2=1; 3:18=0; else=NA")
# val_labels(ces21$non_charter_language)<-c(Charter=0, Non_Charter=1)
# table(as_factor(ces21$pes21_lang),ces21$non_charter_language , useNA = "ifany" )
# #checks
# val_labels(ces21$non_charter_language)
# table(ces21$non_charter_language)

#recode Employment (cps25_employment)
look_for(ces25b, "employment")
ces25b$employment<-Recode(ces25b$cps25_employment, "4:8=0; 1:3=1; 9:11=1; else=NA")
val_labels(ces25b$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces25b$employment)
table(ces25b$employment , ces25b$cps25_employment , useNA = "ifany" )

# #recode Sector (cps25_sector)
# lookfor(ces25b, "sector")
# ces25b$sector<-Recode(ces25b$cps25_sector, "1=0; 3=0; 2=1; else=NA")
# val_labels(ces25b$sector)<-c(`Public`=1, `Private`=0)
# #checks
# val_labels(ces25b$sector)
# table(ces25b$sector , ces25b$cps25_sector , useNA = "ifany" )

#### Create party vote ####
ces25b$vote<-Recode(ces25b$cps25_votechoice , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 8=2; else=NA")
val_labels(ces25b$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(as_factor(ces25b$vote))

#### Create party vote - splitting out PPC from Cons ####
ces25b$vote3<-Recode(ces25b$cps25_votechoice , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 8=6; else=NA")
val_labels(ces25b$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PPC=6)
table(as_factor(ces25b$vote3))

#### Create party ID####
ces25b$partyid<-Recode(ces25b$cps25_fed_id , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 9=2; else=NA")
val_labels(ces25b$partyid)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(as_factor(ces25b$partyid))

#### Create party ID - splitting out PPC from Cons ####
ces25b$partyid2<-Recode(ces25b$cps25_fed_id , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 9=6; else=NA")
val_labels(ces25b$partyid2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PPC=6)
table(as_factor(ces25b$partyid2))

#### Create income ####
lookfor(ces25b, "income")
# This is a hacky way to assign respondents to income tertiles
# convert original income to numeric
ces25b$cps25_income2<-as.numeric(ces25b$cps25_income)

# Set don't knows to missing
# We could set these to the median 4 if we want to rescue missing data
# talk to matt (Matt believes it best to leave the NA's out for income as its not that many people)
ces25b %>%
  mutate(cps25_income2=case_when(
    cps25_income==9~ NA_integer_,
    TRUE~as.numeric(cps25_income)
  ))->ces25b
#weight the survey
ces25b_des<-as_survey_design(subset(ces25b, !is.na(cps25_weight_kiss_module)), weights=cps25_weight_kiss_module)
#now calculate the income tertiles
ces25b_des %>%
  summarise(income_tertile=survey_quantile(cps25_income2, c(0.33,0.66)))

# tertiles are 3 and 6
#assignh income values of 3 and below to 1
# between 3 and 7to be middle
# set 7 and above to be highest
ces25b %>%
  mutate(income_tertile=case_when(
    cps25_income2<4~1,
    cps25_income2>3&cps25_income2<7~2,
    cps25_income2>6~3,
      ))->ces25b
#reweight the ces25b file now that the income tertile has been assigned
ces25b_des<-as_survey_design(subset(ces25b, !is.na(cps25_weight_kiss_module)), weights=cps25_weight_kiss_module)
# check the income tertiles after weighting
svytable(~income_tertile, design=ces25b_des)

#recode Income Quintile (cps21_income_cat & cps21_income_number)
look_for(ces25b, "income")
ces25b %>%
  mutate(income=case_when(
    cps25_income==1 ~ 1,
    cps25_income==2 ~ 1,
    cps25_income==3 ~ 2,
    cps25_income==4 ~ 3,
    cps25_income==5 ~ 4,
    cps25_income==6 ~ 4,
    cps25_income==7 ~ 5,
    cps25_income==8 ~ 5,
  ))->ces25b
val_labels(ces25b$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces25b$income)
table(ces25b$income)

#### Create Class ####
#Convert the 5 digit NOC code provided by CDEM to a number
ces25b$NOC21_5<-as.numeric(ces25b$occupation_code)
#Note, that this will turn some 5 digit codes that start with 0000 into 2-digit codes
min(nchar(ces25b$NOC21_5), na.rm=T)
lookfor(ces25b, "employ")
# Class variable
ces25b %>%
  mutate(occupation=case_when(
    (cps25_employment<3|cps25_employment==9)&NOC21_5 <00016~ 2,
    # Major group 10
    (cps25_employment<3|cps25_employment==9)&NOC21_5 >10000&NOC21_5<10999~2,
    #Major Group 20
    (cps25_employment<3|cps25_employment==9)&NOC21_5>20000&NOC21_5<20999~2,
    #Major Group 30
    (cps25_employment<3|cps25_employment==9)&NOC21_5>30000&NOC21_5<30999~2,
    #Major Group 40
    (cps25_employment<3|cps25_employment==9)&NOC21_5>40000&NOC21_5<40999~2,
    #Major Group 50
    (cps25_employment<3|cps25_employment==9)&NOC21_5>50000&NOC21_5<50999~2,
    #Major Group 60
    (cps25_employment<3|cps25_employment==9)&NOC21_5>60000&NOC21_5<60999~2,
    #Major Group 70
    (cps25_employment<3|cps25_employment==9)& NOC21_5>70000&NOC21_5<70999~2,
    (cps25_employment<3|cps25_employment==9)&#Major Group 80
      (cps25_employment<3|cps25_employment==9)&NOC21_5>80000&NOC21_5<80999~2,
    #Major Group 90
    (cps25_employment<3|cps25_employment==9)&NOC21_5>90000&NOC21_5<90999~2,
    #Major Group 11
    (cps25_employment<3|cps25_employment==9)&NOC21_5>11000&NOC21_5<11999~1,
    #Major Group 21
    (cps25_employment<3|cps25_employment==9)&NOC21_5>21000&NOC21_5<21999~1,
    #Major Group 31
    (cps25_employment<3|cps25_employment==9)&NOC21_5>31000&NOC21_5<31999~1,
    #Major Group 41
    (cps25_employment<3|cps25_employment==9)&NOC21_5>41000&NOC21_5<41999~1,
    #Major Group 51
    (cps25_employment<3|cps25_employment==9)&NOC21_5>51000&NOC21_5<51999~1,
    #Major Group 12
    (cps25_employment<3|cps25_employment==9)&NOC21_5>12000&NOC21_5<12999~1,
    #Major Group 22
    (cps25_employment<3|cps25_employment==9)&NOC21_5>22000&NOC21_5<22999~3,
    #Major Group 32
    (cps25_employment<3|cps25_employment==9)&NOC21_5>32000&NOC21_5<32999~3,
    #Major Group 42
    (cps25_employment<3|cps25_employment==9)& NOC21_5>42000&NOC21_5<42999~3,
    #Major Group 52
    (cps25_employment<3|cps25_employment==9)&NOC21_5>52000&NOC21_5<52999~3,
    #Major Group 62
    (cps25_employment<3|cps25_employment==9)&NOC21_5>62000&NOC21_5<62999~3,
    #Major Group 72
    (cps25_employment<3|cps25_employment==9)&NOC21_5>72000&NOC21_5<72999~4,
    #Major Group 82
    (cps25_employment<3|cps25_employment==9)&NOC21_5>82000&NOC21_5<82999~4,
    #Major Group 92
    (cps25_employment<3|cps25_employment==9)& NOC21_5>92000&NOC21_5<92999~4,
    #Major Group 13
    (cps25_employment<3|cps25_employment==9)&NOC21_5>13000&NOC21_5<13999~3,
    #Major Group 33
    (cps25_employment<3|cps25_employment==9)&NOC21_5>14000&NOC21_5<14999~3,
    #Major Group 43
    (cps25_employment<3|cps25_employment==9)&NOC21_5>43000&NOC21_5<43999~3,
    #Major Group 53
    (cps25_employment<3|cps25_employment==9)&NOC21_5>53000&NOC21_5<53999~3,
    #Major Group 63
    (cps25_employment<3|cps25_employment==9)& NOC21_5>63000&NOC21_5<63999~3,
    #Major Group 73
    (cps25_employment<3|cps25_employment==9)&NOC21_5>73000&NOC21_5<73999~4,
    #Major Group 83
    (cps25_employment<3|cps25_employment==9)&NOC21_5>83000&NOC21_5<83999~4,
    #Major Group 93
    (cps25_employment<3|cps25_employment==9)&NOC21_5>93000&NOC21_5<93999~4,
    #Major Group 14
    (cps25_employment<3|cps25_employment==9)& NOC21_5>14000&NOC21_5<14999~3,
    #Major Group 44
    (cps25_employment<3|cps25_employment==9)&NOC21_5>44000&NOC21_5<44999~3,
    #Major Group 54
    (cps25_employment<3|cps25_employment==9)&NOC21_5>54000&NOC21_5<54999~3,
    #Major Group 64
    (cps25_employment<3|cps25_employment==9)&NOC21_5>64000&NOC21_5<64999~3,
    #Major Group 74
    (cps25_employment<3|cps25_employment==9)&NOC21_5>74000&NOC21_5<74999~5,
    #Major Group 84
    (cps25_employment<3|cps25_employment==9)&NOC21_5>84000&NOC21_5<84999~5,
    #Major Group 94
    (cps25_employment<3|cps25_employment==9)&NOC21_5>94000&NOC21_5<94999~5,
    #Major Group 45
    (cps25_employment<3|cps25_employment==9)&NOC21_5>45000&NOC21_5<45999~4,
    #Major Group 55
    (cps25_employment<3|cps25_employment==9)&NOC21_5>55000&NOC21_5<55999~4,
    #Major Group 65
    (cps25_employment<3|cps25_employment==9)& NOC21_5>65000&NOC21_5<65999~4,
    #Major Group 75
    (cps25_employment<3|cps25_employment==9)&NOC21_5>75000&NOC21_5<75999~5,
    #Major Group 85
    (cps25_employment<3|cps25_employment==9)&NOC21_5>85000&NOC21_5<85999~5,
    #Major Group 95
    (cps25_employment<3|cps25_employment==9)& NOC21_5>95000&NOC21_5<95999~5
  ))->ces25b

# ADd value labels
val_labels(ces25b$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)

# Make occupation3 as self-employed and unskilled and skilled grouped together
library(labelled)

#lookfor(ces21, "employed")
ces25b$occupation3<-ifelse(ces25b$cps25_employment==3, 6, ces25b$occupation)

# Add value labels for occupation3
val_labels(ces25b$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)

# Create Oesch variable

#Check, did I get everyone?
ces25b %>%
  filter(is.na(occupation3)) %>%
  select(cps25_employment, occupation_name, occupation3) %>%
  as_factor()
ces25b$NOC21_5

#code logic of authority

#First extract the first two digits of each NOC

ces25b$sector_teer<-str_extract_all(ces25b$NOC21_5, "^\\d{2}") %>% unlist()
#Now separate
ces25b %>%
  separate_wider_position(., cols=sector_teer, widths=c("sector"=1, "teer"=1))->ces25b
# Check employment status
lookfor(ces25b, "status")
#Create working variable
ces25b %>%
  mutate(working=case_when(
    cps25_employment<4|(cps25_employment>8&cps25_employment<12)~1,
    TRUE~0)
  )->ces25b
ces25b %>%
  mutate(logic=case_when(
   working==1&cps25_employment!=3 &sector==0~"Organizational",
   working==1&cps25_employment!=3&sector==1~"Organizational",
   working==1&cps25_employment!=3&sector==2~"Technical",
   working==1&cps25_employment!=3&sector==3~"Interpersonal",
   working==1&cps25_employment!=3&sector==4~"Interpersonal",
   working==1&cps25_employment!=3&sector==5~"Interpersonal",
   working==1&cps25_employment!=3&sector==6~"Interpersonal",
   working==1&cps25_employment!=3&sector==7~"Technical",
   working==1&cps25_employment!=3&sector==8~"Technical",
   working==1&cps25_employment!=3&sector==8~"Technical",
   working==1&cps25_employment!=3&sector==9~"Technical"
  ))->ces25b
table(ces25b$logic)

# Introduce level of authority for the 8-class schema
#Note that Rehm and Kitchelt have four gradations here; Oesch has only two.
ces25b %>%
  mutate(authority=case_when(
    working==1&cps25_employment!=3&teer<3~"Higher",
    working==1&cps25_employment!=3&teer>2~"Lower"
  ))->ces25b
table(ces25b$authority)
ces25b %>%
  select(cps25_employment, teer, authority) %>%
  count(cps25_employment, teer, authority)

#Check most frequent self-employed
ces25b %>%
  filter(cps25_employment==3) %>%
  select(NOC21_5) %>%
  count(NOC21_5)

#Note, most doctors are not self-employed
ces25b %>%
  filter(NOC21_5==31102) %>%
  count(cps25_employment)

ces25b %>%
  mutate(occupation_oesch=case_when(
    logic=="Technical"& authority=="Higher"~'Technical Professionals',
    logic=="Organizational"&authority=="Higher"~'(Associate) Managers',
    logic=="Interpersonal"&authority=="Higher"~'Socio-cultural (semi) Professionals',
    logic=="Technical"&authority=="Lower"~'Production workers',
    logic=="Organizational"&authority=="Lower"~'Office clerks',
    logic=="Interpersonal"&authority=="Lower"~'Service workers'
  ))->ces25b

ces25b %>%
  mutate(occupation_oesch=case_when(
    cps25_employment==3~"Self-employed",
    TRUE~ occupation_oesch
  ))->ces25b
table(ces25b$logic)
table(ces25b$authority)
table(ces25b$occupation_oesch, useNA = "ifany")

#This code checks to see that everyone with an NOC21_5 has also been given an oesch
ces25b %>%
  filter(working==1) %>%
  select(NOC21_5, occupation_oesch) %>% filter(!is.na(!NOC21_5)&is.na(occupation_oesch))
table(ces25b$occupation_oesch)
#How many non-missing oesch do we hvave
table(is.na(ces25b$occupation_oesch)) #1999

#How many more will we get from the open-ended
ces25b %>%
  filter(working==1) %>%
  filter(occupation_code=="") %>%
  select(occupation_code, NOC21_5, pes25_occ_select_2) %>%
  filter(pes25_occ_select_2!="-99"&pes25_occ_select_2!="") %>%
  nrow()

# check self-employed
table(as_factor(ces25b$cps25_employment))
with(ces25b, table(cps25_employment, occupation_oesch))

# Calculate Oesch-5
ces25b %>%
  mutate(occupation_oesch_6=case_when(
    teer==0~"Managers",
    teer==1~"Professionals",
    teer==2~"Semi-Professionals Associate Managers",
    teer==3~"Skilled Workers",
    teer>3~"Unskilled Workers",
    cps25_employment==3~"Self-employed"
  ))->ces25b
table(ces25b$occupation_oesch_6)
ces25b$occupation_oesch_6<-factor(ces25b$occupation_oesch_6, levels=c("Unskilled Workers", "Skilled Workers",
                                           "Semi-Professionals Associate Managers",
                                           "Self-employed","Professionals", "Managers"))

#recode Religiosity (ces25_rel_imp)
look_for(ces25b, "relig")
ces25b$religiosity<-Recode(ces25b$cps25_rel_imp, "1=5; 2=4; 5=3; 3=2; 4=1; else=NA")
val_labels(ces25b$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces25b$religiosity)
table(ces25b$religiosity, ces25b$cps25_rel_imp , useNA = "ifany")

# #recode Community Size (ces25_rural_urban)
# look_for(ces25b, "urban")
# ces25b$size<-Recode(ces25b$pes21_rural_urban, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
# val_labels(ces25b$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
# #checks
# val_labels(ces25b$size)
# table(ces25b$size, ces25b$pes21_rural_urban , useNA = "ifany" )

#recode Native-born (cps25_bornin_canada)
ces25b$native<-Recode(ces25b$cps25_bornin_canada, "1=1; 2=0; else=NA")
val_labels(ces25b$native)<-c(Foreign=0, Native=1)
#checks
val_labels(ces25b$native)
table(ces25b$native, ces25b$cps25_bornin_canada , useNA = "ifany" )

#recode Liberal leader
ces25b$liberal_leader<-Recode(as.numeric(ces25b$cps25_lead_rating_23), "-99=NA")
#checks
#ces25b$liberal_leader<-(ces25b$liberal_leader2 /100)
table(ces25b$liberal_leader)

#recode Conservative leader
ces25b$conservative_leader<-Recode(as.numeric(ces25b$cps25_lead_rating_24), "-99=NA")
#checks
#ces25b$conservative_leader<-(ces25b$conservative_leader2 /100)
table(ces25b$conservative_leader)

#recode NDP leader
ces25b$ndp_leader<-Recode(as.numeric(ces25b$cps25_lead_rating_25), "-99=NA")
#checks
#ces25b$ndp_leader<-(ces25b$ndp_leader2 /100)
table(ces25b$ndp_leader)

#recode Bloc leader
ces25b$bloc_leader<-Recode(as.numeric(ces25b$cps25_lead_rating_26), "-99=NA")
#checks
#ces25b$bloc_leader<-(ces25b$bloc_leader2 /100)
table(ces25b$bloc_leader)

#recode Green leader
ces25b$green_leader<-Recode(as.numeric(ces25b$cps25_lead_rating_27), "-99=NA")
#checks
#ces25b$green_leader<-(ces25b$green_leader2 /100)
table(ces25b$green_leader)

#recode PPC leader
ces25b$ppc_leader<-Recode(as.numeric(ces25b$cps25_lead_rating_29), "-99=NA")
#checks
#ces25b$ppc_leader<-(ces25b$ppc_leader2 /100)
table(ces25b$ppc_leader)

#recode Liberal rating
look_for(ces25b, "parties")
ces25b$liberal_rating<-Recode(as.numeric(ces25b$cps25_party_rating_23), "-99=NA")
table(ces25b$liberal_rating)

#recode Conservative rating
ces25b$conservative_rating<-Recode(as.numeric(ces25b$cps25_party_rating_24), "-99=NA")
table(ces25b$conservative_rating)

#recode NDP rating
ces25b$NDP_rating<-Recode(as.numeric(ces25b$cps25_party_rating_25), "-99=NA")
table(ces25b$NDP_rating)

#recode Bloc rating
ces25b$bloc_rating<-Recode(as.numeric(ces25b$cps25_party_rating_26), "-99=NA")
table(ces25b$bloc_rating)

#recode Green rating
ces25b$green_rating<-Recode(as.numeric(ces25b$cps25_party_rating_27), "-99=NA")
table(ces25b$green_rating)

#recode PPC rating
ces25b$ppc_rating<-Recode(as.numeric(ces25b$cps25_party_rating_29), "-99=NA")
table(ces25b$ppc_rating)

#### recode Redistribution (kiss_module_Q8 )####
look_for(ces25b, "rich")
ces25b$redistribution_mod<-Recode(as.numeric(ces25b$kiss_module_Q8), "; 1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#val_labels(ces25b$redistribution_mod)<-c(Much_more=0, Somewhat_more=0.25, Same_amount=0.5, Somewhat_less=0.75, Much_less=1) #LEFT TO RIGHT#
#checks
#val_labels(ces25b$redistribution_mod)
table(ces25b$redistribution_mod)

#recode Pro-Redistribution (p44)
ces25b$pro_redistribution_mod<-Recode(ces25b$kiss_module_Q7, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces25b$pro_redistribution_mod)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces25b$pro_redistribution_mod)
table(ces25b$pro_redistribution_mod)

#### Inequality - problem (kiss_module_Q7) ####
look_for(ces25b, "poor")
ces25b$inequality_mod<-Recode(as.numeric(ces25b$kiss_module_Q7), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; else=NA")
#checks
table(ces25b$inequality_mod, ces25b$kiss_module_Q7, useNA = "ifany")

#recode Personal Retrospective (cps25_own_fin_retro)
look_for(ces25b, "situation")
ces25b$personal_retrospective<-Recode(as.numeric(ces25b$cps25_own_fin_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces25b$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces25b$personal_retrospective)
table(ces25b$personal_retrospective , ces25b$cps25_own_fin_retro, useNA = "ifany" )

#recode National Retrospective (cps25_econ_retro)
look_for(ces25b, "economy")
ces25b$national_retrospective<-Recode(as.numeric(ces25b$cps25_econ_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces25b$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces25b$national_retrospective)
table(ces25b$national_retrospective, ces25b$cps25_econ_retro, useNA = "ifany" )

#recode Ideology (cps25_lr_scale_bef_1)
look_for(ces25b, "scale")
ces25b$ideology<-Recode(as.numeric(ces25b$cps25_lr_scale_bef_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; -99=NA; else=NA")
#val_labels(ces25b$ideology)<-c(Left=0, Right=1)
#checks
#val_labels(ces25b$ideology)
table(ces25b$ideology, ces25b$cps25_lr_scale_bef_1 , useNA = "ifany" )

# # recode turnout (pes21_turnout2021)
# look_for(ces21, "vote")
# ces21$turnout<-Recode(ces21$pes21_turnout2021, "1=1; 2:5=0; 6:8=NA; else=NA")
# val_labels(ces21$turnout)<-c(No=0, Yes=1)
# #checks
# val_labels(ces21$turnout)
# table(ces21$turnout)
# table(ces21$turnout, ces21$vote)

#### recode political efficacy ####
#recode No Say (kiss_module_Q10_3)
#look_for(ces25b, "have any say")
ces25b$efficacy_internal<-Recode(as.numeric(ces25b$kiss_module_Q10_3), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#val_labels(ces25b$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces25b$efficacy_internal)
table(ces25b$efficacy_internal, ces25b$kiss_module_Q10_3 , useNA = "ifany" )

#recode MPs lose touch (kiss_module_Q10_1)
#look_for(ces25b, "touch")
ces25b$efficacy_external<-Recode(as.numeric(ces25b$kiss_module_Q10_1), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#val_labels(ces25b$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces25b$efficacy_external)
table(ces25b$efficacy_external, ces25b$kiss_module_Q10_1 , useNA = "ifany" )

#recode Official Don't Care (kiss_module_Q10_2)
#look_for(ces25b, "doesn't care")
ces25b$efficacy_external2<-Recode(as.numeric(ces25b$kiss_module_Q10_2), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#val_labels(ces25b$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces25b$efficacy_external2)
table(ces25b$efficacy_external2, ces25b$kiss_module_Q10_2 , useNA = "ifany" )

ces25b %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces25b

ces25b %>%
  select(starts_with("efficacy")) %>%
  summary()

# recode satisfaction with democracy (cps25_demsat & pes25_dem_sat)
# look_for(ces21, "dem")
# ces21$satdem<-Recode(as.numeric(ces21$pes21_dem_sat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
# #checks
# table(ces21$satdem, ces21$pes21_dem_sat, useNA = "ifany" )

ces25b$satdem2<-Recode(as.numeric(ces25b$cps25_demsat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces25b$satdem2, ces25b$cps25_demsat, useNA = "ifany" )

# recode political interest (cps25_interest_gen_1)
look_for(ces25b, "interest")
ces25b$pol_interest<-Recode(as.numeric(ces25b$cps25_interest_gen_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
table(ces25b$pol_interest, ces25b$cps25_interest_gen_1, useNA = "ifany" )

#recode Foreign-born (cps25_bornin_canada)
ces25b$foreign<-Recode(ces25b$cps25_bornin_canada, "1=0; 2=1; else=NA")
val_labels(ces25b$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces25b$foreign)
table(ces25b$foreign, ces25b$cps25_bornin_canada , useNA = "ifany" )

#recode Previous Vote (cps25_vote_2021)
look_for(ces25b, "party did you vote")
ces25b$past_vote<-Recode(ces25b$cps25_vote_2021, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; else=NA")
val_labels(ces25b$past_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces25b$past_vote)
table(ces25b$past_vote, ces25b$cps25_vote_2021 , useNA = "ifany" )

# #recode Provincial Vote (pes21_provvote)
# # look_for(ces21, "vote")
# ces21$prov_vote<-car::Recode(as.numeric(ces21$pes21_provvote), "1=1; 7=2; 11=2; 2=3; 3=5; 4=10; 5=4; 6=11; 8:9=0; 10=7; 12:14=0; else=NA")
# val_labels(ces21$prov_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, PQ=4, Green=5, Reform=6, Sask=7,
#                                ADQ=8, Wildrose=9, CAQ=10, QS=11)
# #checks
# val_labels(ces21$prov_vote)
# table(ces21$prov_vote)

#### recode Homeowner(cps25_property_1) ####
look_for(ces25b, "home")
ces25b %>%
  mutate(homeowner=case_when(
    cps25_property_1==1 ~1,
    cps25_property_5==1 ~0,
    cps25_property_6==1 ~NA_real_ ,
    cps25_property_2==1 ~0,
    cps25_property_3==1 ~0,
    cps25_property_4==1 ~0,
  ))->ces25b
#checks
table(ces25b$homeowner, ces25b$cps25_property_1, useNA = "ifany")

#### Add Subjective social class ####
ces25b %>%
  mutate(sub_class=case_when(
    kiss_module_Q2==1~"Upper Class",
    kiss_module_Q2==2~"Upper-Middle Class",
    kiss_module_Q2==3~"Middle Class",
    kiss_module_Q2==4~"Working Class",
    kiss_module_Q2==5~"Lower Class",
    TRUE~NA_character_
  ))->ces25b
#lookfor(ces25b, "class")
ces25b$sub_class<-factor(ces25b$sub_class, levels=c("Lower Class", "Working Class", "Middle Class", "Upper-Middle Class", "Upper Class"))
table(ces25b$sub_class)

# recode Subjective class - belong to class (kiss_module_Q1)
look_for(ces25b, "class")
ces25b$class_belong<-Recode(ces25b$kiss_module_Q1, "2=0; 1=1; else=NA")
val_labels(ces25b$class_belong)<-c(No=0, Yes=1)
#checks
val_labels(ces25b$class_belong)
table(ces25b$class_belong)

# recode Subjective class - closeness to others in class (kiss_module_Q3)
look_for(ces25b, "class")
ces25b$class_close<-Recode(ces25b$kiss_module_Q3, "3=0; 1=1; 2=0.5; else=NA")
val_labels(ces25b$class_close)<-c(Not_close=0, Fairly_close=0.5, Very_close=1)
#checks
val_labels(ces25b$class_close)
table(ces25b$class_close)

# recode Class conflict (kiss_module_Q4)
look_for(ces25b, "class")
ces25b$class_conflict<-Recode(ces25b$kiss_module_Q4, "2=0; 1=1; else=NA")
val_labels(ces25b$class_conflict)<-c(No=0, Yes=1)
#checks
val_labels(ces25b$class_conflict)
table(ces25b$class_conflict)

# recode Liberal class favouritism (kiss_module_Q9_1)
look_for(ces25b, "class")
ces25b$liberal_class<-Recode(as.numeric(ces25b$kiss_module_Q9_1) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces25b$liberal_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces25b4$liberal_class)
table(ces25b$liberal_class, ces25b$kiss_module_Q9_1  , useNA = "ifany")

# recode Conservative class favouritism (kiss_module_Q9_2)
ces25b$conservative_class<-Recode(as.numeric(ces25b$kiss_module_Q9_2) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces25b$conservative_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces25b$conservative_class)
table(ces25b$conservative_class, ces25b$kiss_module_Q9_2  , useNA = "ifany")

# recode NDP class favouritism (kiss_module_Q9_3)
ces25b$ndp_class<-Recode(as.numeric(ces25b$kiss_module_Q9_3) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces25b$ndp_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces25b$ndp_class)
table(ces25b$ndp_class, ces25b$kiss_module_Q9_3  , useNA = "ifany")

# recode Green class favouritism (kiss_module_Q9_4)
ces25b$green_class<-Recode(as.numeric(ces25b$kiss_module_Q9_4) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces25b$green_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces25b$green_class)
table(ces25b$green_class, ces25b$kiss_module_Q9_4  , useNA = "ifany")

# recode PPC class favouritism (kiss_module_Q9_5)
ces25b$ppc_class<-Recode(as.numeric(ces25b$kiss_module_Q9_5) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces25b$ppc_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces25b$ppc_class)
table(ces25b$ppc_class, ces25b$kiss_module_Q9_5  , useNA = "ifany")

# recode Bloc class favouritism (kiss_module_Q9_6)
ces25b$bloc_class<-Recode(as.numeric(ces25b$kiss_module_Q9_6) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces25b$bloc_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces25b$bloc_class)
table(ces25b$bloc_class, ces25b$kiss_module_Q9_6  , useNA = "ifany")

#### Add Own vs Rent ####
look_for(ces25b, "rent")
ces25b %>%
  mutate(own_rent=case_when(
    cps25_property_1==1~"Own",
    cps25_rent==1 ~"Rent"
  ))->ces25b
ces25b$own_rent<-factor(ces25b$own_rent, levels=c("Own", "Rent", "Other"))
table(ces25b$own_rent)

# recode Future home (kiss_module_Q5)
look_for(ces25b, "purchase")
ces25b$home_future<-Recode(ces25b$kiss_module_Q5, "1=3; 2=2; 3=1; else=NA")
val_labels(ces25b$home_future)<-c(New_rental=1, Stay=2, Purchase=3)
#checks
val_labels(ces25b$home_future)
table(ces25b$home_future)

# recode Housing - gov't provide adequate (kiss_module_Q6)
look_for(ces25b, "housing")
ces25b$housing<-Recode(as.numeric(ces25b$kiss_module_Q6) , "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#val_labels(ces25b$home_future)<-c(Agree=0, Disagree=1) # LEFT TO rIGHT #
#checks
#val_labels(ces25b$housing)
table(ces25b$housing)

#recode Racial minority thermometer
look_for(ces25b, "therm")
ces25b$racial_rating<-Recode(as.numeric(ces25b$cps25_groups_therm_1 /100), "-0.99=NA")
table(ces25b$racial_rating)

#recode Racial minority thermometer
ces25b$immigrant_rating<-Recode(as.numeric(ces25b$cps25_groups_therm_2 /100), "-0.99=NA")
table(ces25b$immigrant_rating)

#recode Francophones thermometer
ces25b$francophone_rating<-Recode(as.numeric(ces25b$cps25_groups_therm_3 /100), "-0.99=NA")
table(ces25b$francophone_rating)

#recode Indigenous thermometer
ces25b$indigenous_rating<-Recode(as.numeric(ces25b$cps25_groups_therm_4 /100), "-0.99=NA")
table(ces25b$indigenous_rating)

#Add mode and election
ces25b$mode<-rep("Web", nrow(ces25b))
ces25b$election<-rep(2025, nrow(ces25b))

#Write out the dataset
# #### Resave the file in the .rda file
save(ces25b, file=here("data/ces25b.rda"))

#write_sav(ces25b, path=here("data-raw/ces25b_with_occupation.sav"))
