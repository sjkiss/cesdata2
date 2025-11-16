library(haven)
library(here)
library(tidyverse)
library(srvyr)
library(survey)

#Load data
ces25<-read_dta(here("data-raw/ces25.dta"))

library(labelled)
library(car)

#### recode Gender ####
ces25$male<-Recode(ces25$cps25_genderid, "1=1; 2=0; else=NA")
val_labels(ces25$male)<-c(Female=0, Male=1)
#checks
val_labels(ces25$male)
table(ces25$male)

# recode Union Household (cps25_union)
ces25$union<-Recode(ces25$cps25_union, "1=1; 2=0; else=NA")
val_labels(ces25$union)<-c(None=0, Union=1)
# Checks
val_labels(ces25$union)
table(ces25$union , useNA = "ifany" )

#Union Combined variable (identical copy of union) ### Respondent only
ces25$union_both<-ces25$union
#checks
val_labels(ces25$union_both)
table(ces25$union_both , useNA = "ifany" )

#recode Education (cps25_education)
ces25$degree<-Recode(ces25$cps25_education, "9:11=1; 1:8=0; else=NA")
val_labels(ces25$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces25$degree)
table(ces25$degree, ces25$cps25_education , useNA = "ifany" )

#recode Education 2 (cps25_education)
ces25$education<-Recode(ces25$cps25_education, "1:4=1; 5=2; 6:7=3; 8=4; 9:11=5; else=NA")
val_labels(ces25$education)<-c(Less_than_HS=1, HS=2, College=3, Some_uni=4, Bachelor=5)
#checks
val_labels(ces25$education)
table(ces25$education)

#recode Region (cps25_province)
look_for(ces25, "province")
ces25$region<-Recode(ces25$cps25_province, "1:3=3; 4:5=1; 7=1; 9=2; 10=1; 12=3; else=NA")
val_labels(ces25$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces25$region)
table(ces25$region , ces25$cps25_province , useNA = "ifany" )

#### Create province and quebec variable ####
ces25$cps25_province
ces25$prov<-Recode(ces25$cps25_province, "5=1; 10=2; 7=3; 4=4; 11=5; 9=6; 3=7; 12=8; 1=9; 2=10; else=NA")
val_labels(ces25$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
table(ces25$cps25_province)
table(ces25$prov)
val_labels(ces25$cps25_province)
ces25$quebec<-Recode(ces25$cps25_province, "1:5=0; 11=1; 7=0;9:10=0;12=0; else=NA")
val_labels(ces25$quebec)<-c(Other=0, Quebec=1)
table(ces25$quebec)

#### recode Age (cps25_age_in_years) ####
look_for(ces25, "age")
ces25$age<-ces25$cps25_age_in_years
#checks
table(ces25$age)

#recode Religion (cps25_religion)
look_for(ces25, "relig")
ces25$religion<-Recode(ces25$cps25_religion, "1:2=0; 3:7=3; 8:9=2; 10:11=1; 12:21=2; 22=3; else=NA")
val_labels(ces25$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces25$religion)
table(ces25$religion , ces25$cps25_religion , useNA = "ifany" )

#recode Language (cps25_UserLanguage)
look_for(ces25, "language")
ces25$language<-Recode(ces25$cps25_UserLanguage, "'FR-CA'=0; 'EN'=1; else=NA")
val_labels(ces25$language)<-c(French=0, English=1)
#checks
val_labels(ces25$language)
table(ces25$language)

#recode Non-charter Language (pes25_lang)
look_for(ces25, "language")
ces25$non_charter_language<-Recode(ces25$pes25_lang, "1:2=1; 3:18=0; else=NA")
val_labels(ces25$non_charter_language)<-c(Charter=0, Non_Charter=1)
table(as_factor(ces25$pes25_lang),ces25$non_charter_language , useNA = "ifany" )
#checks
val_labels(ces25$non_charter_language)
table(ces25$non_charter_language)

#recode Employment (cps25_employment)
look_for(ces25, "employment")
ces25$employment<-Recode(ces25$cps25_employment, "4:8=0; 1:3=1; 9:11=1; else=NA")
val_labels(ces25$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces25$employment)
table(ces25$employment , ces25$cps25_employment , useNA = "ifany" )

#recode Sector (cps25_sector)
lookfor(ces25, "sector")
ces25$sector<-Recode(ces25$cps25_sector, "1=0; 3=0; 2=1; else=NA")
val_labels(ces25$sector)<-c(`Public`=1, `Private`=0)
#checks
val_labels(ces25$sector)
table(ces25$sector , ces25$cps25_sector , useNA = "ifany" )

#### Create party vote ####
ces25$vote<-Recode(ces25$pes25_votechoice2025 , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 8=2; else=NA")
val_labels(ces25$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(as_factor(ces25$vote))

#### Create party vote - splitting out PPC from Cons ####
ces25$vote3<-Recode(ces25$pes25_votechoice2025 , "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=0; else=NA")
val_labels(ces25$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PPC=6)
table(as_factor(ces25$vote3))

#### Create party ID####
ces25$party_id<-Recode(ces25$cps25_fed_id , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 9=2; else=NA")
val_labels(ces25$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(as_factor(ces25$party_id))
table(ces25$party_id)

#### Create party ID - splitting out PPC from Cons ####
ces25$party_id2<-Recode(ces25$cps25_fed_id , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 9=6; else=NA")
val_labels(ces25$party_id2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PPC=6)
table(as_factor(ces25$party_id2))
table(ces25$party_id2)

#recode Party closeness (cps25_fed_id_str)
look_for(ces25, "fed_id_str")
ces25$party_close<-Recode(ces25$cps25_fed_id_str, "1=1; 2=0.5; 3=0; else=NA")
#checks
table(ces25$cps25_fed_id_str , ces25$party_close, useNA = "ifany" )

#### Create income ####
lookfor(ces25, "income")
# Matt - I moved tertile 3 down a category so those over $110K to match with ces21 and provides an even balance in respondents
ces25 %>%
  mutate(income_tertile=case_when(
    cps25_income<4~1,
    cps25_income>3&cps25_income<6~2,
    cps25_income>5&cps25_income<9~3,
  ))->ces25
table(ces25$income_tertile)

#recode Income Quintile (cps21_income_cat & cps21_income_number)
look_for(ces25, "income")
ces25 %>%
  mutate(income=case_when(
    cps25_income==1 ~ 1,
    cps25_income==2 ~ 1,
    cps25_income==3 ~ 2,
    cps25_income==4 ~ 3,
    cps25_income==5 ~ 4,
    cps25_income==6 ~ 4,
    cps25_income==7 ~ 5,
    cps25_income==8 ~ 5,
  ))->ces25
val_labels(ces25$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces25$income)
table(ces25$income)

#### Create Class ####
#Convert the 5 digit NOC code provided by CDEM to a number
ces25$NOC21_5<-as.numeric(ces25$occupation_code)
#Note, that this will turn some 5 digit codes that start with 0000 into 2-digit codes
min(nchar(ces25$NOC21_5), na.rm=T)
lookfor(ces25, "employ")
# Class variable
ces25 %>%
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
  ))->ces25

# ADd value labels
val_labels(ces25$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)

# Make occupation3 as self-employed and unskilled and skilled grouped together

#lookfor(ces21, "employed")
ces25$occupation3<-ifelse(ces25$cps25_employment==3, 6, ces25$occupation)

# Add value labels for occupation3
val_labels(ces25$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)

# Create Oesch variable

#Check, did I get everyone?
ces25 %>%
  filter(is.na(occupation3)) %>%
  select(cps25_employment, occupation_name, occupation3) %>%
  as_factor()
ces25$NOC21_5

#code logic of authority

#First extract the first two digits of each NOC

ces25$occupational_category_teer<-str_extract_all(ces25$NOC21_5, "^\\d{2}") %>% unlist()
#Now separate
ces25 %>%
  separate_wider_position(., cols=occupational_category_teer, widths=c("occupational_category"=1, "teer"=1))->ces25
# Check employment status
lookfor(ces25, "status")
#Create working variable
ces25 %>%
  mutate(working=case_when(
    cps25_employment<4|(cps25_employment>8&cps25_employment<12)~1,
    TRUE~0)
  )->ces25
ces25 %>%
  mutate(logic=case_when(
    working==1&cps25_employment!=3 &occupational_category==0~"Organizational",
    working==1&cps25_employment!=3&occupational_category==1~"Organizational",
    working==1&cps25_employment!=3&occupational_category==2~"Technical",
    working==1&cps25_employment!=3&occupational_category==3~"Interpersonal",
    working==1&cps25_employment!=3&occupational_category==4~"Interpersonal",
    working==1&cps25_employment!=3&occupational_category==5~"Interpersonal",
    working==1&cps25_employment!=3&occupational_category==6~"Interpersonal",
    working==1&cps25_employment!=3&occupational_category==7~"Technical",
    working==1&cps25_employment!=3&occupational_category==8~"Technical",
    working==1&cps25_employment!=3&occupational_category==8~"Technical",
    working==1&cps25_employment!=3&occupational_category==9~"Technical"
  ))->ces25
table(ces25$logic)

# Introduce level of authority for the 8-class schema
#Note that Rehm and Kitchelt have four gradations here; Oesch has only two.
ces25 %>%
  mutate(authority=case_when(
    working==1&cps25_employment!=3&teer<3~"Higher",
    working==1&cps25_employment!=3&teer>2~"Lower"
  ))->ces25
table(ces25$authority)
ces25 %>%
  select(cps25_employment, teer, authority) %>%
  count(cps25_employment, teer, authority)

#Check most frequent self-employed
ces25 %>%
  filter(cps25_employment==3) %>%
  select(NOC21_5) %>%
  count(NOC21_5)

#Note, most doctors are not self-employed
ces25 %>%
  filter(NOC21_5==31102) %>%
  count(cps25_employment)

ces25 %>%
  mutate(occupation_oesch=case_when(
    logic=="Technical"& authority=="Higher"~'Technical Professionals',
    logic=="Organizational"&authority=="Higher"~'(Associate) Managers',
    logic=="Interpersonal"&authority=="Higher"~'Socio-cultural (semi) Professionals',
    logic=="Technical"&authority=="Lower"~'Production workers',
    logic=="Organizational"&authority=="Lower"~'Office clerks',
    logic=="Interpersonal"&authority=="Lower"~'Service workers'
  ))->ces25

ces25 %>%
  mutate(occupation_oesch=case_when(
    cps25_employment==3~"Self-employed",
    TRUE~ occupation_oesch
  ))->ces25
table(ces25$logic)
table(ces25$authority)
table(ces25$occupation_oesch, useNA = "ifany")

#This code checks to see that everyone with an NOC21_5 has also been given an oesch
ces25 %>%
  filter(working==1) %>%
  select(NOC21_5, occupation_oesch) %>% filter(!is.na(!NOC21_5)&is.na(occupation_oesch))
table(ces25$occupation_oesch)
#How many non-missing oesch do we hvave
table(is.na(ces25$occupation_oesch)) #1999

#How many more will we get from the open-ended
ces25 %>%
  filter(working==1) %>%
  filter(occupation_code=="") %>%
  select(occupation_code, NOC21_5, pes25_occ_select_2) %>%
  filter(pes25_occ_select_2!="-99"&pes25_occ_select_2!="") %>%
  nrow()

# check self-employed
table(as_factor(ces25$cps25_employment))
with(ces25, table(cps25_employment, occupation_oesch))

# Calculate Oesch-5
ces25 %>%
  mutate(occupation_oesch_6=case_when(
    teer==0~"Managers",
    teer==1~"Professionals",
    teer==2~"Semi-Professionals Associate Managers",
    teer==3~"Skilled Workers",
    teer>3~"Unskilled Workers",
    cps25_employment==3~"Self-employed"
  ))->ces25
table(ces25$occupation_oesch_6)
ces25$occupation_oesch_6<-factor(ces25$occupation_oesch_6, levels=c("Unskilled Workers", "Skilled Workers",
                                                                      "Semi-Professionals Associate Managers",
                                                                      "Self-employed","Professionals", "Managers"))

#### recode Age (cps25_age_in_years) ####
look_for(ces25, "age")
ces25$age<-ces25$cps25_age_in_years
table(ces25$age)

#recode Religiosity (ces25_rel_imp)
look_for(ces25, "relig")
ces25$religiosity<-Recode(ces25$cps25_rel_imp, "1=5; 2=4; 5=3; 3=2; 4=1; else=NA")
val_labels(ces25$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces25$religiosity)
table(ces25$religiosity, ces25$cps25_rel_imp , useNA = "ifany")

#recode Community Size (pes25_place_live)
look_for(ces25, "place")
ces25$size<-Recode(ces25$pes25_place_live, "3=1; 2=3; 1=5; else=NA")
val_labels(ces25$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces25$size)
table(ces25$size, ces25$pes25_place_live , useNA = "ifany" )

#recode Native-born (cps25_bornin_canada)
ces25$native<-Recode(ces25$cps25_bornin_canada, "1=1; 2=0; else=NA")
val_labels(ces25$native)<-c(Foreign=0, Native=1)
#checks
val_labels(ces25$native)
table(ces25$native, ces25$cps25_bornin_canada , useNA = "ifany" )

#recode Liberal leader
ces25$liberal_leader<-Recode(as.numeric(ces25$cps25_lead_rating_23), "-99=NA")
#checks
#ces25$liberal_leader<-(ces25$liberal_leader2 /100)
table(ces25$liberal_leader)

#recode Conservative leader
ces25$conservative_leader<-Recode(as.numeric(ces25$cps25_lead_rating_24), "-99=NA")
#checks
#ces25$conservative_leader<-(ces25$conservative_leader2 /100)
table(ces25$conservative_leader)

#recode NDP leader
ces25$ndp_leader<-Recode(as.numeric(ces25$cps25_lead_rating_25), "-99=NA")
#checks
#ces25$ndp_leader<-(ces25$ndp_leader2 /100)
table(ces25$ndp_leader)

#recode Bloc leader
ces25$bloc_leader<-Recode(as.numeric(ces25$cps25_lead_rating_26), "-99=NA")
#checks
#ces25$bloc_leader<-(ces25$bloc_leader2 /100)
table(ces25$bloc_leader)

#recode Green leader
ces25$green_leader<-Recode(as.numeric(ces25$cps25_lead_rating_27), "-99=NA")
#checks
#ces25$green_leader<-(ces25$green_leader2 /100)
table(ces25$green_leader)

#recode PPC leader
ces25$ppc_leader<-Recode(as.numeric(ces25$cps25_lead_rating_29), "-99=NA")
#checks
#ces25$ppc_leader<-(ces25$ppc_leader2 /100)
table(ces25$ppc_leader)

#recode Liberal rating
look_for(ces25, "parties")
ces25$liberal_rating<-Recode(as.numeric(ces25$cps25_party_rating_23), "-99=NA")
table(ces25$liberal_rating)

#recode Conservative rating
ces25$conservative_rating<-Recode(as.numeric(ces25$cps25_party_rating_24), "-99=NA")
table(ces25$conservative_rating)

#recode NDP rating
ces25$ndp_rating<-Recode(as.numeric(ces25$cps25_party_rating_25), "-99=NA")
table(ces25$ndp_rating)

#recode Bloc rating
ces25$bloc_rating<-Recode(as.numeric(ces25$cps25_party_rating_26), "-99=NA")
table(ces25$bloc_rating)

#recode Green rating
ces25$green_rating<-Recode(as.numeric(ces25$cps25_party_rating_27), "-99=NA")
table(ces25$green_rating)

#recode PPC rating
ces25$ppc_rating<-Recode(as.numeric(ces25$cps25_party_rating_29), "-99=NA")
table(ces25$ppc_rating)

#### recode Redistribution (pes25_gap)####
look_for(ces25, "rich")
ces25$redistribution<-Recode(as.numeric(ces25$pes25_gap), "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; else=NA", as.numeric=T)
#val_labels(ces25$redistribution)<-c(Much_more=0, Somewhat_more=0.25, Same_amount=0.5, Somewhat_less=0.75, Much_less=1) #LEFT TO RIGHT#
#checks
#val_labels(ces25$redistribution)
table(ces25$redistribution)

#recode Pro-Redistribution (pes25_gap)
ces25$pro_redistribution<-Recode(ces25$pes25_gap, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces25$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces25$pro_redistribution)
table(ces25$pro_redistribution)

#### Inequality - problem (pes25_inequal) ####
look_for(ces25, "inequality")
ces25$inequality<-Recode(as.numeric(ces25$pes25_inequal), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; else=NA")
#checks
table(ces25$inequality, ces25$pes25_inequal, useNA = "ifany")

#recode efficacy rich (pes25_populism_8)
look_for(ces25, "rich")
ces25$efficacy_rich<-Recode(as.numeric(ces25$pes25_populism_8), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; else=NA")
#checks
table(ces25$efficacy_rich)
table(ces25$efficacy_rich, ces25$pes25_populism_8)

#recode Personal Retrospective (cps25_own_fin_retro)
look_for(ces25, "situation")
ces25$personal_retrospective<-Recode(as.numeric(ces25$cps25_own_fin_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces25$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces25$personal_retrospective)
table(ces25$personal_retrospective , ces25$cps25_own_fin_retro, useNA = "ifany" )

#recode National Retrospective (cps25_econ_retro)
look_for(ces25, "economy")
ces25$national_retrospective<-Recode(as.numeric(ces25$cps25_econ_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces25$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces25$national_retrospective)
table(ces25$national_retrospective, ces25$cps25_econ_retro, useNA = "ifany" )

#recode Ideology (cps25_lr_scale_bef_1)
look_for(ces25, "scale")
ces25$ideology<-Recode(as.numeric(ces25$cps25_lr_scale_bef_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; -99=NA; else=NA")
#val_labels(ces25$ideology)<-c(Left=0, Right=1)
#checks
#val_labels(ces25$ideology)
table(ces25$ideology, ces25$cps25_lr_scale_bef_1 , useNA = "ifany" )

# recode turnout (pes25_turnout2025)
look_for(ces25, "vote")
ces25$turnout<-Recode(ces25$pes25_turnout2025, "1=1; 2:5=0; 6:8=NA; else=NA")
val_labels(ces25$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces25$turnout)
table(ces25$turnout)
table(ces25$turnout, ces25$vote)

#### recode political efficacy ####
#recode No Say (cps25_govt_say)
look_for(ces25, "have any say")
ces25$efficacy_internal<-Recode(as.numeric(ces25$cps25_govt_say), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#val_labels(ces25$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces25$efficacy_internal)
table(ces25$efficacy_internal)
table(ces25$efficacy_internal, ces25$cps25_govt_say , useNA = "ifany" )

#recode MPs lose touch (pes25_losetouch)
look_for(ces25, "lose touch")
ces25$efficacy_external<-Recode(as.numeric(ces25$pes25_losetouch), "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces25$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces25$efficacy_external)
table(ces25$efficacy_external)
table(ces25$efficacy_external, ces25$pes25_losetouch , useNA = "ifany" )

#recode Official Don't Care (pes25_populism_3)
look_for(ces25, "care much")
ces25$efficacy_external2<-Recode(as.numeric(ces25$pes25_populism_3), "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces25$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces25$efficacy_external2)
table(ces25$efficacy_external2)
table(ces25$efficacy_external2, ces25$pes25_populism_3 , useNA = "ifany" )

ces25 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces25

ces25 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
qplot(ces21$political_efficacy, geom="histogram")
table(ces21$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces25 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  psych::alpha(.)
#Check correlation
ces25 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  cor(., use="complete.obs")

#recode satisfaction with democracy (cps25_demsat & pes25_dem_sat)
look_for(ces25, "dem")
ces25$satdem<-Recode(as.numeric(ces25$pes25_dem_sat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces25$satdem, ces25$pes25_dem_sat, useNA = "ifany" )

ces25$satdem2<-Recode(as.numeric(ces25$cps25_demsat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces25$satdem2, ces25$cps25_demsat, useNA = "ifany" )

# recode political interest (cps25_interest_gen_1)
look_for(ces25, "interest")
ces25$pol_interest<-Recode(as.numeric(ces25$cps25_interest_gen_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
table(ces25$pol_interest, ces25$cps25_interest_gen_1, useNA = "ifany" )

#recode Foreign-born (cps25_bornin_canada)
ces25$foreign<-Recode(ces25$cps25_bornin_canada, "1=0; 2=1; else=NA")
val_labels(ces25$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces25$foreign)
table(ces25$foreign, ces25$cps25_bornin_canada , useNA = "ifany" )

#recode Previous Vote (cps25_vote_2021)
look_for(ces25, "party did you vote")
ces25$past_vote<-Recode(ces25$cps25_vote_2021, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; else=NA")
val_labels(ces25$past_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces25$past_vote)
table(ces25$past_vote, ces25$cps25_vote_2021 , useNA = "ifany" )

#recode Provincial Vote (pes21_provvote)
# look_for(ces25, "vote")
ces25$prov_vote<-car::Recode(as.numeric(ces25$pes25_provvote), "1=1; 7=2; 11=2; 2=3; 3=5; 4=10; 5=4; 6=11; 8:9=0; 10=7; 12:14=0; 17=0; else=NA")
val_labels(ces25$prov_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, PQ=4, Green=5, Reform=6, Sask=7,
                               ADQ=8, Wildrose=9, CAQ=10, QS=11)
#checks
val_labels(ces25$prov_vote)
table(ces25$prov_vote)

#### recode Homeowner(cps25_property_1) ####
look_for(ces25, "home")
ces25 %>%
  mutate(homeowner=case_when(
    cps25_property_1==1 ~1,
    cps25_property_5==1 ~0,
    cps25_property_6==1 ~NA_real_ ,
    cps25_property_2==1 ~0,
    cps25_property_3==1 ~0,
    cps25_property_4==1 ~0,
  ))->ces25
#checks
table(ces25$homeowner, ces25$cps25_property_1, useNA = "ifany")

# recode Housing - gov't spend (L-R)
look_for(ces25, "housing")
ces25$housing<-Recode(as.numeric(ces25$cps25_spend_afford_h) , "1=1; 2=0.5; 3=0; else=NA")
#checks
table(ces25$housing)

# recode Rent security (less worry to more worry)
look_for(ces25, "housing")
ces25$rent_security<-Recode(as.numeric(ces25$cps25_spend_afford_h) , "1=0; 2=0.33; 3=0.67; 4=1; else=NA")
#checks
table(ces25$rent_security)

#recode Racial minority thermometer
look_for(ces25, "therm")
ces25$racial_rating<-Recode(as.numeric(ces25$cps25_groups_therm_1 /100), "-0.99=NA")
table(ces25$racial_rating)

#recode Immigrant minority thermometer
ces25$immigrant_rating<-Recode(as.numeric(ces25$cps25_groups_therm_2 /100), "-0.99=NA")
table(ces25$immigrant_rating)

#recode Francophones thermometer
ces25$francophone_rating<-Recode(as.numeric(ces25$cps25_groups_therm_3 /100), "-0.99=NA")
table(ces25$francophone_rating)

#recode Indigenous thermometer
ces25$indigenous_rating<-Recode(as.numeric(ces25$cps25_groups_therm_4 /100), "-0.99=NA")
table(ces25$indigenous_rating)

#recode Manage economy (cps25_issue_handle_8)
# look_for(ces25, "economy")
ces25$cps25_issue_handle_8
ces25$manage_economy<-Recode(ces25$cps25_issue_handle_8, "1=1; 2=2; 3=3; 4=4; 5=5; 6:7=NA; else=NA")
val_labels(ces25$manage_economy)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces25$manage_economy)
table(ces25$manage_economy)

#recode Manage environment (cps25_issue_handle_5)
# look_for(ces25, "environment")
ces25$cps25_issue_handle_3
ces25$manage_environment<-Recode(ces25$cps25_issue_handle_3, "1=1; 2=2; 3=3; 4=4; 5=5; 6:7=NA; else=NA")
val_labels(ces25$manage_environment)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces25$manage_environment)
table(ces25$manage_environment)

#recode Manage immigration (cps25_issue_handle_7)
# look_for(ces25, "immigration")
ces25$cps25_issue_handle_7
ces25$manage_immigration<-Recode(ces25$cps25_issue_handle_7, "1=1; 2=2; 3=3; 4=4; 5=5; 6:7=NA; else=NA")
val_labels(ces25$manage_immigration)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces25$manage_immigration)
table(ces25$manage_immigration)

#recode Addressing Main Issue (cps25_imp_iss_party)
# look_for(ces25, "issue")
ces25$address_issue<-Recode(ces25$cps25_imp_iss_party, "1=1; 2=2; 3=3; 4=4; 5=5; 6:7=NA; else=NA")
val_labels(ces25$address_issue)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces25$address_issue)
table(ces25$address_issue)

#recode Environment Spend (cps25_spend_env) (R-L)
look_for(ces25, "env")
ces25$enviro_spend<-Recode(as.numeric(ces25$cps25_spend_env), "1=0; 2=0.5; 3=1; else=NA")
#checks
table(ces25$enviro_spend , ces25$cps25_spend_env , useNA = "ifany" )

#### recode Environment vs Jobs
look_for(ces25, "env")
ces25$enviro<-Recode(as.numeric(ces25$cps25_pos_jobs), "5=1; 4=0.75; 3=0.5; 2=0.25; 1=0; 6=0.5; else=NA")
table(ces25$cps25_pos_jobs, ces25$enviro)

#recode duty (cps25_duty_choice )
look_for(ces25, "duty")
ces25$duty<-Recode(ces25$cps25_duty_choice , "1=1; 2=0; else=NA")
val_labels(ces25$duty)<-c(No=0, Yes=1)
#checks
val_labels(ces25$duty)
table(ces25$duty, ces25$cps25_duty_choice, useNA="ifany")

#### recode Quebec Sovereignty (cps25_quebec_sov) (Right=more sovereignty)
# look_for(ces25, "sovereignty")
ces25$quebec_sov<-Recode(as.numeric(ces25$cps25_quebec_sov), "1=1; 2=0.75; 5=0.5; 3=0.25; 4=0; else=NA")
#checks
# table(ces21$quebec_sov, ces21$cps21_quebec_sov, useNA = "ifany" )
#val_labels(ces21$quebec_sov)<-NULL

#### recode Women - how much should be done (pes25_donew) ####
look_for(ces25, "women")
table(ces25$pes25_donew)
ces25$women<-Recode(as.numeric(ces25$pes25_donew), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces25$women,  useNA = "ifany")

#### recode Race - how much should be done (pes25_donerm) ####
look_for(ces25, "racial")
table(ces25$pes25_donerm)
ces25$race<-Recode(as.numeric(ces25$pes25_donerm), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces25$race,  useNA = "ifany")

#recode Postgrad (cps25_education)
look_for(ces25, "education")
ces25$postgrad<-Recode(as.numeric(ces25$cps25_education), "10:11=1; 1:9=0; else=NA")
#checks
table(ces25$postgrad)

#recode Education (cps25_spend_educ)
look_for(ces25, "education")
ces25$education_spend<-Recode(as.numeric(ces25$cps25_spend_educ), "2=0.5; 1=0; 3=1; 4=0.5; else=NA")
#val_labels(ces25$education)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
#val_labels(ces25$education)
table(ces25$education_spend, ces25$cps25_spend_educ , useNA = "ifany" )

#recode Break Promise (pes25_keepromises)
look_for(ces25, "promise")
ces25$promise<-Recode(as.numeric(ces25$pes25_keepromises), "1=0; 2=0.5; 3=1; 4=1; 5=0.5; else=NA", as.numeric=T)
#val_labels(ces21$promise)<-c(low=0, high=1)
#checks
val_labels(ces25$promise)
table(ces25$promise)
table(ces25$promise, ces25$pes25_keepromises , useNA = "ifany" )

#recode Trust (pes25_trust)
look_for(ces25, "trust")
val_labels(ces25$pes25_trust)
ces25$trust<-Recode(ces25$pes25_trust, "1=1; 2=0; else=NA", as.numeric=T)
val_labels(ces25$trust)<-c(no=0, yes=1)
#checks
val_labels(ces25$trust)
table(ces25$trust)
table(ces25$trust, ces25$pes25_trust , useNA = "ifany" )

#recode Quebec Accommodation (pes25_doneqc ) (Left=more accom)
look_for(ces25, "quebec")
ces25$quebec_accom<-Recode(as.numeric(ces25$pes25_doneqc), "2=0.25; 1=0; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces25$quebec_accom)

# recode Immigration sentiment (pes25_immigjobs)
look_for(ces25, "immigr")
ces25$immigration_job<-Recode(as.numeric(ces25$pes25_immigjobs), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 6=0.5; else=NA", as.numeric=T)
#checks
table(ces25$immigration_job, ces25$pes25_immigjobs, useNA = "ifany" )

#recode Immigration (cps25_imm)
look_for(ces25, "admit")
ces25$immigration_rates<-Recode(as.numeric(ces25$cps25_imm), "1=0; 2=1; 3=0.5; 4=0.5; else=NA", as.numeric=T)
#checks
table(ces25$immigration_rates, ces25$cps25_imm , useNA = "ifany" )

#Calculate Cronbach's alpha
library(psych)

#recode Market Liberalism (pes21_privjobs and pes21_blame)
look_for(ces25, "leave")
look_for(ces25, "blame")

ces25$market1<-Recode(as.numeric(ces25$pes25_privjobs), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
ces25$market2<-Recode(as.numeric(ces25$pes25_blame), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#checks
table(ces25$market1, ces25$pes25_privjobs , useNA = "ifany" )
table(ces25$market2, ces25$pes25_blame , useNA = "ifany" )
ces25$market1
ces25 %>%
  mutate(market_liberalism=rowMeans(select(., num_range("market", 1:2)), na.rm=T))->ces25

ces25 %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
qplot(ces25$market_liberalism, geom="histogram")
table(ces25$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
ces25 %>%
  select(market1, market2) %>%
  alpha(.)
#For some reason the cronbach's alpha doesn't work here.
#Check correlation
ces25 %>%
  select(market1, market2) %>%
  cor(., use="complete.obs")

#recode Capital Punishment (Missing)

#recode Crime (cps25_spend_just_law) (spending question)
look_for(ces25, "crime")
ces25$crime<-Recode(as.numeric(ces25$cps25_spend_just_law), "1=0; 2=0; 3=1; else=NA")
#checks
table(ces25$crime, ces25$cps25_spend_just_law , useNA = "ifany" )

#recode Gay Rights (pes25_donegl) (should do more question)
look_for(ces25, "gays")
ces25$gay_rights<-Recode(as.numeric(ces25$pes25_donegl), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces25$gay_rights, ces25$pes25_donegl , useNA = "ifany" )

#recode Abortion (pes21_abort2)
look_for(ces25, "abortion")
ces25$abortion<-Recode(as.numeric(ces25$pes25_abort2), "1=1; 2=0.5; 3=0; else=NA")
#checks
table(ces25$abortion, ces25$pes25_abort2 , useNA = "ifany" )

#recode Lifestyle (pes25_newerlife)
look_for(ces25, "lifestyle")
ces25$lifestyles<-Recode(as.numeric(ces25$pes25_newerlife), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces25$lifestyles, ces25$pes25_newerlife, useNA = "ifany")

#recode Moral Trad (abortion, lifestyles, stay home, values, marriage, childen, morals)
ces25$trad1<-ces25$women
ces25$trad2<-ces25$gay_rights
ces25$trad3<-ces25$abortion
table(ces25$trad1)
table(ces25$trad2)
table(ces25$trad3)
ces25 %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", 1:3)), na.rm=T))->ces25
#Check distribution of traditionalism
#qplot(ces25$traditionalism, geom="histogram")
table(ces25$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces25 %>%
  select(trad1, trad2, trad3) %>%
  psych::alpha(.)
#Check correlation
ces25 %>%
  select(trad1, trad2, trad3) %>%
  cor(., use="complete.obs")

#recode Moral Traditionalism 2 (women & gay rights) (Left-Right)
ces25 %>%
  mutate(traditionalism2=rowMeans(select(., num_range("trad", 1:2)), na.rm=T))->ces25

#Check distribution of traditionalism2
qplot(ces25$traditionalism2, geom="histogram")
table(ces25$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces25 %>%
  select(trad1, trad2) %>%
  psych::alpha(.)
#Check correlation
ces25 %>%
  select(trad1, trad2) %>%
  cor(., use="complete.obs")

#recode 2nd Dimension (lifestyles, immigration, gay rights, crime)
ces25$author1<-ces25$lifestyles
ces25$author2<-ces25$immigration_rates
ces25$author3<-ces25$gay_rights
ces25$author4<-ces25$crime
table(ces25$author1)
table(ces25$author2)
table(ces25$author3)
table(ces25$author4)

ces25 %>%
  mutate(authoritarianism=rowMeans(select(. ,num_range("author", 1:4)), na.rm=T))->ces25

ces25 %>%
  select(starts_with("author")) %>%
  summary()
tail(names(ces25))
#Check distribution of traditionalism
qplot(ces25$authoritarianism, geom="histogram")

#Calculate Cronbach's alpha
ces25 %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces25 %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")

#recode Women (pes25_womenhome)
look_for(ces25, "women")
ces25$stay_home<-Recode(as.numeric(ces25$pes25_womenhome), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces25$stay_home, ces25$pes25_womenhome, useNA = "ifany")

#Add mode and election
ces25$mode<-rep("Web", nrow(ces25))
ces25$election<-rep(2025, nrow(ces25))

#Write out the dataset
# #### Resave the file in the .rda file
save(ces25, file=here("data/ces25.rda"))

#write_sav(ces25, path=here("data-raw/ces25_with_occupation.sav"))
