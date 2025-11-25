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

#recode Education 2 (cps25_education)
ces25b$education<-Recode(ces25b$cps25_education, "1:4=1; 5=2; 6:7=3; 8=4; 9:11=5; else=NA")
val_labels(ces25b$education)<-c(Less_than_HS=1, HS=2, College=3, Some_uni=4, Bachelor=5)
#checks
val_labels(ces25b$education)
table(ces25b$education)

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

#Check, did I get everyone?
ces25b %>%
  filter(is.na(occupation3)) %>%
  select(cps25_employment, occupation_name, occupation3) %>%
  as_factor()


#First extract the first two digits of each NOC
ces25b$occupational_category_teer<-str_extract_all(ces25b$NOC21_5, "^\\d{2}") %>% unlist()
#Now separate
ces25b %>%
  separate_wider_position(., cols=occupational_category_teer, widths=c("occupational_category"=1, "teer"=1))->ces25b
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
  ))->ces25b
table(ces25b$logic)





#How many more will we get from the open-ended
# ces25b %>%
#   filter(working==1) %>%
#   filter(occupation_code=="") %>%
#   select(occupation_code, NOC21_5, pes25_occ_select_2) %>%
#   filter(pes25_occ_select_2!="-99"&pes25_occ_select_2!="") %>%
#   nrow()
?save
ces25b %>%
  select(NOC21_5) %>%
write.csv(., file="~/Desktop/ces25b.csv")
# Calculate Oesch
ces25b %>%
  mutate(occupation_oesch=case_when(
    #Legislators
NOC21_5<16~9,
NOC21_5>=10010&NOC21_5<=10022~9,
NOC21_5==10029~10,
NOC21_5==10030~9,
NOC21_5>=20010&NOC21_5<=20012~9,
NOC21_5==30010~9,
#Police military officers, fire chiefs
NOC21_5>=40010&NOC21_5<=40042~9,
NOC21_5>=50010&NOC21_5<=50012~10,
#Middle Management
NOC21_5>=60010&NOC21_5<=60019~9,
#Retail hotel managers
NOC21_5 >=60020&NOC21_5<=60049~10,
NOC21_5>=70010&NOC21_5<=70021~9,
NOC21_5==80010~9,
NOC21_5>=80020&NOC21_5<=80022~10,
NOC21_5>=90010&NOC21_5<=90011~9,
#The following are exceptions
#Forestry agricultural managers
NOC21_5 >= 80010 &NOC21_5<=80029~10,
#5001 – Managers in art, culture, recreation and sport
NOC21_5 >=50010&NOC21_5<=50012~10,
#Major group 11
#Finance Professionals
NOC21_5>=11100&NOC21_5<=11102~9,
#Finance Associate Professionals
NOC21_5>=11103&NOC21_5<=11100~10,
#Major group 21
NOC21_5>=21100&NOC21_5<=21399~5,
#Major group 31
#Physicians
NOC21_5>=31100&NOC21_5<=31104~13,
#Dentists
NOC21_5==31110~13,
#Optomotrists
NOC21_5==31111~14,
#Audiologists
NOC21_5==31112~14,
#Psychologists
NOC21_5==3200~13,
#chiropractors, phsyiotherapists, occupational therapists
NOC21_5>=31201&NOC21_5<=31209~13,
#Nursing
NOC21_5>=31300&NOC21_5<=31303~13,
##Major Group 41
# Judges & lawyers
NOC21_5>=41100&NOC21_5<=41101~9,
#Teachers & Professors
NOC21_5>=41200&NOC21_5<=41229~13,
#elementary teachers
NOC21_5==41220~14,
#Social Work and counselling
NOC21_5==41300|NOC21_5==41301~14,
#Religious leaders
NOC21_5==41302~13,
# Police investogators
NOC21_5==41310~10,
#Parole
NOC21_5==41311~14,
#Guidance counsellor
NOC21_5==41320~13,
#Employment Counsellr
NOC21_5==41321~9,
#Policy professionals
NOC21_5>=41400&NOC21_5<=41409~9,
#Major group 51
# librarians 14
NOC21_5==51100~14,
NOC21_5==51101|NOC21_5==51102~13,
#Editors
NOC21_5==51110~13,
#Authors
NOC21_5==51111~14,
# Journalists
NOC21_5==51113~13,
# technical writers
NOC21_5==51112~13,
# Translators
NOC21_5==51114~13,
#Musicians; dancers; choreographers
NOC21_5>=51120&NOC21_5<=51122~14,
###Major group 12
#Office supervisors
NOC21_5>=12010&NOC21_5<=12013~11,
#Executiv assistants
NOC21_5==12100~11,
# HR recruitment
NOC21_5==12101~11,
NOC21_5>=12101&NOC21_5<=12104~10,
#Court reporters
NOC21_5==12110 ~10,
#Health information Records
NOC21_5==12111 ~6,
NOC21_5==12112 ~11,
#Statistical research assistants non -university
NOC21_5==12113~10,
#Insurance associates
NOC21_5>=12200&NOC21_5<=12203~10,
#Major Gtroup 22
#Technicians
NOC21_5>=22100 &NOC21_5<=22114~6,
#Draughtsperson architectural technicains
NOC21_5==22210 ~6,
#industrial designers
NOC21_5==22211 ~5,
#Draughting technicians; surveying technicians
NOC21_5>=22212&NOC21_5<=22222 ~6,
#Engineering inspectors
NOC21_5==22230|NOC21_5==22231~6,
#Environmental health and safety officer
NOC21_5==22232~14,
#construction inspectors
NOC21_5==22233~6,
NOC21_5>=22300&NOC21_5<=22314~6,
#major Group 32
#Opticians
NOC21_5==32100~6,
#Veterinary technologists
NOC21_5==32100~6,
#LPN
NOC21_5>=32101&NOC21_5<=32103~14,
NOC21_5==32109~14,
#denturists
NOC21_5==32110~6,
#Dental hygienists
NOC21_5==32111|NOC21_5==32112~15,
#Medical imaging technicians
NOC21_5>=32120&NOC21_5<=32129~6,
#traditional medicine
NOC21_5>=32200&NOC21_5<=32209~14,
NOC21_5>=42100&NOC21_5<=42102~15,
#Paralegals
NOC21_5==42200~11,
NOC21_5>=4201&NOC21_5<=42202~14,
NOC21_5==42203~13,
NOC21_5==42204~14,
## major group 52
NOC21_5==52100~14,
#Camera operators
NOC21_5==52110~6,
#graphic arts technicians
NOC21_5==52111~14,
#broadcast technicians
NOC21_5==52112|NOC21_5==52113~6,
#Announers
NOC21_5==52114~14,
NOC21_5==52119~6,
#graphic designers
NOC21_5==52120|NOC21_5==52121~14,
#Retail supervisors
NOC21_5==62010~15,
# Food supervisor
NOC21_5==62011~15,
#Executive housekeeping
NOC21_5==62012|NOC21_5==62013~16,
#Call centre supervisor
NOC21_5==62023~11,
NOC21_5==62024~16,
NOC21_5==62029~15,
#Technical sales, retail buyers
NOC21_5==62100|NOC21_5==62101~10,
#Chef
NOC21_5==62200~15,
#Jewellers
NOC21_5==62202~7,
#funeral director
NOC21_5==62201~15,
#Contractors supervisors up to 72021
NOC21_5>=72010&NOC21_5<=72021~6,
#Railway and truck transport supervisors
NOC21_5>=72023&NOC21_5<=72024~11,
NOC21_5>=72100&NOC21_5<=72106~7,
NOC21_5>=72200&NOC21_5<=72205~7,
NOC21_5>=72300&NOC21_5<=72321~7,
NOC21_5>=72420&NOC21_5<=72429~7,
NOC21_5>=72500&NOC21_5<=72501~7,
NOC21_5==72999~7,
#pilots, air traffic ontrollers
NOC21_5>=72600&NOC21_5<=72604~6,
#messenger service supervisors
NOC21_5==72025~11,
#Supervisors logging and forestry
NOC21_5>=82010&NOC21_5<=82031~10,
#Manufacturing Supervisors
NOC21_5>=92010&NOC21_5<=92101~6,
##Major group 13
NOC21_5>=13100&NOC21_5<=13102~11,
NOC21_5>=13110&NOC21_5<=13111~11,
#Medical assistants
NOC21_5==13112~14,
NOC21_5==13200~11,
NOC21_5==13201~11,
#Dental assistants
NOC21_5==33100~15,
#Medical lab technicians
NOC21_5==33101~6,
NOC21_5>=33102&NOC21_5<=33109~15,
NOC21_5==43100~15,
NOC21_5==43109~15,
NOC21_5>=43200&NOC21_5<=43204~15,
#museum interpreters
NOC21_5==53100~15,
NOC21_5>=53110&NOC21_5<=53111~14,
NOC21_5>=53120&NOC21_5<=53125~14,
NOC21_5>=53200&NOC21_5<=53202~15,
#Real estate and insurance agents
NOC21_5>=63100&NOC21_5<=63102~10,
#Butchers and Bakers
NOC21_5==63201|NOC21_5==63202~7,
#cooks
NOC21_5==63200~15,
#hairstylists
NOC21_5>=63210&NOC21_5<=63211~15,
#Shusters and upholsters,
NOC21_5>=63220&NOC21_5<=63221~7,
#concrete plastereres
## Major gruop 73
NOC21_5>=73100&NOC21_5<=73102~7,
NOC21_5>=73110&NOC21_5<=7313~7,
NOC21_5==73200|NOC21_5==73202~7,
NOC21_5==73201~15,
NOC21_5==73209~7,
#Transport Truck drivers
NOC21_5==73300~7,
NOC21_5==73310|NOC21_5==73311~7,
NOC21_5==73301~15,
NOC21_5>=73400&NOC21_5<=73402~7,
##Major Group 83
NOC21_5>=83100&NOC21_5<=83101~8,
NOC21_5>=83110&NOC21_5<=83121~7,
##Major Group 93
#Plant process operators
#aircraft assemblers
NOC21_5>=93100&NOC21_5<=93200~8,
##major Group 14
#Office clerks
NOC21_5>=14100&NOC21_5<=14103~11,
NOC21_5==14110~12,
NOC21_5==14111~11,
NOC21_5==14112~11,
NOC21_5>=14200&NOC21_5<=14202~11,
NOC21_5>=14300&NOC21_5<=14301~11,
NOC21_5>=14400&NOC21_5<=14405~11,
#Major Group 44
NOC21_5>=44100&NOC21_5<=44101~16,
NOC21_5==44200~10,
#Major Group 54
NOC21_5==54100~15,
#Major Group 64
NOC21_5==64100~15,
NOC21_5==64101~10,
NOC21_5==64200~7,
NOC21_5>=64300&NOC21_5<=64301~16,
NOC21_5==64310~11,
NOC21_5==64311~15,
NOC21_5==64312~11,
NOC21_5==64313~12,
NOC21_5==64314~11,
NOC21_5>=64320&NOC21_5<=64322~15,
NOC21_5>=64400&NOC21_5<=64409~11,
#Security guards
NOC21_5==64411~16,
#letter carriers
NOC21_5==74100|NOC21_5==74101~11,
#Messengers
NOC21_5==74102~12,
# railway and deck workers
NOC21_5>=74200&NOC21_5<=7202~8,
NOC21_5==74203~7,
NOC21_5==74204~7,
#Garbage truck
NOC21_5==74205~8,
## Major Group 84
NOC21_5>=84100&NOC21_5<=84121 ~8,
## Major Group 94
NOC21_5>=94100&NOC21_5<=94107~8,
NOC21_5>=94110&NOC21_5<=94112~8,
NOC21_5>=94120&NOC21_5<=94129~8,
NOC21_5>=94130&NOC21_5<=94133~8,
NOC21_5>=94140&NOC21_5<=94143~8,
#printing
NOC21_5>=94150&NOC21_5<=94153~7,
#Assemblers
NOC21_5>=94200&NOC21_5<=94205~8,
NOC21_5>=94210&NOC21_5<=94219~8,
NOC21_5==45100~16,
NOC21_5==54109~16,
NOC21_5>=65100&NOC21_5<=65109~12,
NOC21_5>=65200&NOC21_5<=65329~12,
NOC21_5==75100~8,
NOC21_5==75101~8,
NOC21_5==75110~8,
NOC21_5>=75200&NOC21_5<=75201~16,
NOC21_5>=75210&NOC21_5<=75212~8,
NOC21_5>=85110&NOC21_5<=85121~8,
NOC21_5>=95100&NOC21_5<=95109~8
))->ces25b
#tests
ces25b %>%
  filter(NOC21_5>=84100&NOC21_5<=84123) %>%
  select(NOC21_5, occupation_oesch)
#Tests
ces25b %>%
  filter(NOC21_5>94100&NOC21_5<=94143) %>%
  select(NOC21_5, occupation_oesch)

#Add self-employed
ces25b$cps25_employment
ces25b %>%
  mutate(occupation_oesch=case_when(
    cps25_employment>0&cps25_employment<3~occupation_oesch,
    cps25_employment>8&cps25_employment<12~occupation_oesch,
    cps25_employment==3~4
  ))->ces25b
val_labels(ces25b$occupation_oesch)<-c(`Self-employed`=4,`Technical experts`=5, `Technicians`=6,
                                       `Skilled manual`=7, `Low-skilled manual`=8,
                                       `Higher-grade managers`=9, `Lower-grade managers`=10,
                                       `Skilled clerks`=11, `Unskilled clerks`=12,
                                       `Socio-cultural professionals`=13, `Socio-cultural (semi-professionals)`=14,
                                       `Skilled service`=15, `Low-skilled service`=16)
#table(ces25b$occupation_oesch)
# with(ces25b, table(as_factor(occupation_oesch)))
#
# with(ces25b, prop.table(table(as_factor(occupation_oesch))))
#
# with(ces25b, prop.table(table(as_factor(occupation_oesch_5))))

#check employment status
# ces25b %>%
#   count(cps25_employment, occupation_oesch) %>% as_factor() %>% view()
#table(ces25b$employment, as_factor(ces25b$cps25_employment))

#Create Occupation_oesch_5
ces25b %>%
  mutate(occupation_oesch_5=case_when(
    cps25_employment==3~3,
    occupation_oesch==1|occupation_oesch==2|occupation_oesch==5|occupation_oesch==9|occupation_oesch==13~1,
    occupation_oesch==6|occupation_oesch==10|occupation_oesch==14~2,
    occupation_oesch==7|occupation_oesch==11|occupation_oesch==15~4,
    occupation_oesch==8|occupation_oesch==12|occupation_oesch==16~5
  ))->ces25b
val_labels(ces25b$occupation_oesch_5)<-c(`Higher-grade service`=1,
                                         `Lower-grade service`=2,
                                         `Self-employed`=3,
                                         `Skilled workers`=4,
                                         `Unskilled workers`=5)
#Check


# ces25b %>%
#   mutate(occupation_oesch_6=case_when(
#     cps25_employment==3~4,
#     occupation_oesch==1|occupation_oesch==2|occupation_oesch==5|occupation_oesch==9~1,
#     occupation_oesch==13~2,
#     occupation_oesch==6|occupation_oesch==10|occupation_oesch==14~3,
#     occupation_oesch==7|occupation_oesch==11|occupation_oesch==15~5,
#     occupation_oesch==8|occupation_oesch==12|occupation_oesch==16~6
#   ))->ces25b

# val_labels(ces25b$occupation_oesch_6)<-c(`Managers`=1,
#                                          `Socio-Cultural Professionals`=2,
#                                          `Lower service`=3,
#                                          `Self-employed`=4,
#                                          `Skilled manual`=5,
#                                          `Unskilled manual`=6)
# #Check
# table(as_factor(ces25b$occupation_oesch_6))

# ces25b %>%
#   mutate(occupation_oesch_6=case_when(
#     teer==0~"Managers",
#     teer==1~"Professionals",
#     teer==2~"Semi-Professionals Associate Managers",
#     teer==3~"Skilled Workers",
#     teer>3~"Unskilled Workers",
#     cps25_employment==3~"Self-employed"
#   ))->ces25b

#ces25b$occupation_oesch_6<-factor(ces25b$occupation_oesch_6, levels=c("Unskilled Workers", "Skilled Workers",
#                                           "Semi-Professionals Associate Managers",
#                                           "Self-employed","Professionals", "Managers"))


# Add Subjective social class
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

ces25b %>%
  mutate(own_rent=case_when(
    cps25_property_1==1~"Own",
    cps25_rent==1 ~"Rent",
    cps25_rent==4~"Other"
  ))->ces25b
ces25b$own_rent<-factor(ces25b$own_rent, levels=c("Own", "Rent", "Other"))

#Use property1 and cps25_rent to cobble together

#### recode Age (cps25_age_in_years) ####
look_for(ces25b, "age")
ces25b$age<-ces25b$cps25_age_in_years

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
ces25b$ndp_rating<-Recode(as.numeric(ces25b$cps25_party_rating_25), "-99=NA")
table(ces25b$ndp_rating)

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

#recode Immigrant minority thermometer
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
