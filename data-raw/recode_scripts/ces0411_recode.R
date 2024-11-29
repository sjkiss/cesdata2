#File to Recode 2004-2011 CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces0411<-read_sav(file=here("data-raw/CES_04060811_ISR_revised.sav"))


####recode Gender (GENDER)####
#Gender is the same variable for all elections 2004-11

# look_for(ces0411, "sex")
ces0411$male<-Recode(ces0411$GENDER, "1=1; 5=0")
val_labels(ces0411$male)<-c(Female=0, Male=1)
#checks
val_labels(ces0411$male)
# table(ces0411$male)

#### Union ####
#recode Union Respondent (ces04_CPS_S6A)
ces0411$union04<-Recode(ces0411$ces04_CPS_S6A,
                        "1=1; 5=0; else=NA")
val_labels(ces0411$union04)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union04)
# table(ces0411$union04)

#recode Union Combined (ces04_CPS_S6A and ces04_CPS_S6B)
# table(ces0411$ces04_CPS_S6A, ces0411$ces04_CPS_S6B, useNA = "ifany")

ces0411 %>%
  mutate(union_both04=case_when(
    ces04_CPS_S6A==1 | ces04_CPS_S6B==1 ~ 1,
    ces04_CPS_S6A==5 ~ 0,
    ces04_CPS_S6B==5 ~ 0,
    ces04_CPS_S6A==8 & ces04_CPS_S6B==8 ~ NA_real_,
    ces04_CPS_S6A==9 & ces04_CPS_S6B==9 ~ NA_real_,
    ces04_CPS_S4==1 ~ 0,
    (ces06_CPS_S6A==1 | ces06_CPS_S6B==1) & ces04_rtype1==1 ~ 1,
    ces06_CPS_S6A==5 & ces04_rtype1==1 ~ 0,
    ces06_CPS_S6B==5 & ces04_rtype1==1 ~ 0,
  ))->ces0411

# table(ces0411$union_both04, useNA = "ifany")
1061+984
val_labels(ces0411$union_both04)<-c(None=0, Union=1)

#checks
val_labels(ces0411$union_both04)<-c(None=0, Union=1)
# table(ces0411$union_both04)

ces0411 %>%
  select(ces04_CPS_S6A, ces04_CPS_S6B, union_both04) %>%
  group_by(ces04_CPS_S6A, ces04_CPS_S6B, union_both04) %>%
  summarize(n=n())

# Some checks
# table( as_factor(ces0411$ces04_CPS_S6A), as_factor(ces0411$ces04_CPS_S6B), useNA = "ifany")
# table(as_factor(ces0411$union_both04), as_factor(ces0411$ces04_CPS_S6A), useNA = "ifany")
# table(as_factor(ces0411$union_both04), as_factor(ces0411$ces04_CPS_S6B), useNA = "ifany")

####Education (ces04_CPS_S3)####
# look_for(ces0411, "education")
ces0411$degree04<-Recode(ces0411$ces04_CPS_S3, "9:11=1; 1:8=0; else=NA")
val_labels(ces0411$degree04)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree04)
# table(ces0411$degree04)

#### Region (ces04_PROVINCE)####
# lookfor(ces0411, "province")
ces0411$region04<-Recode(ces0411$ces04_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region04)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region04)
# table(ces0411$region04)

####recode Province (ces04_PROVINCE)####
# look_for(ces0411, "province")
ces0411$prov04<-Recode(ces0411$ces04_PROVINCE, "10=1; 11=2; 12=3; 13=4; 24=5; 35=6; 46=7; 47=8; 48=9; 59=10; else=NA")
val_labels(ces0411$prov04)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces0411$prov04)
table(ces0411$prov04)

#recode Quebec (ces04_PROVINCE)
# look_for(ces0411, "province")
ces0411$quebec04<-Recode(ces0411$ces04_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec04)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec04)
# table(ces0411$quebec04)

#recode Age (YEARofBIRTH)
# look_for(ces0411, "birth")
#provide easy to use survey variable
ces0411$survey<-as_factor(ces0411$Survey_Type04060811)
#meake easy to use year of birth variable
ces0411$yob<-Recode(ces0411$YEARofBIRTH, "9998:9999=NA")
#This checks if "04" appears in the survey variable
str_detect(ces0411$survey, "04")
#check
# table(str_detect(ces0411$survey, "04"))
#pipe data frame
ces0411 %>%
  #mutate making new variable age04
  mutate(age04=case_when(
    #if 04 appears in the values for survey then define age-4 as the result of 2004-yob
    str_detect(ces0411$survey, "04")~2004-yob
  ))-> ces0411
#check
# table(ces0411$age04)
####recode Religion (ces04_CPS_S9) ####
#
# look_for(ces0411, "relig")
ces0411$religion04<-Recode(ces0411$ces04_CPS_S9, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")
val_labels(ces0411$religion04)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion04)
# table(ces0411$religion04)

####recode Language (ces04_CPS_INTLANG) ####
# look_for(ces0411, "language")
ces0411$language04<-Recode(ces0411$ces04_CPS_INTLANG, "2=0; 1=1; else=NA")
val_labels(ces0411$language04)<-c(French=0, English=1)
#checks
val_labels(ces0411$language04)
# table(ces0411$language04)

#### #recode Non-charter Language (ces04_CPS_S17)####
# look_for(ces0411, "language")
ces0411$non_charter_language04<-Recode(ces0411$ces04_CPS_S17, "1:5=0; 8:64=1; 65:66=0; 95:97=1; else=NA")
val_labels(ces0411$non_charter_language04)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language04)
# table(ces0411$non_charter_language04)

#### #recode Employment (ces04_CPS_S4) ####
# look_for(ces0411, "employed")
#ces0411$employment04<-Recode(ces0411$ces04_CPS_S4, "3:7=0; 1:2=1; 8:11=1; else=NA")
#ces0411$employment06<-Recode(ces0411$ces06_CPS_S4, "3:7=0; 1:2=1; 8:12=1; 13=0; 14:15=1; else=NA")

ces0411 %>%
  mutate(employment04=case_when(
    ces04_CPS_S4==1 ~ 1,
    ces04_CPS_S4==2 ~ 1,
    ces04_CPS_S4>2 & ces04_CPS_S4<8 ~ 0,
    ces04_CPS_S4>7 & ces04_CPS_S4<12 ~ 1,
    ces06_CPS_S4==1 & ces04_rtype1==1~ 1,
    ces06_CPS_S4==2 & ces04_rtype1==1~ 1,
    ces06_CPS_S4>2 & ces06_CPS_S4<8 & ces04_rtype1==1~ 0,
    ces06_CPS_S4>7 & ces06_CPS_S4<13 & ces04_rtype1==1~ 1,
    ces06_CPS_S4==13 & ces04_rtype1==1~ 0,
    ces06_CPS_S4==14 & ces04_rtype1==1~ 1,
    ces06_CPS_S4==15 & ces04_rtype1==1~ 1
  ))->ces0411

val_labels(ces0411$employment04)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment04)
# table(ces0411$employment04)

#### #recode Sector (ces04_CPS_S5 & ces04_CPS_S4) ####
# look_for(ces0411, "self-employed")
ces0411 %>%
  mutate(sector04=case_when(
    ces04_CPS_S5==5 ~1,
    ces04_CPS_S5==1 ~0,
    ces04_CPS_S5==0 ~0,
    ces04_CPS_S4==1 ~0,
    ces04_CPS_S4> 2 & ces04_CPS_S4< 12 ~ 0,
    ces04_CPS_S5==9 ~NA_real_ ,
    ces04_CPS_S5==8 ~NA_real_ ,
    ces06_CPS_S5==5 & ces04_rtype1==1~1,
    ces06_CPS_S5==1 & ces04_rtype1==1~0,
    ces06_CPS_S5==0 & ces04_rtype1==1~0,
    ces06_CPS_S4==1 & ces04_rtype1==1~0,
    ces06_CPS_S4> 2 & ces06_CPS_S4< 15 & ces04_rtype1==1~ 0,
  ))->ces0411

val_labels(ces0411$sector04)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector04)
# table(ces0411$sector04)
lookfor(ces0411, "occupation")
ces0411 %>%
  mutate(sector_welfare04=case_when(
    #Socioal Scientists
    sector04==1&( ces0411$ces04_PES_SD3 >2312& ces0411$ces04_PES_SD3 <2320)~1,
    #Social Workers
    sector04==1&( ces0411$ces04_PES_SD3 >2330& ces0411$ces04_PES_SD3 <2340)~1,
    #Counsellors
    sector04==1& ces0411$ces04_PES_SD3 ==2391~1,
        # Education
    sector04==1&( ces0411$ces04_PES_SD3 >2710& ces0411$ces04_PES_SD3 <2800)~1,
    #Health
    sector04==1&( ces0411$ces04_PES_SD3 >3111& ces0411$ces04_PES_SD3 <3160)~1,
    sector04==0 ~0,
TRUE~ NA
  ))->ces0411
with(ces0411, table(ces0411$sector_welfare04, useNA = "ifany"))
#### #recode Party ID (ces04_CPS_Q1A@3 and  ces04_CPS_Q1B@3`) ***note needs `...` to recognize the variable***####
look_for(ces0411, "yourself")
ces0411 %>%
  mutate(party_id04=case_when(
    `ces04_CPS_Q1A@3`==1 | `ces04_CPS_Q1B@3`==1 ~ 1,
    `ces04_CPS_Q1A@3`==2 | `ces04_CPS_Q1B@3`==2 ~ 2,
    `ces04_CPS_Q1A@3`==3 | `ces04_CPS_Q1B@3`==3 ~ 3,
    `ces04_CPS_Q1A@3`==5 | `ces04_CPS_Q1B@3`==5 ~ 2,
    `ces04_CPS_Q1A@3`==6 | `ces04_CPS_Q1B@3`==6 ~ 2,
    `ces04_CPS_Q1A@3`==8 | `ces04_CPS_Q1B@3`==8 ~ 5,
    `ces04_CPS_Q1A@3`==4 | `ces04_CPS_Q1B@3`==4 ~ 4,
    `ces04_CPS_Q1A@3`==9 | `ces04_CPS_Q1B@3`==9 ~ 0,
    `ces04_CPS_Q1A@3`==10 | `ces04_CPS_Q1B@3`==10 ~ 0,
    `ces04_CPS_Q1A@3`==97 | `ces04_CPS_Q1B@3`==97 ~ 0,
  ))->ces0411

val_labels(ces0411$party_id04)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id04)
table(ces0411$party_id04)

#### #recode Vote (ces04_PES_A3@3')####
# ***note needs `...`` to recognize the variable***
# look_for(ces0411, "party did you vote")
ces0411$vote04<-Recode(ces0411$`ces04_PES_A3@3`, "1=1; 2=2; 3=3; 4=4; 8=5; 6=2; 10=0; 0=0; else=NA")
val_labels(ces0411$vote04)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote04)
# table(ces0411$vote04)

#### #recode Occupation (ces04_PINPORR)####
# look_for(ces0411, "occupation")
# look_for(ces0411, "pinporr")
lookfor(ces0411, "occupation")
#ces0411$occupation04<-Recode(ces0411$ces04_PINPORR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
#ces0411$occupation08<-Recode(ces0411$ces08_PES_S3_NOCS, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#Panel respondents are added in from ces08 - ces04_rtype1==1 indicates that a respondent participated in the ces04 survey
#Both Pinporr and NOCS available in ces04
ces0411 %>%
  mutate(occupation04=case_when(
    ces04_PES_SD3 >0 & ces04_PES_SD3 <1100 ~ 2,
    ces04_PINPORR==1 ~ 1,
    ces04_PINPORR==2 ~ 1,
    ces04_PINPORR==4 ~ 1,
    ces04_PINPORR==5 ~ 1,
    ces04_PINPORR==3 ~ 2,
    ces04_PINPORR==6 ~ 2,
    ces04_PINPORR==7 ~ 2,
    ces04_PINPORR==9 ~ 3,
    ces04_PINPORR==12 ~ 3,
    ces04_PINPORR==14 ~ 3,
    ces04_PINPORR==8 ~ 4,
    ces04_PINPORR==10 ~ 4,
    ces04_PINPORR==13 ~ 5,
    ces04_PINPORR==15 ~ 5,
    ces04_PINPORR==16 ~ 5,
    ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 & ces04_rtype1==1~ 2,
    ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 & ces04_rtype1==1~ 1,
    ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 & ces04_rtype1==1~ 1,
    ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 & ces04_rtype1==1~ 1,
    ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 & ces04_rtype1==1~ 1,
    ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 & ces04_rtype1==1~ 1,
    ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 & ces04_rtype1==1~ 3,
    ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 & ces04_rtype1==1~ 3,
    ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 & ces04_rtype1==1~ 3,
    ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 & ces04_rtype1==1~ 3,
    ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 & ces04_rtype1==1~ 3,
    ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 & ces04_rtype1==1~ 4,
    ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 & ces04_rtype1==1~ 5,
    ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 & ces04_rtype1==1~ 4,
    ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 & ces04_rtype1==1~ 5,
    ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 & ces04_rtype1==1~ 4,
    ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 & ces04_rtype1==1~ 5,
  ))->ces0411
val_labels(ces0411$occupation04)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation04)
# table(ces0411$occupation04)
ces0411$ces04_CPS_S4

#recode Occupation3 as 6 class schema with self-employed (ces04_CPS_S4)
# look_for(ces0411, "employ")
ces0411$occupation043<-ifelse(ces0411$ces04_CPS_S4==1, 6, ces0411$occupation04)
val_labels(ces0411$occupation043)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation043)
# table(ces0411$occupation04_3)
####  #recode Income (ces04_CPS_S18)####
# look_for(ces0411, "income")
ces0411$ces04_CPS_S18
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")

ces0411 %>%
  mutate(income04=case_when(
    ces04_CPS_S18==1 ~ 1,
    ces04_CPS_S18==2 ~ 2,
    ces04_CPS_S18==3 ~ 2,
    ces04_CPS_S18==4 ~ 3,
    ces04_CPS_S18==5 ~ 3,
    ces04_CPS_S18==6 ~ 4,
    ces04_CPS_S18==7 ~ 4,
    ces04_CPS_S18==8 ~ 5,
    ces04_CPS_S18==9 ~ 5,
    ces04_CPS_S18==10 ~ 5,
    # ces06_CPS_S18==1 & ces04_rtype1==1 ~ 1,
    # ces06_CPS_S18==2 & ces04_rtype1==1 ~ 2,
    # ces06_CPS_S18==3 & ces04_rtype1==1 ~ 2,
    # ces06_CPS_S18==4 & ces04_rtype1==1 ~ 3,
    # ces06_CPS_S18==5 & ces04_rtype1==1 ~ 3,
    # ces06_CPS_S18==6 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==7 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==8 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==9 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==10 & ces04_rtype1==1 ~ 5,
  ))->ces0411

val_labels(ces0411$income04)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income04)
# table(ces0411$income04)
ces0411$ces04_CPS_S18

#Simon's Version
ces0411 %>%
  mutate(income042=case_when(
    ces04_CPS_S18==1 ~ 1,
    ces04_CPS_S18==2 ~ 2,
    ces04_CPS_S18==3 ~ 2,
    ces04_CPS_S18==4 ~ 3,
    ces04_CPS_S18==5 ~ 3,
    ces04_CPS_S18==6 ~ 3,
    ces04_CPS_S18==7 ~ 4,
    ces04_CPS_S18==8 ~ 4,
    ces04_CPS_S18==9 ~ 4,
    ces04_CPS_S18==10 ~ 5,
    # ces06_CPS_S18==1 & ces04_rtype1==1 ~ 1,
    # ces06_CPS_S18==2 & ces04_rtype1==1 ~ 2,
    # ces06_CPS_S18==3 & ces04_rtype1==1 ~ 2,
    # ces06_CPS_S18==4 & ces04_rtype1==1 ~ 3,
    # ces06_CPS_S18==5 & ces04_rtype1==1 ~ 3,
    # ces06_CPS_S18==6 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==7 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==8 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==9 & ces04_rtype1==1 ~ 4,
    # ces06_CPS_S18==10 & ces04_rtype1==1 ~ 5,
  ))->ces0411

val_labels(ces0411$income042)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#Income tertile 04
ces0411 %>%
  mutate(income_tertile04=case_when(
    #First Tertile
    ces04_CPS_S18<3 ~ 1,
    #Secon Tertiile
    ces04_CPS_S18>2 & ces04_CPS_S18 <6  ~ 2,
    #Third Tertile
    ces04_CPS_S18>5 & ces04_CPS_S18 <98  ~ 3,
  ))->ces0411
ces0411$ces04_CPS_S18
val_labels(ces0411$income_tertile04)<-c("Lowest"=1, "Middle"=2, "Highest"=3)

#Check
# ces0411 %>%
#   filter(income042==3&income_tertile04==3) %>%
#   select(ces04_CPS_S18, income042, income_tertile04) %>%
#   View()

#### recode Household size (ces04_CESHHWGT)####
# look_for(ces0411, "household")
ces0411$household04<-Recode(ces0411$ces04_CESHHWGT, "0.5078=0.5; 1.0156=1; 1.5234=1.5; 2.0312=2; 2.5391=2.5; 3.0469=3; 3.5547=3.5")
#checks
# table(ces0411$household04)

#### recode income / household size ####
ces0411$inc043<-Recode(ces0411$ces04_CPS_S18, "98:999=NA")
# table(ces0411$inc043)
ces0411$inc044<-ces0411$inc043/ces0411$household04
# table(ces0411$inc044)

ces0411 %>%
  mutate(income_house04=case_when(
    #First Tertile
    inc044<3 ~ 1,
    #Secon Tertiile
    inc044>2.99 & inc044 <6  ~ 2,
    #Third Tertile
    inc044>5.99 & inc044 <98  ~ 3,
  ))->ces0411

# table(ces0411$income_house04)
# table(ces0411$income_tertile04)
# table(ces0411$income_tertile04, ces0411$income_house04)

#### recode Religiosity (ces04_CPS_S11)####
# look_for(ces0411, "relig")
ces0411$religiosity04<-Recode(ces0411$ces04_CPS_S11, "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces0411$religiosity04)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$religiosity04)
# table(ces0411$religiosity04)

#### #recode Redistribution (ces04_CPS_F6)####
# look_for(ces0411, "rich")
val_labels(ces0411$ces04_CPS_F6)
ces0411$ces04_CPS_F6
ces0411$redistribution04<-Recode(as.numeric(ces0411$ces04_CPS_F6), "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution04)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution04)<-NULL
# table(ces0411$redistribution04)

#### #recode Pro-Redistribution (ces04_CPS_F6)####
ces0411$pro_redistribution04<-Recode(ces0411$ces04_CPS_F6, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution04)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution04)
# table(ces0411$pro_redistribution04)

#### #recode Market Liberalism (ces04_CPS_P11 and ces04_PES_G11)####
# look_for(ces0411, "private")
# look_for(ces0411, "blame")
ces0411$market041<-Recode(as.numeric(ces0411$ces04_CPS_P11), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces0411$market042<-Recode(as.numeric(ces0411$ces04_PES_G11), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$market041)<-NULL
#val_labels(ces0411$market042)<-NULL

#checks
# table(ces0411$market041, useNA = "ifany")
# table(ces0411$market042, useNA = "ifany")

#Scale Averaging
ces0411 %>%
  mutate(market_liberalism04=rowMeans(select(., market041:market042), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("market04")) %>%
  summary()
#Check distribution of market_liberalism04
# qplot(ces0411$market_liberalism04, geom="histogram")
# table(ces0411$market_liberalism04, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces0411 %>%
  select(market041, market042) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(market041, market042) %>%
  cor(., use="complete.obs")

#### #recode Immigration (ces04_CPS_P9)####
# look_for(ces0411, "imm")
ces0411$immigration_rates04<-Recode(as.numeric(ces0411$ces04_CPS_P9), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_rates04)
#val_labels(ces0411$immigration_rates04)<-NULL

#### recode Environment vs Jobs (ces04_MBS_A6)####
# look_for(ces0411, "env")
ces0411$enviro04<-Recode(as.numeric(ces0411$ces04_MBS_A6), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces0411$enviro04)
#val_labels(ces0411$enviro04)<-NULL

#### #recode Capital Punishment (ces04_CPS_P10) ####
# look_for(ces0411, "death")
ces0411$death_penalty04<-Recode(as.numeric(ces0411$ces04_CPS_P10), "1=1; 5=0.; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$death_penalty04)
#val_labels(ces0411$death_penalty04)<-NULL

#### #recode Crime (ces04_MBS_G5)####
# look_for(ces0411, "crime")
ces0411$crime04<-Recode(as.numeric(ces0411$ces04_MBS_G5), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$crime04)
#val_labels(ces0411$crime04)<-NULL

#### #recode Gay Rights (ces04_PES_G12@3) ####
# ***note needs `...` to recognize the variable***
# look_for(ces0411, "gays")
ces0411$gay_rights04<-Recode(as.numeric(ces0411$`ces04_PES_G12@3`), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
# table(ces0411$gay_rights04)
#val_labels(ces0411$gay_rights04)<-NULL

#### #recode Abortion (ces04_PES_G13)####
# look_for(ces0411, "abort")
ces0411$abortion04<-Recode(as.numeric(ces0411$ces04_PES_G13), "1=0; 2=0.25; 3=0.75; 4=1; 6=1; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$abortion04)
#val_labels(ces0411$abortion04)<-NULL

#### #recode Lifestyle (ces04_MBS_A7)####
# look_for(ces0411, "lifestyle")
ces0411$ces04_MBS_A7
# table(ces0411$ces04_MBS_A7, useNA = "ifany" )
ces0411$lifestyles04<-Recode(as.numeric(ces0411$ces04_MBS_A7), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
#val_labels(ces0411$lifestyles04)<-NULL

# table(ces0411$lifestyles04)

#### #recode Stay Home (ces04_CPS_P14)####
# look_for(ces0411, "home")
ces0411$stay_home04<-Recode(as.numeric(ces0411$ces04_CPS_P14), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$stay_home04)
#val_labels(ces0411$stay_home04)<-NULL

#### #recode Marriage Children (ces04_MBS_G4)####
# look_for(ces0411, "children")
ces0411$marriage_children04<-Recode(as.numeric(ces0411$ces04_MBS_G4), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$marriage_children04)
#val_labels(ces0411$marriage_children04)<-NULL

#### #recode Values (ces04_MBS_A9)####
# look_for(ces0411, "traditional")
ces0411$values04<-Recode(as.numeric(ces0411$ces04_MBS_A9), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$values04)
#val_labels(ces0411$values04)<-NULL

#### #recode Morals (ces04_MBS_A8)####
# look_for(ces0411, "moral")
ces0411$morals04<-Recode(as.numeric(ces0411$ces04_MBS_A8), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces0411$morals04)
#val_labels(ces0411$morals04)<-NULL

#### #recode Moral Traditionalism (abortion, lifestyles, stay home, values, marriage childen, morals)####
ces0411$trad043<-ces0411$abortion04
ces0411$trad047<-ces0411$lifestyles04
ces0411$trad041<-ces0411$stay_home04
ces0411$trad044<-ces0411$values04
ces0411$trad045<-ces0411$marriage_children04
ces0411$trad046<-ces0411$morals04
ces0411$trad042<-ces0411$gay_rights04
# table(ces0411$trad041)
# table(ces0411$trad042)
# table(ces0411$trad043)
# table(ces0411$trad044)
# table(ces0411$trad045)
# table(ces0411$trad046)
# table(ces0411$trad047)


#Remove Value Labels
ces0411 %>%
  mutate(across(.cols=num_range('trad04', 1:7), remove_val_labels)) ->ces0411

ces0411 %>%
  mutate(traditionalism04=rowMeans(select(., trad041:trad047), na.rm = T))->ces0411

ces0411 %>%
  select(starts_with("trad04")) %>%
  summary()
#Check distribution of traditionalism04
# qplot(ces0411$traditionalism04, geom="histogram")
# table(ces0411$traditionalism04, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad041, trad042, trad043, trad044, trad045, trad046, trad047) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad041, trad042, trad043, trad044, trad045, trad046, trad047) %>%
  cor(., use="complete.obs")

####recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)####
#Check value labels
ces0411 %>%
  select(trad041:trad042)

ces0411 %>%
  mutate(traditionalism204=rowMeans(select(., c('trad041', 'trad042')), na.rm=T))->ces0411
#Test recode
ces0411 %>%
  select(trad041, trad042, traditionalism204) %>%
  slice(823:840)

ces0411 %>%
  select(starts_with("trad04")) %>%
  summary()
#Check distribution of traditionalism204
# qplot(ces0411$traditionalism204, geom="histogram")
# table(ces0411$traditionalism204, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad041, trad042) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad041, trad042) %>%
  cor(., use="complete.obs")

#### recode 2nd Dimension (stay home, immigration, gay rights, crime)####
ces0411$author041<-ces0411$stay_home04
ces0411$author042<-ces0411$immigration_rates04
ces0411$author043<-ces0411$gay_rights04
ces0411$author044<-ces0411$crime04
# table(ces0411$author041)
# table(ces0411$author042)
# table(ces0411$author043)
# table(ces0411$author044)

#Check for value labels
ces0411 %>%
  select(starts_with('author04'))

# ces0411 %>%
#   rowwise() %>%
#   mutate(authoritarianism04=mean(c_across(author041:author044)
#                                , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('author041', 'author042', 'author043', 'author044', 'authoritarianism04')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<6)
#Scale Averaging
ces0411 %>%
  mutate(authoritarianism04=rowMeans(select(., author041:author044), na.rm=T))->ces0411

#Check distribution of authoritarianism04
# qplot(ces0411$authoritarianism04, geom="histogram")
# table(ces0411$authoritarianism04, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(author041, author042, author043, author044) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(author041, author042, author043, author044) %>%
  cor(., use="complete.obs")

#### #recode Quebec Accommodation (ces04_CPS_F9) (Left=more accom) ####
# look_for(ces0411, "quebec")
ces0411$quebec_accom04<-Recode(as.numeric(ces0411$ces04_CPS_F9), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_accom04)
#val_labels(ces0411$quebec_accom04)<-NULL

#recode Liberal leader (ces04_PES_F2 & ces04_CPS_G2)
# look_for(ces0411, "Martin")
ces0411$liberal_leader04<-Recode(as.numeric(ces0411$ces04_CPS_G2), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_leader04)
#val_labels(ces0411$liberal_leader04)<-NULL
#recode conservative leader (ces04_PES_F1 & ces04_CPS_G1)
# look_for(ces0411, "Harper")
ces0411$conservative_leader04<-Recode(as.numeric(ces0411$ces04_CPS_G1), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_leader04)
#val_labels(ces0411$conservative_leader04)<-NULL

#recode NDP leader (ces04_PES_F3 & ces04_CPS_G3)
# look_for(ces0411, "Layton")
ces0411$ndp_leader04<-Recode(as.numeric(ces0411$ces04_CPS_G3), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_leader04)
#val_labels(ces0411$ndp_leader04)<-NULL

#recode Bloc leader (ces04_PES_F4 & ces04_CPS_G4)
# look_for(ces0411, "Duceppe")
ces0411$bloc_leader04<-Recode(as.numeric(ces0411$ces04_CPS_G4), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_leader04)
#val_labels(ces0411$bloc_leader04)<-NULL

#recode liberal rating (ces04_PES_C1B & ces04_CPS_G8)
# look_for(ces0411, "liberal")
ces0411$liberal_rating04<-Recode(as.numeric(ces0411$ces04_CPS_G8), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_rating04)
#val_labels(ces0411$liberal_rating04)<-NULL

#recode conservative rating (ces04_PES_C1A & ces04_CPS_G7)
# look_for(ces0411, "conservative")
ces0411$conservative_rating04<-Recode(as.numeric(ces0411$ces04_CPS_G7), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_rating04)
#val_labels(ces0411$conservative_rating04)<-NULL

#recode NDP rating (ces04_PES_C1C & ces04_CPS_G9)
# look_for(ces0411, "new democratic")
ces0411$ndp_rating04<-Recode(as.numeric(ces0411$ces04_CPS_G9), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_rating04)
#val_labels(ces0411$ndp_rating04)<-NULL

#recode Bloc rating (ces04_PES_C1E & ces04_CPS_G10)
# look_for(ces0411, "bloc")
ces0411$bloc_rating04<-Recode(as.numeric(ces0411$ces04_CPS_G10), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_rating04)
#val_labels(ces0411$bloc_rating04)<-NULL

#recode Green rating (ces04_CPS_G11)
# look_for(ces0411, "green")
ces0411$green_rating04<-Recode(as.numeric(ces0411$ces04_CPS_G11), "0=1; 996:999=NA")
#checks
# table(ces0411$green_rating04)
#val_labels(ces0411$green_rating04)<-NULL

#### recode Education (ces04_PES_D1D)####
# look_for(ces0411, "edu")
ces0411$education04<-Recode(as.numeric(ces0411$ces04_PES_D1D), "3=0; 5=0.5; 1=1; 8=0.5; else=NA")
#checks
#table(ces0411$education04, ces0411$ces04_PES_D1D , useNA = "ifany" )
#val_labels(ces0411$education04)<-NULL

#### recode Personal Retrospective (ces04_CPS_F1) ####
# look_for(ces0411, "financial")
ces0411$personal_retrospective04<-Recode(as.numeric(ces0411$ces04_CPS_F1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$personal_retrospective04)<-c(Worse=0, Same=0.5, Better=1)
#val_labels(ces0411$personal_retrospective04)<-NULL

#checks
#val_labels(ces0411$personal_retrospective04)
# table(ces0411$personal_retrospective04, ces0411$ces04_CPS_F1 , useNA = "ifany" )

#### recode National Retrospective (ces04_CPS_M1) ####
# look_for(ces0411, "economy")
ces0411$national_retrospective04<-Recode(as.numeric(ces0411$ces04_CPS_M1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$national_retrospective04)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces0411$national_retrospective04)<-NULL
# table(ces0411$national_retrospective04, ces0411$ces04_CPS_M1 , useNA = "ifany" )

#### recode Ideology (ces04_MBS_H10) ####
# look_for(ces0411, "scale")
ces0411$ideology04<-Recode(as.numeric(ces0411$ces04_MBS_H10), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
#val_labels(ces0411$ideology04)<-c(Left=0, Right=1)
#checks
#val_labels(ces0411$ideology04)<-NULL
# table(ces0411$ideology04, ces0411$ces04_MBS_H10 , useNA = "ifany")

#### recode turnout (ces04_PES_A2A) ####
# look_for(ces0411, "vote")
ces0411$turnout04<-Recode(ces0411$ces04_PES_A2A, "1=1; 5=0; 8=0; else=NA")
val_labels(ces0411$turnout04)<-c(No=0, Yes=1)
#checks
#val_labels(ces0411$turnout04)
# table(ces0411$turnout04)
# table(ces0411$turnout04, ces0411$vote04)

#### recode political efficacy ####

#recode No Say (ces04_MBS_E11)
# look_for(ces0411, "have any say")
ces0411$efficacy_internal04<-Recode(as.numeric(ces0411$ces04_MBS_E11), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_internal04)<-c(low=0, high=1)
#checks
#val_labels(ces0411$efficacy_internal04)<-NULL
# table(ces0411$efficacy_internal04)
# table(ces0411$efficacy_internal04, ces0411$ces04_MBS_E11 , useNA = "ifany" )

#recode MPs lose touch (ces04_MBS_E5)
# look_for(ces0411, "lose touch")
ces0411$efficacy_external04<-Recode(as.numeric(ces0411$ces04_MBS_E5), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external04)<-c(low=0, high=1)
#checks
#val_labels(ces0411$efficacy_external04)<-NULL
# table(ces0411$efficacy_external04)
# table(ces0411$efficacy_external04, ces0411$ces04_MBS_E5 , useNA = "ifany" )

#recode Official Don't Care (ces04_PES_G3)
# look_for(ces0411, "cares much")
ces0411$efficacy_external204<-Recode(as.numeric(ces0411$ces04_PES_G3), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external204)<-c(low=0, high=1)
#checks
#val_labels(ces0411$efficacy_external204)<-NULL
# table(ces0411$efficacy_external204)
# table(ces0411$efficacy_external204, ces0411$ces04_PES_G3 , useNA = "ifany" )

ces0411 %>%
  mutate(political_efficacy04=rowMeans(select(., c("efficacy_external04", "efficacy_external204", "efficacy_internal04")), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
# qplot(ces0411$political_efficacy04, geom="histogram")
# table(ces0411$political_efficacy04, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces0411 %>%
  select(efficacy_external04, efficacy_external204, efficacy_internal04) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(efficacy_external04, efficacy_external204, efficacy_internal04) %>%
  cor(., use="complete.obs")

#### recode Most Important Question (ces04_CPS_A7) ####
# look_for(ces0411, "important issue")
ces0411$mip04<-Recode(ces0411$ces04_CPS_A7, "1=0; 10=6; 20:26=10; 30=7; 35:36=0; 39=18; 48=12; 50=9; 57:59=8; 60:64=15;
                                            65:66=4; 68=15; 69=8; 70=14; 71=2; 72=15; 73:74=14; 75=1; 76=14; 77=2; 78=1;
                                            79=12; 80:81=16; 90:91=3; 92:93=11; 94:95=0; else=NA")
val_labels(ces0411$mip04)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                             Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18)
 table(ces0411$mip04)

#### recode satisfaction with democracy (ces04_CPS_B10, ces04_PES_A8) ####
# look_for(ces0411, "dem")
ces0411$satdem04<-Recode(as.numeric(ces0411$ces04_PES_A8), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem04, ces0411$ces04_PES_A8, useNA = "ifany" )
#val_labels(ces0411$satdem04)<-NULL
ces0411$satdem204<-Recode(as.numeric(ces0411$ces04_CPS_B10), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$satdem204)<-NULL
#checks
# table(ces0411$satdem204, ces0411$ces04_CPS_B10, useNA = "ifany" )

#### recode Quebec Sovereignty (ces04_PES_C10) (Quebec only & Right=more sovereignty) ####
# look_for(ces0411, "quebec")
ces0411$quebec_sov04<-Recode(as.numeric(ces0411$ces04_PES_C10), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_sov04, ces0411$ces04_PES_C10, useNA = "ifany" )
#val_labels(ces0411$quebec_sov04)<-NULL

#### recode provincial alienation (ces04_CPS_P5) ####
# look_for(ces0411, "treat")
ces0411$prov_alienation04<-Recode(as.numeric(ces0411$ces04_CPS_P5), "3=1; 1=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$prov_alienation04, ces0411$ces04_CPS_P5, useNA = "ifany" )
#val_labels(ces0411$prov_alienation04)<-NULL

#### recode immigration society (ces04_MBS_G3) ####
# look_for(ces0411, "fit")
ces0411$immigration_soc04<-Recode(as.numeric(ces0411$ces04_MBS_G3), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_soc04, ces0411$ces04_MBS_G3, useNA = "ifany" )
#val_labels(ces0411$immigration_soc04)<-NULL

#recode welfare (ces04_PES_D1B)
# look_for(ces0411, "spend")
ces0411$welfare04<-Recode(as.numeric(ces0411$ces04_PES_D1B), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$welfare04, ces0411$ces04_PES_D1B, useNA = "ifany" )
#val_labels(ces0411$welfare04)<-NULL

#### Postgrad (ces04_CPS_S3) ####
# look_for(ces0411, "education")
ces0411$postgrad04<-Recode(as.numeric(ces0411$ces04_CPS_S3), "10:11=1; 1:9=0; else=NA")
#checks
# table(ces0411$postgrad04)

#### recode Break Promise (ces04_CPS_P6) ####
# look_for(ces0411, "promise")
ces0411$promise04<-Recode(as.numeric(ces0411$ces04_CPS_P6), "1=0; 3=0.5; 5=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$promise04)<-c(low=0, high=1)
#checks
#val_labels(ces0411$promise04)<-NULL
# table(ces0411$promise04)
# table(ces0411$promise04, ces0411$ces04_CPS_P6 , useNA = "ifany" )

#### recode Trust (ces04_PES_D7) ####
# look_for(ces0411, "trust")
ces0411$trust04<-Recode(ces0411$ces04_PES_D7, "1=1; 5=0; else=NA", as.numeric=T)
val_labels(ces0411$trust04)<-c(no=0, yes=1)
#checks
val_labels(ces0411$trust04)
# table(ces0411$trust04)
# table(ces0411$trust04, ces0411$ces04_PES_D7 , useNA = "ifany" )

#### recode political interest (ces04_CPS_A6) ####
# look_for(ces0411, "interest")
ces0411$pol_interest04<-Recode(as.numeric(ces0411$ces04_CPS_A6), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checkst
# table(ces0411$pol_interest04, ces0411$ces04_CPS_A6, useNA = "ifany" )

#### recode foreign born (ces04_CPS_S12) ####
# look_for(ces0411, "born")
ces0411$foreign04<-Recode(ces0411$ces04_CPS_S12, "1=0; 2:97=1; else=NA")
val_labels(ces0411$foreign04)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$foreign04)
# table(ces0411$foreign04, ces0411$ces04_CPS_S12, useNA="ifany")

#### recode Environment Spend (ces04_PES_D1F) ####
# look_for(ces0411, "env")
ces0411$enviro_spend04<-Recode(as.numeric(ces0411$ces04_PES_D1F), "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$enviro_spend04, useNA = "ifany" )
#val_labels(ces0411$enviro_spend04)<-NULL

#### recode duty (ces04_CPS_P16 ) ####
look_for(ces0411, "duty")
ces0411$duty04<-Recode(ces0411$ces04_CPS_P16 , "1=1; 2:7=0; else=NA")
val_labels(ces0411$duty04)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$duty04)
table(ces0411$duty04, ces0411$ces04_CPS_P16, useNA="ifany")

#### recode Women - how much should be done (ces04_CPS_F7) ####
look_for(ces0411, "women")
table(ces0411$ces04_CPS_F7)
ces0411$women04<-Recode(as.numeric(ces0411$ces04_CPS_F7), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$women04,  useNA = "ifany")

#### recode Race - how much should be done (ces04_CPS_F8) ####
look_for(ces0411, "racial")
table(ces0411$ces04_CPS_F8)
ces0411$race04<-Recode(as.numeric(ces0411$ces04_CPS_F8), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$race04,  useNA = "ifany")

#recode Previous Vote (ces04_CPS_Q6)
# look_for(ces0411, "vote")
ces0411$previous_vote04<-Recode(ces0411$ces04_CPS_Q6, "1=1; 2=2; 3=3; 4=4; 5:7=2; 8=5; 0=0; 9:10=0; else=NA")
val_labels(ces0411$previous_vote04)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$previous_vote04)
table(ces0411$previous_vote04)

#recode Previous Vote splitting Conservatives (ces04_CPS_Q6)
# look_for(ces0411, "vote")
ces0411$previous_vote043<-car::Recode(as.numeric(ces0411$ces04_CPS_Q6), "1=1; 2=2; 3=3; 4=4; 5:7=6; 8=5; 0=0; 10=0; else=NA")
val_labels(ces0411$previous_vote043)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces0411$previous_vote043)
table(ces0411$previous_vote043)


# Add election04 variable
ces0411 %>%
  mutate(election04=case_when(
    str_detect(survey, "04")~ 2004
  ))->ces0411
table(ces0411$election04, useNA = "ifany")
###Recode 2006 2nd ####

# Gender done at top
#### #recode Union Respondent (ces06_CPS_S6B)####

ces0411$union06<-Recode(ces0411$ces06_CPS_S6A, "1=1; 5=0; else=NA")
val_labels(ces0411$union06)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union06)
# table(ces0411$union06)

#### recode Union Combined (ces06_CPS_S6A and ces06_CPS_S6B)####
ces0411 %>%
  mutate(union_both06=case_when(
    ces06_CPS_S6A==1 | ces06_CPS_S6B==1 ~ 1,
    ces06_CPS_S6A==5 ~ 0,
    ces06_CPS_S6B==5 ~ 0,
    ces06_CPS_S6A==8 & ces06_CPS_S6B==8 ~ NA_real_,
    ces06_CPS_S6A==9 & ces06_CPS_S6B==9 ~ NA_real_,
    ces06_CPS_S4==1 ~ 0,
    (ces04_CPS_S6A==1 | ces04_CPS_S6B==1) & ces06_RECALL==1~ 1,
    ces04_CPS_S6A==5 & ces06_RECALL==1~ 0,
    ces04_CPS_S6B==5 & ces06_RECALL==1~ 0,
    ces04_CPS_S4==1 & ces06_RECALL==1~ 0,
    #    ces08_CPS_S6A==1 | ces04_CPS_S6B==1 & ces06_RECALL==1~ 1,
    #    ces08_CPS_S6A==5 & ces06_RECALL==1~ 0,
    #    ces08_CPS_S6B==5 & ces06_RECALL==1~ 0,
  ))->ces0411

val_labels(ces0411$union_both06)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union_both06)
# table(ces0411$union_both06)
# table(as_factor(ces0411$ces06_CPS_S6A), as_factor(ces0411$union_both06), useNA = "ifany")

#recode Education (ces06_CPS_S3)
# look_for(ces0411, "education")
#ces0411$degree06<-Recode(ces0411$ces06_CPS_S3, "9:11=1; 1:8=0; else=NA")
# table(ces0411$ces04_CPS_S3, ces0411$ces06_CPS_S3, useNA="ifany")

ces0411 %>%
  mutate(degree06=case_when(
    ces06_CPS_S3 >0 & ces06_CPS_S3 <9 ~ 0,
    ces06_CPS_S3 >8 & ces06_CPS_S3 <12 ~ 1,
    (ces04_CPS_S3 >0 & ces04_CPS_S3 <9) & ces06_RECALL==1~ 0,
    (ces04_CPS_S3 >8 & ces04_CPS_S3 <12) & ces06_RECALL==1~ 1,
  ))->ces0411

val_labels(ces0411$degree06)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree06)
# table(ces0411$degree06)

#### #recode Region (ces06_PROVINCE)####
# look_for(ces0411, "province")
ces0411$region06<-Recode(ces0411$ces06_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region06)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region06)
# table(ces0411$region06)

####recode Province (ces06_PROVINCE)####
# look_for(ces0411, "province")
ces0411$prov06<-Recode(ces0411$ces06_PROVINCE, "10=1; 11=2; 12=3; 13=4; 24=5; 35=6; 46=7; 47=8; 48=9; 59=10; else=NA")
val_labels(ces0411$prov06)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces0411$prov06)
table(ces0411$prov06)

#### #recode Quebec (ces06_PROVINCE)####
# look_for(ces0411, "province")
ces0411$quebec06<-Recode(ces0411$ces06_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec06)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec06)
# table(ces0411$quebec06)

#### #recode Age (YEARofBIRTH)####
# table(str_detect(ces0411$survey, "06"))
#pipe data frame
ces0411 %>%
  #mutate making new variable age06
  mutate(age06=case_when(
    str_detect(ces0411$survey, "06")~2006-yob
  ))-> ces0411
#check
# table(ces0411$age06)

#### #recode Religion (ces06_CPS_S9)####
# look_for(ces0411, "relig")
#ces0411$religion06<-Recode(ces0411$ces06_CPS_S9, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:21=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")

ces0411 %>%
  mutate(religion06=case_when(
    ces06_CPS_S9==0 ~ 0,
    ces06_CPS_S9==1 ~ 2,
    ces06_CPS_S9==2 ~ 2,
    ces06_CPS_S9==3 ~ 3,
    ces06_CPS_S9==4 ~ 1,
    ces06_CPS_S9==5 ~ 1,
    ces06_CPS_S9==6 ~ 3,
    ces06_CPS_S9==7 ~ 2,
    ces06_CPS_S9==8 ~ 3,
    ces06_CPS_S9==9 ~ 2,
    ces06_CPS_S9==10 ~ 2,
    ces06_CPS_S9==11 ~ 3,
    ces06_CPS_S9==12 ~ 2,
    ces06_CPS_S9==13 ~ 2,
    ces06_CPS_S9==14 ~ 2,
    ces06_CPS_S9==15 ~ 3,
    ces06_CPS_S9 >15 & ces06_CPS_S9 <22 ~ 2,
    ces06_CPS_S9==97 ~ 3,
    ces04_CPS_S9==0 & ces06_RECALL==1~ 0,
    ces04_CPS_S9==1 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==2 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==3 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==4 & ces06_RECALL==1~ 1,
    ces04_CPS_S9==5 & ces06_RECALL==1~ 1,
    ces04_CPS_S9==6 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==7 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==8 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==9 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==10 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==11 & ces06_RECALL==1~ 3,
    ces04_CPS_S9==12 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==13 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==14 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==15 & ces06_RECALL==1~ 3,
    ces04_CPS_S9 >15 & ces04_CPS_S9 <22 & ces06_RECALL==1~ 2,
    ces04_CPS_S9==97 & ces06_RECALL==1~ 3,
  ))->ces0411

val_labels(ces0411$religion06)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion06)
# table(ces0411$religion06)

#### #recode Language (ces06_CPS_INTLANG)####
# look_for(ces0411, "language")
ces0411$language06<-Recode(ces0411$ces06_CPS_INTLANG, "2=0; 1=1; else=NA")
val_labels(ces0411$language06)<-c(French=0, English=1)
#checks
val_labels(ces0411$language06)
# table(ces0411$language06)

#### #recode Non-charter Language (ces06_CPS_S17)####
# look_for(ces0411, "language")
# table(ces0411$ces06_CPS_S17)
#ces0411$non_charter_language06<-Recode(ces0411$ces06_CPS_S17, "1:5=0; 8:64=1; 65:66=0; 95:97=1; else=NA")

ces0411 %>%
  mutate(non_charter_language06=case_when(
    ces06_CPS_S17 >0 & ces06_CPS_S17 <6 ~ 0,
    ces06_CPS_S17 >7 & ces06_CPS_S17 <65 ~ 1,
    ces06_CPS_S17 >64 & ces06_CPS_S17 <67 ~ 0,
    ces06_CPS_S17 >94 & ces06_CPS_S17 <98 ~ 1,
    (ces04_CPS_S17 >0 & ces04_CPS_S17 <6) & ces06_RECALL==1~ 0,
    (ces04_CPS_S17 >7 & ces04_CPS_S17 <65) & ces06_RECALL==1~ 1,
    (ces04_CPS_S17 >64 & ces04_CPS_S17 <67) & ces06_RECALL==1~ 0,
    (ces04_CPS_S17 >94 & ces04_CPS_S17 <98) & ces06_RECALL==1~ 1,
  ))->ces0411

val_labels(ces0411$non_charter_language06)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language06)
# table(ces0411$non_charter_language06)
# table(ces0411$ces06_CPS_S17, ces0411$non_charter_language06, useNA = "ifany")
# table(ces0411$survey, ces0411$non_charter_language06)

#### #recode Employment (ces06_CPS_S4)####
# look_for(ces0411, "employed")
ces0411$employment06<-Recode(ces0411$ces06_CPS_S4, "3:7=0; 1:2=1; 8:12=1; 13=0; 14:15=1; else=NA")

val_labels(ces0411$employment06)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment06)
# table(ces0411$employment06)

#### #recode Sector (ces06_CPS_S5 & ces06_CPS_S4)####
# look_for(ces0411, "self-employed")
ces0411 %>%
  mutate(sector06=case_when(
    ces06_CPS_S5==5 ~1,
    ces06_CPS_S5==1 ~0,
    ces06_CPS_S5==0 ~0,
    ces06_CPS_S4==1 ~0,
    ces06_CPS_S4> 2 & ces06_CPS_S4< 16 ~ 0,
    ces06_CPS_S5==9 ~NA_real_ ,
    ces06_CPS_S5==8 ~NA_real_ ,
  ))->ces0411

val_labels(ces0411$sector06)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector06)
# table(ces0411$sector06)
ces0411$ces06_CPS_S5
ces0411 %>%
  mutate(sector_welfare06=case_when(
    #Community and social service workers
ces0411$ces06_CPS_S5==5&ces06_PES_SD3 ==4212~ 1,
ces0411$ces06_CPS_S5==5&ces06_PES_SD3 ==4213~ 1,
ces0411$ces06_CPS_S5==5&ces06_PES_SD3 ==4214~1,
ces0411$ces06_CPS_S5==5&ces06_PES_SD3 ==4215~1,
ces0411$ces06_CPS_S5==5&ces06_PES_SD3 ==4216~1,
ces0411$ces06_CPS_S5==5&ces06_PES_SD3 ==4212~1,
#University professors
ces0411$ces06_CPS_S5==5&(ces06_PES_SD3>4120 &ces06_PES_SD3<4153) ~1,
#Health
ces0411$ces06_CPS_S5==5&(ces06_PES_SD3>3110 &ces06_PES_SD3<3415) ~1,
#probation officers
ces0411$ces06_CPS_S5==5&(ces06_PES_SD3==4155)~1,
#PRivate sector
ces0411$ces06_CPS_S5==1~0,
#Self-employed
ces06_CPS_S4==1 ~0,
#Not working
ces06_CPS_S4==0 ~0,
TRUE~ NA))->ces0411

# Public Sector Security
ces0411 %>%
  mutate(sector_security06=case_when(
    #Police officers
    ces0411$ces06_CPS_S5==5&  ces06_PES_SD3==6261~1,
    #Security Guards
    ces06_PES_SD3==6651~1,
    # CAF officers
    ces0411$ces06_CPS_S5==5& ces06_PES_SD3==0643~1,
    #CAF occupations
    ces0411$ces06_CPS_S5==5&ces06_PES_SD3==6464~1,
    #PRivate sector
    ces0411$ces06_CPS_S5==1~0,
    #Self-employed
    ces06_CPS_S4==1 ~0,
    #Not working
    ces06_CPS_S4==0 ~0,
    TRUE~ NA
  )
         )->ces0411

table(ces0411$sector_welfare06)
table(ces0411$sector_security06)
#### #recode Party ID (ces06_CPS_Q1A and ces06_CPS_Q1B)####
# look_for(ces0411, "yourself")
ces0411 %>%
  mutate(party_id06=case_when(
    ces06_CPS_Q1A==1 | ces06_CPS_Q1B==1 ~ 1,
    ces06_CPS_Q1A==2 | ces06_CPS_Q1B==2 ~ 2,
    ces06_CPS_Q1A==3 | ces06_CPS_Q1B==3 ~ 3,
    ces06_CPS_Q1A==5 | ces06_CPS_Q1B==5 ~ 5,
    ces06_CPS_Q1A==0 | ces06_CPS_Q1B==0 ~ 0,
    ces06_CPS_Q1A==4 | ces06_CPS_Q1B==4 ~ 4,
    ces06_CPS_Q1A==9 | ces06_CPS_Q1B==9 ~ 0,
  ))->ces0411

val_labels(ces0411$party_id06)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id06)
table(ces0411$party_id06)

#### #recode Vote (ces06_PES_B4A and ces06_PES_B4B) ####
# look_for(ces0411, "party did you vote")
ces0411$ces06_PES_B4A
ces0411 %>%
  mutate(vote06=case_when(
    ces06_PES_B4A==1 | ces06_PES_B4B==1 ~ 1,
    ces06_PES_B4A==2 | ces06_PES_B4B==2 ~ 2,
    ces06_PES_B4A==3 | ces06_PES_B4B==3 ~ 3,
    ces06_PES_B4A==5 | ces06_PES_B4B==5 ~ 5,
    ces06_PES_B4A==0 | ces06_PES_B4B==0 ~ 0,
    ces06_PES_B4A==4 | ces06_PES_B4B==4 ~ 4,
  ))->ces0411
# table(ces0411$ces06_PES_B4A, ces0411$vote06)
val_labels(ces0411$vote06)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote06)
# table(ces0411$vote06)

#### #recode Occupation (ces06_PES_SD3)####
# look_for(ces0411, "occupation")
# look_for(ces0411, "employ")
#ces0411$occupation06<-Recode(ces0411$ces06_PES_SD3, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#ces0411$occupation04<-Recode(ces0411$ces04_PINPORR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
#Panel respondents are added in from both ces04 and ces08 - ces06_RECALL==1 indicates that a respondent participated in the ces06 survey
ces0411 %>%
  mutate(occupation06=case_when(
    ces06_PES_SD3 >0 & ces06_PES_SD3 <1100 ~ 2,
    ces06_PES_SD3 >1099 & ces06_PES_SD3 <1200 ~ 1,
    ces06_PES_SD3 >2099 & ces06_PES_SD3 <2200 ~ 1,
    ces06_PES_SD3 >3099 & ces06_PES_SD3 <3200 ~ 1,
    ces06_PES_SD3 >4099 & ces06_PES_SD3 <4200 ~ 1,
    ces06_PES_SD3 >5099 & ces06_PES_SD3 <5200 ~ 1,
    ces06_PES_SD3 >1199 & ces06_PES_SD3 <1501 ~ 3,
    ces06_PES_SD3 >2199 & ces06_PES_SD3 <3100 ~ 3,
    ces06_PES_SD3 >3199 & ces06_PES_SD3 <4100 ~ 3,
    ces06_PES_SD3 >4199 & ces06_PES_SD3 <5100 ~ 3,
    ces06_PES_SD3 >5199 & ces06_PES_SD3 <6701 ~ 3,
    ces06_PES_SD3 >7199 & ces06_PES_SD3 <7400 ~ 4,
    ces06_PES_SD3 >7399 & ces06_PES_SD3 <7701 ~ 5,
    ces06_PES_SD3 >8199 & ces06_PES_SD3 <8400 ~ 4,
    ces06_PES_SD3 >8399 & ces06_PES_SD3 <8701 ~ 5,
    ces06_PES_SD3 >9199 & ces06_PES_SD3 <9300 ~ 4,
    ces06_PES_SD3 >9299 & ces06_PES_SD3 <9701 ~ 5,
    (ces04_PES_SD3 >0 & ces04_PES_SD3 <1001) & ces06_RECALL==1~ 2,
    ces04_PINPORR==1 & ces06_RECALL==1~ 1,
    ces04_PINPORR==2 & ces06_RECALL==1~ 1,
    ces04_PINPORR==4 & ces06_RECALL==1~ 1,
    ces04_PINPORR==5 & ces06_RECALL==1~ 1,
    ces04_PINPORR==3 & ces06_RECALL==1~ 2,
    ces04_PINPORR==6 & ces06_RECALL==1~ 2,
    ces04_PINPORR==7 & ces06_RECALL==1~ 2,
    ces04_PINPORR==9 & ces06_RECALL==1~ 3,
    ces04_PINPORR==12 & ces06_RECALL==1~ 3,
    ces04_PINPORR==14 & ces06_RECALL==1~ 3,
    ces04_PINPORR==8 & ces06_RECALL==1~ 4,
    ces04_PINPORR==10 & ces06_RECALL==1~ 4,
    ces04_PINPORR==13 & ces06_RECALL==1~ 5,
    ces04_PINPORR==15 & ces06_RECALL==1~ 5,
    ces04_PINPORR==16 & ces06_RECALL==1~ 5,
    ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 & ces06_RECALL==1~ 2,
    ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 & ces06_RECALL==1~ 1,
    ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 & ces06_RECALL==1~ 1,
    ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 & ces06_RECALL==1~ 1,
    ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 & ces06_RECALL==1~ 1,
    ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 & ces06_RECALL==1~ 1,
    ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 & ces06_RECALL==1~ 3,
    ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 & ces06_RECALL==1~ 3,
    ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 & ces06_RECALL==1~ 3,
    ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 & ces06_RECALL==1~ 3,
    ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 & ces06_RECALL==1~ 3,
    ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 & ces06_RECALL==1~ 4,
    ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 & ces06_RECALL==1~ 5,
    ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 & ces06_RECALL==1~ 4,
    ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 & ces06_RECALL==1~ 5,
    ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 & ces06_RECALL==1~ 4,
    ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 & ces06_RECALL==1~ 5,
  ))->ces0411
val_labels(ces0411$occupation06)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation06)
# table(ces0411$occupation06)

#recode Occupation3 as 6 class schema with self-employed (ces06_CPS_S4)
# look_for(ces0411, "employ")
ces0411$occupation063<-ifelse(ces0411$ces06_CPS_S4==1, 6, ces0411$occupation06)
#ces0411$occupation063<-ifelse((ces0411$ces04_CPS_S4==1 & ces0411$ces06_RECALL==1), 6, ces0411$occupation06)
val_labels(ces0411$occupation063)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation063)
# table(ces0411$occupation06_3)

#### #recode Income (ces06_CPS_S18)####
# look_for(ces0411, "income")

# look_for(ces0411, "income")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
ces0411 %>%
  mutate(income06=case_when(
    ces06_CPS_S18==1 ~ 1,
    ces06_CPS_S18==2 ~ 2,
    ces06_CPS_S18==3 ~ 2,
    ces06_CPS_S18==4 ~ 3,
    ces06_CPS_S18==5 ~ 3,
    ces06_CPS_S18==6 ~ 4,
    ces06_CPS_S18==7 ~ 4,
    ces06_CPS_S18==8 ~ 4,
    ces06_CPS_S18==9 ~ 4,
    ces06_CPS_S18==10 ~ 5,
    ces04_CPS_S18==1 & ces06_RECALL==1 ~ 1,
    ces04_CPS_S18==2 & ces06_RECALL==1 ~ 2,
    ces04_CPS_S18==3 & ces06_RECALL==1 ~ 2,
    ces04_CPS_S18==4 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==5 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==6 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==7 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==8 & ces06_RECALL==1 ~ 5,
    ces04_CPS_S18==9 & ces06_RECALL==1 ~ 5,
    ces04_CPS_S18==10 & ces06_RECALL==1 ~ 5,
    # ces08_PES_S9A> -1 & ces08_PES_S9A < 30 & ces06_RECALL==1~ 1,
    # ces08_PES_S9A> -1 & ces08_PES_S9A < 30 & ces06_RECALL==1~ 1,
    # ces08_PES_S9A> 29 & ces08_PES_S9A < 50 & ces06_RECALL==1~ 2,
    # ces08_PES_S9A> 29 & ces08_PES_S9A < 50 & ces06_RECALL==1~ 2,
    # ces08_PES_S9A> 49 & ces08_PES_S9A < 70 & ces06_RECALL==1~ 3,
    # ces08_PES_S9A> 49 & ces08_PES_S9A < 70 & ces06_RECALL==1~ 3,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 99 & ces08_PES_S9A < 998 & ces06_RECALL==1~ 5,
    # ces08_PES_S9A> 99 & ces08_PES_S9A < 998 & ces06_RECALL==1~ 5,
  ))->ces0411

val_labels(ces0411$income06)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income06)
# table(ces0411$income06)

#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
#Simon's Version
ces0411 %>%
  mutate(income062=case_when(
    ces06_CPS_S18==1 ~ 1,
    ces06_CPS_S18==2 ~ 2,
    ces06_CPS_S18==3 ~ 2,
    ces06_CPS_S18==4 ~ 3,
    ces06_CPS_S18==5 ~ 3,
    ces06_CPS_S18==6 ~ 3,
    ces06_CPS_S18==7 ~ 4,
    ces06_CPS_S18==8 ~ 4,
    ces06_CPS_S18==9 ~ 4,
    ces06_CPS_S18==10 ~ 5,
    ces04_CPS_S18==1 & ces06_RECALL==1 ~ 1,
    ces04_CPS_S18==2 & ces06_RECALL==1 ~ 2,
    ces04_CPS_S18==3 & ces06_RECALL==1 ~ 2,
    ces04_CPS_S18==4 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==5 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==6 & ces06_RECALL==1 ~ 3,
    ces04_CPS_S18==7 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==8 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==9 & ces06_RECALL==1 ~ 4,
    ces04_CPS_S18==10 & ces06_RECALL==1 ~ 5,
    # ces08_PES_S9A> -1 & ces08_PES_S9A < 30 & ces06_RECALL==1~ 1,
    # ces08_PES_S9A> -1 & ces08_PES_S9A < 30 & ces06_RECALL==1~ 1,
    # ces08_PES_S9A> 29 & ces08_PES_S9A < 50 & ces06_RECALL==1~ 2,
    # ces08_PES_S9A> 29 & ces08_PES_S9A < 50 & ces06_RECALL==1~ 2,
    # ces08_PES_S9A> 49 & ces08_PES_S9A < 70 & ces06_RECALL==1~ 3,
    # ces08_PES_S9A> 49 & ces08_PES_S9A < 70 & ces06_RECALL==1~ 3,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 69 & ces08_PES_S9A < 100 & ces06_RECALL==1~ 4,
    # ces08_PES_S9A> 99 & ces08_PES_S9A < 998 & ces06_RECALL==1~ 5,
    # ces08_PES_S9A> 99 & ces08_PES_S9A < 998 & ces06_RECALL==1~ 5,
  ))->ces0411

val_labels(ces0411$income06)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
ces0411$ces06_CPS_S18
ces0411$ces06_RECALL
#Income tertile 06
ces0411 %>%
  mutate(income_tertile06=case_when(
    #First Tertile
    ces06_CPS_S18<3 ~ 1,
    #Secon Tertiile
    ces06_CPS_S18>2 & ces06_CPS_S18 <6  ~ 2,
    #Third Tertile
    ces06_CPS_S18>5 & ces06_CPS_S18 <98  ~ 3,
    #First Tertile_Panel
    ces04_CPS_S18<3 & ces06_RECALL==1 ~1,
    #Secon Tertiile_Panel
    ces04_CPS_S18>2 & ces04_CPS_S18 <6 & ces06_RECALL==1 ~ 2,
    #Third Tertile_Panel
    ces04_CPS_S18>5 & ces04_CPS_S18 <98 & ces06_RECALL==1 ~ 3,
  ))->ces0411

val_labels(ces0411$income_tertile06)<-c("Lowest"=1, "Middle"=2, "Highest"=3)

#checks
val_labels(ces0411$income06)
# table(ces0411$income06)

#### recode Household size (ces06_CESHHWGT)####
# look_for(ces0411, "household")
ces0411$household06<-Recode(ces0411$ces06_CESHHWGT, "0.542586=0.5; 1.085172=1; 1.627758=1.5; 2.170344=2; 2.71293=2.5; 3.255515=3; 3.798101=3.5; 5.968445=5.5")
#checks
# table(ces0411$household06)

#### recode income / household size ####
ces0411$inc063<-Recode(ces0411$ces06_CPS_S18, "98:999=NA")
# table(ces0411$inc063)
ces0411$inc064<-ces0411$inc063/ces0411$household06
# table(ces0411$inc064)

ces0411 %>%
  mutate(income_house06=case_when(
    #First Tertile
    inc064<3  ~ 1,
    #Secon Tertiile
    inc064>2.99 & inc064 <7 ~ 2,
    #Third Tertile
    inc064>6.99 & inc064 <98 ~ 3,
    #First Tertile
    inc064<3 & ces06_RECALL==1 ~ 1,
    #Secon Tertiile
    inc064>2.99 & inc064 <7 & ces06_RECALL==1 ~ 2,
    #Third Tertile
    inc064>6.99 & inc064 <98 & ces06_RECALL==1 ~ 3,
  ))->ces0411

# table(ces0411$income_house06)
# table(ces0411$income_tertile06)
# table(ces0411$income_tertile06, ces0411$income_house06)

#### recode Religiosity (ces06_CPS_S11)####
# look_for(ces0411, "relig")
ces0411$religiosity06<-Recode(ces0411$ces06_CPS_S11, "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces0411$religiosity06)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$religiosity06)
# table(ces0411$religiosity06)

#### #recode Redistribution (ces06_CPS_F6)####
# look_for(ces0411, "rich")
val_labels(ces0411$ces06_CPS_F6)
ces0411$redistribution06<-Recode(as.numeric(ces0411$ces06_CPS_F6), "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution06)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution06)
# table(ces0411$redistribution06)

#recode Pro-Redistribution (ces06_CPS_F6)
ces0411$pro_redistribution06<-Recode(ces0411$ces06_CPS_F6, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution06)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution06)
# table(ces0411$pro_redistribution06)

#### #recode Market Liberalism (ces06_CPS_I2 and ces06_PES_G9)####
# look_for(ces0411, "private")
# look_for(ces0411, "blame")
ces0411$market061<-Recode(as.numeric(ces0411$ces06_CPS_I2), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces0411$market062<-Recode(as.numeric(ces0411$ces06_PES_G9), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$market061)
# table(ces0411$market062)

#Check for value labels
ces0411 %>%
  select(starts_with('market06'))

ces0411 %>%
  mutate(across(num_range('market06', 1:2), remove_val_labels))->ces0411
#
# ces0411 %>%
#   rowwise() %>%
#   mutate(market_liberalism06=mean(
#     c_across(market061:market062)
#     , na.rm=T )) -> out
#
# out %>%
#   ungroup() %>%
#   select(c('market061', 'market062', 'market_liberalism06')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces0411 %>%
  mutate(market_liberalism06=rowMeans(select(., market061:market062), na.rm = T))->ces0411


ces0411 %>%
  select(starts_with("market06")) %>%
  summary()
#Check distribution of market_liberalism04
# qplot(ces0411$market_liberalism06, geom="histogram")
# table(ces0411$market_liberalism06, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(market061, market062) %>%
  psych::alpha(.)

#Check correlation
ces0411 %>%
  select(market061, market062) %>%
  cor(., use="complete.obs")

#### #recode Immigration (ces06_CPS_P7)####
# look_for(ces0411, "imm")
ces0411$immigration_rates06<-Recode(as.numeric(ces0411$ces06_CPS_P7), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_rates06, useNA = "ifany")

#recode Capital Punishment (ces06_CPS_I11)
# look_for(ces0411, "death")
ces0411$death_penalty06<-Recode(as.numeric(ces0411$ces06_CPS_I11), "1=1; 3=0; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$death_penalty06, useNA = "ifany" )

#recode Crime (ces06_CPS_I13) (youth offenders question)
# look_for(ces0411, "crime")
ces0411$crime06<-Recode(as.numeric(ces0411$ces06_CPS_I13), "1=1; 2=0; 3=0.25; 4=0.75; 7=0.5; 6:8=0.5; else=NA")
#checks
# table(ces0411$crime06, useNA = "ifany" )

#recode Gay Rights (ces06_PES_G7)
# look_for(ces0411, "gays")
ces0411$gay_rights06<-Recode(as.numeric(ces0411$ces06_PES_G7), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
# table(ces0411$gay_rights06, useNA = "ifany")

#recode Abortion (ces06_PES_G11)
# look_for(ces0411, "abort")
ces0411$abortion06<-Recode(as.numeric(ces0411$ces06_PES_G11), "1=0; 2=0.25; 3=0.75; 4=1; 6=1; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$abortion06, useNA = "ifany")

#recode Stay Home (ces06_CPS_P3 )
# look_for(ces0411, "home")
ces0411$stay_home06<-Recode(as.numeric(ces0411$ces06_CPS_P3) , "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$stay_home06, useNA = "ifany")

#recode Moral Traditionalism (abortion, stay home)
ces0411$trad063<-ces0411$abortion06
ces0411$trad062<-ces0411$stay_home06
ces0411$trad061<-ces0411$gay_rights06
# table(ces0411$trad061)
# table(ces0411$trad062)
# table(ces0411$trad063)

#Remove value labels
ces0411 %>%
  select(starts_with('trad06'))

ces0411 %>%
  mutate(across(num_range('trad06', 1:3), remove_val_labels))->ces0411
# ces0411 %>%
#   rowwise() %>%
#   mutate(traditionalism06=mean(
#     c_across(trad061:trad063)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('trad061', 'trad062', 'trad063', 'traditionalism06')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<4)
#Scale Averaging
ces0411 %>%
  mutate(traditionalism06=rowMeans(select(., trad061:trad063), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("trad06")) %>%
  summary()
#Check distribution of traditionalism04
# qplot(ces0411$traditionalism06, geom="histogram")
# table(ces0411$traditionalism06, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad061, trad062, trad063) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad061, trad062, trad063) %>%
  cor(., use="complete.obs")

####recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)####
# ces0411 %>%
#   rowwise() %>%
#   mutate(traditionalism206=mean(c_across(trad061:trad062)
#                                 , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('trad061', 'trad062', 'traditionalism206')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<6)
#Scale Averaging
ces0411 %>%
  mutate(traditionalism206=rowMeans(select(., c('trad061', 'trad062')), na.rm=T))->ces0411

ces0411$traditionalism206
ces0411 %>%
  select(starts_with("trad06")) %>%
  summary()
#Check distribution of traditionalism206
# qplot(ces0411$traditionalism206, geom="histogram")
# table(ces0411$traditionalism206, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad061, trad062) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad061, trad062) %>%
  cor(., use="complete.obs")

#### recode 2nd Dimension (stay home, immigration, gay rights, crime)####
ces0411$author061<-ces0411$stay_home06
ces0411$author062<-ces0411$immigration_rates06
ces0411$author063<-ces0411$gay_rights06
ces0411$author064<-ces0411$crime06
# table(ces0411$author061)
# table(ces0411$author062)
# table(ces0411$author063)
# table(ces0411$author064)

ces0411 %>%
  mutate(across(.cols=num_range('author06', 1:4), remove_val_labels))->ces0411
# ces0411 %>%
#   rowwise() %>%
#   mutate(authoritarianism06=mean(c_across(author061:author064)
#                                  , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('author061', 'author062', 'author063', 'author064', 'authoritarianism06')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<6)
#Scale Averaging
ces0411 %>%
  mutate(authoritarianism06=rowMeans(select(., author061:author064), na.rm=T))->ces0411


ces0411 %>%
  select(starts_with("author06")) %>%
  summary()
#Check distribution of authoritarianism06
# qplot(ces0411$authoritarianism06, geom="histogram")
# table(ces0411$authoritarianism06, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(author061, author062, author063, author064) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(author061, author062, author063, author064) %>%
  cor(., use="complete.obs")

#### recode Quebec Accommodation (ces06_CPS_F7) (Left=more accom) ####
# look_for(ces0411, "quebec")
ces0411$quebec_accom06<-Recode(as.numeric(ces0411$ces06_CPS_F7), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_accom06)


#recode Liberal leader (ces06_PES_F2 & ces06_CPS_G2)
# look_for(ces0411, "Martin")
ces0411$liberal_leader06<-Recode(as.numeric(ces0411$ces06_CPS_G2), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_leader06)

#recode conservative leader (ces06_PES_F1 & ces06_CPS_G1)
# look_for(ces0411, "Harper")
ces0411$conservative_leader06<-Recode(as.numeric(ces0411$ces06_CPS_G1), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_leader06)

#recode NDP leader (ces06_PES_F3 & ces06_CPS_G3)
# look_for(ces0411, "Layton")
ces0411$ndp_leader06<-Recode(as.numeric(ces0411$ces06_CPS_G3), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_leader06)

#recode Bloc leader (ces06_PES_F4 & ces06_CPS_G4)
# look_for(ces0411, "Duceppe")
ces0411$bloc_leader06<-Recode(as.numeric(ces0411$ces06_CPS_G4), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_leader06)

#recode liberal rating (ces06_PES_C1B & ces06_CPS_G8)
# look_for(ces0411, "liberal")
ces0411$liberal_rating06<-Recode(as.numeric(ces0411$ces06_CPS_G8), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_rating06)

#recode conservative rating (ces06_PES_C1A & ces06_CPS_G7)
# look_for(ces0411, "conservative")
ces0411$conservative_rating06<-Recode(as.numeric(ces0411$ces06_CPS_G7), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_rating06)

#recode NDP rating (ces06_PES_C1C & ces06_CPS_G9)
# look_for(ces0411, "new democratic")
ces0411$ndp_rating06<-Recode(as.numeric(ces0411$ces06_CPS_G9), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_rating06)

#recode Bloc rating (ces06_PES_C1E & ces06_CPS_G10)
# look_for(ces0411, "bloc")
ces0411$bloc_rating06<-Recode(as.numeric(ces0411$ces06_CPS_G10), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_rating06)

#recode Green rating (ces06_PES_C1F & ces06_CPS_G11)
# look_for(ces0411, "green")
ces0411$green_rating06<-Recode(as.numeric(ces0411$ces06_CPS_G11), "0=1; 996:999=NA")
#checks
# table(ces0411$green_rating06)

#### recode Education (ces06_PES_D1D)####
# look_for(ces0411, "edu")
ces0411$education06<-Recode(as.numeric(ces0411$ces06_PES_D1D), "3=0; 5=0.5; 1=1; 8=0.5; else=NA")
#checks
# table(ces0411$education06, ces0411$ces06_PES_D1D , useNA = "ifany" )

#### recode Personal Retrospective (ces06_CPS_F1) ####
# look_for(ces0411, "financial")
ces0411$personal_retrospective06<-Recode(as.numeric(ces0411$ces06_CPS_F1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$personal_retrospective06)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces0411$personal_retrospective06)
# table(ces0411$personal_retrospective06, ces0411$ces06_CPS_F1 , useNA = "ifany" )

#### recode National Retrospective (ces06_CPS_M1) ####
# look_for(ces0411, "economy")
ces0411$national_retrospective06<-Recode(as.numeric(ces0411$ces06_CPS_M1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$national_retrospective06)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces0411$national_retrospective06)
# table(ces0411$national_retrospective06, ces0411$ces06_CPS_M1 , useNA = "ifany" )

#### #recode Immigrants take Jobs away (ces06_PES_G10)####
# look_for(ces0411, "jobs")
ces0411$immigration_job06<-Recode(as.numeric(ces0411$ces06_PES_G10), "7=0; 5=0.25; 3=0.75; 1=1; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_job06, ces0411$ces06_PES_G10, useNA = "ifany" )

#### recode political efficacy ####

#recode No Say N/A
#recode Lose Touch N/A

#recode Official Don't Care (ces06_PES_G2)
# look_for(ces0411, "cares much")
ces0411$efficacy_external206<-Recode(as.numeric(ces0411$ces06_PES_G2), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external206)<-c(low=0, high=1)
#checks
#val_labels(ces0411$efficacy_external206)
# table(ces0411$efficacy_external206)
# table(ces0411$efficacy_external206, ces0411$ces06_PES_G2 , useNA = "ifany" )

ces0411$political_efficacy06<-ces0411$efficacy_external206
# table(ces0411$political_efficacy06)

#### recode turnout (ces06_PES_B1) ####
# look_for(ces0411, "vote")
ces0411$turnout06<-Recode(ces0411$ces06_PES_B1, "1=1; 5=0; 8=0; else=NA")
val_labels(ces0411$turnout06)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$turnout06)
# table(ces0411$turnout06)
# table(ces0411$turnout06, ces0411$vote06)

#### recode Most Important Question (ces06_CPS_A2) ####
# look_for(ces0411, "important issue")
ces0411$mip06<-Recode(ces0411$ces06_CPS_A2, "1:6=0; 10=6; 20:26=10; 30=7; 35:36=0; 39=18; 48=12; 50=9; 57:59=8; 60:64=15;
                                            65=4; 70=14; 71=2; 72=15; 73:74=14; 75=1; 76=14; 77=2; 78=13; 79=12; 80:82=16;
					                                  83=11; 84=0; 90:91=3; 92:93=11; 94:95=0; else=NA")
val_labels(ces0411$mip06)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                             Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18)
 table(ces0411$mip06)

#### recode satisfaction with democracy (ces06_CPS_A1, ces06_PES_B10) ####
# look_for(ces0411, "dem")
ces0411$satdem06<-Recode(as.numeric(ces0411$ces06_PES_B10), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem06, ces0411$ces06_PES_B10, useNA = "ifany" )

ces0411$satdem206<-Recode(as.numeric(ces0411$ces06_CPS_A1), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem206, ces0411$ces06_CPS_A1, useNA = "ifany" )

#### recode Quebec Sovereignty (ces06_CPS_I8) (Quebec only & Right=more sovereignty) ####
# look_for(ces0411, "quebec")
ces0411$quebec_sov06<-Recode(as.numeric(ces0411$ces06_CPS_I8), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_sov06, ces0411$ces06_CPS_I8, useNA = "ifany" )

#### recode provincial alienation (ces06_CPS_I12) ####
# look_for(ces0411, "treat")
ces0411$prov_alienation06<-Recode(as.numeric(ces0411$ces06_CPS_I12), "3=1; 1=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$prov_alienation06, ces0411$ces06_CPS_I12, useNA = "ifany" )

#recode welfare (ces06_PES_D1B)
# look_for(ces0411, "spend")
ces0411$welfare06<-Recode(as.numeric(ces0411$ces06_PES_D1B), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$welfare06, ces0411$ces06_PES_D1B, useNA = "ifany" )

#### Postgrad (ces06_CPS_S3) ####
# look_for(ces0411, "education")
ces0411 %>%
  mutate(postgrad06=case_when(
    ces06_CPS_S3 >0 & ces06_CPS_S3 <10 ~ 0,
    ces06_CPS_S3 >9 & ces06_CPS_S3 <12 ~ 1,
    (ces04_CPS_S3 >0 & ces04_CPS_S3 <10) & ces06_RECALL==1~ 0,
    (ces04_CPS_S3 >9 & ces04_CPS_S3 <12) & ces06_RECALL==1~ 1,
  ))->ces0411

#checks
# table(ces0411$postgrad06)

#### recode Break Promise (ces06_CPS_P9) ####
# look_for(ces0411, "promise")
ces0411$promise06<-Recode(as.numeric(ces0411$ces06_CPS_P9), "1=0; 3=0.5; 5=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$promise06)<-c(low=0, high=1)
#checks
#val_labels(ces0411$promise06)
# table(ces0411$promise06)
# table(ces0411$promise06, ces0411$ces06_CPS_P9 , useNA = "ifany" )

#### recode Trust (not available in 2006) ####

#### recode political interest (ces06_CPS_A8) ####
# look_for(ces0411, "interest")
ces0411$pol_interest06<-Recode(as.numeric(ces0411$ces06_CPS_A8), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checkst
# table(ces0411$pol_interest06, ces0411$ces06_CPS_A8, useNA = "ifany" )

#### recode foreign born (ces06_CPS_S12) ####
# look_for(ces0411, "born")
ces0411$foreign06<-Recode(as.numeric(ces0411$ces06_CPS_S12), "1=0; 2:97=1; 0=0; else=NA")
val_labels(ces0411$foreign06)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$foreign06)
# table(ces0411$foreign06, ces0411$ces06_CPS_S12, useNA="ifany")

#### recode Environment Spend (ces06_PES_D1F) ####
# look_for(ces0411, "env")
ces0411$enviro_spend06<-Recode(as.numeric(ces0411$ces06_PES_D1F), "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$enviro_spend06, useNA = "ifany" )

#### recode duty (ces06_CPS_P5 ) ####
look_for(ces0411, "duty")
ces0411$duty06<-Recode(ces0411$ces06_CPS_P5 , "1=1; 2:7=0; else=NA")
val_labels(ces0411$duty06)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$duty06)
table(ces0411$duty06, ces0411$ces06_CPS_P5, useNA="ifany")

#### recode Women - how much should be done (ces06_PES_I4) ####
look_for(ces0411, "women")
table(ces0411$ces06_PES_I4)
ces0411$women06<-Recode(as.numeric(ces0411$ces06_PES_I4), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$women06,  useNA = "ifany")

#### recode Race - how much should be done (ces06_PES_I3) ####
look_for(ces0411, "racial")
table(ces0411$ces06_PES_I3)
ces0411$race06<-Recode(as.numeric(ces0411$ces06_PES_I3), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$race06,  useNA = "ifany")

#recode Previous Vote (ces06_CPS_Q6B)
# look_for(ces0411, "vote")
ces0411$previous_vote06<-Recode(ces0411$ces06_CPS_Q6B, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$previous_vote06)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$previous_vote06)
table(ces0411$previous_vote06)

ces0411 %>%
  mutate(election06=case_when(
    str_detect(survey, "06")~ 2006
  ))->ces0411
table(ces0411$election06, useNA = "ifany")

####Recode 2008 3rd ####

# Gender done at top

#### #recode Union Respondent (ces08_CPS_S6A)####
# look_for(ces0411, "union")
#Make sure the union variable is coming from the respondent question
ces0411$union08<-Recode(ces0411$ces08_CPS_S6A, "1=1; 5=0; else=NA")
val_labels(ces0411$union08)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union08)
# table(ces0411$union08)

#### #recode Union Combined (ces08_CPS_S6A and ces08_CPS_S6B)####
ces0411 %>%
  mutate(union_both08=case_when(
    ces08_CPS_S6A==1 | ces08_CPS_S6B==1 ~ 1,
    ces08_CPS_S6A==5 ~ 0,
    ces08_CPS_S6B==5 ~ 0,
    ces08_CPS_S6A==8 & ces08_CPS_S6B==8 ~ NA_real_,
    ces08_CPS_S6A==9 & ces08_CPS_S6B==9 ~ NA_real_,
    (ces06_CPS_S6A==1 | ces06_CPS_S6B==1) & ces08_CPS_REPLICATE==9999~ 1,
    (ces06_CPS_S6A==5) & ces08_CPS_REPLICATE==9999~ 0,
    (ces06_CPS_S6B==5) & ces08_CPS_REPLICATE==9999~ 0,
    (ces06_CPS_S4==1) & ces08_CPS_REPLICATE==9999~ 0,
    # (ces04_CPS_S6A==1 | ces04_CPS_S6B==1) & ces08_CPS_REPLICATE==9999~ 1,
    # (ces04_CPS_S6A==5) & ces08_CPS_REPLICATE==9999~ 0,
    # (ces04_CPS_S6B==5) & ces08_CPS_REPLICATE==9999~ 0,
  ))->ces0411

val_labels(ces0411$union_both08)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union_both08)
# table(ces0411$union_both08)
# table(as_factor(ces0411$ces08_CPS_S6A), as_factor(ces0411$union_both08), useNA = "ifany")

#### #recode Education (ces08_CPS_S3)####
# look_for(ces0411, "education")
#ces0411$degree08<-Recode(ces0411$ces08_CPS_S3, "9:11=1; 1:8=0; else=NA")

ces0411 %>%
  mutate(degree08=case_when(
    ces08_CPS_S3 >0 & ces08_CPS_S3 <9 ~ 0,
    ces08_CPS_S3 >8 & ces08_CPS_S3 <12 ~ 1,
    # (ces04_CPS_S3 >0 & ces04_CPS_S3 <9) & ces08_CPS_REPLICATE==9999 ~ 0,
    # (ces04_CPS_S3 >8 & ces04_CPS_S3 <12) & ces08_CPS_REPLICATE==9999 ~ 1,
  ))->ces0411

val_labels(ces0411$degree08)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree08)
# table(ces0411$degree08)

#### #recode Region (ces08_PROVINCE)####
# look_for(ces0411, "province")
ces0411$region08<-Recode(ces0411$ces08_PROVINCE, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region08)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region08)
# table(ces0411$region08)

####recode Province (ces08_PROVINCE)####
# look_for(ces0411, "province")
ces0411$prov08<-Recode(ces0411$ces08_PROVINCE, "10=1; 11=2; 12=3; 13=4; 24=5; 35=6; 46=7; 47=8; 48=9; 59=10; else=NA")
val_labels(ces0411$prov08)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces0411$prov08)
table(ces0411$prov08)

#### #recode Quebec (ces08_PROVINCE)####
# look_for(ces0411, "province")
ces0411$quebec08<-Recode(ces0411$ces08_PROVINCE, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec08)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec08)
# table(ces0411$quebec08)

#### #recode Age (YEARofBIRTH)####
# table(str_detect(ces0411$survey, "08"))
#pipe data frame
ces0411 %>%
  #mutate making new variable age08
  mutate(age08=case_when(
    str_detect(ces0411$survey, "08")~2008-yob
    #dump in a test dataframe
  ))-> ces0411
#check
# table(ces0411$age08, useNA = "ifany")

#### #recode Religion (ces08_CPS_S9)####
# look_for(ces0411, "relig")
#ces0411$religion08<-Recode(ces0411$ces08_CPS_S9, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")


ces0411 %>%
  filter(str_detect(survey,"PES08"))->ces08

ces0411 %>%
  mutate(religion08=case_when(
    ces08_CPS_S9==0 ~ 0,
    ces08_CPS_S9==1 ~ 2,
    ces08_CPS_S9==2 ~ 2,
    ces08_CPS_S9==3 ~ 3,
    ces08_CPS_S9==4 ~ 1,
    ces08_CPS_S9==5 ~ 1,
    ces08_CPS_S9==6 ~ 3,
    ces08_CPS_S9==7 ~ 2,
    ces08_CPS_S9==8 ~ 3,
    ces08_CPS_S9==9 ~ 2,
    ces08_CPS_S9==10 ~ 2,
    ces08_CPS_S9==11 ~ 3,
    ces08_CPS_S9==12 ~ 2,
    ces08_CPS_S9==13 ~ 2,
    ces08_CPS_S9==14 ~ 2,
    ces08_CPS_S9==15 ~ 3,
    ces08_CPS_S9 >15 & ces08_CPS_S9 <21 ~ 2,
    ces08_CPS_S9==97 ~ 3,
    # ces04_CPS_S9==0 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S9==1 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==2 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==3 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==4 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S9==5 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S9==6 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==7 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==8 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==9 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==10 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==11 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9==12 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==13 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==14 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==15 & ces08_CPS_REPLICATE==9999~ 3,
    # ces04_CPS_S9 >15 & ces04_CPS_S9 <22 & ces08_CPS_REPLICATE==9999~ 2,
    # ces04_CPS_S9==97 & ces08_CPS_REPLICATE==9999~ 3,
  ))->ces0411

val_labels(ces0411$religion08)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#table(ces08$religion08, useNA = "ifany")
table(ces08$survey, ces08$ces08_CPS_S9, useNA = "ifany") %>% view()
#with(ces08, table(ces08_CPS_S9, religion08 , useNA = "ifany"))
#checks
val_labels(ces0411$religion08)
# table(ces0411$religion08, useNA = "ifany" )

#### #recode Language (ces08_CPS_INTLANG) ####
# look_for(ces0411, "language")
#ces0411$language08<-Recode(ces0411$ces08_CPS_INTLANG, "2=0; 1=1; else=NA")

ces0411 %>%
  mutate(language08=case_when(
    ces08_CPS_INTLANG==2 ~ 0,
    ces08_CPS_INTLANG==1 ~ 1,
    # ces04_CPS_INTLANG==2 & ces08_CPS_REPLICATE==9999 ~ 0,
    # ces04_CPS_INTLANG==1 & ces08_CPS_REPLICATE==9999 ~ 1,
  ))->ces0411

val_labels(ces0411$language08)<-c(French=0, English=1)
#checks
val_labels(ces0411$language08)
# table(ces0411$language08, useNA = "ifany" )

#### #recode Non-charter Language (ces08_CPS_S17)####
# look_for(ces0411, "language")
#ces0411$non_charter_language08<-Recode(ces0411$ces08_CPS_S17, "1:5=0; 8:64=1; 65:66=0; 95:97=1; else=NA")

ces0411 %>%
  mutate(non_charter_language08=case_when(
    ces08_CPS_S17 >0 & ces08_CPS_S17 <6 ~ 0,
    ces08_CPS_S17 >7 & ces08_CPS_S17 <65 ~ 1,
    ces08_CPS_S17 >64 & ces08_CPS_S17 <67 ~ 0,
    ces08_CPS_S17 >94 & ces08_CPS_S17 <98 ~ 1,
    # ces04_CPS_S17 >0 & ces04_CPS_S17 <6 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S17 >7 & ces04_CPS_S17 <65 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S17 >64 & ces04_CPS_S17 <67 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S17 >94 & ces04_CPS_S17 <98 & ces08_CPS_REPLICATE==9999~ 1,
  ))->ces0411

val_labels(ces0411$non_charter_language08)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language08)
# table(ces0411$non_charter_language08)
# table(ces0411$survey, ces0411$non_charter_language08)

#recode Employment (ces08_CPS_S4)
# look_for(ces0411, "employed")
#ces0411$employment08<-Recode(ces0411$ces08_CPS_S4, "3:7=0; 1:2=1; 8:11=1; else=NA")
#ces0411$employment06<-Recode(ces0411$ces06_CPS_S4, "3:7=0; 1:2=1; 8:12=1; 13=0; 14:15=1; else=NA")
#ces0411$employment04<-Recode(ces0411$ces04_CPS_S4, "3:7=0; 1:2=1; 8:11=1; else=NA")

ces0411 %>%
  mutate(employment08=case_when(
    ces08_CPS_S4 >2 & ces08_CPS_S4 <8 ~ 0,
    ces08_CPS_S4 >7 & ces08_CPS_S4 <12 ~ 1,
    ces08_CPS_S4==1 ~ 1,
    ces08_CPS_S4==2 ~ 1,
    # ces06_CPS_S4 >2 & ces06_CPS_S4 <8 & ces08_CPS_REPLICATE==9999~ 0,
    # ces06_CPS_S4 >7 & ces06_CPS_S4 <13 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==1 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==2 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==14 & ces08_CPS_REPLICATE==9999~ 1,
    # ces06_CPS_S4==15 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S4 >2 & ces04_CPS_S4 <8 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S4 >7 & ces04_CPS_S4 <13 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S4==1 & ces08_CPS_REPLICATE==9999~ 1,
    # ces04_CPS_S4==2 & ces08_CPS_REPLICATE==9999~ 1,
  ))->ces0411

val_labels(ces0411$employment08)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment08)
# table(ces0411$employment08)

#recode Sector (ces08_CPS_S5 & ces08_CPS_S4)
# look_for(ces0411, "self-employed")
ces0411 %>%
  mutate(sector08=case_when(
    ces08_CPS_S5==5 ~1,
    ces08_CPS_S5==1 ~0,
    ces08_CPS_S5==0 ~0,
    ces08_CPS_S4==1 ~0,
    ces08_CPS_S4> 2 & ces08_CPS_S4< 12 ~ 0,
    ces08_CPS_S5==9 ~NA_real_ ,
    ces08_CPS_S5==8 ~NA_real_ ,
    # ces06_CPS_S5==5 & ces08_CPS_REPLICATE==9999~1,
    # ces06_CPS_S5==1 & ces08_CPS_REPLICATE==9999~0,
    # ces06_CPS_S5==0 & ces08_CPS_REPLICATE==9999~0,
    # ces06_CPS_S4==1 & ces08_CPS_REPLICATE==9999~0,
    # ces06_CPS_S4> 2 & ces06_CPS_S4< 16 & ces08_CPS_REPLICATE==9999~ 0,
    # ces04_CPS_S5==5 & ces08_CPS_REPLICATE==9999~1,
    # ces04_CPS_S5==1 & ces08_CPS_REPLICATE==9999~0,
    # ces04_CPS_S5==0 & ces08_CPS_REPLICATE==9999~0,
    # ces04_CPS_S4==1 & ces08_CPS_REPLICATE==9999~0,
    # ces04_CPS_S4> 2 & ces04_CPS_S4< 12 & ces08_CPS_REPLICATE==9999~ 0,
  ))->ces0411

val_labels(ces0411$sector08)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector08)
ces0411 %>%
  mutate(sector_welfare08=case_when(
    #Community and social service workers
  ces08_CPS_S5==5&ces08_PES_S3_NOCS ==4212~ 1,
   ces08_CPS_S5==5&ces08_PES_S3_NOCS ==4213~ 1,
    ces08_CPS_S5==5&ces08_PES_S3_NOCS ==4214~1,
    ces08_CPS_S5==5&ces08_PES_S3_NOCS ==4215~1,
   ces08_CPS_S5==5&ces08_PES_S3_NOCS ==4216~1,
    ces08_CPS_S5==5&ces08_PES_S3_NOCS ==4212~1,
    #University professors
    ces08_CPS_S5==5&(ces08_PES_S3_NOCS>4120 &ces08_PES_S3_NOCS<4153) ~1,
    #Health
    ces08_CPS_S5==5&(ces08_PES_S3_NOCS>3110 &ces08_PES_S3_NOCS<3415) ~1,
    #probation officers
    ces08_CPS_S5==5&(ces08_PES_S3_NOCS==4155)~1,
     ces08_CPS_S5==1 ~0,
    ces08_CPS_S5==0 ~0,
    ces08_CPS_S4==1 ~0,
    ces08_CPS_S4> 2 & ces08_CPS_S4< 12 ~ 0,
    ces08_CPS_S5==9 ~NA_real_ ,
    ces08_CPS_S5==8 ~NA_real_))->ces0411
# Sector Security 08
ces0411 %>%
  mutate(sector_security08=case_when(
    #Police officers
    ces08_CPS_S5==5&  ces08_PES_S3_NOCS==6261~1,
    #Security Guards
    ces08_PES_S3_NOCS==6651~1,
    # CAF officers
    ces08_CPS_S5==5& ces08_PES_S3_NOCS==0643~1,
    #CAF occupations
   ces08_CPS_S5==5&ces08_PES_S3_NOCS==6464~1,
    #PRivate sector
    ces08_CPS_S5==1~0,
    #Self-employed
    ces08_CPS_S4==1 ~0,
    #Not working
    ces08_CPS_S4==0 ~0,
    TRUE~ NA
  )
  )->ces0411

# table(ces0411$sector08)

#recode Vote (ces08_PES_B4B)
look_for(ces0411, "party did you vote")
ces0411$vote08<-Recode(ces0411$ces08_PES_B4B, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$vote08)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote08)
table(ces0411$vote08)

#recode Party ID (ces08_PES_K1)
# look_for(ces0411, "yourself")
ces0411$party_id08<-Recode(ces0411$ces08_PES_K1, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$party_id08)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id08)
table(ces0411$party_id08)

#### #recode Occupation (ces08_PES_S3_NOCS)####
# look_for(ces0411, "occupation")
# look_for(ces0411, "employ")
#ces0411$occupation08<-Recode(ces0411$ces08_PES_S3_NOCS, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#ces0411$occupation06<-Recode(ces0411$ces06_PES_SD3, "1:1000=2; 1100:1199=1; 2100:3300=1; 4100:6300=1; 1200:1500=3; 6400:6700=3; 3400:3500=3; 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
#ces0411$occupation04<-Recode(ces0411$ces04_PINPORR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
#Panel respondents are added in from both ces04 and ces06 - ces08_CPS_REPLICATE==9999 indicates that a respondent participated in the ces08 survey
ces0411 %>%
  mutate(occupation08=case_when(
    ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 ~ 2,
    ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 ~ 1,
    ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 ~ 1,
    ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 ~ 1,
    ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 ~ 1,
    ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 ~ 1,
    ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 ~ 3,
    ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 ~ 3,
    ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 ~ 3,
    ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 ~ 3,
    ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 ~ 3,
    ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 ~ 4,
    ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 ~ 5,
    ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 ~ 4,
    ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 ~ 5,
    ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 ~ 4,
    ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 ~ 5,
    ces06_PES_SD3 >0 & ces06_PES_SD3 <1100 & ces08_CPS_REPLICATE==9999~ 2,
    ces06_PES_SD3 >1099 & ces06_PES_SD3 <1200 & ces08_CPS_REPLICATE==9999~ 1,
    ces06_PES_SD3 >2099 & ces06_PES_SD3 <2200 & ces08_CPS_REPLICATE==9999~ 1,
    ces06_PES_SD3 >3099 & ces06_PES_SD3 <3200 & ces08_CPS_REPLICATE==9999~ 1,
    ces06_PES_SD3 >4099 & ces06_PES_SD3 <4200 & ces08_CPS_REPLICATE==9999~ 1,
    ces06_PES_SD3 >5099 & ces06_PES_SD3 <5200 & ces08_CPS_REPLICATE==9999~ 1,
    ces06_PES_SD3 >1199 & ces06_PES_SD3 <1501 & ces08_CPS_REPLICATE==9999~ 3,
    ces06_PES_SD3 >2199 & ces06_PES_SD3 <3100 & ces08_CPS_REPLICATE==9999~ 3,
    ces06_PES_SD3 >3199 & ces06_PES_SD3 <4100 & ces08_CPS_REPLICATE==9999~ 3,
    ces06_PES_SD3 >4199 & ces06_PES_SD3 <5100 & ces08_CPS_REPLICATE==9999~ 3,
    ces06_PES_SD3 >5199 & ces06_PES_SD3 <6701 & ces08_CPS_REPLICATE==9999~ 3,
    ces06_PES_SD3 >7199 & ces06_PES_SD3 <7400 & ces08_CPS_REPLICATE==9999~ 4,
    ces06_PES_SD3 >7399 & ces06_PES_SD3 <7701 & ces08_CPS_REPLICATE==9999~ 5,
    ces06_PES_SD3 >8199 & ces06_PES_SD3 <8400 & ces08_CPS_REPLICATE==9999~ 4,
    ces06_PES_SD3 >8399 & ces06_PES_SD3 <8701 & ces08_CPS_REPLICATE==9999~ 5,
    ces06_PES_SD3 >9199 & ces06_PES_SD3 <9300 & ces08_CPS_REPLICATE==9999~ 4,
    ces06_PES_SD3 >9299 & ces06_PES_SD3 <9701 & ces08_CPS_REPLICATE==9999~ 5,
    ces04_PES_SD3 >0 & ces04_PES_SD3 <1001 & ces08_CPS_REPLICATE==9999~ 2,
    ces04_PINPORR==1 & ces08_CPS_REPLICATE==9999~ 1,
    ces04_PINPORR==2 & ces08_CPS_REPLICATE==9999~ 1,
    ces04_PINPORR==4 & ces08_CPS_REPLICATE==9999~ 1,
    ces04_PINPORR==5 & ces08_CPS_REPLICATE==9999~ 1,
    ces04_PINPORR==3 & ces08_CPS_REPLICATE==9999~ 2,
    ces04_PINPORR==6 & ces08_CPS_REPLICATE==9999~ 2,
    ces04_PINPORR==7 & ces08_CPS_REPLICATE==9999~ 2,
    ces04_PINPORR==9 & ces08_CPS_REPLICATE==9999~ 3,
    ces04_PINPORR==12 & ces08_CPS_REPLICATE==9999~ 3,
    ces04_PINPORR==14 & ces08_CPS_REPLICATE==9999~ 3,
    ces04_PINPORR==8 & ces08_CPS_REPLICATE==9999~ 4,
    ces04_PINPORR==10 & ces08_CPS_REPLICATE==9999~ 4,
    ces04_PINPORR==13 & ces08_CPS_REPLICATE==9999~ 5,
    ces04_PINPORR==15 & ces08_CPS_REPLICATE==9999~ 5,
    ces04_PINPORR==16 & ces08_CPS_REPLICATE==9999~ 5,
  ))->ces0411
val_labels(ces0411$occupation08)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation08)
ces0411 %>%
  filter(str_detect(survey, "PES08")) %>%
  select(occupation08) %>%
  group_by(occupation08) %>%
  summarise(total=sum(occupation08), missing=sum(is.na(occupation08)))

#recode Occupation3 as 6 class schema with self-employed (ces08_CPS_S4)
# look_for(ces0411, "employ")
ces0411$occupation083<-ifelse(ces0411$ces08_CPS_S4==1, 6, ces0411$occupation08)
val_labels(ces0411$occupation083)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation083)
# table(ces0411$occupation08_3)

#### #recode Income (ces08_CPS_S18A, ces08_CPS_S18B, ces08_PES_S9A, ces08_PES_S9B) ####
# look_for(ces0411, "income")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
# table(ces0411$ces08_CPS_S18B)
val_labels(ces0411$ces08_CPS_S18B)
# look_for(ces0411, "income")
#ces0411$income06<-Recode(ces0411$ces06_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:9=4; 10=5; else=NA")
#ces0411$income04<-Recode(ces0411$ces04_CPS_S18, "1=1; 2:3=2; 4:5=3; 6:7=4; 8:10=5; else=NA")
ces0411 %>%
  mutate(income08=case_when(
    ces08_CPS_S18B==1 | ces08_CPS_S18A> -1 & ces08_CPS_S18A < 30 | ces08_PES_S9B==1 | ces08_PES_S9A> -1 & ces08_PES_S9A < 30 ~ 1,
    ces08_CPS_S18B==2 | ces08_CPS_S18A> -1 & ces08_CPS_S18A < 30 | ces08_PES_S9B==2 | ces08_PES_S9A> -1 & ces08_PES_S9A < 30 ~ 1,
    ces08_CPS_S18B==3 | ces08_CPS_S18A> 29 & ces08_CPS_S18A < 50 | ces08_PES_S9B==3 | ces08_PES_S9A> 29 & ces08_PES_S9A < 50 ~ 2,
    ces08_CPS_S18B==4 | ces08_CPS_S18A> 29 & ces08_CPS_S18A < 50 | ces08_PES_S9B==4 | ces08_PES_S9A> 29 & ces08_PES_S9A < 50 ~ 2,
    ces08_CPS_S18B==5 | ces08_CPS_S18A> 49 & ces08_CPS_S18A < 70 | ces08_PES_S9B==5 | ces08_PES_S9A> 49 & ces08_PES_S9A < 70 ~ 3,
    ces08_CPS_S18B==6 | ces08_CPS_S18A> 49 & ces08_CPS_S18A < 70 | ces08_PES_S9B==6 | ces08_PES_S9A> 49 & ces08_PES_S9A < 70 ~ 3,
    ces08_CPS_S18B==7 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==7 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==8 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==8 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==9 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==9 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==10 | ces08_CPS_S18A> 69 & ces08_CPS_S18A < 100 | ces08_PES_S9B==10 | ces08_PES_S9A> 69 & ces08_PES_S9A < 100 ~ 4,
    ces08_CPS_S18B==11 | ces08_CPS_S18A> 99 & ces08_CPS_S18A < 998 | ces08_PES_S9B==11 | ces08_PES_S9A> 99 & ces08_PES_S9A < 998 ~ 5,
    ces08_CPS_S18B==12 | ces08_CPS_S18A> 99 & ces08_CPS_S18A < 998 | ces08_PES_S9B==12 | ces08_PES_S9A> 99 & ces08_PES_S9A < 998 ~ 5,
    # ces06_CPS_S18==1 & ces08_CPS_REPLICATE==9999 ~ 1,
    # ces06_CPS_S18==2 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces06_CPS_S18==3 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces06_CPS_S18==4 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces06_CPS_S18==5 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces06_CPS_S18==6 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==7 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==8 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==9 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces06_CPS_S18==10 & ces08_CPS_REPLICATE==9999 ~ 5,
    # ces04_CPS_S18==1 & ces08_CPS_REPLICATE==9999 ~ 1,
    # ces04_CPS_S18==2 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces04_CPS_S18==3 & ces08_CPS_REPLICATE==9999 ~ 2,
    # ces04_CPS_S18==4 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces04_CPS_S18==5 & ces08_CPS_REPLICATE==9999 ~ 3,
    # ces04_CPS_S18==6 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces04_CPS_S18==7 & ces08_CPS_REPLICATE==9999 ~ 4,
    # ces04_CPS_S18==8 & ces08_CPS_REPLICATE==9999 ~ 5,
    # ces04_CPS_S18==9 & ces08_CPS_REPLICATE==9999 ~ 5,
    # ces04_CPS_S18==10 & ces08_CPS_REPLICATE==9999 ~ 5,
  ))->ces0411

val_labels(ces0411$income08)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#Simon's Version

ces0411$ces08_PES_S9A

ces0411 %>%
  mutate(income082=case_when(
    #First Quintile
    #CPS actual income
    ces08_CPS_S18A > -1 & ces08_CPS_S18A < 25 ~ 1,
    #PES actual income
    ces08_PES_S9A >-1 &ces08_PES_S9A <25 ~ 1,
    #CPS categories_ISR
    ces08_CPS_S18B==1 ~1,
    #  #CPS categories_Jolie
    #  ces08_CPS_S18B==1 ~1,
    #PES categories
    ces08_PES_S9B==1 ~1,

    #SECOND QUINTILE
    #CPS actual income
    ces08_CPS_S18A > 24 & ces08_CPS_S18A < 44 ~ 2,
    #PES actual income
    ces08_PES_S9A >24 &ces08_PES_S9A <44 ~ 2,
    #CPS categories ISR
    ces08_CPS_S18B>1 &ces08_CPS_S18B<3 ~2,
    #  #CPS categories Joli
    #  ces08_CPS_S18B>1 &ces08_CPS_S18B<3 ~2,
    #PES categories
    ces08_PES_S9B >1 & ces08_PES_S9B<3 ~2,

    #THIRD QUINTILE
    #CPS actual income
    ces08_CPS_S18A > 43 & ces08_CPS_S18A < 66 ~ 3,
    #PES actual income
    ces08_PES_S9A >43 &ces08_PES_S9A <66 ~ 3,
    #CPS categories ISR
    ces08_CPS_S18B>2 &ces08_CPS_S18B<7 ~3,
    #  #CPS categories Joli
    #  ces08_CPS_S18B==3 ~4,
    #PES categories ISR
    ces08_PES_S9B >2 & ces08_PES_S9B<7 ~3,

    #FOURTH QUINTILE
    #CPS actual income
    ces08_CPS_S18A > 65 & ces08_CPS_S18A < 99 ~ 4,
    #PES actual income
    ces08_PES_S9A >65 &ces08_PES_S9A <99 ~ 4,
    #CPS categories ISR
    ces08_CPS_S18B>6 &ces08_CPS_S18B<10 ~4,
    #  #CPS categories Joli
    #  ces08_CPS_S18B>3 &ces08_CPS_S18B<6 ~4,
    #PES categories
    ces08_PES_S9B >6 & ces08_PES_S9B<10 ~4,

    #FIFTH QUINTILE
    #CPS actual income
    ces08_CPS_S18A > 98 & ces08_CPS_S18A < 996 ~ 5,
    #PES actual income
    ces08_PES_S9A >98 &ces08_PES_S9A <996 ~ 5,
    #CPS categories ISR
    ces08_CPS_S18B>9 &ces08_CPS_S18B<13 ~5,
    #  #CPS categories Joli
    #  ces08_CPS_S18B>5 &ces08_CPS_S18B<8 ~5,
    #PES categories ISR
    ces08_PES_S9B>9 & ces08_PES_S9B<13 ~5))->ces0411

val_labels(ces0411$income082)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$income08)

#Find the boundaries of the ces08 categories variable
summary(ces0411$ces08_CPS_S18A)
ces0411 %>%
  filter(!is.na(ces0411$ces08_PES_S9A)) %>%
  select(ces08_CPS_S18B, ces08_PES_S9A)

# table(ces0411$income08, ces0411$income082) #There are some high income quintiles in the original variable who have been put into the second and 3 quintile
#This code prints respondents who have 4 on the original and 3 on the second
#I did find some errors in mine here and I corrected them
#Also, the remaining discrepanciesa re people who reported the income of $65 K. This is *right* on the edge of the third and fourth quintiles.
#The boundary for 2006 is $65536. But if someone reports their income as $65, I feel like it should go below that boundary. Super edgy, but isn't this the most consistent
# I mean the data is also for the 2006 census.
#Also, there will be inflation and wage growth, so the boundaries in 2008 will be *higher* so someone saying $65 in 2008 will be even more likely to be in the third quintile

ces0411 %>%
  filter(income08==4&income082==3) %>%
  select(income08, income082, ces08_CPS_S18A, ces08_CPS_S18B, ces08_CPS_S18B, ces08_PES_S9A, ces08_PES_S9B)

#This checks people with 5 on the original and a value of 3 on the new. I think this did catch an error in the original.
#People who were respondeing 2ith 998 in the original were refusing to answer and they were then asked the category question and they answered, mostly with 6,
#Which puts them in the third quintile
ces0411 %>%
  filter(income08==5&income082==3) %>%
  select(income08, income082, ces08_CPS_S18A, ces08_CPS_S18B, ces08_CPS_S18B, ces08_PES_S9A, ces08_PES_S9B)
#This checks people with 5 on the original d 4 on the new and this caught the same error
ces0411 %>%
  filter(income08==5&income082==4) %>%
  select(income08, income082, ces08_CPS_S18A, ces08_CPS_S18B, ces08_CPS_S18B, ces08_PES_S9A, ces08_PES_S9B)

#Income_tertile
ces0411 %>%
  mutate(income_tertile08=case_when(
    #first Tertile
    ces08_CPS_S18A < 32 ~1,
    ces08_PES_S9A < 32  ~1,
    #Second Tertile
    ces08_CPS_S18A > 31 & ces08_CPS_S18A <69~2,
    ces08_PES_S9A > 31 & ces08_PES_S9A <69~2,
    #Third Tertile
    ces08_CPS_S18A > 68 & ces08_CPS_S18A <997~3,
    ces08_PES_S9A > 68 & ces08_PES_S9A <997~3,
    #First Tercile ISR
    ces08_CPS_S18B <3 ~1,
    #Second Tercile ISR
    ces08_CPS_S18B >2 &ces08_CPS_S18B<7  ~2,
    #Third Tercile ISR
    ces08_CPS_S18B >6 &ces08_CPS_S18B<13  ~3,
    #First Tercile Joli
    ces08_CPS_S18B < 3 ~ 1,
    #Second Tercile Joli
    ces08_CPS_S18B >2 & ces08_CPS_S18B<4 ~ 2,
    #Third Quintile Joli
    ces08_CPS_S18B >3 & ces08_CPS_S18B<8 ~ 3,

    #First Tercile ISR
    ces08_PES_S9B <3 ~1,
    #Second Tercile ISR
    ces08_PES_S9B >2 & ces08_PES_S9B<7  ~2,
    #Third Tercile ISR
    ces08_PES_S9B >6 & ces08_PES_S9B<13  ~3,
    #First Tercile Joli
    ces08_PES_S9B < 3 ~ 1,
    #Second Tercile Joli
    ces08_PES_S9B >2 & ces08_PES_S9B<4 ~ 2,
    #Third Quintile Joli
    ces08_PES_S9B >3 & ces08_PES_S9B<8 ~ 3

  ))->ces0411


# ces0411 %>%
#   filter(income08==5 &income_tertile08==1) %>%
#   select(ces08_CPS_S18A, ces08_CPS_S18B, ces08_CPS_S18B_ISR, ces08_CPS_S18B_Joli, ces08_PES_S9A, ces08_PES_S9A, income08, income_tertile08) %>%
#   View()
val_labels(ces0411$income_tertile08)<-c(Lowest=1, Middle=2, Highest=3)

# table(ces0411$income_tertile08, ces0411$ces08_CPS_S18B, useNA="ifany")
# table( ces0411$ces08_CPS_S18A,ces0411$income_tertile08, useNA="ifany")
# table( ces0411$ces08_PES_S9A,ces0411$income_tertile08, useNA="ifany")

#### recode Household size (ces08_HHWGT)####
# look_for(ces0411, "household")
#ces0411$household08<-Recode(ces0411$ces08_HHWGT, "0.518795794839121=0.5; 1.03759158967824=1; 1.55638738451736=1.5; 2.07518317935648=2; 2.5939789741956=2.5; 3.11277476903472=3; 5.18795794839121=5")
ces0411 %>%
  mutate(household08=case_when(
    ces08_HHWGT> 0 & ces08_HHWGT< 1 ~ 0.5,
    ces08_HHWGT> 1 & ces08_HHWGT< 1.5 ~ 1,
    ces08_HHWGT> 1.5 & ces08_HHWGT< 2 ~ 1.5,
    ces08_HHWGT> 2 & ces08_HHWGT< 2.5 ~ 2,
    ces08_HHWGT> 2.5 & ces08_HHWGT< 3 ~ 2.5,
    ces08_HHWGT> 3 & ces08_HHWGT< 3.5 ~ 3,
    ces08_HHWGT> 5 & ces08_HHWGT< 5.5 ~ 5,
  ))->ces0411
#checks
# table(ces0411$household08)

#### recode income / household size ####
ces0411$inc081<-Recode(ces0411$ces08_CPS_S18A, "998:999=NA")
# table(ces0411$inc081)
ces0411$inc082<-ces0411$inc081/ces0411$household08
# table(ces0411$inc082)
# household weights not available for ces08_PES_S9A & ces08_PES_S9B
ces0411$inc083<-Recode(ces0411$ces08_CPS_S18B_ISR, "98:99=NA")
# table(ces0411$inc083)
ces0411$inc084<-ces0411$inc083/ces0411$household08
# table(ces0411$inc084)
ces0411$inc085<-Recode(ces0411$ces08_CPS_S18B_Joli, "1=1; 2=2; 3=4; 4=6; 5=8; 6=10; 7=12; 8:9=NA")
# table(ces0411$inc085)
ces0411$inc086<-ces0411$inc085/ces0411$household08
# table(ces0411$inc086)
ces0411 %>%
  mutate(inc087=case_when(
    ces08_CPS_S18B==98  ~ NA_real_,
    ces08_CPS_S18B==99  ~ NA_real_,
    ces08_CPS_S18B==1  ~ 1,
    ces08_CPS_S18B==2  ~ 2,
    ces08_CPS_S18B==3  ~ 3,
    ces08_CPS_S18B==4  ~ 4,
    ces08_CPS_S18B==5  ~ 5,
    ces08_CPS_S18B==6  ~ 6,
    ces08_CPS_S18B==7  ~ 7,
    ces08_CPS_S18B==8  ~ 8,
    ces08_CPS_S18B==9  ~ 9,
    ces08_CPS_S18B==10  ~ 10,
    ces08_CPS_S18B==11  ~ 11,
    ces08_CPS_S18B==12  ~ 12,
    ces08_CPS_S18B>0 & ces08_CPS_S18B_Joli>0 ~ NA_real_,
  ))->ces0411
# table(ces0411$inc087)
ces0411$inc088<-ces0411$inc087/ces0411$household08
# table(ces0411$inc088)

ces0411 %>%
  mutate(income_house08=case_when(
    #first Tertile
    inc082 < 32 ~1,
    ces08_PES_S9A < 32  ~1,
    ces08_PES_S9B < 32  ~1,
    #Second Tertile
    inc082 > 31 & inc082 <69~2,
    ces08_PES_S9A > 31 & ces08_PES_S9A <69~2,
    ces08_PES_S9B > 31 & ces08_PES_S9B <69~2,
    #Third Tertile
    inc082 > 68 & inc082 <997~3,
    ces08_PES_S9A > 68 & ces08_PES_S9A <997~3,
    ces08_PES_S9B > 68 & ces08_PES_S9B <997~3,

    #First Tercile ISR
    inc084 <3 ~1,
    #Second Tercile ISR
    inc084 >2 & inc088<7  ~2,
    #Third Tercile ISR
    inc084 >6 & inc088<13  ~3,
    #First Tercile ISR
    inc086 <3 ~1,
    #Second Tercile ISR
    inc086 >2 & inc088<7  ~2,
    #Third Tercile ISR
    inc086 >6 & inc088<13  ~3,
    #First Tercile Joli
    inc088 < 3 ~ 1,
    #Second Tercile Joli
    inc088 >2 & inc088<7 ~ 2,
    #Third Quintile Joli
    inc088 >6 & inc088<13 ~ 3,

  ))->ces0411
# table(ces0411$income_house08)
# table(ces0411$income_tertile08)
# table(ces0411$income_tertile08, ces0411$income_house08)

#### recode Religiosity (ces08_CPS_S11)####
# look_for(ces0411, "relig")
ces0411$religiosity08<-Recode(ces0411$ces08_CPS_S11, "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces0411$religiosity08)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$religiosity08)
# table(ces0411$religiosity08)

#### #recode Redistribution (ces08_PES_F6)####
# look_for(ces0411, "rich")
val_labels(ces0411$ces08_PES_F6)
ces0411$redistribution08<-Recode(as.numeric(ces0411$ces08_PES_F6), "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution08)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution08)
# table(ces0411$redistribution08)

#recode Pro-Redistribution (ces08_PES_F6)
ces0411$pro_redistribution08<-Recode(ces0411$ces08_PES_F6, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution08)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution08)
# table(ces0411$pro_redistribution08, useNA = "ifany" )

#### recode Market Liberalism (ces08_PES_I2N and ces08_PES_G7)####
# look_for(ces0411, "private")
# look_for(ces0411, "blame")
ces0411$market081<-Recode(as.numeric(ces0411$ces08_PES_I2N), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces0411$market082<-Recode(as.numeric(ces0411$ces08_PES_G7), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$market081, useNA = "ifany" )
# table(ces0411$market082, useNA = "ifany" )

ces0411 %>%
  select(starts_with('market08'))
options(max.print=5000)
names(ces0411)
ces0411 %>%
  mutate(market_liberalism08=rowMeans(select(., num_range("market08", 1:2)), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("market08")) %>%
  summary()
#Check distribution of market_liberalism08
# qplot(ces0411$market_liberalism08, geom="histogram")
# table(ces0411$market_liberalism08, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(market081, market082) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(market081, market082) %>%
  cor(., use="complete.obs")

#### #recode Immigration (ces08_PES_P6)####
# look_for(ces0411, "imm")
ces0411$immigration_rates08<-Recode(as.numeric(ces0411$ces08_PES_P6), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_rates08, useNA = "ifany" )

#### recode Environment vs Jobs (ces08_MBS_A15)####
# look_for(ces0411, "env")
ces0411$enviro08<-Recode(as.numeric(ces0411$ces08_MBS_A15), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces0411$enviro08, useNA = "ifany" )

#### #recode Capital Punishment (ces08_PES_P9)####
# look_for(ces0411, "death")
ces0411$death_penalty08<-Recode(as.numeric(ces0411$ces08_PES_P9), "1=1; 3=0; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$death_penalty08, useNA = "ifany" )

#### #recode Crime (ces08_MBS_H5)####
# look_for(ces0411, "crime")
ces0411$crime08<-Recode(as.numeric(ces0411$ces08_MBS_H5), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$crime08, useNA = "ifany" )

#### #recode Gay Rights (ces08_PES_G5)####
# look_for(ces0411, "gays")
ces0411$gay_rights08<-Recode(as.numeric(ces0411$ces08_PES_G5), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
# table(ces0411$gay_rights08, useNA = "ifany" )

#### #recode Abortion (ces08_PES_G11)####
# look_for(ces0411, "abort")
ces0411$abortion08<-Recode(as.numeric(ces0411$ces08_PES_G11), "1=0; 2=0.25; 3=0.75; 4=1; 6=1; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$abortion08, useNA = "ifany" )

#### #recode Lifestyle (ces08_MBS_A7) ####
# look_for(ces0411, "lifestyle")
ces0411$lifestyles08<-Recode(as.numeric(ces0411$ces08_MBS_A7), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$lifestyles08, useNA = "ifany" )

#### #recode Stay Home (ces08_PES_P3)####
# look_for(ces0411, "home")
ces0411$stay_home08<-Recode(as.numeric(ces0411$ces08_PES_P3), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$stay_home08 , useNA = "ifany" )

#### #recode Marriage Children (ces08_MBS_H4)####
# look_for(ces0411, "children")
ces0411$marriage_children08<-Recode(as.numeric(ces0411$ces08_MBS_H4), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$marriage_children08, useNA = "ifany" )

#### #recode Values (ces08_MBS_A9)####
# look_for(ces0411, "traditional")
ces0411$values08<-Recode(as.numeric(ces0411$ces08_MBS_A9), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$values08, useNA = "ifany" )

#### #recode Morals (ces08_MBS_A8)####
# look_for(ces0411, "moral")
ces0411$morals08<-Recode(as.numeric(ces0411$ces08_MBS_A8), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces0411$morals08, useNA = "ifany" )

#### #recode Moral Traditionalism (abortion, lifestyles, stay home, values, marriage childen, morals)####
ces0411$trad083<-ces0411$abortion08
ces0411$trad087<-ces0411$lifestyles08
ces0411$trad081<-ces0411$stay_home08
ces0411$trad084<-ces0411$values08
ces0411$trad085<-ces0411$marriage_children08
ces0411$trad086<-ces0411$morals08
ces0411$trad082<-ces0411$gay_rights08
# table(ces0411$trad081, useNA = "ifany" )
# table(ces0411$trad082, useNA = "ifany" )
# table(ces0411$trad083, useNA = "ifany" )
# table(ces0411$trad084, useNA = "ifany" )
# table(ces0411$trad085, useNA = "ifany" )
# table(ces0411$trad086, useNA = "ifany" )
# table(ces0411$trad087, useNA = "ifany" )
#Remove value labels

ces0411 %>%
  mutate(across(.cols=num_range('trad08', 1:7), remove_val_labels))->ces0411
ces0411 %>%
  mutate(traditionalism08=rowMeans(select(., num_range("trad08", 1:7)), na.rm=T))->ces0411


ces0411 %>%
  select(starts_with("trad08")) %>%
  summary()
#Check distribution of traditionalism08
# qplot(ces0411$traditionalism08, geom="histogram")
# table(ces0411$traditionalism08, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad081, trad082, trad083, trad084, trad085, trad086, trad087) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad081, trad082, trad083, trad084, trad085, trad086, trad087) %>%
  cor(., use="complete.obs")

####recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)####
ces0411 %>%
  mutate(traditionalism208=rowMeans(select(., c("trad081", "trad082")), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("trad08")) %>%
  summary()
#Check distribution of traditionalism208
# qplot(ces0411$traditionalism208, geom="histogram")
# table(ces0411$traditionalism208, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad081, trad082) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad081, trad082) %>%
  cor(., use="complete.obs")

#### recode 2nd Dimension (stay home, immigration, gay rights, crime)####
ces0411$author081<-ces0411$stay_home08
ces0411$author082<-ces0411$immigration_rates08
ces0411$author083<-ces0411$gay_rights08
ces0411$author084<-ces0411$crime08
# table(ces0411$author081)
# table(ces0411$author082)
# table(ces0411$author083)
# table(ces0411$author084)


#Remove Value Labels
ces0411 %>%
  mutate(across(num_range('author08', 1:4), remove_val_labels))->ces0411

ces0411 %>%
  mutate(authoritarianism08=rowMeans(select(., num_range("author08", 1:4)), na.rm=T))->ces0411
ces0411$authoritarianism08
ces0411 %>%
  select(author081:authoritarianism08) %>%
  summary()
#Check distribution of authoritarianism08
# qplot(ces0411$authoritarianism08, geom="histogram")

#Calculate Cronbach's alpha
ces0411 %>%
  select(author081, author082, author083, author084) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(author081, author082, author083, author084) %>%
  cor(., use="complete.obs")

#### recode Quebec Accommodation (ces08_PES_f7) (Left=more accom) ####
# look_for(ces0411, "quebec")
ces0411$quebec_accom08<-Recode(as.numeric(ces0411$ces08_PES_F7), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_accom08)


#recode Liberal leader (ces08_PES_F2 & ces08_CPS_G2)
# look_for(ces0411, "Dion")
ces0411$liberal_leader08<-Recode(as.numeric(ces0411$ces08_CPS_G2), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_leader08)

#recode conservative leader (ces08_PES_F1 & ces08_CPS_G1)
# look_for(ces0411, "Harper")
ces0411$conservative_leader08<-Recode(as.numeric(ces0411$ces08_CPS_G1), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_leader08)

#recode NDP leader (ces08_PES_F3 & ces08_CPS_G3)
# look_for(ces0411, "Layton")
ces0411$ndp_leader08<-Recode(as.numeric(ces0411$ces08_CPS_G3), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_leader08)

#recode Bloc leader (ces08_PES_F4 & ces08_CPS_G4)
# look_for(ces0411, "Duceppe")
ces0411$bloc_leader08<-Recode(as.numeric(ces0411$ces08_CPS_G4), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_leader08)

#recode Green leader (ces08_PES_F5 & ces08_CPS_G5)
# look_for(ces0411, "May")
ces0411$green_leader08<-Recode(as.numeric(ces0411$ces08_CPS_G5), "0=1; 996:999=NA")
#checks
# table(ces0411$green_leader08)

#recode liberal rating (ces08_PES_C1B & ces08_CPS_G8)
# look_for(ces0411, "liberal")
ces0411$liberal_rating08<-Recode(as.numeric(ces0411$ces08_CPS_G8), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_rating08)

#recode conservative rating (ces08_PES_C1A & ces08_CPS_G7)
# look_for(ces0411, "conservative")
ces0411$conservative_rating08<-Recode(as.numeric(ces0411$ces08_CPS_G7), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_rating08)

#recode NDP rating (ces08_PES_C1C & ces08_CPS_G9)
# look_for(ces0411, "new democratic")
ces0411$ndp_rating08<-Recode(as.numeric(ces0411$ces08_CPS_G9), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_rating08)

#recode Bloc rating (ces08_PES_C1E & ces08_CPS_G10)
# look_for(ces0411, "bloc")
ces0411$bloc_rating08<-Recode(as.numeric(ces0411$ces08_CPS_G10), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_rating08)

#recode Green rating (ces08_PES_C1F & ces08_CPS_G11)
# look_for(ces0411, "green")
ces0411$green_rating08<-Recode(as.numeric(ces0411$ces08_CPS_G11), "0=1; 996:999=NA")
#checks
# table(ces0411$green_rating08)

#### recode Education (ces08_PES_D1D)####
# look_for(ces0411, "edu")
ces0411$education08<-Recode(as.numeric(ces0411$ces08_PES_D1D), "3=0; 5=0.5; 1=1; 8=0.5; else=NA")
#checks
# table(ces0411$education08, ces0411$ces08_PES_D1D , useNA = "ifany" )

####recode Personal Retrospective (ces08_CPS_F1 & ces08_PES_F1N) ####
# look_for(ces0411, "financial")

ces0411 %>%
  mutate(personal_retrospective08=case_when(
    ces08_CPS_F1==1 | ces08_PES_F1N==1~ 1,
    ces08_CPS_F1==3 | ces08_PES_F1N==3~ 0,
    ces08_CPS_F1==5 | ces08_PES_F1N==5~ 0.5,
    ces08_CPS_F1==8 | ces08_PES_F1N==8~ 0.5,
  ))->ces0411
#val_labels(ces0411$personal_retrospective08)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces0411$personal_retrospective08)
# table(ces0411$personal_retrospective08, ces0411$ces08_CPS_F1, ces0411$ces08_PES_F1N, useNA = "ifany" )
# table(ces0411$personal_retrospective08, useNA = "ifany" )

#### recode National Retrospective (ces08_CPS_M1 & ces08_PES_M1) ####
# look_for(ces0411, "economy")

ces0411 %>%
  mutate(national_retrospective08=case_when(
    ces08_CPS_M1==1 | ces08_PES_M1==1~ 1,
    ces08_CPS_M1==3 | ces08_PES_M1==3~ 0,
    ces08_CPS_M1==5 | ces08_PES_M1==5~ 0.5,
    ces08_CPS_M1==8 | ces08_PES_M1==8~ 0.5,
  ))->ces0411
#val_labels(ces0411$national_retrospective08)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces0411$national_retrospective08)
# table(ces0411$national_retrospective08, ces0411$ces08_CPS_M1, ces0411$ces08_PES_M1, useNA = "ifany" )
# table(ces0411$national_retrospective08, useNA = "ifany" )

#### recode Ideology (ces08_MBS_I12) ####
# look_for(ces0411, "scale")
ces0411$ideology08<-Recode(as.numeric(ces0411$ces08_MBS_I12), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
#val_labels(ces0411$ideology08)<-c(Left=0, Right=1)
#checks
#val_labels(ces0411$ideology08)
# table(ces0411$ideology08, ces0411$ces08_MBS_I12 , useNA = "ifany")

#### #recode Immigrants take Jobs away (ces08_MBS_H10)####
# look_for(ces0411, "jobs")
ces0411$immigration_job08<-Recode(as.numeric(ces0411$ces08_MBS_H10), "4=0; 3=0.25; 2=0.75; 1=1; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_job08, ces0411$ces08_MBS_H10, useNA = "ifany" )

#### recode turnout (ces08_PES_B1) ####
# look_for(ces0411, "vote")
ces0411$turnout08<-Recode(ces0411$ces08_PES_B1, "1=1; 5=0; 8=0; else=NA")
val_labels(ces0411$turnout08)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$turnout08)
# table(ces0411$turnout08)
# table(ces0411$turnout08, ces0411$vote08)

#### recode political efficacy ####

#recode No Say (ces08_MBS_E11)
# look_for(ces0411, "have any say")
ces0411$efficacy_internal08<-Recode(as.numeric(ces0411$ces08_MBS_E11), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_internal08)<-c(low=0, high=1)
#checks
#val_labels(ces0411$efficacy_internal08)
# table(ces0411$efficacy_internal08)
# table(ces0411$efficacy_internal08, ces0411$ces08_MBS_E11 , useNA = "ifany" )

#recode MPs lose touch (ces08_MBS_E5)
# look_for(ces0411, "lose touch")
ces0411$efficacy_external08<-Recode(as.numeric(ces0411$ces08_MBS_E5), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external08)<-c(low=0, high=1)
#checks
#val_labels(ces0411$efficacy_external08)
# table(ces0411$efficacy_external08)
# table(ces0411$efficacy_external08, ces0411$ces08_MBS_E5 , useNA = "ifany" )

#recode Official Don't Care (ces08_PES_G2)
# look_for(ces0411, "cares much")
ces0411$efficacy_external208<-Recode(as.numeric(ces0411$ces08_PES_G2), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external208)<-c(low=0, high=1)
#checks
val_labels(ces0411$efficacy_external208)
# table(ces0411$efficacy_external208)
# table(ces0411$efficacy_external208, ces0411$ces08_PES_G2 , useNA = "ifany" )

ces0411 %>%
  mutate(political_efficacy08=rowMeans(select(., c("efficacy_external08", "efficacy_external208", "efficacy_internal08")), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
# qplot(ces0411$political_efficacy08, geom="histogram")
# table(ces0411$political_efficacy08, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces0411 %>%
  select(efficacy_external08, efficacy_external208, efficacy_internal08) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(efficacy_external08, efficacy_external208, efficacy_internal08) %>%
  cor(., use="complete.obs")

#### recode Most Important Question (ces08_CPS_A2) ####
# look_for(ces0411, "important issue")
ces0411$mip08<-Recode(ces0411$ces08_CPS_A2, "1:6=0; 10=6; 20:26=10; 30:32=7; 33=8; 35:36=0; 39=18; 45=12; 46=11; 47=0; 48=12; 48=12;
					                                  49=14; 50=9; 57:59=8; 60:64=15; 65=4; 70=14; 71=2; 72=15; 73:74=14; 75=1; 76=14; 77=2; 78=13;
					                                  79=12; 80:82=16; 83=11; 84=0; 90:91=3; 92:93=11; 94:95=0; else=NA")
val_labels(ces0411$mip08)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                             Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18)
 table(ces0411$mip08)

#### recode satisfaction with democracy (ces08_CPS_A1) ####
# look_for(ces0411, "dem")
ces0411$satdem08<-Recode(as.numeric(ces0411$ces08_CPS_A1), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem08, ces0411$ces08_CPS_A1, useNA = "ifany" )

ces0411$satdem208<-Recode(as.numeric(ces0411$ces08_CPS_A1), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem208, ces0411$ces08_CPS_A1, useNA = "ifany" )

#### recode Quebec Sovereignty (ces08_CPS_Q9) (Quebec only & Right=more sovereignty) ####
# look_for(ces0411, "quebec")
ces0411$quebec_sov08<-Recode(as.numeric(ces0411$ces08_CPS_Q9), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_sov08, ces0411$ces08_CPS_Q9, useNA = "ifany" )

#### recode provincial alienation (ces08_PES_P12) ####
# look_for(ces0411, "treat")
ces0411$prov_alienation08<-Recode(as.numeric(ces0411$ces08_PES_P12), "3=1; 1=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$prov_alienation08, ces0411$ces08_PES_P12, useNA = "ifany" )

#### recode immigration society (ces08_MBS_H3 ) ####
# look_for(ces0411, "fit")
ces0411$immigration_soc08<-Recode(as.numeric(ces0411$ces08_MBS_H3) , "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_soc08, ces0411$ces08_MBS_H3 , useNA = "ifany" )

#recode welfare (ces08_PES_D1B)
# look_for(ces0411, "spend")
ces0411$welfare08<-Recode(as.numeric(ces0411$ces08_PES_D1B), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$welfare08, ces0411$ces08_PES_D1B, useNA = "ifany" )

#### recode Postgrad (ces08_CPS_S3)####
# look_for(ces0411, "education")
ces0411 %>%
  mutate(postgrad08=case_when(
    ces08_CPS_S3 >0 & ces08_CPS_S3 <10 ~ 0,
    ces08_CPS_S3 >9 & ces08_CPS_S3 <12 ~ 1,
    # (ces04_CPS_S3 >0 & ces04_CPS_S3 <9) & ces08_CPS_REPLICATE==9999 ~ 0,
    # (ces04_CPS_S3 >8 & ces04_CPS_S3 <12) & ces08_CPS_REPLICATE==9999 ~ 1,
  ))->ces0411
#checks
# table(ces0411$postgrad08)

#### recode Break Promise (ces08_PES_G12) ####
# look_for(ces0411, "promise")
ces0411$promise08<-Recode(as.numeric(ces0411$ces08_PES_G12), "1=0; 3=0.5; 5=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$promise08)<-c(low=0, high=1)
#checks
val_labels(ces0411$promise08)
# table(ces0411$promise08)
# table(ces0411$promise08, ces0411$ces08_PES_G12 , useNA = "ifany" )

#### recode Trust (ces08_PES_TRUST_1) ####
# look_for(ces0411, "trust")
ces0411$trust08<-Recode(as.numeric(ces0411$ces08_PES_TRUST_1), "1=1; 5=0; else=NA", as.numeric=T)
#val_labels(ces0411$trust08)<-c(no=0, yes=1)
#checks
val_labels(ces0411$trust08)
# table(ces0411$trust08)
# table(ces0411$trust08, ces0411$ces08_PES_TRUST_1 , useNA = "ifany" )

#### recode political interest (ces08_CPS_A4) ####
# look_for(ces0411, "interest")
ces0411$pol_interest08<-Recode(as.numeric(ces0411$ces08_CPS_A4), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checkst
# table(ces0411$pol_interest08, ces0411$ces08_CPS_A4, useNA = "ifany" )

#### recode foreign born (ces08_CPS_S12) ####
# look_for(ces0411, "born")
ces0411$foreign08<-Recode(ces0411$ces08_CPS_S12, "1=0; 2:97=1; 0=0; else=NA")
val_labels(ces0411$foreign08)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$foreign08)
# table(ces0411$foreign08, ces0411$ces08_CPS_S12, useNA="ifany")

#### recode duty (ces08_CPS_P1 ) ####
look_for(ces0411, "duty")
ces0411$duty08<-Recode(ces0411$ces08_CPS_P1 , "1=1; 2:7=0; else=NA")
val_labels(ces0411$duty08)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$duty08)
table(ces0411$duty08, ces0411$ces08_CPS_P1, useNA="ifany")

#### recode Women - how much should be done (ces08_PES_I4) ####
look_for(ces0411, "women")
table(ces0411$ces08_PES_I4)
ces0411$women08<-Recode(as.numeric(ces0411$ces08_PES_I4), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$women08,  useNA = "ifany")

#### recode Women - how much should be done (ces08_PES_I3) ####
look_for(ces0411, "racial")
table(ces0411$ces08_PES_I3)
ces0411$race08<-Recode(as.numeric(ces0411$ces08_PES_I3), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$race08,  useNA = "ifany")

#recode Previous Vote (ces08_PES_K7)
# look_for(ces0411, "vote")
ces0411$previous_vote08<-Recode(ces0411$ces08_PES_K7, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$previous_vote08)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$previous_vote08)
table(ces0411$previous_vote08)

ces0411 %>%
  mutate(election08=case_when(
    str_detect(survey, "08")~ 2008
  ))->ces0411
ces0411 %>%
  filter(str_detect(survey, "CPS08")) %>%
  select(survey, election08)
table(ces0411$election08, useNA = "ifany")

#### recode Environment Spend (ces04_PES_D1F)
# look_for(ces0411, "env")
ces0411$enviro_spend08<-Recode(as.numeric(ces0411$ces08_PES_D1F), "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$enviro_spend08, useNA = "ifany" )


####Recode 2011 4th ####

# Gender done at top

#### #recode Union Respondent (PES11_93) ####
# look_for(ces0411, "union")
ces0411$union11<-Recode(ces0411$PES11_93, "1=1; 5=0; else=NA")
val_labels(ces0411$union11)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union11)
# table(ces0411$union11)

#### #recode Union Combined (PES11_93 and PES11_94)####
ces0411 %>%
  mutate(union_both11=case_when(
    #If the person is in a union OR if the household is in a union, then they get a 1
    PES11_93==1 | PES11_94==1 ~ 1,
    PES11_94==5 ~ 0,
    PES11_93==5 ~ 0,
    PES11_93==8 & PES11_94==8 ~ NA_real_,
    PES11_93==9 & PES11_94==9 ~ NA_real_,
  ))->ces0411

# table(as_factor(ces0411$union_both11), as_factor(ces0411$PES11_93))
# table(as_factor(ces0411$union_both11), as_factor(ces0411$PES11_94))
val_labels(ces0411$union_both11)<-c(None=0, Union=1)
#checks
val_labels(ces0411$union_both11)
# table(ces0411$union_both11)

#### #recode Education (CPS11_79)####
# look_for(ces0411, "education")
ces0411$degree11<-Recode(ces0411$CPS11_79, "9:11=1; 1:8=0; else=NA")
val_labels(ces0411$degree11)<-c(nodegree=0, degree=1)
#checks
val_labels(ces0411$degree11)
# table(ces0411$degree11, useNA = "ifany" )

####= #recode Region (PROVINCE11)####
# look_for(ces0411, "province")
ces0411$region11<-Recode(ces0411$PROVINCE11, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces0411$region11)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces0411$region11)
# table(ces0411$region11, useNA = "ifany" )

####recode Province (PROVINCE11)####
# look_for(ces0411, "province")
ces0411$prov11<-Recode(ces0411$PROVINCE11, "10=1; 11=2; 12=3; 13=4; 24=5; 35=6; 46=7; 47=8; 48=9; 59=10; else=NA")
val_labels(ces0411$prov11)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces0411$prov11)
table(ces0411$prov11)

####= #recode Quebec (PROVINCE11)####
# look_for(ces0411, "province")
ces0411$quebec11<-Recode(ces0411$PROVINCE11, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces0411$quebec11)<-c(Other=0, Quebec=1)
#checks
val_labels(ces0411$quebec11)
# table(ces0411$quebec11, useNA = "ifany" )

#### #recode Age (YEARofBIRTH)####
# table(str_detect(ces0411$survey, "11"))
#pipe data frame
ces0411 %>%
  #mutate making new variable age08
  mutate(age11=case_when(
    str_detect(ces0411$survey, "11")~2011-yob
    #dump in a test dataframe
  ))-> ces0411
#check
# table(ces0411$age11, useNA = "ifany" )

#### recode Religion (CPS11_80)####
# look_for(ces0411, "relig")
ces0411$religion11<-Recode(ces0411$CPS11_80, "0=0; 1:2=2; 4:5=1; 7=2; 9:10=2; 12:14=2; 16:20=2; 98:99=NA; 3=3; 6=3; 8=3; 11=3; 15=3; 97=3;")
val_labels(ces0411$religion11)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces0411$religion11)
# table(ces0411$religion11, useNA = "ifany" )

#### #recode Language (CPS_INTLANG11)####
# look_for(ces0411, "language")
ces0411$language11<-Recode(ces0411$CPS_INTLANG11, "5=0; 1=1; else=NA")
val_labels(ces0411$language11)<-c(French=0, English=1)
#checks
val_labels(ces0411$language11)
# table(ces0411$language11, useNA = "ifany" )

#### #recode Non-charter Language (CPS11_90)####
# look_for(ces0411, "language")
ces0411$non_charter_language11<-Recode(ces0411$CPS11_90, "0=1; 1:5=0; 8:64=1; 65=0; 95:97=1; else=NA")
val_labels(ces0411$non_charter_language11)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces0411$non_charter_language11)
# table(ces0411$non_charter_language11, useNA = "ifany" )

#### #recode Employment (CPS11_91)####
# look_for(ces0411, "employment")
ces0411$employment11<-Recode(ces0411$CPS11_91, "3:7=0; 1:2=1; 8:11=1; else=NA")
val_labels(ces0411$employment11)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces0411$employment11)
# table(ces0411$employment11, useNA = "ifany" )

#### #recode Sector (PES11_92 & CPS11_91)####
# look_for(ces0411, "company")
ces0411 %>%
  mutate(sector11=case_when(
    PES11_92==5 ~1,
    PES11_92==1 ~0,
    PES11_92==0 ~0,
    CPS11_91==1 ~0,
    CPS11_91> 2 & CPS11_91< 12 ~ 0,
    PES11_92==9 ~NA_real_ ,
    PES11_92==8 ~NA_real_ ,
  ))->ces0411

val_labels(ces0411$sector11)<-c(Private=0, Public=1)
#checks
val_labels(ces0411$sector11)
# table(ces0411$sector11, useNA = "ifany" )
ces0411 %>%
  mutate(sector11=case_when(
    PES11_92==5 ~1,
    PES11_92==1 ~0,
    PES11_92==0 ~0,
    CPS11_91==1 ~0,
    CPS11_91> 2 & CPS11_91< 12 ~ 0,
    PES11_92==9 ~NA_real_ ,
    PES11_92==8 ~NA_real_ ,
  ))->ces0411

#### Sector Welfare
lookfor(ces0411, "occupation")
ces0411$NOC_PES11
#Unclass NOC_PES11
ces0411$out<-unclass(ces0411$NOC_PES11)
#Convert to numeric
ces0411$out<-as.numeric(ces0411$out)
#Convert to labelled double
ces0411$NOC_PES11<-labelled(ces0411$out, labels=attr(ces0411$out, "labels"))

ces0411 %>%
  mutate(sector_welfare11=case_when(
    #Health
    PES11_92==5&(NOC_PES11>3010&NOC_PES11<3415) ~1,
    #Education
    PES11_92==5&(NOC_PES11>4010&NOC_PES11<4034) ~1,
    #Social Workers, Counsellors
    PES11_92==5&(NOC_PES11>4151&NOC_PES11<4157) ~1,
    #Social Workers, Counsellors
    PES11_92==5&(NOC_PES11>4210&NOC_PES11<4216) ~1,
    PES11_92==1 ~0,
    PES11_92==0 ~0,
    CPS11_91==1 ~0,
    CPS11_91> 2 & CPS11_91< 12 ~ 0,
    PES11_92==9 ~NA_real_ ,
    PES11_92==8 ~NA_real_ ,
  ))->ces0411

#### SEctor security

  ces0411 %>%
  mutate(sector_security11=case_when(
    #Police
    PES11_92==5&(NOC_PES11==4311) ~1,
    #CAF
    PES11_92==5&(NOC_PES11==4313) ~1,
    #comissioned police
    PES11_92==5&(NOC_PES11==0431) ~1,
    #Security guards
    (NOC_PES11==6541) ~1,
    PES11_92==1 ~0,
    PES11_92==0 ~0,
    CPS11_91==1 ~0,
    CPS11_91> 2 & CPS11_91< 12 ~ 0,
    PES11_92==9 ~NA_real_ ,
    PES11_92==8 ~NA_real_ ,
  ))->ces0411

#### #recode Party ID (PES11_59a)####
# look_for(ces0411, "identify")
ces0411$party_id11<-Recode(ces0411$PES11_59a, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$party_id11)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces0411$party_id11)
table(ces0411$party_id11, useNA = "ifany" )

#### #recode Vote (PES11_6)####
# look_for(ces0411, "party did you vote")
ces0411$vote11<-Recode(ces0411$PES11_6, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$vote11)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$vote11)
# table(ces0411$vote11, useNA = "ifany" )

#### #recode Occupation (NOC_PES11)####
# look_for(ces0411, "occupation")
class(ces0411$NOC_PES11)
val_labels(ces0411$NOC_PES11)

ces0411$occupation11<-Recode(as.numeric(ces0411$NOC_PES11), "1:1099=2; 1100:1199=1;
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
 6400:6799=3;
 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9399=4; 9400:9700=5; else=NA")
val_labels(ces0411$occupation11)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation11)
# table(ces0411$occupation11, useNA = "ifany" )

#Show occupations of those missing responses on NOC variable
ces0411 %>%
  select(NOC_PES11, occupation11, survey) %>%
  group_by(occupation11) %>%
  filter(is.na(occupation11)) %>%
  filter(str_detect(survey, "CPS11")) %>%
  count(NOC_PES11)

#recode Occupation3 as 6 class schema with self-employed (CPS11_91)
# look_for(ces0411, "employ")
# table(ces0411$occupation11, useNA = "ifany")
ces0411$CPS11_91

ces0411$occupation113<-ifelse(ces0411$CPS11_91==1, 6, ces0411$occupation11)
val_labels(ces0411$occupation113)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
# table(ces0411$occupation11, ces0411$occupation11_3)
#checks
val_labels(ces0411$occupation113)
# table(ces0411$occupation11_3, useNA = "ifany" )

#### #recode Income (CPS11_92 and CPS11_93) ####
# look_for(ces0411, "income")
ces0411 %>%
  mutate(income11=case_when(
    CPS11_93==1 | CPS11_92> -1 & CPS11_92 < 30 ~ 1,
    CPS11_93==2 | CPS11_92> 29 & CPS11_92 < 60 ~ 2,
    CPS11_93==3 | CPS11_92> 59 & CPS11_92 < 90 ~ 3,
    CPS11_93==4 | CPS11_92> 89 & CPS11_92 < 110 ~ 4,
    CPS11_93==5 | CPS11_92> 109 & CPS11_92 < 998 ~ 5,
  ))->ces0411

#Simon's Version
# look_for(ces0411, "income")
ces0411 %>%
  mutate(income112=case_when(
    #First Quintile
    CPS11_93==1 | (CPS11_92> -1 & CPS11_92 < 28) ~ 1,
    #Second Quintile
    CPS11_93==2 | (CPS11_92> 27 & CPS11_92 < 50) ~ 2,
    #Third Quintile
    CPS11_93==3 | (CPS11_92> 49 & CPS11_92 < 76) ~ 3,
    #Fourth Quintile
    CPS11_93==4 | (CPS11_92> 75 & CPS11_92 < 115) ~ 4,
    #Fifth Quintile
    CPS11_93==5 | (CPS11_92> 114 & CPS11_92 < 998) ~ 5,
  ))->ces0411

val_labels(ces0411$income11)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
# table(ces0411$income11, ces0411$income112)
#Check
# ces0411 %>%
#   filter(income11==5&income112==4) %>%
#   select(income11, income112, CPS11_92, CPS11_93) %>%
#   View()
#Terciles 2011

ces0411 %>%
  mutate(income_tertile11=case_when(
    #First Tercile
    CPS11_93==1 | (CPS11_92> -1 & CPS11_92 < 36) ~ 1,
    #Second Tercile
    (CPS11_93>1 & CPS11_93 <4) | (CPS11_92> 35 & CPS11_92 < 77) ~ 2,
    #Third tercile
    (CPS11_93>3 & CPS11_93<97) | (CPS11_92> 76 & CPS11_92 < 995) ~ 3,
  ))->ces0411

#checks
val_labels(ces0411$income_tertile11)<-c(Lowest=1,  Middle=2, Highest=3)
# table(ces0411$income11, useNA = "ifany" )

#### recode Household size (NADULTS11)####
# look_for(ces0411, "household")
ces0411$household11<-Recode(ces0411$NADULTS11, "1=0.5; 2=1; 3=1.5; 4=2; 5=2.5; 6=3; 7=3.5; 8=4")
#checks
# table(ces0411$household11, useNA = "ifany" )

#### recode income / household size ####
ces0411$inc111<-Recode(ces0411$CPS11_92, "998:999=NA")
# table(ces0411$inc111)
ces0411$inc112<-ces0411$inc111/ces0411$household11
# table(ces0411$inc112)
ces0411$inc113<-Recode(ces0411$CPS11_93, "98:99=NA")
# table(ces0411$inc113)
ces0411$inc114<-ces0411$inc113/ces0411$household11
# table(ces0411$inc114)

ces0411 %>%
  mutate(income_house11=case_when(
    as.numeric(inc114)<3 |(as.numeric(inc112)> -1 & as.numeric(inc112) < 36) ~ 1,
    (as.numeric(inc114)>2.99 &as.numeric(inc114) <8 )| as.numeric(inc112)> 35 & as.numeric(inc112) < 77 ~ 2,
    (as.numeric(inc114)>7.99 & as.numeric(inc114)<97) | as.numeric(inc112)> 76 & as.numeric(inc112) <1101 ~ 3
  ))->ces0411
# table(ces0411$income_house11)
# table(ces0411$income_tertile11)
# table(ces0411$income_tertile11, ces0411$income_house11)

#### recode Religiosity (CPS11_82)####
# look_for(ces0411, "relig")
ces0411$religiosity11<-Recode(ces0411$CPS11_82, "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces0411$religiosity11)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces0411$religiosity11)
# table(ces0411$religiosity11)

#### #recode Redistribution (PES11_41)####
# look_for(ces0411, "rich")
val_labels(ces0411$PES11_41)
ces0411$redistribution11<-Recode(as.numeric(ces0411$PES11_41), "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$redistribution11)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces0411$redistribution11)
# table(ces0411$redistribution11, useNA = "ifany" )

#recode Pro-Redistribution (PES11_41)
ces0411$pro_redistribution11<-Recode(ces0411$PES11_41, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces0411$pro_redistribution11)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces0411$pro_redistribution11)
# table(ces0411$pro_redistribution11, useNA = "ifany" )

#### #recode Market Liberalism (PES11_22 and PES11_49)####
# look_for(ces0411, "private")
# look_for(ces0411, "blame")
ces0411$market111<-Recode(as.numeric(ces0411$PES11_22), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces0411$market112<-Recode(as.numeric(ces0411$PES11_49), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$market111, useNA = "ifany" )
# table(ces0411$market112, useNA = "ifany" )

#Remvoe value labels
ces0411 %>%
  mutate(across(.cols=num_range('market11', 1:2), remove_val_labels))->ces0411

ces0411 %>%
  mutate(market_liberalism11=rowMeans(select(., num_range("market11", 1:2)),  na.rm=T))->ces0411

#Scale Averaging

ces0411 %>%
  select(market111, market112, market_liberalism11) %>%
  summary()
#Check distribution of market_liberalism11
# qplot(ces0411$market_liberalism11, geom="histogram", bins=10)
# table(ces0411$market_liberalism11, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(market111, market112) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(market111, market112) %>%
  cor(., use="complete.obs")

#### #recode Immigration (PES11_28)####
# look_for(ces0411, "imm")
ces0411$immigration_rates11<-Recode(as.numeric(ces0411$PES11_28), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_rates11, useNA = "ifany" )

#### recode Environment vs Jobs (MBS11_C14)####
# look_for(ces0411, "env")
ces0411$enviro11<-Recode(as.numeric(ces0411$MBS11_C14), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces0411$enviro11, useNA = "ifany" )

#### #recode Capital Punishment (PES11_36)####
# look_for(ces0411, "death")
ces0411$death_penalty11<-Recode(as.numeric(ces0411$PES11_36), "1=1; 5=0; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$death_penalty11, useNA = "ifany" )
# table(ces0411$death_penalty11, ces0411$PES11_36, useNA = "ifany" )
#### recode Crime (MBS11_I5)####
# look_for(ces0411, "crime")
ces0411$crime11<-Recode(as.numeric(ces0411$MBS11_I5), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$crime11)

#### recode Gay Rights (PES11_29)####
# look_for(ces0411, "gays")
ces0411$gay_rights11<-Recode(as.numeric(ces0411$PES11_29), "1=0; 5=1; 8=0.5; else=NA")
#checks
# table(ces0411$gay_rights11, ces0411$PES11_29, useNA = "ifany" )

#### #recode Abortion (PES11_53) ####
# look_for(ces0411, "abort")
ces0411$abortion11<-Recode(as.numeric(ces0411$PES11_53), "1=1; 5=0; 8=0.5; else=NA")
#checks
# table(ces0411$abortion11, ces0411$PES11_53, useNA = "ifany" )

#### #recode Lifestyle (MBS11_C6)####
# look_for(ces0411, "lifestyle")
ces0411$lifestyles11<-Recode(as.numeric(ces0411$MBS11_C6), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$lifestyles11, useNA = "ifany" )

#### #recode Stay Home (PES11_26)####
# look_for(ces0411, "home")
ces0411$PES11_26
ces0411$stay_home11<-Recode(as.numeric(ces0411$PES11_26), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$stay_home11, ces0411$PES11_26, useNA = "ifany" )

#### #recode Marriage Children (MBS11_I4)####
# look_for(ces0411, "children")
ces0411$marriage_children11<-Recode(as.numeric(ces0411$MBS11_I4), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$marriage_children11, useNA = "ifany" )

#### #recode Values (MBS11_C8)####
# look_for(ces0411, "traditional")
ces0411$values11<-Recode(as.numeric(ces0411$MBS11_C8), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces0411$values11, useNA = "ifany" )

#### recode Morals (MBS11_C7)####
# look_for(ces0411, "moral")
ces0411$morals11<-Recode(as.numeric(ces0411$MBS11_C7), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces0411$morals11, ces0411$MBS11_C7, useNA="ifany")

#recode Moral Traditionalism (abortion, lifestyles, stay home, values, marriage childen, morals)
ces0411$trad113<-ces0411$abortion11
ces0411$trad117<-ces0411$lifestyles11
ces0411$trad111<-ces0411$stay_home11
ces0411$trad114<-ces0411$values11
ces0411$trad115<-ces0411$marriage_children11
ces0411$trad116<-ces0411$morals11
ces0411$trad112<-ces0411$gay_rights11
# table(ces0411$trad111, useNA = "ifany" )
# table(ces0411$trad112, useNA = "ifany" )
# table(ces0411$trad113, useNA = "ifany" )
# table(ces0411$trad114, useNA = "ifany" )
# table(ces0411$trad115, useNA = "ifany" )
# table(ces0411$trad116, useNA = "ifany" )
# table(ces0411$trad117, useNA = "ifany" )

#remove value labels

ces0411 %>%
  mutate(across(.cols=num_range('trad11', 1:7), remove_val_labels))->ces0411

ces0411 %>%
  mutate(traditionalism11=rowMeans(select(., num_range("trad11", 1:7)), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("trad11")) %>%
  summary()
#Check distribution of traditionalism11
# qplot(ces0411$traditionalism11, geom="histogram")


#Calculate Cronbach's alpha
ces0411 %>%
  select(trad111, trad112, trad113, trad114, trad115, trad116, trad117) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad111, trad112, trad113, trad114, trad115, trad116, trad117) %>%
  cor(., use="complete.obs")

####recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)####
ces0411 %>%
  mutate(traditionalism211=rowMeans(select(., c("trad111", "trad112")), na.rm=T))->ces0411

ces0411$traditionalism211
ces0411 %>%
  select(starts_with("trad11")) %>%
  summary()
#Check distribution of traditionalism211
# qplot(ces0411$traditionalism211, geom="histogram")
# table(ces0411$traditionalism211, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(trad111, trad112) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(trad111, trad112) %>%
  cor(., use="complete.obs")


#### recode 2nd Dimension (stay home, immigration, gay rights, crime)####
ces0411$author111<-ces0411$stay_home11
ces0411$author112<-ces0411$immigration_rates11
ces0411$author113<-ces0411$gay_rights11
ces0411$author114<-ces0411$crime11
# table(ces0411$author111)
# table(ces0411$author112)
# table(ces0411$author113)
# table(ces0411$author114)

#Remove value labels

ces0411 %>%
  mutate(across(.cols=num_range('author11', 1:4), remove_val_labels))->ces0411
ces0411 %>%
  mutate(authoritarianism11=rowMeans(select(., num_range("author11", 1:4)), na.rm=T))->ces0411

#Check distribution of authoritarianism11
# qplot(ces0411$authoritarianism11, geom="histogram")
# table(ces0411$authoritarianism11, useNA="ifany")

#Calculate Cronbach's alpha
ces0411 %>%
  select(author111, author112, author113, author114) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(author111, author112, author113, author114) %>%
  cor(., use="complete.obs")

####recode Quebec Accommodation (PES11_44) (Left=more accom) ####
# look_for(ces0411, "quebec")
ces0411$quebec_accom11<-Recode(as.numeric(ces0411$PES11_44), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_accom11)


#recode Liberal leader (CPS11_24)
# look_for(ces0411, "Ignatieff")
ces0411$liberal_leader11<-Recode(as.numeric(ces0411$CPS11_24), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_leader11)

#recode conservative leader (CPS11_23)
# look_for(ces0411, "Harper")
ces0411$conservative_leader11<-Recode(as.numeric(ces0411$CPS11_23), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_leader11)

#recode NDP leader (CPS11_25)
# look_for(ces0411, "Layton")
ces0411$ndp_leader11<-Recode(as.numeric(ces0411$CPS11_25), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_leader11)

#recode Bloc leader (CPS11_26)
# look_for(ces0411, "Duceppe")
ces0411$bloc_leader11<-Recode(as.numeric(ces0411$CPS11_26), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_leader11)

#recode Green leader (CPS11_27)
# look_for(ces0411, "May")
ces0411$green_leader11<-Recode(as.numeric(ces0411$CPS11_27), "0=1; 996:999=NA")
#checks
# table(ces0411$green_leader11)

#recode liberal rating (CPS11_19)
# look_for(ces0411, "liberal")
ces0411$liberal_rating11<-Recode(as.numeric(ces0411$CPS11_19), "0=1; 996:999=NA")
#checks
# table(ces0411$liberal_rating11)

#recode conservative rating (CPS11_18)
# look_for(ces0411, "conservative")
ces0411$conservative_rating11<-Recode(as.numeric(ces0411$CPS11_18), "0=1; 996:999=NA")
#checks
# table(ces0411$conservative_rating11)

#recode NDP rating (CPS11_20)
# look_for(ces0411, "new democratic")
ces0411$ndp_rating11<-Recode(as.numeric(ces0411$CPS11_20), "0=1; 996:999=NA")
#checks
# table(ces0411$ndp_rating11)

#recode Bloc rating (CPS11_21)
# look_for(ces0411, "bloc")
ces0411$bloc_rating11<-Recode(as.numeric(ces0411$CPS11_21), "0=1; 996:999=NA")
#checks
# table(ces0411$bloc_rating11)

#recode Green rating (CPS11_22)
# look_for(ces0411, "green")
ces0411$green_rating11<-Recode(as.numeric(ces0411$CPS11_22), "0=1; 996:999=NA")
#checks
# table(ces0411$green_rating11)

#### recode Education (CPS11_35)####
# look_for(ces0411, "edu")
ces0411$education11<-Recode(as.numeric(ces0411$CPS11_35), "3=0; 5=0.5; 1=1; 8=0.5; else=NA")
#checks
# table(ces0411$education11, ces0411$CPS11_35 , useNA = "ifany" )

#### recode Personal Retrospective (CPS11_66) ####
# look_for(ces0411, "financial")
ces0411$personal_retrospective11<-Recode(as.numeric(ces0411$CPS11_66), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$personal_retrospective11)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces0411$personal_retrospective11)
# table(ces0411$personal_retrospective11, ces0411$CPS11_66 , useNA = "ifany" )

#### recode National Retrospective (CPS11_39) ####
# look_for(ces0411, "economy")
ces0411$national_retrospective11<-Recode(as.numeric(ces0411$CPS11_39), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$national_retrospective11)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces0411$national_retrospective11)
# table(ces0411$national_retrospective11, ces0411$CPS11_39 , useNA = "ifany" )

#### recode Ideology (MBS11_K5) ####
# look_for(ces0411, "scale")
ces0411$ideology11<-Recode(as.numeric(ces0411$MBS11_K5), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
#val_labels(ces0411$ideology11)<-c(Left=0, Right=1)
#checks
val_labels(ces0411$ideology11)
# table(ces0411$ideology11, ces0411$MBS11_K5 , useNA = "ifany")

#### #recode Immigrants take Jobs away (PES11_51)####
# look_for(ces0411, "jobs")
ces0411$immigration_job11<-Recode(as.numeric(ces0411$PES11_51), "7=0; 5=0.25; 3=0.75; 1=1; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_job11, ces0411$PES11_51, useNA = "ifany" )

#### recode turnout (PES11_3) ####
# look_for(ces0411, "vote")
ces0411$turnout11<-Recode(ces0411$PES11_3, "1=1; 5=0; 8=0; else=NA")
val_labels(ces0411$turnout11)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$turnout11)
# table(ces0411$turnout11)
# table(ces0411$turnout11, ces0411$vote11)

#### recode political efficacy ####

#recode No Say (MBS11_A10)
# look_for(ces0411, "have any say")
ces0411$efficacy_internal11<-Recode(as.numeric(ces0411$MBS11_A10), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_internal11)<-c(low=0, high=1)
#checks
val_labels(ces0411$efficacy_internal11)
# table(ces0411$efficacy_internal11)
# table(ces0411$efficacy_internal11, ces0411$MBS11_A10 , useNA = "ifany" )

#recode MPs lose touch (MBS11_A8)
# look_for(ces0411, "lose touch")
ces0411$efficacy_external11<-Recode(as.numeric(ces0411$MBS11_A8), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external11)<-c(low=0, high=1)
#checks
val_labels(ces0411$efficacy_external11)
# table(ces0411$efficacy_external11)
# table(ces0411$efficacy_external11, ces0411$MBS11_A8 , useNA = "ifany" )

#recode Official Don't Care (PES11_48)
# look_for(ces0411, "care much")
ces0411$efficacy_external211<-Recode(as.numeric(ces0411$PES11_48), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$efficacy_external211)<-c(low=0, high=1)
#checks
val_labels(ces0411$efficacy_external211)
# table(ces0411$efficacy_external211)
# table(ces0411$efficacy_external211, ces0411$PES11_48 , useNA = "ifany" )

ces0411 %>%
  mutate(political_efficacy11=rowMeans(select(., c("efficacy_external11", "efficacy_external211", "efficacy_internal11")), na.rm=T))->ces0411

ces0411 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
# qplot(ces0411$political_efficacy11, geom="histogram")
# table(ces0411$political_efficacy11, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces0411 %>%
  select(efficacy_external11, efficacy_external211, efficacy_internal11) %>%
  psych::alpha(.)
#Check correlation
ces0411 %>%
  select(efficacy_external11, efficacy_external211, efficacy_internal11) %>%
  cor(., use="complete.obs")

#### recode Most Important Question (CPS11_1) ####
# look_for(ces0411, "important issue")
ces0411$mip11<-Recode(ces0411$CPS11_1, "0=1=0; 2=3; 3=0; 4=3; 5=11; 6=0; 7=11; 10=6; 20:26=10; 29=18; 30:32=7; 33=8; 34=13; 35=0;
					                              39=18; 45=12; 46=11; 47=0; 48=12; 49=14; 50=9; 56:59=8; 60:64=14; 65=4; 71=2; 72=15;
					                              73:74=14; 75=1; 76=14; 77=2; 79=12; 80:82=16; 83=11; 84=0; 90:91=3; 92:93=11; 94:96=0; else=NA")
val_labels(ces0411$mip11)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                             Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18)
 table(ces0411$mip11)

#### recode satisfaction w democracy (CPS11_0) ####
# look_for(ces0411, "dem")
ces0411$satdem11<-Recode(as.numeric(ces0411$CPS11_0), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem11, ces0411$CPS11_0, useNA = "ifany" )

ces0411$satdem211<-Recode(as.numeric(ces0411$CPS11_0), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$satdem211, ces0411$CPS11_0, useNA = "ifany" )

#### recode Quebec Sovereignty (CPS11_75) (Quebec only & Right=more sovereignty) ####
# look_for(ces0411, "sovereignty")
ces0411$quebec_sov11<-Recode(as.numeric(ces0411$CPS11_75), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces0411$quebec_sov11, ces0411$CPS11_75, useNA = "ifany" )

#### recode provincial alienation (PES11_38) ####
# look_for(ces0411, "treat")
ces0411$prov_alienation11<-Recode(as.numeric(ces0411$PES11_38), "3=1; 1=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$prov_alienation11, ces0411$PES11_38, useNA = "ifany" )

#### recode immigration society (MBS11_I3 ) ####
# look_for(ces0411, "fit")
ces0411$immigration_soc11<-Recode(as.numeric(ces0411$MBS11_I3) , "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$immigration_soc11, ces0411$MBS11_I3 , useNA = "ifany" )

#recode welfare (CPS11_33)
# look_for(ces0411, "spend")
ces0411$welfare11<-Recode(as.numeric(ces0411$CPS11_33), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces0411$welfare11, ces0411$CPS11_33, useNA = "ifany" )

#### recode Postgrad (CPS11_79) ####
# look_for(ces0411, "education")
ces0411$postgrad11<-Recode(as.numeric(ces0411$CPS11_79), "10:11=1; 1:9=0; else=NA")
#checks
# table(ces0411$postgrad11, useNA = "ifany" )

#### recode Break Promise (PES11_54) ####
# look_for(ces0411, "promise")
ces0411$promise11<-Recode(as.numeric(ces0411$PES11_54), "1=0; 3=0.5; 5=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces0411$promise11)<-c(low=0, high=1)
#checks
val_labels(ces0411$promise11)
# table(ces0411$promise11)
# table(ces0411$promise11, ces0411$PES11_54 , useNA = "ifany" )

#### recode Trust (PES11_89) ####
# look_for(ces0411, "trust")
ces0411$trust11<-Recode(as.numeric(ces0411$PES11_89), "1=1; 5=0; else=NA", as.numeric=T)
#val_labels(ces0411$trust11)<-c(no=0, yes=1)
#checks
val_labels(ces0411$trust11)
# table(ces0411$trust11)
# table(ces0411$trust11, ces0411$PES11_89 , useNA = "ifany" )

#### recode political interest (PES11_60) ####
# look_for(ces0411, "interest")
ces0411$pol_interest11<-Recode(as.numeric(ces0411$PES11_60), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checkst
# table(ces0411$pol_interest11, ces0411$PES11_60, useNA = "ifany" )

#### recode foreign born (CPS11_83) ####
# look_for(ces0411, "born")
ces0411$foreign11<-Recode(ces0411$CPS11_83, "1=0; 2:97=1; 0=0; else=NA")
val_labels(ces0411$foreign11)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$foreign11)
# table(ces0411$foreign11, ces0411$CPS11_83, useNA="ifany")

#### recode Environment Spend (CPS11_35)
# look_for(ces0411, "env")
ces0411$enviro_spend11<-Recode(as.numeric(ces0411$CPS11_35), "1=1; 3=0; 5=0.5; 8=0.5; else=NA")
#checks
# table(ces0411$enviro_spend11, useNA = "ifany" )

#### recode duty (CPS11_62 )
look_for(ces0411, "duty")
ces0411$duty11<-Recode(ces0411$CPS11_62 , "1=1; 5=0; else=NA")
val_labels(ces0411$duty11)<-c(No=0, Yes=1)
#checks
val_labels(ces0411$duty11)
table(ces0411$duty11, ces0411$CPS11_62, useNA="ifany")

#### recode Women - how much should be done (PES11_43) ####
look_for(ces0411, "women")
table(ces0411$PES11_43)
ces0411$women11<-Recode(as.numeric(ces0411$PES11_43), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$women11,  useNA = "ifany")

#### recode Race - how much should be done (PES11_42) ####
look_for(ces0411, "racial")
table(ces0411$PES11_42)
ces0411$race11<-Recode(as.numeric(ces0411$PES11_42), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces0411$race11,  useNA = "ifany")

#recode Previous Vote (PES11_6)
# look_for(ces0411, "vote")
ces0411$previous_vote11<-Recode(ces0411$PES11_6, "1=1; 2=2; 3=3; 4=4; 5=5; 0=0; else=NA")
val_labels(ces0411$previous_vote11)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces0411$previous_vote11)
table(ces0411$previous_vote11)

# Add 2011 Election Variable
table(ces0411$survey)
ces0411 %>%
  mutate(election11=case_when(
    str_detect(survey, "11")~ 2011
  ))->ces0411
table(ces0411$election11, useNA = "ifany")
# ces0411 %>%
#   filter(election11==2011) %>%
#   select(survey) %>%
#   View()
#Add mode
ces0411$mode<-rep("Phone", nrow(ces0411))

save(ces0411, file=here("data/ces0411.rda"))
