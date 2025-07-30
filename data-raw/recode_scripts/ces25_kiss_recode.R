# This script loads and recodes CES25 Kiss module
library(haven)
library(here)
library(tidyverse)
library(srvyr)
library(survey)
#Load data
ces25b<-read_dta(here("data-raw/CES 25 Kiss Module Final (with occupation Qs).dta"))
library(labelled)
library(car)
look_for(ces25b, "class")
look_for(ces25b, "vote")
look_for(ces25b, "vote")
ces25b$vote<-Recode(ces25b$cps25_votechoice , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 8=2; else=NA")
val_labels(ces25b$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(as_factor(ces25b$vote))

lookfor(ces25b, "income")
# This is a hacky way to assign respondents to income tertiles
# convert original income to numeric
ces25b$cps25_income2<-as.numeric(ces25b$cps25_income)

# Set don't knows to missing
# We could set these to the median 4 if we want to rescue missing data
# talk to matt
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
# Class
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

# Make occupation3 as self-employed and unskilled and skilleed groupi together
library(labelled)
#lookfor(ces21, "employed")

ces25b$occupation3<-ifelse(ces25b$cps25_employment==3, 6, ces25b$occupation)

# ADd value labels for occupation3
val_labels(ces25b$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)


#Check, did I get everyone?

ces25b %>%
  filter(is.na(occupation3)) %>%
  select(cps25_employment, occupation_name, occupation3) %>%
  as_factor()


ces25b$mode<-rep("Web", nrow(ces25b))
ces25b$election<-rep(2025, nrow(ces25b))
#Write out the dataset
# #### Resave the file in the .rda file
save(ces25b, file=here("data/ces25b.rda"))

#write_sav(ces25b, path=here("data-raw/ces25b_with_occupation.sav"))
