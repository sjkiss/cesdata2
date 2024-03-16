#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces84<-read_sav(file=here("data-raw/1984.sav"))


#recode Gender (VAR456)
# look_for(ces84, "sex")
ces84$male<-Recode(ces84$VAR456, "1=1; 2=0")
val_labels(ces84$male)<-c(Female=0, Male=1)
#checks
val_labels(ces84$male)
# table(ces84$male)

#recode Union Respondent (VAR381)
# look_for(ces84, "union")
# table(ces84$VAR381)
ces84$union<-Recode(ces84$VAR381, "1=1; 2:8=0; else=NA")
val_labels(ces84$union)<-c(None=0, Union=1)
#checks
val_labels(ces84$union)
# table(ces84$union)
# table(ces84$VAR378, ces84$VAR381)
ces84$VAR378

#recode Union Combined (VAR378 and VAR381)
ces84 %>%
  mutate(union_both=case_when(
    VAR378==1 | VAR381==1 ~ 1,
    VAR378==2 | VAR381==2 ~ 0,
    VAR378==8  ~ NA_real_,
    VAR381==8  ~ NA_real_
  ))->ces84

val_labels(ces84$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces84$union_both)
# table(ces84$union_both)

#recode Education (VAR262)
# look_for(ces84, "education")
ces84$degree<-Recode(ces84$VAR362, "8=1; 1:7=0; 9=0; else=NA")
val_labels(ces84$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces84$degree)
# table(ces84$degree)

#recode Region (VAR003)
# look_for(ces84, "region")
ces84$region<-Recode(ces84$VAR003, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces84$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces84$region)
# table(ces84$region)

#recode Quebec (VAR003)
# look_for(ces84, "region")
ces84$quebec<-Recode(ces84$VAR003, "0:3=0; 5:9=0; 4=1")
val_labels(ces84$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces84$quebec)
# table(ces84$quebec)

#recode Age (VAR437)
# look_for(ces84, "age")
ces84$age<-Recode(ces84$VAR437, "0=NA")
val_labels(ces84$age)<-NULL
ces84$age
#check
# table(ces84$age)

#recode Religion (VAR371)
# look_for(ces84, "relig")
ces84$religion<-Recode(ces84$VAR371, "0=0; 34=0; 1=1; 2:6=2; 7:8=1; 10:29=2; 88=NA; else=3")
val_labels(ces84$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces84$religion)
# table(ces84$religion)

#recode Language (VAR457)
# look_for(ces84, "language")
ces84$language<-Recode(ces84$VAR457, "2=0; 1=1; else=NA")
val_labels(ces84$language)<-c(French=0, English=1)
#checks
val_labels(ces84$language)
# table(ces84$language)

#recode Non-charter Language (VAR375)
# look_for(ces84, "language")
ces84$non_charter_language<-Recode(ces84$VAR375, "1:5=0; 6:7=1; else=NA")
val_labels(ces84$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces84$non_charter_language)
# table(ces84$non_charter_language)

#recode Employment (VAR524)
# look_for(ces84, "employment")
ces84$employment<-Recode(ces84$VAR524, "2:6=0; 1=1; else=NA")
val_labels(ces84$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces84$employment)
# table(ces84$employment)

#recode Sector (VAR530, VAR526, VAR525 & VAR524)
# look_for(ces84, "company")
# look_for(ces84, "occupation")
# look_for(ces84, "employment")

ces84 %>%
  mutate(sector=case_when(
    #what does your company do (government)
    VAR530==13 ~1,
    # VAR530<13~0,
    #     VAR524 >1 ~ 0,
    #assume these are teachers and nurses
    VAR526> 2710 & VAR526 < 2800 ~ 1,
    VAR526> 3129 & VAR526 < 3136 ~ 1,
    #   VAR530==99 ~NA_real_ ,
    #all else gets as per Blais' footnote (reading between the lines)
    TRUE ~ 0
  ))->ces84

# table(as_factor(ces84$VAR524), ces84$sector, useNA='ifany')
# table(as_factor(ces84$VAR524), as_factor(ces84$VAR530), useNA="ifany")
val_labels(ces84$sector)<-c(Private=0, Public=1)
ces84$sector
#checks
# table(as_factor(ces84$VAR530), as_factor(ces84$sector))
# ces84 %>%
#   filter(VAR526>2710 & VAR526< 3000) %>%
#   filter(VAR526>3129 & VAR526< 3136) %>%
#   select(VAR526, sector)

val_labels(ces84$sector)
# table(ces84$sector)

#recode Party ID (VAR081)
# look_for(ces84, "fed. id")
ces84$party_id<-Recode(ces84$VAR081, "1=1; 2=2; 3=3; 0=0; 4:19=0; else=NA")
val_labels(ces84$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces84$party_id)
# table(ces84$party_id)

#recode Vote (VAR125)
# look_for(ces84, "vote")
ces84$vote<-Recode(ces84$VAR125, "1=1; 2=2; 3=3; 9=5; 15=2; 4:8=0; 10:12=0; 14=0; 16:20=0; else=NA")
val_labels(ces84$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces84$vote)
# table(ces84$vote)

#recode Occupation (VAR525)
# look_for(ces84, "occupation")
ces84$occupation<-Recode(ces84$VAR525, "1=1; 2=2; 3:4=3; 5=4; 6:7=5; else=NA")
val_labels(ces84$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces84$occupation)
# table(ces84$occupation)

#recode Occupation3 as 6 class schema with self-employed (VAR533)
# look_for(ces84, "employed")
ces84$occupation3<-ifelse(ces84$VAR533==1, 6, ces84$occupation)
val_labels(ces84$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces84$occupation3)
# table(ces84$occupation3)

#recode Income (VAR442 and VAR443)
#recode Income (VAR442 and VAR443)
# look_for(ces84, "income")
ces84 %>%
  mutate(income=case_when(
    VAR442==1 | VAR443==1 ~ 1,
    VAR442==2 | VAR443==2 ~ 1,
    VAR442==3 | VAR443==3 ~ 1,
    VAR442==4 | VAR443==4 ~ 2,
    VAR442==5 | VAR443==5 ~ 2,
    VAR442==6 | VAR443==6 ~ 3,
    VAR442==7 | VAR443==7 ~ 3,
    VAR442==8 | VAR443==8 ~ 4,
    VAR442==9 | VAR443==9 ~ 5,
    VAR442==10 | VAR443==10 ~ 5,
    VAR442==11 | VAR443==11 ~ 5,
  ))->ces84

val_labels(ces84$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

#Simon's Version
# look_for(ces84, "income")
ces84 %>%
  mutate(income2=case_when(
    VAR442==1 | VAR443==1 ~ 1,
    VAR442==2 | VAR443==2 ~ 1,
    VAR442==3 | VAR443==3 ~ 1,
    VAR442==4 | VAR443==4 ~ 1,
    VAR442==5 | VAR443==5 ~ 2,
    VAR442==6 | VAR443==6 ~ 2,
    VAR442==7 | VAR443==7 ~ 3,
    VAR442==8 | VAR443==8 ~ 3,
    VAR442==9 | VAR443==9 ~ 4,
    VAR442==10 | VAR443==10 ~ 5,
    VAR442==11 | VAR443==11 ~ 5,
  ))->ces84

val_labels(ces84$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces84$income)
# table(ces84$income)

# look_for(ces84, "income")
ces84 %>%
  mutate(income_tertile=case_when(
    VAR442==1 | VAR443==1 ~ 1,
    VAR442==2 | VAR443==2 ~ 1,
    VAR442==3 | VAR443==3 ~ 1,
    VAR442==4 | VAR443==4 ~ 1,
    VAR442==5 | VAR443==5 ~ 1,
    VAR442==6 | VAR443==6 ~ 2,
    VAR442==7 | VAR443==7 ~ 2,
    VAR442==8 | VAR443==8 ~ 2,
    VAR442==9 | VAR443==9 ~ 3,
    VAR442==10 | VAR443==10 ~ 3,
    VAR442==11 | VAR443==11 ~ 3,
  ))->ces84

val_labels(ces84$income_tertile)<-c(Lowest=1, Middle=2, Highest=3)
#checks
val_labels(ces84$income_tertile)
# table(ces84$income_tertile)
#recode Community Size (VAR464)
# look_for(ces84, "community")
# look_for(ces84, "city")
ces84$size<-Recode(ces84$VAR464, "6=1; 5=2; 3:4=3; 2=4; 1=5; else=NA")
val_labels(ces84$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces84$size)
# table(ces84$size)

#recode Religiosity (VAR372)
# look_for(ces84, "church")
ces84$religiosity<-Recode(ces84$VAR372, "5=1; 4=2; 3=3; 2=4; 1=5; else=NA")
val_labels(ces84$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces84$religiosity)
# table(ces84$religiosity)

#recode Liberal leader (VAR301)
# look_for(ces84, "Turner")
ces84$liberal_leader<-Recode(as.numeric(ces84$VAR301), "0=1; 997:999=NA")
#checks
# table(ces84$liberal_leader)

#recode conservative leader (VAR302)
# look_for(ces84, "Mulroney")
ces84$conservative_leader<-Recode(as.numeric(ces84$VAR302), "0=1; 997:999=NA")
#checks
# table(ces84$conservative_leader)

#recode NDP leader (VAR303)
# look_for(ces84, "Broadbent")
ces84$ndp_leader<-Recode(as.numeric(ces84$VAR303), "0=1; 997:999=NA")
#checks
# table(ces84$ndp_leader)

#recode Ideology (VAR507)
# look_for(ces84, "scale")
ces84$ideology<-Recode(as.numeric(ces84$VAR507) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces84$ideology)<-c(Left=0, Right=1)
#checks
val_labels(ces84$ideology)
# table(ces84$ideology, ces84$VAR507  , useNA = "ifany")

#recode turnout (VAR124)
# look_for(ces84, "vote")
ces84$turnout<-Recode(ces84$VAR124, "1=1; 2=0;  8=0; else=NA")
val_labels(ces84$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces84$turnout)
# table(ces84$turnout)
# table(ces84$turnout, ces84$vote)

#### recode political efficacy ####
#recode No Say (VAR032)
# look_for(ces84, "have any say")
ces84$efficacy_internal<-Recode(as.numeric(ces84$VAR032), "1=0; 2=0.25; 3=0.75; 4=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces84$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces84$efficacy_internal)
# table(ces84$efficacy_internal)
# table(ces84$efficacy_internal, ces84$VAR032 , useNA = "ifany" )

#recode MPs lose touch (VAR029)
# look_for(ces84, "touch")
ces84$efficacy_external<-Recode(as.numeric(ces84$VAR029), "1=0; 2=0.25; 3=0.75; 4=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces84$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces84$efficacy_external)
# table(ces84$efficacy_external)
# table(ces84$efficacy_external, ces84$VAR029 , useNA = "ifany" )

#recode Official Don't Care (VAR030)
# look_for(ces84, "doesn't care")
ces84$efficacy_external2<-Recode(as.numeric(ces84$VAR030), "1=0; 2=0.25; 3=0.75; 4=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces84$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces84$efficacy_external2)
# table(ces84$efficacy_external2)
# table(ces84$efficacy_external2, ces84$VAR030 , useNA = "ifany" )

ces84 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces84

ces84 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
# qplot(ces84$political_efficacy, geom="histogram")
# table(ces84$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
# library(psych)
# ces84 %>%
#   select(efficacy_external, efficacy_external2, efficacy_internal) %>%
#   psych::alpha(.)
# #Check correlation
# ces84 %>%
#   select(efficacy_external, efficacy_external2, efficacy_internal) %>%
#   cor(., use="complete.obs")

#recode foreign born (VAR400)
# look_for(ces84, "birth")
ces84$foreign<-Recode(ces84$VAR400, "1=0; 2:17=1; else=NA")
val_labels(ces84$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces84$foreign)
# table(ces84$foreign, ces84$VAR400, useNA="ifany")

#recode Most Important Question (VAR065)
# look_for(ces84, "issue")
ces84$mip<-Recode(ces84$VAR065, "1:2=18; 3=19; 4:6=7; 7=10; 8:9=10; 10=3; 11:12=9; 13=18; 14=10; 15:16=6; 17=8; 18:19=15; 20:22=12; 23=0;
				                        24:27=5; 28=1; 29=12; 30=7; 31=4; 37=0; 38=11; 39:42=0; 43:44=3; 45:49=16; 50=14; 51=6;
				                        52:56=14; 57=6; 58:60=15; 61:62=2; 63=0; 64=13; 65:66=1; 67=14; 68=2; 69=16;
				                        70=0; 71:72=7; 73=3; 74=5; 75=12; 76=15; 77=11; 78=7; 79=15; 80=16; 81=7; 82=15;
				                        83=0; 84=14; 85=7; 86:87=3; 90=14; 91=0; 92=14; 93=8; 94=0; 95=3; 96=0; 97=7; 98=0; else=NA")
val_labels(ces84$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Inflation=18, Housing=19)
# table(ces84$mip)

#### recode Women - how much should be done (VAR332)
look_for(ces84, "women")
table(ces84$VAR332)
ces84$women<-Recode(as.numeric(ces84$VAR332), "1=0; 2=0.25; 3=0.75; 4=1; 7=0.5; else=NA")
#checks
table(ces84$women,  useNA = "ifany")

#recode Previous Vote (VAR157)
# look_for(ces84, "vote")
ces84$previous_vote<-Recode(ces84$VAR157, "1=1; 2=2; 3=3; 4:10=0; else=NA")
val_labels(ces84$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
#val_labels(ces84$previous_vote)
#table(ces84$previous_vote)

#Empty variables that are not available pre-88
# ces84$redistribution<-rep(NA, nrow(ces84))
# ces84$market_liberalism<-rep(NA, nrow(ces84))
# ces84$traditionalism2<-rep(NA, nrow(ces84))
# ces84$immigration_rates<-rep(NA, nrow(ces84))
glimpse(ces84)

#Add mode
ces84$mode<-rep("Phone", nrow(ces84))
#Add Election
ces84$election<-rep(1984, nrow(ces84))
# Save the file
save(ces84, file=here("data/ces84.rda"))
