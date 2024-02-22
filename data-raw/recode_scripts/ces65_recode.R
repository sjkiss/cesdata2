#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces65<-read_dta(file=here("data-raw/ces1965.dta"))

#recode Gender (v337)
# look_for(ces65, "sex")
ces65$male<-Recode(ces65$v337, "1=1; 2=0")
val_labels(ces65$male)<-c(Female=0, Male=1)
names(ces65)
#checks
val_labels(ces65$male)
# table(ces65$male)

#recode Union Household (V327)
# look_for(ces65, "union")
ces65$union<-Recode(ces65$v327, "1=1; 5=0")
val_labels(ces65$union)<-c(None=0, Union=1)
#checks
val_labels(ces65$union)
# table(ces65$union)

#Union Combined variable (identical copy of union)
ces65$union_both<-ces65$union
#checks
val_labels(ces65$union_both)
# table(ces65$union_both)

#recode Education (v307)
# look_for(ces65, "school")
ces65$degree<-Recode(ces65$v307, "16:30=1; 0:15=0")
val_labels(ces65$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces65$degree)
# table(ces65$degree)

#recode Region (v5)
# look_for(ces65, "province")
ces65$region<-Recode(ces65$v5, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces65$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces65$region)
# table(ces65$region)

#recode Quebec (v5)
# look_for(ces65, "province")
ces65$quebec<-Recode(ces65$v5, "0:3=0; 5:9=0; 4=1")
val_labels(ces65$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces65$quebec)
# table(ces65$quebec)

#recode Age (v335)
# look_for(ces65, "age")

ces65$age<-ces65$v335
#check
# table(ces65$age)

#recode Religion (v309)
# look_for(ces65, "church")
ces65$religion<-Recode(ces65$v309, "0=0; 10:19=2; 20=1; 70:71=1; else=3")
val_labels(ces65$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces65$religion)
# table(ces65$religion)

#recode Language (v314)
# look_for(ces65, "language")
ces65$language<-Recode(ces65$v314, "2=0; 1=1; else=NA")
val_labels(ces65$language)<-c(French=0, English=1)
#checks
val_labels(ces65$language)
# table(ces65$language)

#recode Non-charter Language (v314)
# look_for(ces65, "language")
ces65$non_charter_language<-Recode(ces65$v314, "1:2=0; 3:8=1; else=NA")
val_labels(ces65$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces65$non_charter_language)
# table(ces65$non_charter_language)

#recode Employment (v54)
# look_for(ces65, "employ")
ces65$employment<-Recode(ces65$v54, "2=1; 1=0; else=NA")
val_labels(ces65$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces65$employment)
# table(ces65$employment)

#No Sector variable

#recode Party ID (v221)
# look_for(ces65, "identification")
ces65$party_id<-Recode(ces65$v221, "20=1; 10=2; 30=3; 40:60=0; else=NA")
val_labels(ces65$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces65$party_id)
# table(ces65$party_id)

#recode Vote (v262)
# look_for(ces65, "vote")
ces65$vote<-Recode(ces65$v262, "12=1; 11=2; 13=3; 14:19=0; else=NA")
val_labels(ces65$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces65$vote)
# table(ces65$vote)

#recode Occupation (v306)
# look_for(ces65, "occupation")
ces65$occupation<-Recode(ces65$v306, "10=1; 20=2; 32:37=3; 69=3; 40=4; 70:80=5; else=NA")
val_labels(ces65$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces65$occupation)
# table(ces65$occupation)

#recode Income (v336)

# look_for(ces65, "income")
ces65$income<-Recode(ces65$v336, "1:3=1; 4:5=2; 6=3; 7:8=4; 9:11=5; else=NA")
val_labels(ces65$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#Simon's Version
val_labels(ces65$income)
# table(ces65$income)
# look_for(ces65, "income")
ces65$v336
ces65$income2<-Recode(ces65$v336, "1:4=1; 5:7=2; 8:9=3; 10=4; 11=5; else=NA")
val_labels(ces65$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

#checks
val_labels(ces65$income)
# table(ces65$income)
val_labels(ces65$v336)
ces65$income_tertile<-Recode(ces65$v336, "1:5=1; 6:8=2;9:11=3; 97=NA")
prop.table(table(ces65$income_tertile))
val_labels(ces65$income_tertile)<-c(Lowest=1, Middle=2, Highest=3)
#recode Religiosity (v310)
# look_for(ces65, "church")
ces65$religiosity<-Recode(ces65$v310, "5=1; 4=2; 3=3; 2=4; 1=5; else=NA")
val_labels(ces65$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces65$religiosity)
# table(ces65$religiosity)

#recode Personal Retrospective (v49)
# look_for(ces65, "situation")
ces65$personal_retrospective<-Recode(as.numeric(ces65$v49), "1=1; 2=0; 3=0.5; else=NA", as.numeric=T)
#val_labels(ces65$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces65$personal_retrospective)
# table(ces65$personal_retrospective, ces65$v49 , useNA = "ifany" )

#recode turnout (v262)
# look_for(ces65, "vote")
ces65$turnout<-Recode(ces65$v262, "11:19=1; 0=0;98=0; else=NA")
val_labels(ces65$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces65$turnout)
# table(ces65$turnout)
# table(ces65$turnout, ces65$vote)

#### recode political efficacy ####
#recode No Say (v45)
# look_for(ces65, "political power")
ces65$efficacy_internal<-Recode(as.numeric(ces65$v45), "1=0; 2=1; else=NA", as.numeric=T)
#val_labels(ces65$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces65$efficacy_internal)
# table(ces65$efficacy_internal)
# table(ces65$efficacy_internal, ces65$v45 , useNA = "ifany" )

#recode MPs lose touch (v46)
# look_for(ces65, "lose touch")
ces65$efficacy_external<-Recode(as.numeric(ces65$v46), "1=0; 2=1; else=NA", as.numeric=T)
#val_labels(ces65$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces65$efficacy_external)
# table(ces65$efficacy_external)
# table(ces65$efficacy_external, ces65$v46 , useNA = "ifany" )

#recode Official Don't Care (v43)
# look_for(ces65, "don't care")
ces65$efficacy_external2<-Recode(as.numeric(ces65$v43), "1=0; 2=1; else=NA", as.numeric=T)
#val_labels(ces65$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces65$efficacy_external2)
# table(ces65$efficacy_external2)
# table(ces65$efficacy_external2, ces65$v43 , useNA = "ifany" )

ces65 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces65

ces65 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
#qplot(ces65$political_efficacy, geom="histogram")
# table(ces65$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces65 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  psych::alpha(.)
#Check correlation
ces65 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  cor(., use="complete.obs")

#recode foreign born (v312)
# look_for(ces65, "birth")
ces65$foreign<-Recode(ces65$v312, "209=0; 199=1; 300:999=0; else=NA")
val_labels(ces65$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces65$foreign)
# table(ces65$foreign, ces65$v312, useNA="ifany")

#recode Most Important Question (v8a)
# look_for(ces65, "most")
ces65$mip<-Recode(ces65$v8a, "11=9; 12:13=7; 14=6; 15=0; 16:17=15; 18=8; 19=0; 20=4; 21:22=14; 23:26=12;
			                       27:29=16; 30:32=11; 33:34=3; 35:36=0; 37=13; 38=2; 39=6; 40:81=0; else=NA")
val_labels(ces65$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16)
# table(ces65$mip)

#Empty variables that are not available pre-88
# ces65$redistribution<-rep(NA, nrow(ces65))
# ces65$market_liberalism<-rep(NA, nrow(ces65))
# ces65$traditionalism2<-rep(NA, nrow(ces65))
# ces65$immigration_rates<-rep(NA, nrow(ces65))
# Add mode
ces65$mode<-rep("Phone", nrow(ces65))

#Add Election
ces65$election<-c(rep(1965, nrow(ces65)))

ces65$blah<-rep(1, nrow(ces65))
glimpse(ces65)
save(ces65, file=here("data/ces65.rda"))
