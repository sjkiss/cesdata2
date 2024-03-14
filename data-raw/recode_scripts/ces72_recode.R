#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data

ces72_nov<-read_sav(file=here("data-raw/CES-E-1972-nov_F1.sav"))


#recode Gender (V480)
# look_for(ces72_nov, "sex")
#recode Gender (qxiz)
# look_for(ces72_nov, "sex")
ces72_nov$male<-Recode(ces72_nov$qxi, "1=1; 2=0")
val_labels(ces72_nov$male)<-c(Female=0, Male=1)
#checks
val_labels(ces72_nov$male)
table(ces72_nov$male)

#recode Union Household (qviiia)
# look_for(ces72_nov, "union")
ces72_nov$union<-Recode(ces72_nov$qviiia, "2:3=1; 0:1=0")
val_labels(ces72_nov$union)<-c(None=0, Union=1)
#checks
val_labels(ces72_nov$union)
table(ces72_nov$union)

#Union Combined variable (identical copy of union)
ces72_nov$union_both<-ces72_nov$union
#checks
val_labels(ces72_nov$union_both)
table(ces72_nov$union_both)

#recode Education (qvi)
# look_for(ces72_nov, "school")
ces72_nov$degree<-Recode(ces72_nov$qvi, "6:7=1; 1:5=0")
val_labels(ces72_nov$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces72_nov$degree)
table(ces72_nov$degree)

#recode Region (m_area_a)
# look_for(ces72_nov, "area")
ces72_nov$region<-Recode(ces72_nov$m_area_a, "6:9=1; 0=2; 2:5=3; 1=NA")
val_labels(ces72_nov$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces72_nov$region)
table(ces72_nov$region)

#recode Quebec (m_area_a)
# look_for(ces72_nov, "area")
ces72_nov$quebec<-Recode(ces72_nov$m_area_a, "0=0; 2:9=0; 1=1")
val_labels(ces72_nov$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces72_nov$quebec)
table(ces72_nov$quebec)

#recode Age (qv)
# look_for(ces72_nov, "age")
ces72_nov$age<-Recode(as.numeric(ces72_nov$qv), "0=NA; 1=19; 2=22; 3=27; 4=32; 5=37; 6=42; 7=47; 8=52; 9=57; 10=62; 11=70")

#check
table(ces72_nov$age)

#recode Religion (qvii)
# look_for(ces72_nov, "relig")
ces72_nov$religion<-Recode(ces72_nov$qvii, "0=NA; 1=1; 2=2; 3:4=3; 5=0")
val_labels(ces72_nov$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces72_nov$religion)
table(ces72_nov$religion)
table(ces72_nov$qvii)

#recode Language (qixb)
# look_for(ces72_nov, "language")
ces72_nov$language<-Recode(ces72_nov$qixb, "2=0; 1=1; else=NA")
val_labels(ces72_nov$language)<-c(French=0, English=1)
#checks
val_labels(ces72_nov$language)
table(ces72_nov$language)

#recode Non-charter Language (qixb)
# look_for(ces72_nov, "language")
ces72_nov$non_charter_language<-Recode(ces72_nov$qixb, "1:2=0; 3=1; else=NA")
val_labels(ces72_nov$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces72_nov$non_charter_language)
table(ces72_nov$non_charter_language)

#recode Employment (qiv)
# look_for(ces72_nov, "employ")
# look_for(ces72_nov, "occupation")
ces72_nov$employment<-Recode(ces72_nov$qiv, "0=0; 8:9=0; 1:7=1; else=NA")
val_labels(ces72_nov$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces72_nov$employment)
table(ces72_nov$employment)

#No Sector variable

#recode Party ID (qi)
# look_for(ces72_nov, "federal")
ces72_nov$party_id<-Recode(ces72_nov$qi, "3=1; 1=2; 4=3; 2=0; else=NA")
val_labels(ces72_nov$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces72_nov$party_id)
table(ces72_nov$party_id)

#recode Vote (qa13a1a)
# look_for(ces72_nov, "vote")
ces72_nov$vote<-Recode(ces72_nov$qa13a1a, "2=1; 1=2; 3=3; 4:8=0; else=NA")
val_labels(ces72_nov$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces72_nov$vote)
table(ces72_nov$vote)

#recode Occupation (qiv)
# look_for(ces72_nov, "occupation")
ces72_nov$occupation<-Recode(ces72_nov$qiv, "1=1; 2=2; 3:4=3; 5=4; 6:7=5; else=NA")
val_labels(ces72_nov$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces72_nov$occupation)
table(ces72_nov$occupation)

#recode Income (qxii)
# look_for(ces72_nov, "income")
ces72_nov$income<-Recode(ces72_nov$qxii, "1:2=1; 3:4=2; 5=3; 6=4; 7:8=5; else=NA")
val_labels(ces72_nov$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces72_nov$income)
table(ces72_nov$income)
#Simon's Version is identical
# look_for(ces72_nov, "income")
ces72_nov$qxii
ces72_nov$income2<-Recode(ces72_nov$qxii, "1:2=1; 3:4=2; 5=3; 6=4; 7:8=5; else=NA")
val_labels(ces72_nov$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces72_nov$income)
table(ces72_nov$income)
#tertiles
# look_for(ces72_nov, "income")
ces72_nov$income_tertile<-Recode(ces72_nov$qxii, "1:3=1; 4:5=2; 6:8=3; else=NA")
val_labels(ces72_nov$income_tertile)<-c(Lowest=1, Middle=2,Highest=3)
#checks
val_labels(ces72_nov$income)
table(ces72_nov$income)

#recode foreign born (qx) - CES seems mis-coded as no foreign respondents
# look_for(ces72_nov, "born")
ces72_nov$foreign<-Recode(ces72_nov$qx, "3=1; 1:2=0; 4=0; else=NA")
val_labels(ces72_nov$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces72_nov$foreign)
table(ces72_nov$foreign, ces72_nov$qx, useNA="ifany")
table(ces72_nov$foreign)

#Empty variables that are not available pre-88
# ces72_nov$redistribution<-rep(NA, nrow(ces72_nov))
# ces72_nov$market_liberalism<-rep(NA, nrow(ces72_nov))
# ces72_nov$traditionalism2<-rep(NA, nrow(ces72_nov))
# ces72_nov$immigration_rates<-rep(NA, nrow(ces72_nov))
# ces72_nov$turnout<-rep(NA, nrow(ces72_nov))
# ces72_nov$mip<-rep(NA, nrow(ces72_nov))
lookfor(ces72_nov, "problem")
# Add mode
ces72_nov$mode<-rep("Phone", nrow(ces72_nov))

#Add Election
ces72_nov$election<-c(rep(1972, nrow(ces72_nov)))
#glimpse(ces72_nov)
lookfor(ces72_nov, "problem")

table(ces72_nov$q24a1a)
#This variable was a mess
# it is essentially stored in q24a1a to q24a8a or something like that.
# Up until q24a5a, it seems to be coded if a respondent mentioned
# any of about 10 core issues
# So, in the absence of any documentation I sort of assumed the
# first variable was a respondent's first response
# So I just used that.
# but then in q24a5a and afterward, it seemed to be
# a respondent's offering a second set of secondary issues.h
# So I basically coded this as follows:
# I took the respondent's response to teh first variable q24a2a and
# just mapped that onto our categorization
# Then, if *all* of q24a1a to q24a4a was 0, I assume it meant the repsondent did not offer
# One of the core responses, so then I took the variables with teh secondary set of responses and maped those
# to our variable.
# Fucking nightmare. Were they all drunk at the time?
ces72_nov %>%
  mutate(mip=case_when(
    # 0 is don't know
    #q24a1a==0~ NA,
    # jobs
    q24a1a==1~ 6,
    # inflation
    q24a1a==2~ 18,
    # Taxes
    q24a1a==3~ 9,
    # welfare into social programs
    q24a1a==4~ 15,
    # poverty into economy
    q24a1a==5~ 7,
    # economy
    q24a1a==6~ 7,
    # old people/ SENIORS INTO SOCIAL PROGRAMS
    q24a1a==7~ 15,
    # Housing
    q24a1a==8~ 0,
    #IMMIGRATION
    q24a1a=='-'~ 13,
    #FARMERS
    q24a1a=='&'~ 0,
    #Ecology into environment
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==0 ~ 1,
    #Bilingualism # into brokerage
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==1~ 16,
    #independence of quebec into brokerage
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==2~ 16,
    #Canada's Unityh # into brokerage
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==4~ 16,
    #Strikes - into economy
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==7~ 7,
    #Education - into education
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a=="-"~ 4,
    #Youths Teenagers - into socio-cultural
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==6~ 14,
    #Leadership - into other
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==5~ 0,
    #Drugs into socio-cultural
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==5~ 14,
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a=="-"~ 0,
    q24a1a==0&q24a2a==0&q24a3a==0&q24a4a==0&q24a5a==9~ 0
      ))->ces72_nov

val_labels(ces72_nov$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Inflation=18)
table(as_factor(ces72_nov$mip))

save(ces72_nov, file=here("data/ces72_nov.rda"))
