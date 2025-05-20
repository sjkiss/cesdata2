#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces00<-read_sav(file=here("data-raw/CES-E-2000_F1.sav"))


#### gender####
#recode Gender (cpsrgen)
# look_for(ces00, "gender")
ces00$male<-Recode(ces00$cpsrgen, "1=1; 5=0")
val_labels(ces00$male)<-c(Female=0, Male=1)
#checks
val_labels(ces00$male)
# table(ces00$male)

####union ####
#recode Union Household (cpsm9)
# look_for(ces00, "union")
ces00$union<-Recode(ces00$cpsm9, "1=1; 5=0; else=NA")
val_labels(ces00$union)<-c(None=0, Union=1)
#checks
val_labels(ces00$union)
# table(ces00$union)

#Union Combined variable (identical copy of union)
ces00$union_both<-ces00$union
#checks
val_labels(ces00$union_both)
# table(ces00$union_both)

####education ####
#recode Education (cpsm3)
# look_for(ces00, "education")
ces00$degree<-Recode(ces00$cpsm3, "9:11=1; 1:8=0; else=NA")
val_labels(ces00$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces00$degree)
# table(ces00$degree)

####region ####
#recode Region (province)
# look_for(ces00, "province")
ces00$region<-Recode(ces00$province, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces00$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces00$region)
# table(ces00$region)

#### Province ####
#recode Province (province)
# look_for(ces00, "province")
ces00$prov<-Recode(ces00$province, "10=1; 11=2; 12=3; 13=4; 24=5; 35=6; 46=7; 47=8; 48=9; 59=10; else=NA")
val_labels(ces00$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces00$prov)
table(ces00$prov)

#### quebec####
#recode Quebec (province)
# look_for(ces00, "province")
ces00$quebec<-Recode(ces00$province, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces00$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces00$quebec)
# table(ces00$quebec)

#### age ####
#recode Age (cpsage)
# look_for(ces00, "age")
ces00$yob<-Recode(ces00$cpsage, "9999=NA")
ces00$age<-2000-ces00$yob
#check
# table(ces00$age)

####religion ####
#recode Religion (cpsm10)
# look_for(ces00, "relig")
ces00$religion<-Recode(ces00$cpsm10, "0=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces00$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces00$religion)
# table(ces00$religion)

####language ####
#recode Language (cpslang)
# look_for(ces00, "language")
ces00$language<-Recode(ces00$cpslang, "1=1; 2=0; else=NA")
val_labels(ces00$language)<-c(French=0, English=1)
#checks
val_labels(ces00$language)
# table(ces00$language)

####non-charter language ####
#recode Non-charter Language (cpsm15)
# look_for(ces00, "language")
ces00$non_charter_language<-Recode(ces00$cpsm15, "1:5=0; 0=1; else=NA")
val_labels(ces00$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces00$non_charter_language)
# table(ces00$non_charter_language)

####employment ####
#recode Employment (cpsm4)
# look_for(ces00, "employ")
ces00$employment<-Recode(ces00$cpsm4, "4:8=0; 1:3=1; else=NA")
val_labels(ces00$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces00$employment)
# table(ces00$employment)

#recode Unemployed (cpsm4)
# look_for(ces00, "employment")
ces00$unemployed<-Recode(ces00$cpsm4, "1:3=0; 4=1; else=NA")
val_labels(ces00$unemployed)<-c(Employed=0, Unemployed=1)
#checks
val_labels(ces00$unemployed)
table(ces00$unemployed)

####Sector ####
#recode Sector (cpsm7 & cpsm4)
# look_for(ces00, "company")
ces00 %>%
  mutate(sector=case_when(
    cpsm7==3 ~1,
    cpsm7==5 ~1,
    cpsm7==7 ~1,
    cpsm7==1 ~0,
    cpsm7==0 ~0,
    cpsm4==1 ~0,
    cpsm4> 3 & cpsm4< 9 ~ 0,
    cpsm7==9 ~NA_real_ ,
    cpsm7==8 ~NA_real_ ,
  ))->ces00

#ces00$sector2<-Recode(ces00$cpsm7, "3:7=1; 0:1=0; else=NA")

val_labels(ces00$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces00$sector)
# table(ces00$sector)

####party id ####
#recode Party ID (pesk1a, pesk1ab and pesk4)
look_for(ces00, "yourself")
ces00 %>%
  mutate(party_id=case_when(
    pesk1a==1 | pesk1b==1 | pesk4==1 ~ 1,
    pesk1a==2 | pesk1b==2 | pesk4==2 ~ 2,
    pesk1a==3 | pesk1b==3 | pesk4==3 ~ 3,
    pesk1a==4 | pesk1b==4 | pesk4==4 ~ 2,
    pesk1a==0 | pesk1b==0 | pesk4==0 ~ 0,
    pesk1a==5 | pesk1b==5 | pesk4==5 ~ 4,
  ))->ces00

val_labels(ces00$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces00$party_id)
table(ces00$party_id)

#recode Party ID 2 (pesk1a, pesk1ab and pesk4)
look_for(ces00, "yourself")
ces00 %>%
  mutate(party_id2=case_when(
    pesk1a==1 | pesk1b==1 | pesk4==1 ~ 1,
    pesk1a==2 | pesk1b==2 | pesk4==2 ~ 2,
    pesk1a==3 | pesk1b==3 | pesk4==3 ~ 3,
    pesk1a==4 | pesk1b==4 | pesk4==4 ~ 6,
    pesk1a==0 | pesk1b==0 | pesk4==0 ~ 0,
    pesk1a==5 | pesk1b==5 | pesk4==5 ~ 4,
  ))->ces00

val_labels(ces00$party_id2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces00$party_id2)
table(ces00$party_id2)

#recode Party ID 3 (cpsk1a, cpsk1ab, cpsk3b and cpsk4a)
look_for(ces00, "yourself")
ces00 %>%
  mutate(party_id3=case_when(
    cpsk1a==1 | cpsk1b==1 | cpsk3b==1 | cpsk4a==1 ~ 1,
    cpsk1a==2 | cpsk1b==2 | cpsk3b==2 | cpsk4a==2 ~ 4,
    cpsk1a==3 | cpsk1b==3 | cpsk3b==3 | cpsk4a==3 ~ 6,
    cpsk1a==4 | cpsk1b==4 | cpsk3b==4 | cpsk4a==4 ~ 2,
    cpsk1a==97 | cpsk1b==97 | cpsk3b==97 | cpsk4a==97 ~ 0,
    cpsk1a==5 | cpsk1b==5 | cpsk3b==5 | cpsk4a==5 ~ 3,
  ))->ces00

val_labels(ces00$party_id3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces00$party_id3)
table(ces00$party_id3)

#recode Party closeness (pesk2 )
look_for(ces00, "strongly")
ces00$party_close<-Recode(ces00$pesk2 , "1=1; 3=0.5; 5=0; else=NA")
#checks
table(ces00$pesk2  , ces00$party_close, useNA = "ifany" )

####vote ####
#recode Vote (pesa3a and pesa3b)
# look_for(ces00, "vote")
ces00 %>%
  mutate(vote=case_when(
    pesa3a==1 | pesa3b==1 ~ 1,
    pesa3a==2 | pesa3b==2 ~ 2,
    pesa3a==3 | pesa3b==3 ~ 3,
    pesa3a==4 | pesa3b==4 ~ 2,
    pesa3a==0 | pesa3b==0 ~ 0,
    pesa3a==5 | pesa3b==5 ~ 4,
  ))->ces00

val_labels(ces00$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces00$vote)
# table(ces00$vote)

#recode Vote splitting Conservatives (pesa3a)
# look_for(ces00, "vote")
ces00 %>%
  mutate(vote3=case_when(
    pesa3a==1 | pesa3b==1 ~ 1,
    pesa3a==2 | pesa3b==2 ~ 2,
    pesa3a==3 | pesa3b==3 ~ 3,
    pesa3a==4 | pesa3b==4 ~ 6,
    pesa3a==0 | pesa3b==0 ~ 0,
    pesa3a==5 | pesa3b==5 ~ 4,
  ))->ces00
val_labels(ces00$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces00$vote3)
# table(ces00$vote3)
# table(ces00$pesa4)

# No occupation variable
#### income####
#recode Income (cpsm16 and cpsm16a)
# look_for(ces00, "income")
ces00 %>%
  mutate(income=case_when(
    cpsm16a==1 | cpsm16> -1 & cpsm16 < 20 ~ 1,
    cpsm16a==2 | cpsm16> 19 & cpsm16 < 40 ~ 2,
    cpsm16a==3 | cpsm16> 19 & cpsm16 < 40 ~ 2,
    cpsm16a==4 | cpsm16> 39 & cpsm16 < 60 ~ 3,
    cpsm16a==5 | cpsm16> 39 & cpsm16 < 60 ~ 3,
    cpsm16a==6 | cpsm16> 59 & cpsm16 < 80 ~ 4,
    cpsm16a==7 | cpsm16> 59 & cpsm16 < 80 ~ 4,
    cpsm16a==8 | cpsm16> 79 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 79 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 79 & cpsm16 < 998 ~ 5,
  ))->ces00

# look_for(ces00, "income")
ces00 %>%
  mutate(income2=case_when(
    #First Quintile
    cpsm16a==1 | cpsm16> -1 & cpsm16 < 21 ~ 1,
    #second Quintile
    cpsm16a==2 | cpsm16> 20 & cpsm16 < 38 ~ 2,
    cpsm16a==3 | cpsm16> 20& cpsm16 < 38 ~ 2,
    #Third Quintile
    cpsm16a==4 | cpsm16> 37 & cpsm16 < 58 ~ 3,
    cpsm16a==5 | cpsm16> 37 & cpsm16 < 58 ~ 3,
    #Fourth Quintile
    cpsm16a==6 | cpsm16> 57 & cpsm16 < 86 ~ 4,
    cpsm16a==7 | cpsm16> 57 & cpsm16 < 86 ~ 4,
    #Fifth Quintile
    cpsm16a==8 | cpsm16> 85 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 85 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 85 & cpsm16 < 998 ~ 5,
  ))->ces00
val_labels(ces00$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces00$income)
# table(ces00$income)
# table(ces00$income, ces00$cpsm16)
# table(ces00$income, ces00$cpsm16a)

#income_tertiles

# look_for(ces00, "income")
ces00$cpsm16a
ces00$cpsm16
ces00$cpsm16a
ces00 %>%
  mutate(income_tertile=case_when(
    #First tertile
    cpsm16a<3 | cpsm16<32~1,
    #Second Tertile
    cpsm16a>2 &cpsm16a< 6 | cpsm16 >31 & cpsm16<65 ~2,
    #third Tertile
    cpsm16a>5 &cpsm16a<11 |cpsm16 > 64 & cpsm16<998 ~ 3
  ))->ces00
val_labels(ces00$income_tertile)<-c("Lowest"=1, "Middle"=2, "Highest"=3)
# table(ces00$income)
# table(ces00$income_tertile, ces00$cpsm16)
# table(ces00$income_tertile, ces00$cpsm16a)

ces00 %>%
  select(contains("income"), cpsm16a, cpsm16) %>%
  filter(cpsm16a==998&income_tertile==1)
# look_for(ces00, "noc")
# look_for(ces00, "employment")
# look_for(ces00, "career")
ces00$bycat_15

#####recode Household size (ceshhwgt)####
# look_for(ces00, "house")
ces00$household<-Recode(ces00$ceshhwgt, "0.5174=0.5; 1.0347=1; 1.5521=1.5; 2.0694=2; 2.5868=2.5; 3.1042=3; 3.6215=3.5; 4.6562=4.5; 5.1736=5")
#checks
# table(ces00$household)

#### recode income / household size ####
ces00$inc<-Recode(ces00$cpsm16, "998:999=NA")
# table(ces00$inc)
ces00$inc2<-ces00$inc/ces00$household
# table(ces00$inc2)
ces00$inc3<-Recode(ces00$cpsm16a, "98:999=NA")
# table(ces00$inc3)
ces00$inc4<-ces00$inc3/ces00$household
# table(ces00$inc4)

ces00 %>%
  mutate(income_house=case_when(
    as.numeric(inc4)<3.2 |(as.numeric(inc2)> 0 & as.numeric(inc2) < 32) ~ 1,
    (as.numeric(inc4)>3.1 &as.numeric(inc4) <6.5 )| as.numeric(inc2)> 31 & as.numeric(inc2) < 65 ~ 2,
    (as.numeric(inc4)>6.4 & as.numeric(inc4)<31) | as.numeric(inc2)> 64 & as.numeric(inc2) <1501 ~ 3
  ))->ces00
# table(ces00$income_house)
# table(ces00$income_tertile)
# table(ces00$income_tertile, ces00$income_house)

####recode Religiosity (cpsm10b)####
# look_for(ces00, "relig")
ces00$religiosity<-Recode(ces00$cpsm10b, "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces00$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces00$religiosity)
# table(ces00$religiosity)

####Redistribution ####
#recode Redistribution (cpsc13)
# look_for(ces00, "rich")
val_labels(ces00$cpsc13)
ces00$redistribution<-Recode(ces00$cpsc13, "; 1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces00$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces00$redistribution)
# table(ces00$redistribution)
val_labels(ces00$redistribution)<-NULL
#recode Pro-Redistribution (cpsc13)
ces00$pro_redistribution<-Recode(ces00$cpsc13, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces00$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces00$pro_redistribution)
# table(ces00$pro_redistribution)
####Market Liberalism  ####
#recode Market Liberalism (cpsf6 and pesg15)
# look_for(ces00, "private")
# look_for(ces00, "blame")
ces00$market1<-Recode(as.numeric(ces00$cpsf6), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces00$market2<-Recode(as.numeric(ces00$pesg15), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
# val_labels(ces00$market1)<-NULL
# val_labels(ces00$market2)<-NULL
#these are some checks to see why there are any missing values in cpsf6.
ces00 %>%
  select(cpsf6) %>%
  filter(cpsf6==9)
val_labels(ces00$cpsf6)
# table(ces00$cpsf6)#In theory, there should be 8s and 9s tin the variable, but there are none. Why?
# table(ces00$cpsf6, useNA = "ifany")

#this just returns NA values on cpsf6, there are 171 of them.
ces00 %>%
  filter(is.na(cpsf6)) %>%
  select(cpsf6)
#There was a problem down below with a ton of missing values in market2, so I am checking into this.
# table(ces00$pesg15)
val_labels(ces00$pesg15)
val_labels(ces00$cpsf6)
#checks
# table(ces00$market1)
# table(ces00$market2)
# table(ces00$cpsf6, ces00$market1, useNA = "ifany")
# table(ces00$pesg15, ces00$market2, useNA = "ifany")
#Note there are a ton of missing values (929) in the original variable pesg15
#This must be from the Campaign Period Survey.

#this computes the average of market1 and market2 in a dataframe called out.


#Scale Averaging
ces00 %>%
  mutate(market_liberalism=
           rowMeans(select(., c('market1', 'market2')), na.rm=T))->ces00

ces00 %>%
  select(starts_with("market")) %>%
  summary()
#Note that there are a ton of missing values in market2
#Check distribution of market_liberalism
#qplot(ces00$market_liberalism, geom="histogram")
# table(ces00$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces00 %>%
  select(market1, market2) %>%
  alpha(.)
#Check correlation
ces00 %>%
  select(market1, market2) %>%
  cor(., use="complete.obs")

#### immigration ####
#recode Immigration (cpsj18)
# look_for(ces00, "imm")
ces00$immigration_rates<-Recode(as.numeric(ces00$cpsj18), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces00$immigration_rates)

#### recode Environment vs Jobs
#recode Environment (mbsa6)
# look_for(ces00, "env")
ces00$enviro<-Recode(as.numeric(ces00$mbsa6), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces00$enviro)

#### capital punishment####
#recode Capital Punishment (cpsc15)
# look_for(ces00, "death")
ces00$death_penalty<-Recode(as.numeric(ces00$cpsc15), "1=1; 5=0; 7=0.5; 8=0.5; else=NA")
#checks
# table(ces00$death_penalty)
#### crime####
#recode Crime (mbse5)
# look_for(ces00, "crime")
ces00$crime<-Recode(as.numeric(ces00$mbse5), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces00$crime)

#recode Gay Rights (cpsf18)
# look_for(ces00, "gays")
ces00$gay_rights<-Recode(as.numeric(ces00$cpsf18), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
# table(ces00$gay_rights)

#recode Abortion (pesg8)
# look_for(ces00, "abort")
ces00$abortion<-Recode(as.numeric(ces00$pesg8), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
# table(ces00$abortion)

#recode Lifestyle (mbsa7)
# look_for(ces00, "lifestyle")
ces00$lifestyles<-Recode(as.numeric(ces00$mbsa7), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces00$lifestyles)

#recode Stay Home (cpsf3)
# look_for(ces00, "home")
ces00$stay_home<-Recode(as.numeric(ces00$cpsf3), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces00$stay_home)

#recode Marriage Children (mbse4)
# look_for(ces00, "children")
ces00$marriage_children<-Recode(as.numeric(ces00$mbse4), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces00$marriage_children)

#recode Values (mbsa9)
# look_for(ces00, "traditional")
ces00$values<-Recode(as.numeric(ces00$mbsa9), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# table(ces00$values)

#recode Morals (mbsa8)
# look_for(ces00, "moral")
ces00$morals<-Recode(as.numeric(ces00$mbsa8), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces00$morals)

#recode Moral Traditionalism (abortion, lifestyles, stay home, values, marriage childen, morals)
ces00$trad3<-ces00$abortion
ces00$trad7<-ces00$lifestyles
ces00$trad1<-ces00$stay_home
ces00$trad4<-ces00$values
ces00$trad5<-ces00$marriage_children
ces00$trad6<-ces00$morals
ces00$trad2<-ces00$gay_rights
# table(ces00$trad1)
# table(ces00$trad2)
# table(ces00$trad3)
# table(ces00$trad4)
# table(ces00$trad5)
# table(ces00$trad6)
# table(ces00$trad7)


#Scale Averaging
ces00 %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", 1:7)), na.rm=T))->ces00


ces00 %>%
  select(starts_with("trad")) %>%
  summary()
#Check distribution of traditionalism
qplot(ces00$traditionalism, geom="histogram")
# table(ces00$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces00 %>%
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>%
  psych::alpha(.)
#Check correlation
ces00 %>%
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>%
  cor(., use="complete.obs")

#recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)
ces00 %>%
  mutate(traditionalism2=rowMeans(select(., c('trad1', 'trad2')), na.rm=T))->ces00



#Check distribution of traditionalism
qplot(ces00$traditionalism2, geom="histogram")
# table(ces00$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces00 %>%
  select(trad1, trad2) %>%
  psych::alpha(.)

#Check correlation
ces00 %>%
  select(trad1, trad2) %>%
  cor(., use="complete.obs")

#recode 2nd Dimension (stay home, immigration, gay rights, crime)
ces00$author1<-ces00$stay_home
ces00$author2<-ces00$immigration_rates
ces00$author3<-ces00$gay_rights
ces00$author4<-ces00$crime
# table(ces00$author1)
# table(ces00$author2)
# table(ces00$author3)
# table(ces00$author4)

#Remove value labels
ces00 %>%
  mutate(across(num_range('author', 1:4), remove_val_labels))->ces00

ces00 %>%
  mutate(authoritarianism=rowMeans(select(., num_range("author", 1:4)), na.rm = T))->ces00


ces00 %>%
  select(starts_with("author")) %>%
  summary()
#Check distribution of traditionalism
qplot(ces00$authoritarianism, geom="histogram")
# table(ces00$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
ces00 %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces00 %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")

#recode Quebec Accommodation (cpsc12) (Left=more accom)
# look_for(ces00, "quebec")
ces00$quebec_accom<-Recode(as.numeric(ces00$cpsc12), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; 8=0.5; else=NA")
#checks
# table(ces00$quebec_accom)


#recode Liberal leader (pesf2)
# look_for(ces00, "Chretien")
ces00$liberal_leader<-Recode(as.numeric(ces00$pesf2), "0=1; 996:999=NA")
#checks
# table(ces00$liberal_leader)

#recode conservative leader (pesf1)
# look_for(ces00, "Clark")
ces00$conservative_leader<-Recode(as.numeric(ces00$pesf1), "0=1; 996:999=NA")
#checks
# table(ces00$conservative_leader)

#recode NDP leader (pesf3)
# look_for(ces00, "McDonough")
ces00$ndp_leader<-Recode(as.numeric(ces00$pesf3), "0=1; 996:999=NA")
#checks
# table(ces00$ndp_leader)

#recode Bloc leader (pesf5)
# look_for(ces00, "Duceppe")
ces00$bloc_leader<-Recode(as.numeric(ces00$pesf5), "0=1; 996:999=NA")
#checks
# table(ces00$bloc_leader)

#recode liberal rating (pesc1b)
# look_for(ces00, "liberal")
ces00$liberal_rating<-Recode(as.numeric(ces00$pesc1b), "0=1; 996:999=NA")
#checks
# table(ces00$liberal_rating)

#recode conservative rating (pesc1a)
# look_for(ces00, "conservative")
ces00$conservative_rating<-Recode(as.numeric(ces00$pesc1a), "0=1; 996:999=NA")
#checks
# table(ces00$conservative_rating)

#recode NDP rating (pesc1c)
# look_for(ces00, "new democratic")
ces00$ndp_rating<-Recode(as.numeric(ces00$pesc1c), "0=1; 996:999=NA")
#checks
# table(ces00$ndp_rating)

#recode Bloc rating (pesc1e)
# look_for(ces00, "bloc")
ces00$bloc_rating<-Recode(as.numeric(ces00$pesc1e), "0=1; 996:999=NA")
#checks
# table(ces00$bloc_rating)

#recode Education (pesd1f)
# look_for(ces00, "edu")
ces00$education<-Recode(as.numeric(ces00$pesd1f), "3=0; 5=0.5; 1=1; 8=0.5; else=NA")
#checks
# table(ces00$education, ces00$pesd1f , useNA = "ifany" )

#recode Personal Retrospective (cpsc1)
# look_for(ces00, "financ")
ces00$personal_retrospective<-Recode(as.numeric(ces00$cpsc1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces00$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces00$personal_retrospective)
# table(ces00$personal_retrospective, ces00$cpsc1 , useNA = "ifany" )

#recode National Retrospective (cpsg1)
# look_for(ces00, "economy")
ces00$national_retrospective<-Recode(as.numeric(ces00$cpsg1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces00$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces00$national_retrospective)
# table(ces00$national_retrospective, ces00$cpsg1 , useNA = "ifany" )

#recode Ideology (cpspla36)
# look_for(ces00, "the left")
ces00$ideology<-Recode(as.numeric(ces00$cpspla36), "1=0; 3=1; 5=0.5; else=NA")
#val_labels(ces00$ideology)<-c(Left=0, Right=1)
#checks
val_labels(ces00$ideology)
# table(ces00$ideology, ces00$cpspla36 , useNA = "ifany")

#recode turnout (pesa2)
# look_for(ces00, "vote")
ces00$turnout<-Recode(ces00$pesa2, "1=1; 5=0; 8=0; else=NA")
val_labels(ces00$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces00$turnout)
# table(ces00$turnout)
# table(ces00$turnout, ces00$vote)

#### recode political efficacy ####
#recode No Say (mbsc12)
# look_for(ces00, "not have say")
ces00$efficacy_internal<-Recode(as.numeric(ces00$mbsc12), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces00$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces00$efficacy_internal)
# table(ces00$efficacy_internal)
# table(ces00$efficacy_internal, ces00$mbsc12 , useNA = "ifany" )

#recode MPs lose touch (mbsc5)
# look_for(ces00, "lose touch")
ces00$efficacy_external<-Recode(as.numeric(ces00$mbsc5), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces00$efficacy_external)<-c(low=0, high=1)
#checks
val_labels(ces00$efficacy_external)
# table(ces00$efficacy_external)
# table(ces00$efficacy_external, ces00$mbsc5 , useNA = "ifany" )

#recode Official Don't Care (cpsb10d)
# look_for(ces00, "cares much")
ces00$efficacy_external2<-Recode(as.numeric(ces00$cpsb10d), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces00$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces00$efficacy_external2)
# table(ces00$efficacy_external2)
# table(ces00$efficacy_external2, ces00$cpsb10d , useNA = "ifany" )

ces00 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces00

ces00 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
#qplot(ces00$political_efficacy, geom="histogram")
# table(ces00$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
# ces00 %>%
#   select(efficacy_external, efficacy_external2, efficacy_internal) %>%
#   psych::alpha(.)
#Check correlation
# ces00 %>%
#   select(efficacy_external, efficacy_external2, efficacy_internal) %>%
#   cor(., use="complete.obs")

#recode Most Important Question (cpsa1)
# look_for(ces00, "issue")
ces00$mip<-Recode(ces00$cpsa1, "10:15=6; 16=4; 17=0; 20:27=10; 30=7; 31=18; 32:36=7; 40:42=0; 43:44=16; 45=0; 46=3; 47=11;
				                        48:49=0; 50:55=9; 56=0; 57=8; 58=15; 59=8; 60:63=15; 64=6; 65:69=8; 70=0;
				                        71=2; 72=15; 73:74=14; 75=1; 76=14; 80:88=16; 90=0; 91=3; 92=11; 93:96=0; 97=11; else=NA")
val_labels(ces00$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18)
# table(ces00$mip)

# recode satisfaction with democracy (cpsa8, pesa6)
# look_for(ces00, "dem")
ces00$satdem<-Recode(as.numeric(ces00$pesa6), "1=1; 3=0.75; 5=0.25; 7=0; 98=0.5; else=NA", as.numeric=T)
#checks
# table(ces00$satdem, ces00$pesa6, useNA = "ifany" )

ces00$satdem2<-Recode(as.numeric(ces00$cpsa8), "1=1; 3=0.75; 5=0.25; 7=0; 98=0.5; else=NA", as.numeric=T)
#checks
# table(ces00$satdem2, ces00$cpsa8, useNA = "ifany" )

#recode Quebec Sovereignty (pesc6) (Quebec only & Right=more sovereignty)
# look_for(ces00, "sovereignty")
ces00$quebec_sov<-Recode(as.numeric(ces00$pesc6), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# table(ces00$quebec_sov, ces00$pesc6, useNA = "ifany" )

# recode provincial alienation (cpsj12)
# look_for(ces00, "treat")
ces00$prov_alienation<-Recode(as.numeric(ces00$cpsj12), "3=1; 1=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces00$prov_alienation, ces00$cpsj12, useNA = "ifany" )

# recode immigration society (mbse3)
# look_for(ces00, "fit")
ces00$immigration_soc<-Recode(as.numeric(ces00$mbse3), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces00$immigration_soc, ces00$mbse3, useNA = "ifany" )

#recode welfare (pesd1b )
# look_for(ces00, "welfare")
ces00$welfare<-Recode(as.numeric(ces00$pesd1b) , "1=0; 5=0.5; 8=0.5; 3=1; else=NA")
#checks
# table(ces00$welfare)
# table(ces00$welfare, ces00$pesd1b )

#recode Postgrad (cpsm3)
# look_for(ces00, "education")
ces00$postgrad<-Recode(as.numeric(ces00$cpsm3), "10:11=1; 1:9=0; else=NA")
#checks
# table(ces00$postgrad)

#recode Break Promise (cpsj13)
# look_for(ces00, "promise")
ces00$promise<-Recode(as.numeric(ces00$cpsj13), "1=0; 3=0.5; 5=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces00$promise)<-c(low=0, high=1)
#checks
val_labels(ces00$promise)
# table(ces00$promise)
# table(ces00$promise, ces00$cpsj13 , useNA = "ifany" )

#recode Trust (pesg1)
# look_for(ces00, "trust")
ces00$trust<-Recode(as.numeric(ces00$pesg1), "1=1; 2=0; else=NA", as.numeric=T)
#val_labels(ces00$trust)<-c(no=0, yes=1)
#checks
val_labels(ces00$trust)
# table(ces00$trust)
# table(ces00$trust, ces00$cpsj13 , useNA = "ifany" )

# recode political interest (cpsb5)
# look_for(ces00, "interest")
ces00$pol_interest<-Recode(as.numeric(ces00$cpsb5), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
# table(ces00$pol_interest, ces00$cpsb5, useNA = "ifany" )

#recode foreign born (cpsm11)
# look_for(ces00, "born")
ces00$foreign<-Recode(as.numeric(ces00$cpsm11), "1=0; 2:97=1; 0=1; else=NA")
#val_labels(ces00$foreign)<-c(No=0, Yes=1)
#checks
#val_labels(ces00$foreign)
# table(ces00$foreign, ces00$cpsm11, useNA="ifany")

#recode duty (mbsc9 )
look_for(ces00, "duty")
ces00$duty<-Recode(ces00$mbsc9 , "1=1; 2:4=0; else=NA")
val_labels(ces00$duty)<-c(No=0, Yes=1)
#checks
val_labels(ces00$duty)
table(ces00$duty, ces00$mbsc9, useNA="ifany")

#### recode Women - how much should be done (cpsc10) ####
look_for(ces00, "women")
table(ces00$cpsc10)
ces00$women<-Recode(as.numeric(ces00$cpsc10), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces00$women,  useNA = "ifany")

#### recode Race - how much should be done (cpsc11) ####
look_for(ces00, "racial")
table(ces00$cpsc11)
ces00$race<-Recode(as.numeric(ces00$cpsc11), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces00$race,  useNA = "ifany")

#recode Previous Vote (cpsk6)
# look_for(ces00, "vote")
ces00$previous_vote<-Recode(ces00$cpsk6, "1=1; 2=2; 3=3; 4=2; 5=4; else=NA")
val_labels(ces00$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces00$previous_vote)
table(ces00$previous_vote)

#recode Previous Vote splitting Conservatives (cpsk6)
# look_for(ces00, "vote")
ces00$previous_vote3<-car::Recode(as.numeric(ces00$cpsk6), "1=1; 2=2; 3=3; 4=6; 5=4; else=NA")
val_labels(ces00$previous_vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces00$previous_vote3)
table(ces00$previous_vote3)

#recode Provincial Vote (pesn5)
# look_for(ces00, "vote")
ces00$prov_vote<-car::Recode(as.numeric(ces00$pesn5), "1=1; 2=2; 3=3; 4:5=6; 6=0; 7=4; 8=8; 9=7; else=NA")
val_labels(ces00$prov_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, PQ=4, Green=5, Reform=6, Sask=7, ADQ=8)
#checks
val_labels(ces00$prov_vote)
table(ces00$prov_vote)

glimpse(ces00)
#Add mode
ces00$mode<-rep("Phone", nrow(ces00))

ces00$election<-rep(2000, nrow(ces00))
# Save the file


save(ces00, file=here("data/ces00.rda"))
