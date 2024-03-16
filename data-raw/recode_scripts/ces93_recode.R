#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces93<-read_sav(file=here("data-raw/CES-E-1993_F1.sav"))

#### gender####
#recode Gender (CPSRGEN)
look_for(ces93, "gender")
ces93$male<-Recode(ces93$CPSRGEN, "1=1; 5=0")
val_labels(ces93$male)<-c(Female=0, Male=1)
#checks
val_labels(ces93$male)
table(ces93$male)

#### union ####
#recode Union Household (CPSJOB6)
look_for(ces93, "union")
class(ces93$CPSJOB6)
ces93$union<-car::Recode(ces93$CPSJOB6, "1=1; 5=0; else=NA")
val_labels(ces93$union)<-c(None=0, Union=1)
#checks
val_labels(ces93$union)
table(ces93$union)

#Union Combined variable (identical copy of union)
ces93$union_both<-ces93$union
#checks
val_labels(ces93$union_both)
table(ces93$union_both)

####education  ####
#recode Education (CPSO3)
look_for(ces93, "education")
ces93 %>%
  mutate(degree=case_when(
    RTYPE4==1 & (CPSO3==9 | REFN2==9)~ 1,
    RTYPE4==1 & (CPSO3==10 | REFN2==10)~ 1,
    RTYPE4==1 & (CPSO3==11 | REFN2==11)~ 1,
    RTYPE4==1 & (CPSO3==1 | REFN2==1)~ 0,
    RTYPE4==1 & (CPSO3==2 | REFN2==2)~ 0,
    RTYPE4==1 & (CPSO3==3 | REFN2==3)~ 0,
    RTYPE4==1 & (CPSO3==4 | REFN2==4)~ 0,
    RTYPE4==1 & (CPSO3==5 | REFN2==5)~ 0,
    RTYPE4==1 & (CPSO3==6 | REFN2==6)~ 0,
    RTYPE4==1 & (CPSO3==7 | REFN2==7)~ 0,
    RTYPE4==1 & (CPSO3==8 | REFN2==8)~ 0,
  ))->ces93

val_labels(ces93$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces93$degree)
table(ces93$degree)

####region  ####
#recode Region (CPSPROV)
look_for(ces93, "province")
ces93$region<-Recode(ces93$CPSPROV, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces93$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces93$region)
table(ces93$region)

#### quebec####
#recode Quebec (CPSPROV)
look_for(ces93, "province")
ces93$quebec<-Recode(ces93$CPSPROV, "0:3=0; 5:9=0; 4=1")
val_labels(ces93$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces93$quebec)
table(ces93$quebec)

#### age####
#recode Age (CPSAGE)
look_for(ces93, "age")
ces93$yob<-Recode(ces93$CPSAGE, "9997:9999=NA")
ces93$age<-1993-ces93$yob
#check
table(ces93$age)

#### religion ####
#recode Religion (CPSO9)
look_for(ces93, "relig")
ces93$religion<-Recode(ces93$CPSO9, "97=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces93$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces93$religion)
table(ces93$religion)

#### language####
#recode Language (PESLANG)
look_for(ces93, "language")

#val_labels(ces93$PESLANG)<-NULL
ces93$language<-Recode(as.character(ces93$PESLANG), "'E'=1; 'F'=0; else=NA")
val_labels(ces93$language)<-c(French=0, English=1)

#checks
val_labels(ces93$language)
table(ces93$language)

#### non-charter language####
#recode Non-charter Language (CPSO16 and REFN16)
look_for(ces93, "language")
ces93 %>%
  mutate(non_charter_language=case_when(
    RTYPE4==1 & (CPSO15==5 | REFN16==5)~ 1,
    RTYPE4==1 & (CPSO15==3 | REFN16==3)~ 0,
    RTYPE4==1 & (CPSO15==1 | REFN16==1)~ 0,
  ))->ces93

val_labels(ces93$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces93$non_charter_language)
table(ces93$non_charter_language)

#### employment####
#recode Employment (CPSJOB1)
look_for(ces93, "employment")

ces93$employment<-Recode(ces93$CPSJOB1, "2:7=0; 1=1; else=NA")
val_labels(ces93$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces93$employment)
table(ces93$employment)

#### sector####
#recode Sector (CPSJOB5 & CPSJOB1)
look_for(ces93, "sector")
ces93 %>%
  mutate(sector=case_when(
    CPSJOB5==3 ~1,
    CPSJOB5==5 ~1,
    CPSJOB5==7 ~1,
    CPSJOB5==1 ~0,
    CPSJOB1> 1 & CPSJOB1< 8 ~ 0,
    CPSJOB5==9 ~NA_real_ ,
    CPSJOB5==8 ~NA_real_ ,
  ))->ces93

val_labels(ces93$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces93$sector)
table(ces93$sector)

####party id ####
#recode Party ID (PESL1)
look_for(ces93, "identification")
ces93$party_id<-Recode(ces93$PESL1, "1=1; 2=2; 3=3; 4=2; 5:6=0; else=NA")
val_labels(ces93$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces93$party_id)
table(ces93$party_id)

#### vote####
#recode Vote (PESA4)
look_for(ces93, "vote")
table(ces93$PESA4)
ces93$vote<-Recode(ces93$PESA4, "1=2; 2=1; 3=3; 5=4; 4=2; 0=0; else=NA")
val_labels(ces93$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces93$vote)
table(ces93$vote)

#recode Vote splitting Conservatives (PESA4)
look_for(ces93, "vote")
ces93$vote3<-car::Recode(as.numeric(ces93$PESA4), "1=2; 2=1; 3=3; 5=4; 4=6; 0=0; else=NA")
val_labels(ces93$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces93$vote3)
table(ces93$vote3)
table(ces93$PESA4)

#### occupation####
#recode Occupation (CPSPINPR)
look_for(ces93, "occupation")
look_for(ces93, "pinporr")
ces93$occupation<-Recode(ces93$CPSPINPR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces93$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces93$occupation)
table(ces93$occupation)

#recode Occupation3 as 6 class schema with self-employed (REFN5)
look_for(ces93, "employ")
ces93$CPSJOB3
ces93$CPSJOB1
ces93$occupation3<-ifelse(ces93$CPSJOB3==1, 6, ces93$occupation)
val_labels(ces93$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces93$occupation3)
table(ces93$occupation3)

####income ####
#recode Income (CPSO18 and CPSO18A)
look_for(ces93, "income")

ces93 %>%
  mutate(income=case_when(
    CPSO18A==1 | CPSO18> 0 & CPSO18 < 20 ~ 1,
    CPSO18A==2 | CPSO18> 19 & CPSO18 < 30 ~ 2,
    CPSO18A==3 | CPSO18> 29 & CPSO18 < 50 ~ 3,
    CPSO18A==4 | CPSO18> 29 & CPSO18 < 50 ~ 3,
    CPSO18A==5 | CPSO18> 49 & CPSO18 < 70 ~ 4,
    CPSO18A==6 | CPSO18> 49 & CPSO18 < 70 ~ 4,
    CPSO18A==7 | CPSO18> 69 & CPSO18 < 998 ~ 5,
    CPSO18A==8 | CPSO18> 69 & CPSO18 < 998 ~ 5,
    CPSO18A==9 | CPSO18> 69 & CPSO18 < 998 ~ 5,
    CPSO18A==10 | CPSO18> 69 & CPSO18 < 998 ~ 5,
  ))->ces93

val_labels(ces93$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

#checks
val_labels(ces93$income)
table(ces93$income)

#Simon's Version
ces93 %>%
  mutate(income2=case_when(
    CPSO18A==1 | CPSO18> 0 & CPSO18 < 18 ~ 1,
    CPSO18A==2 | CPSO18> 17 & CPSO18 < 33 ~ 2,
    CPSO18A==3 | CPSO18> 32 & CPSO18 < 50 ~ 3,
    CPSO18A==4 | CPSO18> 32 & CPSO18 < 50 ~ 3,
    CPSO18A==5 | CPSO18> 49 & CPSO18 < 72 ~ 4,
    CPSO18A==6 | CPSO18> 49 & CPSO18 < 72 ~ 4,
    CPSO18A==7 | CPSO18> 71 & CPSO18 < 998 ~ 5,
    CPSO18A==8 | CPSO18> 71 & CPSO18 < 998 ~ 5,
    CPSO18A==9 | CPSO18> 71 & CPSO18 < 998 ~ 5,
    CPSO18A==10 | CPSO18> 71 & CPSO18 < 998 ~ 5,
  ))->ces93

val_labels(ces93$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

### Tertiles
look_for(ces93, "income")
table(ces93$CPSO18A, ces93$CPSO18)
val_labels(ces93$CPSO18)
ces93 %>%
  mutate(income_tertile=case_when(
    as.numeric(CPSO18A)<3 |(as.numeric(CPSO18)> 0 & as.numeric(CPSO18) < 27) ~ 1,
    #CPSO18A==2 | CPSO18> 26 & CPSO18 < 52 ~ 2,
    (as.numeric(CPSO18A)>2 &as.numeric(CPSO18A) <5 )| as.numeric(CPSO18)> 26 & as.numeric(CPSO18) < 53 ~ 2,
    (as.numeric(CPSO18A)>4 & as.numeric(CPSO18A)<98) | as.numeric(CPSO18)> 52 & as.numeric(CPSO18) <998 ~ 3
  ))->ces93

val_labels(ces93$income_tertile)<-c(Lowest=1, Middle=2, Highest=3)
table(ces93$income_tertile, ces93$CPSO18)
table(ces93$income_tertile, ces93$CPSO18A)

table(ces93$income, ces93$CPSO18A)
table(ces93$income, ces93$CPSO18)
#checks
val_labels(ces93$income)
table(ces93$income)

#####recode Household size (cpshhwgt)####
look_for(ces93, "house")
ces93$household<-Recode(ces93$CPSHHWGT, "0.506=0.5; 1.012=1; 1.517=1.5; 2.023=2; 2.529=2.5; 3.035=3; 3.541=3.5; 4.047=4; 5.058=5")
#checks
table(ces93$household)

#### recode income / household size ####
ces93$inc<-Recode(ces93$CPSO18, "998:999=NA")
table(ces93$inc)
ces93$inc2<-ces93$inc/ces93$household
table(ces93$inc2)
ces93$inc3<-Recode(ces93$CPSO18A, "98:99=NA")
table(ces93$inc3)
ces93$inc4<-ces93$inc3/ces93$household
table(ces93$inc4)

ces93 %>%
  mutate(income_house=case_when(
    as.numeric(inc4)<2.7 |(as.numeric(inc2)> 0 & as.numeric(inc2) < 27) ~ 1,
    (as.numeric(inc4)>2.6 &as.numeric(inc4) <5.3 )| as.numeric(inc2)> 26 & as.numeric(inc2) < 53 ~ 2,
    (as.numeric(inc4)>5.2 & as.numeric(inc4)<31) | as.numeric(inc2)> 52 & as.numeric(inc2) <1100 ~ 3
  ))->ces93
table(ces93$income_house)
table(ces93$income_tertile)
table(ces93$income_tertile, ces93$income_house)

#####recode Religiosity (CPSO10 & REFN10)####
look_for(ces93, "god")
ces93 %>%
  mutate(religiosity=case_when(
    RTYPE4==1 & (CPSO10==1 | REFN10==1)~ 5,
    RTYPE4==1 & (CPSO10==3 | REFN10==3)~ 4,
    RTYPE4==1 & (CPSO10==8 | REFN10==8)~ 3,
    RTYPE4==1 & (CPSO10==5 | REFN10==5)~ 2,
    RTYPE4==1 & (CPSO10==7 | REFN10==7)~ 1,
  ))->ces93
val_labels(ces93$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces93$religiosity)
table(ces93$religiosity)

#### redistribution####
#recode Redistribution (MBSA8)
look_for(ces93, "rich")
val_labels(ces93$MBSA8)
table(ces93$MBSA8, useNA = "ifany")
ces93$redistribution<-Recode(as.numeric(ces93$MBSA8), "; 1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces93$redistribution)
table(ces93$redistribution)

#recode Pro-Redistribution (MBSA8)
ces93$pro_redistribution<-Recode(ces93$MBSA8, "1:2=1; 3:4=0; else=NA", as.numeric=T)
val_labels(ces93$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces93$pro_redistribution)
table(ces93$pro_redistribution)

#recode Market Liberalism (PESE15 and MBSA)
look_for(ces93, "private")
look_for(ces93, "blame")
table(ces93$PESE15, useNA = "ifany")
ces93$market1<-Recode(as.numeric(ces93$PESE15), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
ces93$market2<-Recode(as.numeric(ces93$MBSA2), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces93$market1, ces93$PESE15, useNA = "ifany" )
table(ces93$market2, ces93$MBSA2 , useNA = "ifany" )

#The code below only works if we remove the value labels.
#very annoying; i have filed a bug request

val_labels(ces93$market1)<-NULL
val_labels(ces93$market2)<-NULL
#This does not work for some reason!
# ces93 %>%
#   rowwise() %>%
#   mutate(market_liberalism=mean(
#   c_across(market1:market2))) -> out
#
# out %>%
#   ungroup() %>%
#   select(c('market1', 'market2', 'market_liberalism')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
# cor(out$market_liberalism, ces93$market_liberalism, use="complete.obs")
#Scale Averaging
ces93 %>%
  mutate(market_liberalism=rowMeans(select(., c("market1", "market2")), na.rm=T))->ces93

ces93 %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
qplot(ces93$market_liberalism, geom="histogram")
table(ces93$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces93 %>%
  select(market1, market2) %>%
  psych::alpha(.)
#Check correlation
ces93 %>%
  select(market1, market2) %>%
  cor(., use="complete.obs")

#recode Immigration (CPSG5)
look_for(ces93, "imm")
ces93$immigration_rates<-Recode(ces93$CPSG5, "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces93$immigration_rates, useNA = "ifany")
val_labels(ces93$immigration_rates)<-NULL

#### recode Environment vs Jobs (MBSA12)
look_for(ces93, "env")
table(ces93$MBSA12, useNA = "ifany")
ces93$enviro<-Recode(as.numeric(ces93$MBSA12), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
table(ces93$enviro, useNA = "ifany")

#recode Capital Punishment (CPSG7C)
look_for(ces93, "punish")
table(ces93$CPSG7C, useNA = "ifany")
ces93$death_penalty<-Recode(as.numeric(ces93$CPSG7C), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
table(ces93$death_penalty, useNA = "ifany")

#recode Crime (PESE15B)
look_for(ces93, "crime")
table(ces93$PESE15B)
ces93$crime<-Recode(as.numeric(ces93$PESE15B), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
table(ces93$crime, useNA = "ifany")

#recode Gay Rights (CPSG7B)
look_for(ces93, "homo")
table(ces93$CPSG7B)
ces93$gay_rights<-Recode(as.numeric(ces93$CPSG7B), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
table(ces93$gay_rights,  useNA = "ifany")

#recode Abortion (CPSG6A CPSG6B CPSG6C)
look_for(ces93, "abort")
table(ces93$CPSG6A)
ces93 %>%
  mutate(abortion=case_when(
    CPSG6A==3 ~0,
    CPSG6A==1 ~1,
    CPSG6A==2 ~0.5,
    CPSG6A==8 ~0.5,
    CPSG6B==3 ~1,
    CPSG6B==2 ~0,
    CPSG6B==1 ~0.5,
    CPSG6B==8 ~0.5,
    CPSG6C==2 ~1,
    CPSG6C==1 ~0,
    CPSG6C==3 ~0.5,
    CPSG6C==8 ~0.5,
  ))->ces93
#checks
table(ces93$abortion, useNA = "ifany")

#recode Censorship (MBSA15)
look_for(ces93, "porn")
ces93$censorship<-Recode(as.numeric(ces93$MBSA15), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces93$censorship, useNA = "ifany")

#recode Stay Home (CPSG7A)
look_for(ces93, "home")
table(ces93$CPSG7A)
ces93$stay_home<-Recode(as.numeric(ces93$CPSG7A), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
table(ces93$stay_home,useNA = "ifany")

#recode Marriage Children (CPSG7E)
look_for(ces93, "children")
ces93$marriage_children<-Recode(as.numeric(ces93$CPSG7E), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
table(ces93$marriage_children, useNA = "ifany")

#recode Values (MBSA17)
look_for(ces93, "traditional")
table(ces93$MBSA17)
ces93$values<-Recode(as.numeric(ces93$MBSA17), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces93$values, useNA = "ifany")

#recode Individualism (MBSA7)
look_for(ces93, "individual")
ces93$individualism<-Recode(as.numeric(ces93$MBSA7), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
table(ces93$individualism)

#recode Moral Traditionalism (abortion, censorship, stay home, values, marriage childen, individualism)
ces93$trad3<-ces93$abortion
ces93$trad6<-ces93$censorship
ces93$trad1<-ces93$stay_home
ces93$trad4<-ces93$values
ces93$trad5<-ces93$marriage_children
#ces93$trad6<-ces93$individualism
ces93$trad2<-ces93$gay_rights
table(ces93$trad1, useNA = "ifany")
table(ces93$trad2, useNA = "ifany")
table(ces93$trad3, useNA = "ifany")
table(ces93$trad4, useNA = "ifany")
table(ces93$trad5, useNA = "ifany")
table(ces93$trad6, useNA = "ifany")

#Remove value labels
#Start with the data frame
ces93 %>%
  #Mutate at is a way of transforming variables *in place*.
  #Using the vars() argument inside mutate_at lets you select which variables you want to transform
  #then you provide the transformation
  #In this case, we are selecting trad1 through trad6 and providing the function remove_value_labels
  #Then resaving the data frame back as ces93
  mutate(across(num_range('trad', 1:6), remove_val_labels))->ces93

# ces93 %>%
#   rowwise() %>%
#   mutate(traditionalism=mean(
#     c_across(trad1:trad6)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('trad1', 'trad2', 'trad3', 'trad4', 'trad5', 'trad6', 'traditionalism')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces93 %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", range=1:6)), na.rm=T))->ces93

ces93 %>%
  select(starts_with("trad")) %>%
  summary()
#Check distribution of traditionalism
#qplot(ces93$traditionalism, geom="histogram")
table(ces93$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces93 %>%
  select(trad1, trad2, trad3, trad4, trad5, trad6) %>%
  psych::alpha(.)
#Check correlation
ces93 %>%
  select(trad1, trad2, trad3, trad4, trad5, trad6) %>%
  cor(., use="complete.obs")

#recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)
names(ces93)
ces93 %>%
  mutate(traditionalism2=rowMeans(select(., c('trad1', 'trad2')), na.rm=T))->ces93
summary(ces93$traditionalism2)
summary(ces93$trad1)
summary(ces93$trad2)
sum(is.na(ces93$trad1))
sum(!is.na(ces93$trad1))
sum(is.na(ces93$trad2))
sum(!is.na(ces93$trad2))
sum(is.na(ces93$traditionalism2))
sum(!is.na(ces93$traditionalism2))

#Check distribution of traditionalism
qplot(ces93$traditionalism2, geom="histogram")
table(ces93$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces93 %>%
  select(trad1, trad2) %>%
  psych::alpha(.)

#Check correlation
ces93 %>%
  select(trad1, trad2) %>%
  cor(., use="complete.obs")

#recode 2nd Dimension (stay home, immigration, gay rights, crime)
ces93$author1<-ces93$stay_home
ces93$author2<-ces93$immigration_rates
ces93$author3<-ces93$gay_rights
ces93$author4<-ces93$crime
table(ces93$author1)
table(ces93$author2)
table(ces93$author3)
table(ces93$author4)

#Remove value labels
ces93 %>%
  mutate(across(num_range('author', 1:4), remove_val_labels))->ces93

# ces93 %>%
#   rowwise() %>%
#   mutate(authoritarianism=mean(
#     c_across(author1:author4)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('author1', 'author2', 'author3', 'author4', 'authoritarianism')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces93 %>%
  mutate(authoritarianism=rowMeans(select(., num_range("author", 1:4)), na.rm=T))->ces93

ces93 %>%
  select(starts_with("author")) %>%
  summary()
#Check distribution of traditionalism
#qplot(ces93$authoritarianism, geom="histogram")
table(ces93$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
ces93 %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces93 %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")

#recode Quebec Accommodation (PRC4 & REFE10) (Left=more accom)
look_for(ces93, "quebec")
ces93$quebec_accom<-Recode(as.numeric(ces93$PRC4), "1=1; 5=0; 3=0.5; 8=0.5; else=NA")
#checks
table(ces93$quebec_accom)

#recode Liberal leader (PESD2B)
look_for(ces93, "Chretien")
ces93$liberal_leader<-Recode(as.numeric(ces93$PESD2B), "0=1; 997:999=NA")
#checks
table(ces93$liberal_leader)

#recode conservative leader (PESD2A)
look_for(ces93, "Campbell")
ces93$conservative_leader<-Recode(as.numeric(ces93$PESD2A), "0=1; 997:999=NA")
#checks
table(ces93$conservative_leader)

#recode NDP leader (PESD2C)
look_for(ces93, "McLaughlin")
ces93$ndp_leader<-Recode(as.numeric(ces93$PESD2C), "0=1; 997:999=NA")
#checks
table(ces93$ndp_leader)

#recode Bloc leader (PESD2E)
look_for(ces93, "Bouchard")
ces93$bloc_leader<-Recode(as.numeric(ces93$PESD2E), "0=1; 997:999=NA")
#checks
table(ces93$bloc_leader)

#recode liberal rating (PESD2H)
look_for(ces93, "liberal")
ces93$liberal_rating<-Recode(as.numeric(ces93$PESD2H), "0=1; 997:999=NA")
#checks
table(ces93$liberal_rating)

#recode conservative rating (PESD2G)
look_for(ces93, "conservative")
ces93$conservative_rating<-Recode(as.numeric(ces93$PESD2G), "0=1; 997:999=NA")
#checks
table(ces93$conservative_rating)

#recode NDP rating (PESD2I)
look_for(ces93, "new democratic")
ces93$ndp_rating<-Recode(as.numeric(ces93$PESD2I), "0=1; 997:999=NA")
#checks
table(ces93$ndp_rating)

#recode Bloc rating (PESD2K)
look_for(ces93, "new democratic")
ces93$bloc_rating<-Recode(as.numeric(ces93$PESD2K), "0=1; 997:999=NA")
#checks
table(ces93$bloc_rating)

#recode Education (CPSL7F)
look_for(ces93, "edu")
ces93$education<-Recode(as.numeric(ces93$CPSL7F), "1=0; 3=0.5; 5=1; 8=0.5; else=NA")
#checks
table(ces93$education, ces93$CPSL7F , useNA = "ifany" )

#recode Personal Retrospective (CPSC1)
look_for(ces93, "financ")
ces93$personal_retrospective<-Recode(as.numeric(ces93$CPSC1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces93$personal_retrospective)
table(ces93$personal_retrospective, ces93$CPSC1 , useNA = "ifany" )

#recode National Retrospective (CPSH1)
look_for(ces93, "economy")
ces93$national_retrospective<-Recode(as.numeric(ces93$CPSH1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces93$national_retrospective)
table(ces93$national_retrospective, ces93$CPSH1 , useNA = "ifany" )

#recode Ideology (REFH211)
look_for(ces93, "self")
ces93$ideology<-Recode(as.numeric(ces93$REFH21) , "1=0; 3=1; 5=0.5; else=NA")
#val_labels(ces93$ideology)<-c(Left=0, Right=1)
#checks
#val_labels(ces93$ideology)
table(ces93$ideology, ces93$REFH21  , useNA = "ifany")

#recode turnout (PESA2)
look_for(ces93, "vote")
ces93$turnout<-Recode(ces93$PESA2, "1=1; 5=0; 8=0; else=NA")
val_labels(ces93$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces93$turnout)
table(ces93$turnout)
table(ces93$turnout, ces93$vote)

#### recode political efficacy ####
#recode No Say (MBSD8)
look_for(ces93, "have any say")
ces93$efficacy_internal<-Recode(as.numeric(ces93$MBSD8), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$efficacy_internal)<-c(low=0, high=1)
#val_labels(ces93$efficacy_internal)<-NULL

#checks
val_labels(ces93$efficacy_internal)
table(ces93$efficacy_internal)
table(ces93$efficacy_internal, ces93$MBSD8 , useNA = "ifany" )

#recode MPs lose touch (MBSD1)
look_for(ces93, "touch")
ces93$efficacy_external<-Recode(as.numeric(ces93$MBSD1), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces93$efficacy_external)<-NULL
table(ces93$efficacy_external)
table(ces93$efficacy_external, ces93$MBSD1 , useNA = "ifany" )

#recode Official Don't Care (MBSD5)
look_for(ces93, "cares what")
ces93$efficacy_external2<-Recode(as.numeric(ces93$MBSD5), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$efficacy_external2)<-c(low=0, high=1)
#val_labels(ces93$efficacy_external2)<-NULL

#checks
#val_labels(ces93$efficacy_external2)
table(ces93$efficacy_external2)
table(ces93$efficacy_external2, ces93$MBSD5 , useNA = "ifany" )

ces93 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces93

ces93 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
#qplot(ces93$political_efficacy, geom="histogram")
table(ces93$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces93 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  psych::alpha(.)
#Check correlation
ces93 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  cor(., use="complete.obs")

#recode Most Important Question (CPSA1)
look_for(ces93, "issue")
ces93$mip<-Recode(ces93$CPSA1, "0=0; 1:4=16; 5=0; 6:7=16; 8=11; 9=16; 10:14=6; 20:30=7; 31=18; 32:34=7; 35=0; 36=7; 40:43=10;
				                        44=13; 45=15; 46=12; 50:55=9; 60:64=15; 66:67=4; 68=15; 69=8; 70:71=14;
				                        72=1; 73=14; 76:79=2; 80:86=17; 90=0; 91=3; 92:93=11; 94:95=0; 96=16; 97=0; else=NA")
val_labels(ces93$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18, Housing=19)
table(ces93$mip)

# recode satisfaction with democracy (PESL5)
look_for(ces93, "dem")
ces93$satdem<-Recode(as.numeric(ces93$PESL5), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$satdem)<-NULL
#checks
table(ces93$satdem, ces93$PESL5, useNA = "ifany" )

ces93$satdem2<-Recode(as.numeric(ces93$PESL5), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$satdem2)<-NULL
#checks
table(ces93$satdem2, ces93$PESL5, useNA = "ifany" )

#recode Quebec Sovereignty (CPSG11) (Quebec only & Right=more sovereignty)
look_for(ces93, "sovereignty")
ces93$quebec_sov<-Recode(as.numeric(ces93$CPSG11), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#val_labels(ces93$quebec_sov)<-NULL
#checks
table(ces93$quebec_sov, ces93$CPSG11, useNA = "ifany" )

# recode immigration society (MBSG6)
look_for(ces93, "fit")
ces93$immigration_soc<-Recode(as.numeric(ces93$MBSG6), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces93$immigration_soc)<-NULL

#checks
table(ces93$immigration_soc, ces93$MBSG6, useNA = "ifany" )

#recode welfare (CPSL7B)
look_for(ces93, "welfare")
ces93$welfare<-Recode(as.numeric(ces93$CPSL7B), "1=1; 3=0.5; 8=0.5; 5=0; else=NA")
#val_labels(ces93$welfare)<-NULL
#checks
table(ces93$welfare)
table(ces93$welfare, ces93$CPSL7B)

#recode Education (CPSO3)
look_for(ces93, "education")
ces93 %>%
  mutate(postgrad=case_when(
    RTYPE4==1 & (CPSO3==9 | REFN2==9)~ 0,
    RTYPE4==1 & (CPSO3==10 | REFN2==10)~ 1,
    RTYPE4==1 & (CPSO3==11 | REFN2==11)~ 1,
    RTYPE4==1 & (CPSO3==1 | REFN2==1)~ 0,
    RTYPE4==1 & (CPSO3==2 | REFN2==2)~ 0,
    RTYPE4==1 & (CPSO3==3 | REFN2==3)~ 0,
    RTYPE4==1 & (CPSO3==4 | REFN2==4)~ 0,
    RTYPE4==1 & (CPSO3==5 | REFN2==5)~ 0,
    RTYPE4==1 & (CPSO3==6 | REFN2==6)~ 0,
    RTYPE4==1 & (CPSO3==7 | REFN2==7)~ 0,
    RTYPE4==1 & (CPSO3==8 | REFN2==8)~ 0,
  ))->ces93
#checks
table(ces93$postgrad)

# recode political interest (CPSB1)
look_for(ces93, "interest")
ces93$pol_interest<-Recode(as.numeric(ces93$CPSB1), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
table(ces93$pol_interest, ces93$CPSB1, useNA = "ifany" )
#val_labels(ces93$pol_interest)<-NULL

#recode foreign born (CPSO11)
look_for(ces93, "birth")
ces93$foreign<-Recode(ces93$CPSO11, "1=0; 2:22=1; 0=1; else=NA")
val_labels(ces93$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces93$foreign)
table(ces93$foreign, ces93$CPSO11, useNA="ifany")

#### recode Women - how much should be done (CPSK2A) ####
look_for(ces93, "women")
table(ces93$CPSK2A)
ces93$women<-Recode(as.numeric(ces93$CPSK2A), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces93$women,  useNA = "ifany")

#### recode Race - how much should be done (CPSK3A) ####
look_for(ces93, "racial")
table(ces93$CPSK3A)
ces93$race<-Recode(as.numeric(ces93$CPSK3A), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces93$race,  useNA = "ifany")

#recode Previous Vote (CPSM6)
# look_for(ces93, "vote")
ces93$previous_vote<-Recode(ces93$CPSM6, "1=1; 2=2; 3=3; 4=2; 5=0; else=NA")
val_labels(ces93$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces93$previous_vote)
table(ces93$previous_vote)

#recode Previous Vote splitting Conservatives (CPSM6)
look_for(ces93, "vote")
ces93$previous_vote3<-car::Recode(as.numeric(ces93$CPSM6), "1=1; 2=2; 3=3; 4=6; 5=0; else=NA")
val_labels(ces93$previous_vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces93$previous_vote3)
table(ces93$previous_vote3)

glimpse(ces93)
lookfor(ces93, "election")
lookfor(ces93, "referendum")
ces93$RTYPE3
table(as_factor(ces93$CESTYPE), as_factor(ces93$RTYPE4), useNA = "ifany")
var_label(ces93$RTYPE4)
table(as_factor(ces93$RTYPE4), useNA = "ifany")
table(as_factor(ces93$CESTYPE), as_factor(ces93$RTYPE3), useNA = "ifany")
table(as_factor(ces93$RTYPE1), as_factor(ces93$RTYPE3), useNA = "ifany")
table(as_factor(ces93$RTYPE2), as_factor(ces93$RTYPE3), useNA = "ifany")

### Filter out ces93 referendum respondents only by removing missing values from RTYPE4 (indicates ces93 respondents)
ces93 %>%
  filter(is.na(ces93$RTYPE4)) %>%
  select(contains("RTYPE"), CESTYPE) %>%
  as_factor()
nrow(ces93)

lookfor(ces93)
#Add mode
ces93$mode<-rep("Phone", nrow(ces93))
#Add Election
# Note: Care should be taken in the master file to consider whether or not the user
# Wants to include Rs from the 1992 referendum or not.
#
ces93$election<-rep(1993, nrow(ces93))
# Save the file
save(ces93, file=here("data/ces93.rda"))
