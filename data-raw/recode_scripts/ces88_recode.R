#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces88<-read_sav(file=here("data-raw/CES1988.sav"))


#recode Gender (rsex)
# look_for(ces88, "sex")
ces88$male<-Recode(ces88$rsex, "1=1; 5=0")
val_labels(ces88$male)<-c(Female=0, Male=1)
#checks
val_labels(ces88$male)
# table(ces88$male)

#recode Union Respondent (n9)
# look_for(ces88, "union")
ces88$union<-Recode(ces88$n9, "1=1; 5=0; else=NA")
val_labels(ces88$union)<-c(None=0, Union=1)
#checks
val_labels(ces88$union)
# table(ces88$union)

#Union Combined variable (identical copy of union) ### Respondent only
ces88$union_both<-ces88$union
#checks
val_labels(ces88$union_both)
# table(ces88$union_both)

#recode Education (n3)
# look_for(ces88, "education")
ces88$degree<-Recode(ces88$n3, "9:11=1; 1:8=0; else=NA")
val_labels(ces88$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces88$degree)
# table(ces88$degree)

#recode Region (province)
# look_for(ces88, "province")
ces88$region<-Recode(ces88$province, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces88$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces88$region)
# table(ces88$region)

#recode Province (province)
# look_for(ces88, "province")
ces88$prov<-Recode(ces88$province, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; 7=8; 8=9; 9=10")
val_labels(ces88$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces88$prov)
table(ces88$prov)

#recode Quebec (province)
# look_for(ces88, "province")
ces88$quebec<-Recode(ces88$province, "0:3=0; 5:9=0; 4=1")
val_labels(ces88$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces88$quebec)
# table(ces88$quebec)

#recode Age (xn1)
# look_for(ces88, "birth")
ces88$yob<-Recode(ces88$xn1, "9998:9999=NA")
ces88$age<-1988-ces88$yob
#check
# table(ces88$age)

#recode Religion (n11)
# look_for(ces88, "relig")
ces88$religion<-Recode(ces88$n11, "7=0; 2=1; 1=2; 8:9=NA; else=3")
val_labels(ces88$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces88$religion)
# table(ces88$religion)

#recode Language (intlang)
# look_for(ces88, "language")
ces88$language<-Recode(ces88$intlang, "1=1; 2=0; else=NA")
val_labels(ces88$language)<-c(French=0, English=1)
#checks
val_labels(ces88$language)
# table(ces88$language)

#recode Non-charter Language (n16)
# look_for(ces88, "language")
ces88$non_charter_language<-Recode(ces88$n16, "1:3=0; 5=1; else=NA")
val_labels(ces88$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces88$non_charter_language)
# table(ces88$non_charter_language)

#recode Employment (n5)
# look_for(ces88, "employment")
ces88$employment<-Recode(ces88$n5, "2:7=0; 1=1; else=NA")
val_labels(ces88$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces88$employment)
# table(ces88$employment)

#recode Unemployed (n5)
# look_for(ces88, "employment")
ces88$unemployed<-Recode(ces88$n5, "1=0; 2:3=1; else=NA")
val_labels(ces88$unemployed)<-c(Employed=0, Unemployed=1)
#checks
val_labels(ces88$unemployed)
table(ces88$unemployed)

#recode Sector (n8 & n5)
# look_for(ces88, "sector")
# look_for(ces88, "firm")
ces88 %>%
  mutate(sector=case_when(
    n8==3 ~1,
    n8==5 ~1,
    n8==1 ~0,
    n5> 1 & n5< 8 ~ 0,
    n8==9 ~NA_real_ ,
    n8==8 ~NA_real_ ,
  ))->ces88

val_labels(ces88$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces88$sector)
# table(ces88$sector)

#recode Party ID (i1)
# look_for(ces88, "identification")
#ces88$party_id<-Recode(ces88$i1, "1=1; 2=2; 3=3; else=NA")
ces88 %>%
  mutate(party_id=case_when(
    xl4==1 | xl7==1 ~ 1,
    xl4==2 | xl7==2 ~ 2,
    xl4==3 | xl7==3 ~ 3,
  ))->ces88
val_labels(ces88$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces88$party_id)
table(ces88$party_id)

#recode Party ID 2 (i1)
# look_for(ces88, "identification")
#ces88$party_id2<-Recode(ces88$i1, "1=1; 2=2; 3=3; else=NA")
ces88 %>%
  mutate(party_id2=case_when(
    xl4==1 | xl7==1 ~ 1,
    xl4==2 | xl7==2 ~ 2,
    xl4==3 | xl7==3 ~ 3,
  ))->ces88
val_labels(ces88$party_id2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces88$party_id2)
table(ces88$party_id2)

#recode Party ID 3 (xl4, xl7)
# look_for(ces88, "identification")
#ces88$party_id3<-Recode(ces88$xl4, "1=1; 2=2; 3=3; else=NA")
ces88 %>%
  mutate(party_id3=case_when(
    i1==1 | i4==1 ~ 1,
    i1==2 | i4==2 ~ 2,
    i1==3 | i4==3 ~ 3,
  ))->ces88
val_labels(ces88$party_id3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces88$party_id3)
table(ces88$party_id3)

#recode Party closeness (i2 )
look_for(ces88, "strongly")
ces88$party_close<-Recode(ces88$i2 , "1=1; 3=0.5; 5=0; else=NA")
#checks
table(ces88$i2  , ces88$party_close, useNA = "ifany" )

#recode Vote (xb2)
# look_for(ces88, "vote")
ces88$vote<-car::Recode(as.numeric(ces88$xb2), "2=1; 1=2; 3=3; 4=2; 5=0; else=NA")
val_labels(ces88$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces88$vote)
# table(ces88$vote)
# table(ces88$xb2)

#recode Vote splitting Conservatives (xb2)
# look_for(ces88, "vote")
ces88$vote3<-car::Recode(as.numeric(ces88$xb2), "2=1; 1=2; 3=3; 4=6; 5=0; else=NA")
val_labels(ces88$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces88$vote3)
# table(ces88$vote3)
# table(ces88$xb2)

#recode Occupation (pinpor81)
# look_for(ces88, "occupation")
ces88$occupation<-Recode(ces88$pinpor81, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces88$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces88$occupation)
# table(ces88$occupation)

#recode Occupation3 as 6 class schema with self-employed (n7)
# look_for(ces88, "employed")
ces88$occupation3<-ifelse(ces88$n7==1, 6, ces88$occupation)
val_labels(ces88$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces88$occupation3)
# table(ces88$occupation3)

#recode Income (n19)
# look_for(ces88, "income")
ces88$income<-Recode(ces88$n19, "1:2=1; 3=2; 4=3; 5:6=4; 7:9=5; else=NA")
val_labels(ces88$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces88$income)
# table(ces88$income)
#Simon's Version
# look_for(ces88, "income")
ces88$income2<-Recode(ces88$n19, "1=1; 2=2; 3:4=3; 5=4; 6:9=5; else=NA")
val_labels(ces88$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces88$income)
# table(ces88$income)

#recode Income (n19)
# look_for(ces88, "income")
ces88$income_tertile<-Recode(ces88$n19, "1:2=1; 3:5=2; 6:9=3; else=NA")
val_labels(ces88$income_tertile)<-c(Lowest=1,Middle=2, Highest=3)
#checks
val_labels(ces88$income_tertile)
# table(ces88$income_tertile)
#recode Religiosity (n12)
# look_for(ces88, "worship")
ces88$religiosity<-Recode(ces88$n12, "0=1; 1:3=2; 4=3; 5:6=4; 7:8=5; else=NA")
val_labels(ces88$religiosity)<-c(Lowest=1, Lower_Middle=2, MIddle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces88$religiosity)
# table(ces88$religiosity)

#recode Market Liberalism (qc13 and qc2) (Left-Right)
# look_for(ces88, "profit")
# look_for(ces88, "regulation")
ces88$qc13
ces88$qc2
ces88$market1<-Recode(as.numeric(ces88$qc13), "1=1; 2=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)
ces88$market2<-Recode(as.numeric(ces88$qc2), "1=1; 2=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)


#checks
# table(ces88$market1)
# table(ces88$market2)

# ces88 %>%
#   rowwise() %>%
#   mutate(market_liberalism=mean(
#     c_across(market1:market2)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('market1', 'market2', 'market_liberalism')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces88 %>%
  mutate(market_liberalism=rowMeans(select(., matches('market[0-9]')), na.rm=T))->ces88


#cor(ces88$market_liberalism, ces88$market_liberalism2, use="complete.obs")
ces88 %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
#qplot(ces88$market_liberalism, geom="histogram")
# table(ces88$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
# library(psych)
# ces88 %>%
#   select(market1, market2) %>%
#   psych::alpha(.)
# #Check correlation
# ces88 %>%
#   select(market1, market2) %>%
#   cor(., use="complete.obs")

#recode Redistribution (xk2 and qc16) (Right-Left)
# look_for(ces88, "wealth")
# look_for(ces88, "poor")
ces88$xk2
# table(ces88$xk2, useNA = "ifany")
# table(ces88$qc16, useNA="ifany")
#val_labels(ces88$xk2)
ces88$redistro1<-Recode(as.numeric(ces88$xk2), "1=1; 3=0.5; 5=0; 8=0.5; else=NA", as.numeric=T)
ces88$redistro2<-Recode(as.numeric(ces88$qc16), "1=1; 2=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)

#checks
# table(ces88$redistro1, ces88$xk2)
# table(ces88$redistro2, ces88$qc16)
#
# ces88 %>%
#   rowwise() %>%
#   mutate(redistribution=mean(
#     c_across(redistro1:redistro2)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('redistro1', 'redistro2', 'redistribution')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces88 %>%
  select(starts_with("redistro"))
ces88 %>%
  mutate(redistribution=rowMeans(select(., starts_with("redistro")), na.rm=T))->ces88


ces88 %>%
  select(starts_with("redistr")) %>%
  summary()
#Check distribution of market_liberalism
#qplot(ces88$redistribution, geom="histogram")
# table(ces88$redistribution, useNA="ifany")

# #Calculate Cronbach's alpha
# ces88 %>%
#   select(redistro1, redistro2) %>%
#   psych::alpha(.)
# #Check correlation
# ces88 %>%
#   select(redistro1, redistro2) %>%
#   cor(., use="complete.obs")

#recode Pro-Redistribution
ces88$pro_redistribution<-Recode(ces88$redistribution, "0.75=1; 1=1; 0:0.5=0; else=NA", as.numeric=T)
val_labels(ces88$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces88$pro_redistribution)
# table(ces88$pro_redistribution)

#recode Immigration (l5, qf2, gf10) (Left-Right)
# look_for(ces88, "imm")
ces88$l5
ces88$immigration_rates<-Recode(as.numeric(ces88$l5), "1=0; 3=0.5; 5=1; 8=0.5; else=NA", as.numeric=T)
ces88$immigration_better<-Recode(as.numeric(ces88$qf2), "1=0; 2=1; 8=0.5; else=NA", as.numeric=T)
ces88$immigration_encourage<-Recode(as.numeric(ces88$qf10), "1=0; 2=1; 8=0.5; else=NA", as.numeric=T)
#checks
# table(ces88$immigration_rates, ces88$l5 , useNA = "ifany" )
# table(ces88$immigration_better, ces88$qf2 , useNA = "ifany" )
# table(ces88$immigration_encourage, ces88$qf10 , useNA = "ifany" )
#
# ces88 %>%
#   rowwise() %>%
#   mutate(immigration=mean(
#     c_across(immigration_rates:immigration_encourage)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('immigration_rates', 'immigration_better', 'immigration_encourage', 'immigration')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces88 %>%
  mutate(immigration=rowMeans(select(., c('immigration_rates', 'immigration_better', 'immigration_encourage')), na.rm=T))->ces88


# ces88 %>%
#   select(starts_with("immigration")) %>%
#   summary()
#Check distribution of immigration
# qplot(ces88$immigration, geom="histogram")
# table(ces88$immigration, useNA="ifany")

#Calculate Cronbach's alpha
# ces88 %>%
#   select(immigration_rates, immigration_better, immigration_encourage) %>%
#   psych::alpha(.)
# #Check correlation
# ces88 %>%
#   select(immigration_rates, immigration_better, immigration_encourage) %>%
#   cor(., use="complete.obs")

#### recode Environment vs Jobs (qf9) (Left-Right)
# look_for(ces88, "env")
ces88$enviro<-Recode(as.numeric(ces88$qf9), "1=0; 2=1; 8=0.5; else=NA")
#checks
# table(ces88$enviro, ces88$qf9) #No one had 8

#recode Capital Punishment (qf1) (Left-Right)
# look_for(ces88, "punish")
ces88$qf1
ces88$death_penalty<-Recode(as.numeric(ces88$qf1), "1=0; 2=1; 8=0.5; else=NA")
#checks
# table(ces88$death_penalty, ces88$qf1)

#recode Crime (qh11) (Left-Right)
# look_for(ces88, "crime")
remotes::install_github('sjkiss/skpersonal')
library(skpersonal)

#ces88$crime<-skpersonal::revScale(as.numeric(ces88$qh11), reverse=T)
#First reverse code item so that 12 is most important and 1 is least important
ces88$crime<-Recode(as.numeric(ces88$qh11), "12=1;
       11=2;
       10=3;
       9=4;
       8=5;
       7=6;
       6=7;
       5=8;
       4=9;
       3=10;
       2=11;
       1=12", as.numeric=T)
#Then scale to 0 and 1

ces88$crime<-scales::rescale(as.numeric(ces88$crime))
summary(ces88$crime)
# table(ces88$crime, ces88$qh11 , useNA = "ifany" )
summary(ces88$qh11)
#ces88$crime<-Recode(ces88$qh11, "1=1; 2=0.9; 3=0.8; 4=0.7; 5=0.6; 6=0.5; 7=0.4; 8=0.3; 9=0.2; 10=0.1; 11:12=0; else=NA")

#checks
# table(ces88$crime)

#recode Gay Rights (qe9) (Left-Right)
# look_for(ces88, "homo")
ces88$gay_rights<-Recode(as.numeric(ces88$qe9), "1=1; 2=0; 3=0.5; 8=0.5; else=NA")
#checks
# table(ces88$gay_rights)

#recode Abortion (l6a and l6b) (Left-Right)
# look_for(ces88, "abort")
ces88 %>%
  mutate(abortion=case_when(
    l6a==5 ~0,
    l6a==1 ~1,
    l6a==3 ~0.5,
    l6b==5 ~1,
    l6b==1 ~0,
    l6b==3 ~0.5,
  ))->ces88
#checks
# table(ces88$abortion)

#recode Censorship (qa10) (Left-Right)
# look_for(ces88, "porn")
ces88$censorship<-Recode(as.numeric(ces88$qa10), "1=1; 2=0; 3=0.5; 8=0.5; else=NA")
#checks
# table(ces88$censorship)

#recode Moral Traditionalism (abortion & censorship) (Left-Right)
ces88$trad1<-ces88$abortion
ces88$trad3<-ces88$censorship
ces88$trad2<-ces88$gay_rights
# table(ces88$trad1)
# table(ces88$trad2)
# table(ces88$trad3)

#Scale Averaging

ces88 %>%
  mutate(traditionalism=rowMeans(select(., c('trad1', 'trad2', 'trad3')), na.rm=T))->ces88

# ces88 %>%
#   select(starts_with("trad")) %>%
#   summary()
#Check distribution of traditionalism
# qplot(ces88$traditionalism, geom="histogram")
# table(ces88$traditionalism, useNA="ifany")

# #Calculate Cronbach's alpha
# ces88 %>%
#   select(trad1, trad2, trad3) %>%
#   psych::alpha(.)
# #Check correlation
# ces88 %>%
#   select(trad1, trad2, trad3) %>%
#   cor(., use="complete.obs")


# #recode Moral Traditionalism 2 (abortion & gay rights) (Left-Right)
# ces88 %>%
#   rowwise() %>%
#   mutate(traditionalism2=mean(
#     c_across(trad1:trad2)
#     , na.rm=T )) -> out
# out %>%
#   ungroup() %>%
#   select(c('trad1', 'trad2', 'traditionalism2')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces88 %>%
  # rowwise() %>%
  mutate(traditionalism2=rowMeans(select(., c('trad1', 'trad2')), na.rm=T)) ->ces88

ces88 %>%
  select(starts_with("trad")) %>%
  summary()
#Check distribution of traditionalism
#qplot(ces88$traditionalism2, geom="histogram")
# table(ces88$traditionalism2, useNA="ifany")

# #Calculate Cronbach's alpha
# ces88 %>%
#   select(trad1, trad2) %>%
#   psych::alpha(.)
#
# #Check correlation
# ces88 %>%
#   select(trad1, trad2) %>%
#   cor(., use="complete.obs")

#recode 2nd Dimension (censorship, immigration, gay rights, crime)
ces88$author1<-ces88$censorship
ces88$author2<-ces88$immigration_rates
ces88$author3<-ces88$gay_rights
ces88$author4<-ces88$crime
# table(ces88$author1)
# table(ces88$author2)
# table(ces88$author3)
# table(ces88$author4)

# ces88 %>%
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
ces88 %>%
  mutate(authoritarianism=rowMeans(select(., c('author1', 'author2', 'author3', 'author4')), na.rm=T))->ces88

ces88 %>%
  select(starts_with("author")) %>%
  summary()
#Check distribution of traditionalism
#qplot(ces88$authoritarianism, geom="histogram")
# table(ces88$authoritarianism, useNA="ifany")

# #Calculate Cronbach's alpha
# ces88 %>%
#   select(author1, author2, author3, author4) %>%
#   psych::alpha(.)
#
# #Check correlation
# ces88 %>%
#   select(author1, author2, author3, author4) %>%
#   cor(., use="complete.obs")

#recode Quebec Accommodation (qa9) (Left=more accom)
# look_for(ces88, "quebec")
ces88$quebec_accom<-Recode(as.numeric(ces88$qa9), "1=1; 2=0; 3=0.5; 8=0.5; else=NA")
#checks
# table(ces88$quebec_accom)

#recode Quebec Sovereignty (b10) (Quebec only & Right=more sovereignty)
look_for(ces88, "independence")
ces88$quebec_sov<-Recode(as.numeric(ces88$b10), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#val_labels(ces88$quebec_sov)<-NULL
#checks
table(ces88$quebec_sov, ces88$b10, useNA = "ifany" )

#recode Liberal leader (xe2b)
# look_for(ces88, "Turner")
ces88$liberal_leader<-Recode(as.numeric(ces88$xe2b), "0=1; 997:999=NA")
#checks
# table(ces88$liberal_leader)

#recode conservative leader (xe2a)
# look_for(ces88, "Mulroney")
ces88$conservative_leader<-Recode(as.numeric(ces88$xe2a), "0=1; 997:999=NA")
#checks
# table(ces88$conservative_leader)

#recode NDP leader (xe2c)
# look_for(ces88, "Broadbent")
ces88$ndp_leader<-Recode(as.numeric(ces88$xe2c), "0=1; 997:999=NA")
#checks
# table(ces88$ndp_leader)

#recode liberal rating (xe2e)
# look_for(ces88, "liberal")
ces88$liberal_rating<-Recode(as.numeric(ces88$xe2e), "0=1; 997:999=NA")
#checks
# table(ces88$liberal_rating)

#recode conservative rating (xe2d)
# look_for(ces88, "conservative")
ces88$conservative_rating<-Recode(as.numeric(ces88$xe2d), "0=1; 997:999=NA")
#checks
# table(ces88$conservative_rating)

#recode NDP rating (xe2f)
# look_for(ces88, "new democratic")
ces88$ndp_rating<-Recode(as.numeric(ces88$xe2f), "0=1; 997:999=NA")
#checks
# table(ces88$ndp_rating)

#recode Education (qb1)
# look_for(ces88, "edu")
ces88$education<-Recode(as.numeric(ces88$qb1), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# table(ces88$education, ces88$qb1 , useNA = "ifany" )

#recode Personal Retrospective (c1)
# look_for(ces88, "financ")
ces88$personal_retrospective<-Recode(as.numeric(ces88$c1), "1=1; 5=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces88$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces88$personal_retrospective)
# table(ces88$personal_retrospective, ces88$c1 , useNA = "ifany" )

#recode National Retrospective (g1)
# look_for(ces88, "economy")
ces88$national_retrospective<-Recode(as.numeric(ces88$g1), "1=1; 5=0; 3=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces88$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces88$national_retrospective)
# table(ces88$national_retrospective, ces88$g1 , useNA = "ifany" )

#recode Ideology (h5a)
# look_for(ces88, "self")
ces88$ideology<-Recode(as.numeric(ces88$h5a) , "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#val_labels(ces88$ideology)<-c(Left=0, Right=1)
#checks

# table(ces88$ideology, ces88$h5a  , useNA = "ifany")

#recode turnout (xb1)
# look_for(ces88, "vote")
ces88$turnout<-Recode(ces88$xb1, "1=1; 5=0;  8=0; else=NA")
val_labels(ces88$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces88$turnout)
# table(ces88$turnout)
# table(ces88$turnout, ces88$vote)

#recode Most Important Question (l1a)
# look_for(ces88, "issue")
ces88$l1a
ces88$mip<-Recode(ces88$l1a, "1=14; 2=10; 3=12; 4=15; 5=12; 6=10; 7=7; 8=6; 9=5; 10=1; 11=0; 12=17; 13=8; 14=19;
				                      15=13; 16=18; 17=0; 18=16; 19=12; 20=16; 21=3; 22=7; 23:24=15; 25=16; 26:27=9;
				                      28=6; 29=14; 30=0; else=NA")
val_labels(ces88$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18, Housing=19)
#table(as_factor(ces88$mip))
# table(ces88$mip)

#recode welfare (qb6)
# look_for(ces88, "welfare")
ces88$welfare<-Recode(as.numeric(ces88$qb6), "1=1; 2=0.75; 8=0.5; 3=0.25; 4=0; else=NA")
#checks
# table(ces88$welfare)
# table(ces88$welfare, ces88$qb6)

#recode Postgrad (n3)
# look_for(ces88, "education")
ces88$postgrad<-Recode(as.numeric(ces88$n3), "10:11=1; 1:9=0; else=NA")
#checks
# table(ces88$postgrad)
ces88$authoritarianism

#recode foreign born (n13)
# look_for(ces88, "birth")
ces88$foreign<-Recode(as.numeric(ces88$n13), "1=0; 2:21=1; 0=1; else=NA")
val_labels(ces88$foreign)<-c(No=0, Yes=1)
#checks
#val_labels(ces88$foreign)
# table(ces88$foreign, ces88$n13, useNA="ifany")

#recode Women - how much should be done (xk9)
look_for(ces88, "women")
table(ces88$xk9)
ces88$women<-Recode(as.numeric(ces88$xk9), "1=0; 3=0.5; 5=1; else=NA")
#checks
table(ces88$women,  useNA = "ifany")

#recode Race - how much should be done (xk5)
look_for(ces88, "ethnic")
table(ces88$xk5)
ces88$race<-Recode(as.numeric(ces88$xk5), "1=0; 3=0.5; 5=1; else=NA")
#checks
table(ces88$race,  useNA = "ifany")

#recode Previous Vote (b7)
# look_for(ces88, "vote")
ces88$previous_vote<-Recode(ces88$b7, "1=1; 2=2; 3=3; 4=0; else=NA")
val_labels(ces88$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
#val_labels(ces88$previous_vote)
#table(ces88$previous_vote)

#recode Provincial Vote (b9)
# look_for(ces88, "vote")
ces88$prov_vote<-car::Recode(as.numeric(ces88$b9), "2=1; 1=2; 3=3; 4=0; 5=4; 6=0; else=NA")
val_labels(ces88$prov_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, PQ=4, Green=5)
#checks
val_labels(ces88$prov_vote)
 table(ces88$prov_vote)

# recode feminism (xh9)
ces88$feminism_rating<-Recode(as.numeric(ces88$xh9 /100), "9.97:9.99=NA")
table(ces88$feminism_rating)

glimpse(ces88)
#Add mode
ces88$mode<-rep("Phone", nrow(ces88))
#Add Election
ces88$election<-rep(1988, nrow(ces88))

# Save the file
save(ces88, file=here("data/ces88.rda"))

