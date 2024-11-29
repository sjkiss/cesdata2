#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces97<-read_sav(file=here("data-raw/CES-E-1997_F1.sav"))

#### #recode Gender (cpsrgen)####

# (ces97, "gender")
ces97$male<-Recode(ces97$cpsrgen, "1=1; 5=0")
val_labels(ces97$male)<-c(Female=0, Male=1)
#checks
val_labels(ces97$male)
# (ces97$male)
#####recode Union Household (cpsm9) ####

# (ces97, "union")
ces97$union<-Recode(ces97$cpsm9, "1=1; 5=0; else=NA")
val_labels(ces97$union)<-c(None=0, Union=1)
#checks
val_labels(ces97$union)
# (ces97$union)
#####Union Combined variable (identical copy of union) ####

ces97$union_both<-ces97$union
#checks
val_labels(ces97$union_both)
# (ces97$union_both)

#### #recode Education (cpsm3)####
# (ces97, "education")
ces97$degree<-Recode(ces97$cpsm3, "9:11=1; 1:8=0; else=NA")
val_labels(ces97$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces97$degree)
# (ces97$degree)

####recode Region (province) ####
# (ces97, "province")
ces97$region<-Recode(ces97$province, "10:13=1; 35=2; 46:59=3; 4=NA; else=NA")
val_labels(ces97$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces97$region)
# (ces97$region)

####recode Province (province) ####
# look_for(ces97, "province")
ces97$prov<-Recode(ces97$province, "10=1; 11=2; 12=3; 13=4; 24=5; 35=6; 46=7; 47=8; 48=9; 59=10; else=NA")
val_labels(ces97$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces97$prov)
table(ces97$prov)

#### recode Quebec (province) ####
# (ces97, "province")
ces97$quebec<-Recode(ces97$province, "10:13=0; 35:59=0; 24=1; else=NA")
val_labels(ces97$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces97$quebec)
# (ces97$quebec)

####recode Age (cpsage) ####
# (ces97, "age")
ces97$yob<-Recode(as.numeric(ces97$cpsage), "9999=NA")
ces97$age<-1997-ces97$yob
#check
# (ces97$age)

####recode Religion (cpsm10) ####
# (ces97, "relig")
ces97$religion<-Recode(ces97$cpsm10, "0=0; 2=1; 1=2; 3:5=3; else=NA")
val_labels(ces97$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces97$religion)
# (ces97$religion)

#### recode Language (cpslang)####

# (ces97, "language")
ces97$language<-Recode(ces97$cpslang, "1=1; 2=0; else=NA")
val_labels(ces97$language)<-c(French=0, English=1)
#checks
val_labels(ces97$language)
# (ces97$language)
#### recode Non-charter Language (cpsm15)####

# (ces97, "language")
ces97$non_charter_language<-Recode(ces97$cpsm15, "1:5=0; 0=1; 10:27=1; else=NA")
val_labels(ces97$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces97$non_charter_language)
# (ces97$non_charter_language)
####recode Employment (cpsm4) ####

# (ces97, "employment")
ces97$employment<-Recode(ces97$cpsm4, "2:7=0; 1=1; 8=1; else=NA")
val_labels(ces97$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces97$employment)
# (ces97$employment)

####recode Sector (cpsm7 & cpsm4) ####
# (ces97, "firm")
ces97 %>%
  mutate(sector=case_when(
    cpsm7==3 ~1,
    cpsm7==5 ~1,
    cpsm7==7 ~1,
    cpsm7==1 ~0,
    cpsm7==0 ~0,
    cpsm4> 1 & cpsm4< 9 ~ 0,
    cpsm7==9 ~NA_real_ ,
    cpsm7==8 ~NA_real_ ,
  ))->ces97

val_labels(ces97$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces97$sector)
# (ces97$sector)

lookfor(ces97, "firm")
ces97 %>%
  mutate(sector_welfare=case_when(
    #Socioal Scientists
    sector==1&( cpsm6 >2312& cpsm6 <2320)~1,
    #Social Workers
    sector==1&( cpsm6 >2330& cpsm6 <2340)~1,
    #Counsellors
    sector==1& cpsm6 ==2391~1,
    # Education
    sector==1&( cpsm6 >2710& cpsm6 <2800)~1,
    #Health
    sector==1&( cpsm6 >3111& cpsm6 <3160)~1,
    sector==0 ~0,
    TRUE~ NA_integer_
  ))->ces97
ces97 %>%
  group_by(sector, sector_welfare) %>%
  count() %>%
  as_factor()
#### recode Party ID (cpsk1 and cpsk4)####
look_for (ces97, "federal")
lookfor(ces97, "occupation")
ces97 %>%
  mutate(party_id=case_when(
    cpsk1==1 | cpsk4==1 ~ 1,
    cpsk1==2 | cpsk4==2 ~ 2,
    cpsk1==3 | cpsk4==3 ~ 3,
    cpsk1==4 | cpsk4==4 ~ 2,
    cpsk1==5 | cpsk4==5 ~ 4,
    cpsk1==6 | cpsk4==6 ~ 0,
  ))->ces97

val_labels(ces97$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces97$party_id)
table(ces97$party_id)

####recode Vote (pesa4) ####
# (ces97, "vote")
ces97$vote<-Recode(ces97$pesa4, "1=1; 2=2; 3=3; 5=4; 4=2; 0=0; else=NA")
val_labels(ces97$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces97$vote)
# (ces97$vote)

#recode Vote splitting Conservatives (pesa4)
# (ces97, "vote")
ces97$vote
ces97$vote3<-Recode(ces97$pesa4, "1=1; 2=2; 3=3; 5=4; 4=6; 0=0; else=NA")
val_labels(ces97$vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces97$vote3)
# (ces97$vote3)
# (ces97$pesa4)

#### #recode Occupation (pinporr)####

# (ces97, "occupation")
# (ces97, "pinporr")
ces97$occupation<-Recode(ces97$pinporr, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces97$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces97$occupation)
# (ces97$occupation)
####recode Occupation3 as 6 class schema with self-employed (cpsm4) ####

# (ces97, "employ")
ces97$occupation3<-ifelse(ces97$cpsm4==8, 6, ces97$occupation)
val_labels(ces97$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces97$occupation3)
# (ces97$occupation3)
#### recode Income (cpsm16 and cpsm16a)####

# (ces97, "income")
ces97 %>%
  mutate(income=case_when(
    cpsm16a==1 | cpsm16> 0 & cpsm16 < 20 ~ 1,
    cpsm16a==2 | cpsm16> 19 & cpsm16 < 30 ~ 2,
    cpsm16a==3 | cpsm16> 29 & cpsm16 < 50 ~ 3,
    cpsm16a==4 | cpsm16> 29 & cpsm16 < 50 ~ 3,
    cpsm16a==5 | cpsm16> 49 & cpsm16 < 70 ~ 4,
    cpsm16a==6 | cpsm16> 49 & cpsm16 < 70 ~ 4,
    cpsm16a==7 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==8 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 69 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 69 & cpsm16 < 998 ~ 5,
  ))->ces97

val_labels(ces97$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks

#Simon's Version
ces97 %>%
  mutate(income2=case_when(
    #First Quintile
    cpsm16a==1 | cpsm16> 0 & cpsm16 < 18 ~ 1,
    #Second Quintile
    cpsm16a==2 | cpsm16> 17 & cpsm16 < 33 ~ 2,
    cpsm16a==3 | cpsm16> 32 & cpsm16 < 50 ~ 3,
    cpsm16a==4 | cpsm16> 32 & cpsm16 < 50 ~ 3,
    cpsm16a==5 | cpsm16> 49 & cpsm16 < 72 ~ 4,
    cpsm16a==6 | cpsm16> 49 & cpsm16 < 72 ~ 4,
    cpsm16a==7 | cpsm16> 71 & cpsm16 < 998 ~ 5,
    cpsm16a==8 | cpsm16> 71 & cpsm16 < 998 ~ 5,
    cpsm16a==9 | cpsm16> 71 & cpsm16 < 998 ~ 5,
    cpsm16a==10 | cpsm16> 71 & cpsm16 < 998 ~ 5,
  ))->ces97

val_labels(ces97$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

# (ces97, "income")
ces97 %>%
  mutate(income_tertile=case_when(
    #first tertile
    as.numeric(cpsm16a)<3 | as.numeric(cpsm16)> 0 & as.numeric(cpsm16) < 27 ~ 1,
    #Second tertile
    as.numeric(cpsm16a)>2 & as.numeric(cpsm16a)<5 | as.numeric(cpsm16)> 26 & as.numeric(cpsm16) < 56 ~ 2,
    #Third Tertile
    as.numeric(cpsm16a)>4 & as.numeric(cpsm16a)<98 | as.numeric(cpsm16)> 55 & as.numeric(cpsm16) < 998~ 3
  ))->ces97

val_labels(ces97$income_tertile)<-c(Lowest=1, Middle=2, Highest=3)
#checks
val_labels(ces97$income_tertile)
# (ces97$income_tertile)

#####recode Household size (cpshhwgt)####
# (ces97, "house")
ces97$household<-Recode(ces97$cpshhwgt, "0.5056=0.5; 1.0111=1; 1.5167=1.5; 2.0223=2; 2.5278=2.5; 3.0334=3; 3.539=3.5; 4.5501=4.5")
#checks
# (ces97$household)

#### recode income / household size ####
ces97$inc<-Recode(as.numeric(ces97$cpsm16), "998:999=NA")

# (ces97$inc)
ces97$inc2<-ces97$inc/ces97$household
# (ces97$inc2)
ces97$inc3<-Recode(as.numeric(ces97$cpsm16a), "98:99=NA")
# (ces97$inc3)
ces97$inc4<-ces97$inc3/ces97$household
# (ces97$inc4)

ces97 %>%
  mutate(income_house=case_when(
    as.numeric(inc4)<2.7 |(as.numeric(inc2)> 0 & as.numeric(inc2) < 27) ~ 1,
    (as.numeric(inc4)>2.6 &as.numeric(inc4) <5.3 )| as.numeric(inc2)> 26 & as.numeric(inc2) < 53 ~ 2,
    (as.numeric(inc4)>5.2 & as.numeric(inc4)<31) | as.numeric(inc2)> 52 & as.numeric(inc2) <998 ~ 3
  ))->ces97
# (ces97$income_house)
# (ces97$income_tertile)
# (ces97$income_tertile, ces97$income_house)

####recode Religiosity (pesm10b)####
# (ces97, "relig")
ces97$religiosity<-Recode(as.numeric(ces97$pesm10b), "7=1; 5=2; 8=3; 3=4; 1=5; else=NA")
val_labels(ces97$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
#val_labels(ces97$religiosity)
# (ces97$religiosity)

#### recode Redistribution (mbsa4)####
# (ces97, "rich")
#val_labels(ces97$mbsa4)
ces97$redistribution<-Recode(as.numeric(ces97$mbsa4), "; 1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces97$redistribution)
# (ces97$redistribution)

####recode Pro-Redistribution (mbsa4) ####
ces97$pro_redistribution<-Recode(ces97$mbsa4, "1:2=1; 3:4=0; else=NA", as.numeric=T)
val_labels(ces97$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
#val_labels(ces97$pro_redistribution)
# (ces97$pro_redistribution)
#### recode Market Liberalism (cpsf6 and pese19)####

# (ces97, "private")
# (ces97, "blame")

ces97$market1<-Recode(as.numeric(ces97$cpsf6), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
ces97$market2<-Recode(as.numeric(ces97$pese19), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# (ces97$market1)
# (ces97$market2)
#
# ces97 %>%
#   rowwise() %>%
#   mutate(market_liberalism=mean(
#     c_across(market1:market2)
#  , na.rm=T )) ->out
#
# out %>%
#   ungroup() %>%
#   select(c('market1', 'market2', 'market_liberalism')) %>%
#   mutate(na=rowSums(is.na(.))) %>%
#   filter(na>0, na<3)
#Scale Averaging
ces97 %>%
  mutate(market_liberalism=rowMeans(select(., num_range("market", 1:2)), na.rm=T))->ces97


ces97 %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
qplot(ces97$market_liberalism, geom="histogram")
# (ces97$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces97 %>%
  select(market1, market2) %>%
  alpha(.)
#Check correlation
ces97 %>%
  select(market1, market2) %>%
  cor(., use="complete.obs")

####recode Immigration (cpsj18) ####

# (ces97, "imm")
ces97$immigration_rates<-Recode(as.numeric(ces97$cpsj18), "1=0; 3=1; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# (ces97$immigration_rates, useNA = "ifany")

#### recode Environment vs Jobs (mbsa6)####
# (ces97, "env")
# (ces97$mbsa6, useNA="ifany")
ces97$enviro<-Recode(as.numeric(ces97$mbsa6), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# (ces97$enviro, useNA="ifany")

####recode Capital Punishment (pese13) ####
# (ces97, "punish")
ces97$death_penalty<-Recode(as.numeric(ces97$pese13), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA")
#checks
# (ces97$death_penalty)

#recode Crime (cpsa2g)
# (ces97, "crime")
ces97$crime<-Recode(as.numeric(ces97$cpsa2g), "1=1; 2=0.5; 3=0; 8=0.5; else=NA")
#checks
# (ces97$crime)

#recode Gay Rights (mbsg3)
# (ces97, "homo")
var_label(ces97$mbsg3)
ces97$gay_rights<-Recode(as.numeric(ces97$mbsg3), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# (ces97$gay_rights)
#### #recode Abortion (pese5a pese5b pese5c)####

# (ces97, "abort")
ces97 %>%
  mutate(abortion=case_when(
    pese5a==3 ~0,
    pese5a==1 ~1,
    pese5a==2 ~0.5,
    pese5a==8 ~0.5,
    pese5b==3 ~1,
    pese5b==2 ~0,
    pese5b==1 ~0.5,
    pese5b==8 ~0.5,
    pese5c==2 ~1,
    pese5c==1 ~0,
    pese5c==3 ~0.5,
    pese5c==8 ~0.5,
  ))->ces97
#checks
# (ces97$abortion)
#### #recode Lifestyle (mbsa7)####

# (ces97, "lifestyle")
ces97$lifestyles<-Recode(as.numeric(ces97$mbsa7), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# (ces97$lifestyles)
#####recode Stay Home (cpsf3) ####

# (ces97, "home")
ces97$stay_home<-Recode(as.numeric(ces97$cpsf3), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# (ces97$stay_home)
#####recode Marriage Children (cpsf2) ####

# (ces97, "children")
ces97$marriage_children<-Recode(as.numeric(ces97$cpsf2), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# (ces97$marriage_children)
#####recode Values (mbsa9) ####

# (ces97, "traditional")
ces97$values<-Recode(as.numeric(ces97$mbsa9), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA")
#checks
# (ces97$values)
#####recode Morals (mbsa8) ####

# (ces97, "moral")
ces97$morals<-Recode(as.numeric(ces97$mbsa8), "1=0; 2=0.25; 3=0.75; 4=1; 8=0.5; else=NA")
#checks
# (ces97$morals)
#### Moral traditionalism####
#recode Moral Traditionalism (abortion, lifestyles, stay home, values, marriage childen, morals)
ces97$trad3<-ces97$abortion
ces97$trad7<-ces97$lifestyles
ces97$trad1<-ces97$stay_home
ces97$trad4<-ces97$values
ces97$trad5<-ces97$marriage_children
ces97$trad6<-ces97$morals
ces97$trad2<-ces97$gay_rights

#This code removes value labels from trad1 to trad6
#Start with data frame
ces97 %>%
  #mutate because changing variables
  #across because we are doing something across a series of columns
  # What columns are we working on? The ones with the names generated in
  #num_range('trad', 1:7)
  mutate(across(.cols=num_range('trad', 1:7),
                #The thing we are doing is removing value labels
                remove_val_labels))->ces97
#remove_val_labels
# (ces97$trad1)
# (ces97$trad2)
# (ces97$trad3)
# (ces97$trad4)
# (ces97$trad5)
# (ces97$trad6)
# (ces97$trad7)

#Scale Averaging
ces97 %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", 1:7)), na.rm=T))->ces97


ces97 %>%
  select(starts_with("trad")) %>%
  head()
#Check distribution of traditionalism
# qplot(ces97$traditionalism, geom="histogram")
# qplot(ces97$market_liberalism, geom="histogram")

# (ces97$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces97 %>%
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>%
  psych::alpha(.)
#Check correlation
ces97 %>%
  select(trad1, trad2, trad3, trad4, trad5, trad6, trad7) %>%
  cor(., use="complete.obs")


#recode Moral Traditionalism 2 (stay home & gay rights) (Left-Right)

#Scale Averaging
ces97 %>%
  mutate(traditionalism2=rowMeans(select(., c('trad1', 'trad2')), na.rm=T))->ces97

ces97 %>%
  select(starts_with("trad")) %>%
  summary()
#Check distribution of traditionalism
#qplot(ces97$traditionalism2, geom="histogram")
# (ces97$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces97 %>%
  select(trad1, trad2) %>%
  psych::alpha(.)

#Check correlation
ces97 %>%
  select(trad1, trad2) %>%
  cor(., use="complete.obs")

#just check the market and traditionalism scales for NAs
ces97 %>%
  select(market_liberalism, traditionalism) %>%
  str()

ggplot(ces97, aes(x=market_liberalism, y=traditionalism))+geom_point()+geom_smooth(method="lm")

#recode 2nd Dimension (stay home, immigration, gay rights, crime)
ces97$author1<-ces97$stay_home
ces97$author2<-ces97$immigration_rates
ces97$author3<-ces97$gay_rights
ces97$author4<-ces97$crime
# (ces97$author1)
# (ces97$author2)
# (ces97$author3)
# (ces97$author4)

#Remove value labels
ces97 %>%
  mutate(across(num_range('author', 1:4), remove_val_labels))->ces97
ces97 %>%
  mutate(authoritarianism=rowMeans(select(., num_range("author", 1:4)), na.rm=T))->ces97


#Check distribution of traditionalism
qplot(ces97$authoritarianism, geom="histogram")
# (ces97$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
ces97 %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces97 %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")

#recode Quebec Accommodation (cpse3a) (Left=more accom)
# (ces97, "quebec")
ces97$quebec_accom<-Recode(as.numeric(ces97$cpse3a), "2=1; 1=0; 3=0.5; 8=0.5; else=NA")
#checks
# (ces97$quebec_accom)


#recode Liberal leader (cpsd1b)
# (ces97, "Chretien")
ces97$liberal_leader<-Recode(as.numeric(ces97$cpsd1b), "0=1; 997:999=NA")
#checks
# (ces97$liberal_leader)

#recode conservative leader (cpsd1a)
# (ces97, "Charest")
ces97$conservative_leader<-Recode(as.numeric(ces97$cpsd1a), "0=1; 997:999=NA")
#checks
# (ces97$conservative_leader)

#recode NDP leader (cpsd1c)
# (ces97, "McDonough")
ces97$ndp_leader<-Recode(as.numeric(ces97$cpsd1c), "0=1; 997:999=NA")
#checks
# (ces97$ndp_leader)

#recode Bloc leader (cpsd1e)
# (ces97, "Duceppe")
ces97$bloc_leader<-Recode(as.numeric(ces97$cpsd1e), "0=1; 997:999=NA")
#checks
# (ces97$bloc_leader)

#recode liberal rating (cpsd1h)
# (ces97, "liberal")
ces97$liberal_rating<-Recode(as.numeric(ces97$cpsd1h), "0=1; 997:999=NA")
#checks
# (ces97$liberal_rating)

#recode conservative rating (cpsd1g)
# (ces97, "conservative")
ces97$conservative_rating<-Recode(as.numeric(ces97$cpsd1g), "0=1; 997:999=NA")
#checks
# (ces97$conservative_rating)

#recode NDP rating (cpsd1i)
# (ces97, "new democratic")
ces97$ndp_rating<-Recode(as.numeric(ces97$cpsd1i), "0=1; 997:999=NA")
#checks
# (ces97$ndp_rating)

#recode Bloc rating (cpsd1k)
# (ces97, "bloc")
ces97$bloc_rating<-Recode(as.numeric(ces97$cpsd1k), "0=1; 997:999=NA")
#checks
# (ces97$bloc_rating)

#recode Education (pese6f)
# (ces97, "edu")
ces97$education<-Recode(as.numeric(ces97$pese6f), "1=0; 3=0.5; 5=1; 8=0.5; else=NA")
#checks
# (ces97$education, ces97$pese6f , useNA = "ifany" )

#recode Personal Retrospective (cpsc1)
# (ces97, "financ")
ces97$personal_retrospective<-Recode(as.numeric(ces97$cpsc1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces97$personal_retrospective)
# (ces97$personal_retrospective, ces97$cpsc1 , useNA = "ifany" )

#recode National Retrospective (cpsg1)
# (ces97, "economy")
ces97$national_retrospective<-Recode(as.numeric(ces97$cpsg1), "1=1; 3=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces97$national_retrospective)
# (ces97$national_retrospective, ces97$cpsg1 , useNA = "ifany" )

#recode Ideology (mbsi16a)
# (ces97, "the left")
ces97$ideology<-Recode(as.numeric(ces97$mbsi16a) , "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
#val_labels(ces97$ideology)<-c(Left=0, Right=1)
#checks
#val_labels(ces97$ideology)
# (ces97$ideology, ces97$mbsi16a  , useNA = "ifany")

#recode turnout (pesa2a & pesa2b)
# (ces97, "vote")
ces97 %>%
  mutate(turnout=case_when(
    pesa2a==1 ~1,
    pesa2a==5 ~0,
    pesa2a==8 ~0,
    pesa2b==1 ~1,
    pesa2b==5 ~0,
    pesa2b==8 ~0,
  ))->ces97
val_labels(ces97$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces97$turnout)
# (ces97$turnout)
# (ces97$turnout, ces97$vote)

#### recode political efficacy ####
#recode No Say (cpsb10b)
# (ces97, "not have say")
ces97$efficacy_internal<-Recode(as.numeric(ces97$cpsb10b), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces97$efficacy_internal)
# (ces97$efficacy_internal)
# (ces97$efficacy_internal, ces97$cpsb10b , useNA = "ifany" )

#recode MPs lose touch (cpsb10a)
# (ces97, "lose touch")
ces97$efficacy_external<-Recode(as.numeric(ces97$cpsb10a), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces97$efficacy_external)
# (ces97$efficacy_external)
# (ces97$efficacy_external, ces97$cpsb10a , useNA = "ifany" )

#recode Official Don't Care (cpsb10d)
# (ces97, "cares what")
ces97$efficacy_external2<-Recode(as.numeric(ces97$cpsb10d), "1=0; 3=0.25; 5=0.75; 7=1; 8=0.5; else=NA", as.numeric=T)
#val_labels(ces97$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces97$efficacy_external2)
# (ces97$efficacy_external2)
# (ces97$efficacy_external2, ces97$cpsb10d , useNA = "ifany" )

ces97 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces97

ces97 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
#qplot(ces97$political_efficacy, geom="histogram")
# (ces97$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces97 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  psych::alpha(.)
#Check correlation
ces97 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  cor(., use="complete.obs")


#recode Most Important Question (cpsa1)
# (ces97, "issue")
ces97$mip<-Recode(ces97$cpsa1, "10:16=6; 20:25=7; 26=10; 30=7; 31=18; 32:34=7; 35=0; 36:38=7; 40:41=10; 42:43=3; 44=13; 45=15;
				                        46=12; 47=11; 48=0; 50:55=9; 57=8; 58=15; 59=8; 60:64=15; 65:67=4; 68:69=8; 70:71=14;
				                        72=1; 73=14; 74=15; 75:79=2; 80:88=16; 89:90=0; 91=3; 92=0; 93=11; 94:95=0; 96-16; 97=0; else=NA")
val_labels(ces97$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18)
 (ces97$mip)

# recode satisfaction with democracy (cpsb9, pesa5b)
# (ces97, "dem")
ces97$satdem<-Recode(as.numeric(ces97$pesa5b), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# (ces97$satdem, ces97$pesa5b, useNA = "ifany" )

ces97$satdem2<-Recode(as.numeric(ces97$cpsb9), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA", as.numeric=T)
#checks
# (ces97$satdem2, ces97$cpsb9, useNA = "ifany" )

#recode Quebec Sovereignty (pese10) (Quebec only & Right=more sovereignty)
# (ces97, "sovereignty")
ces97$quebec_sov<-Recode(as.numeric(ces97$pese10), "1=1; 3=0.75; 5=0.25; 7=0; 8=0.5; else=NA")
#checks
# (ces97$quebec_sov, ces97$pese10, useNA = "ifany" )

# recode provincial alienation (cpsj12)
# (ces97, "treat")
ces97$prov_alienation<-Recode(as.numeric(ces97$cpsj12), "3=1; 1=0; 5=0.5; 8=0.5; else=NA", as.numeric=T)
#checks
# (ces97$prov_alienation, ces97$cpsj12, useNA = "ifany" )

# recode immigration society (mbsg4)
# (ces97, "fit")
ces97$immigration_soc<-Recode(as.numeric(ces97$mbsg4), "1=1; 2=0.75; 3=0.25; 4=0; 8=0.5; else=NA", as.numeric=T)
#checks
# (ces97$immigration_soc, ces97$mbsg4, useNA = "ifany" )

#recode welfare (pese6b)
# (ces97, "welfare")
ces97$welfare<-Recode(as.numeric(ces97$pese6b), "1=1; 3=0.5; 8=0.5; 5=0; else=NA")
#checks
# (ces97$welfare)
# (ces97$welfare, ces97$pese6b)

#recode Postgrad (cpsm3)
# (ces97, "education")
ces97$postgrad<-Recode(as.numeric(ces97$cpsm3), "10:11=1; 1:9=0; else=NA")
#checks
# (ces97$postgrad)

#recode Break Promise (cpsj13)
# (ces97, "promise")
ces97$promise<-Recode(as.numeric(ces97$cpsj13), "1=0; 3=0.5; 5=1; 7=0.5; else=NA", as.numeric=T)
#val_labels(ces97$promise)<-c(low=0, high=1)
#checks
#val_labels(ces97$promise)
# (ces97$promise)
# (ces97$promise, ces97$cpsj13 , useNA = "ifany" )

#recode Trust - not available

# recode political interest (cpsb5)
# (ces97, "interest")
ces97$pol_interest<-Recode(as.numeric(ces97$cpsb5), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
# (ces97$pol_interest, ces97$cpsb5, useNA = "ifany" )

#recode foreign born (cpsm11)
# (ces97, "birth")
ces97$foreign<-Recode(ces97$cpsm11, "1=0; 2:97=1; 0=1; else=NA")
val_labels(ces97$foreign)<-c(No=0, Yes=1)
#checks
#val_labels(ces97$foreign)
# (ces97$foreign, ces97$cpsm11, useNA="ifany")

#### recode Women - how much should be done (pese1) ####
look_for(ces97, "women")
table(ces97$pese1)
ces97$women<-Recode(as.numeric(ces97$pese1), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces97$women,  useNA = "ifany")

#### recode Race - how much should be done (cpsf1) ####
look_for(ces97, "racial")
table(ces97$cpsf1)
ces97$race<-Recode(as.numeric(ces97$cpsf1), "1=0; 2=1; 3=0.5; else=NA")
#checks
table(ces97$race,  useNA = "ifany")

#recode Previous Vote (cpsk6)
# look_for(ces97, "vote")
ces97$previous_vote<-Recode(ces97$cpsk6, "1=1; 2=2; 3=3; 4=2; 5=4; 0=0; else=NA")
val_labels(ces97$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
#val_labels(ces97$previous_vote)
#table(ces97$previous_vote)

#recode Previous Vote splitting Conservatives (cpsk6)
 look_for(ces97, "vote")
ces97$previous_vote3<-car::Recode(as.numeric(ces97$cpsk6), "1=1; 2=2; 3=3; 4=6; 5=4; 0=0; else=NA")
val_labels(ces97$previous_vote3)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, Reform=6)
#checks
val_labels(ces97$previous_vote3)
table(ces97$previous_vote3)

#Add mode
ces97$mode<-rep("Phone", nrow(ces97))
#Add Election
# Note: Care should be taken in the master file to consider whether or not the user
# Wants to include Rs from the 1992 referendum or not.
#
ces97$election<-rep(1997, nrow(ces97))
glimpse(ces97)

# Save the file
save(ces97, file=here("data/ces97.rda"))
