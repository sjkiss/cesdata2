#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
ces19phone<-read_dta(file=here("data-raw/2019 Canadian Election Study - Phone Survey v1.0.dta"), encoding="utf-8")

#recode Gender (q3)
# look_for(ces19phone, "gender")
ces19phone$q3
ces19phone$male<-Recode(ces19phone$q3, "1=1; 2=0; else=NA")
val_labels(ces19phone$male)<-c(Female=0, Male=1)
#checks
val_labels(ces19phone$male)
# table(ces19phone$male)

#recode Union Household (p51)
# look_for(ces19phone, "union")
ces19phone$union<-Recode(ces19phone$p51, "1=1; 2=0; else=NA")
val_labels(ces19phone$union)<-c(None=0, Union=1)
#checks
val_labels(ces19phone$union)
# table(ces19phone$union , useNA = "ifany" )

#Union Combined variable (identical copy of union) ### Respondent only
ces19phone$union_both<-ces19phone$union
#checks
val_labels(ces19phone$union_both)
# table(ces19phone$union_both , useNA = "ifany" )

#recode Education (q61)
# look_for(ces19phone, "education")
ces19phone$degree<-Recode(ces19phone$q61, "9:11=1; 1:8=0; else=NA")
val_labels(ces19phone$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces19phone$degree)
# table(ces19phone$degree ,ces19phone$q61 , useNA = "ifany" )

#recode Region (q4)
# look_for(ces19phone, "province")
ces19phone$region<-Recode(ces19phone$q4, "1:4=1; 6=2; 7:10=3; 4=NA; else=NA")
val_labels(ces19phone$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces19phone$region)
# table(ces19phone$region , ces19phone$q4 , useNA = "ifany" )

#recode Quebec (q4)
# look_for(ces19phone, "province")
ces19phone$quebec<-Recode(ces19phone$q4, "1:4=0; 6:10=0; 5=1; else=NA")
val_labels(ces19phone$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces19phone$quebec)
# table(ces19phone$quebec ,ces19phone$q4 , useNA = "ifany" )

#recode Age (age)
##Just use underlying age variable
#no missing values apparent
# look_for(ces19phone, "age")
ces19phone$age
# ces19phone$age<-ces19phone$age
# summary(ces19phone$age)
# #check
# table(ces19phone$age)
#recode Age2 (0-1 variable)
ces19phone$age2<-(ces19phone$age /100)
#checks
# table(ces19phone$age2)

#recode Religion (q62)
# look_for(ces19phone, "relig")
ces19phone$religion<-Recode(ces19phone$q62, "21=0; 6=1; 8=1; 1=2; 3:4=2; 5=2; 7=2; 9=2; 12:14=2; 2=3; 5=3; 10:11=3; 15=3; 22=3; 16:20=2; 21=0; else=NA")
val_labels(ces19phone$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces19phone$religion)
# table(ces19phone$religion , ces19phone$q62 , useNA = "ifany" )

#recode Language (language_CES)
# look_for(ces19phone, "language")
ces19phone$language<-Recode(ces19phone$language_CES, "2=0; 1=1; else=NA")
val_labels(ces19phone$language)<-c(French=0, English=1)
#checks
val_labels(ces19phone$language)
# table(ces19phone$language , ces19phone$language_CES , useNA = "ifany" )

#recode Non-charter Language (q67)
# look_for(ces19phone, "language")
ces19phone$non_charter_language<-Recode(ces19phone$q67, "1=0; 2:3=1; 4=0; 5:31=1; else=NA")
val_labels(ces19phone$non_charter_language)<-c(Charter=0, Non_Charter=1)
# table(as_factor(ces19phone$q67),ces19phone$non_charter_language , useNA = "ifany" )
#checks
val_labels(ces19phone$non_charter_language)
# table(ces19phone$non_charter_language)

#recode Employment (q68)
# look_for(ces19phone, "employment")
ces19phone$employment<-Recode(ces19phone$q68, "3:8=0; 1:2=1; 9:11=1; else=NA")
val_labels(ces19phone$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces19phone$employment)
# table(ces19phone$employment , ces19phone$q68 , useNA = "ifany" )

#recode Sector (p53 & q68)
# look_for(ces19phone, "public")
ces19phone %>%
  mutate(sector=case_when(
    p53==1 ~1,
    p53==2 ~0,
    p53==3 ~0,
    p53==4 ~0,
    q68>2 & q68< 12 ~ 0,
    p53==-9 ~NA_real_ ,
    p53==-8 ~NA_real_ ,
  ))->ces19phone

val_labels(ces19phone$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces19phone$sector)
# table(ces19phone$sector , ces19phone$p53 , useNA = "ifany" )

#recode Party ID (p47)
# look_for(ces19phone, "closest")
ces19phone$party_id<-Recode(ces19phone$p47, "1=1; 2=2; 3=3; 4=4; 5=0; 7=0; 6=2; else=NA")
val_labels(ces19phone$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4)
#checks
val_labels(ces19phone$party_id)
# table(ces19phone$party_id, ces19phone$p47 , useNA = "ifany" )

#recode Vote (p3)
# look_for(ces19phone, "party did you vote")
ces19phone$vote<-Recode(ces19phone$p3, "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; 6=2; else=NA")
val_labels(ces19phone$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$vote)
# table(ces19phone$vote , ces19phone$p3 , useNA = "ifany" )

# Add in NOC codes from 2016
source(here("data-raw/recode_scripts/ces19phone_noc_recode.R"))

#recode Occupation (p52)
# look_for(ces19phone, "occupation")
ces19phone$NOC
ces19phone$occupation<-Recode(as.numeric(ces19phone$NOC), "0:1099=2;
1100:1199=1;
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
 6400:6799=3; 7200:7399=4;
 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")

val_labels(ces19phone$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)

#checks
val_labels(ces19phone$occupation)
# table(ces19phone$occupation , useNA = "ifany" )
ces19phone %>%
  filter(is.na(NOC)==F&is.na(occupation)==T) %>%
  select(NOC, occupation)

#recode Occupation3 as 6 class schema with self-employed (q68)
# look_for(ces19phone, "employ")
ces19phone$occupation3<-ifelse(ces19phone$q68==3, 6, ces19phone$occupation)
val_labels(ces19phone$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces19phone$occupation3)
# table(ces19phone$occupation3, ces19phone$q68 , useNA = "ifany" )

#recode Income (q70r)
# look_for(ces19phone, "income")
ces19phone$q70r
ces19phone$income<-Recode(ces19phone$q70r, "1:2=1; 3=2; 4=3; 5:6=4; 7:8=5; else=NA")
val_labels(ces19phone$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#Simon's Version identical
# look_for(ces19phone, "income")
ces19phone$income2<-Recode(ces19phone$q70r, "1:2=1; 3=2; 4=3; 5:6=4; 7:8=5; else=NA")
val_labels(ces19phone$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

ces19phone$income_tertile<-Recode(ces19phone$q70r, "1:3=1; 4=2;  5:8=3; else=NA")
val_labels(ces19phone$income_tertile)<-c(Lowest=1, Middle=2, Highest=3)
#checks
val_labels(ces19phone$income)
# table(ces19phone$income_tertile, ces19phone$q70r , useNA = "ifany" )
val_labels(ces19phone$q70r)

#recode Religiosity (q63)
# look_for(ces19phone, "relig")
ces19phone$religiosity<-Recode(ces19phone$q63, "4=1; 3=2; -9=3; 2=4; 1=5; else=NA")
val_labels(ces19phone$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces19phone$religiosity)
# table(ces19phone$religiosity)

#### recode Household size (q71)####
# look_for(ces19phone, "household")
ces19phone$household<-Recode(ces19phone$q71, "1=0.5; 2=1; 3=1.5; 4=2; 5=2.5; 6=3; 7=3.5; 8=4; 9=4.5; 10=5; 12=6; 13=6.5; 15=7.5; else=NA")
#checks
# table(ces19phone$household, useNA = "ifany" )

#### recode income / household size ####
ces19phone$inc<-Recode(ces19phone$q70r, "-8=NA; -9=NA")
# table(ces19phone$inc)
ces19phone$inc2<-ces19phone$inc/ces19phone$household
# table(ces19phone$inc2)

ces19phone %>%
  mutate(income_house=case_when(
    inc2<3.01 ~ 1,
    inc2>3 & inc2 <4.01  ~ 2,
    inc2>4 & inc2 <98  ~ 3,
  ))->ces19phone

# table(ces19phone$income_house)
# table(ces19phone$income_tertile)
# table(ces19phone$income_tertile, ces19phone$income_house)

#recode Community Size (p57)
# look_for(ces19phone, "live")
ces19phone$size<-Recode(ces19phone$p57, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
val_labels(ces19phone$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces19phone$size)
# table(ces19phone$size, ces19phone$p57 , useNA = "ifany" )

#recode Native-born (q64)
ces19phone$q64
ces19phone$native<-Recode(ces19phone$q64, "1:2=1; 3:13=0; else=NA")
val_labels(ces19phone$native)<-c(Foreign=0, Native=1)

#checks
val_labels(ces19phone$native)
# table(ces19phone$native , ces19phone$q64 , useNA = "ifany" )

#recode Immigration sentiment (p22_a, p22_b, p22_c, q39) into an index 0-1
#1 = pro-immigration sentiment 0 = anti-immigration sentiment
# look_for(ces19phone, "immigr")
ces19phone$p22_a
ces19phone$immigration_economy<-Recode(as.numeric(ces19phone$p22_a), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_economy)<-NULL

# table(ces19phone$immigration_economy ,ces19phone$p22_a , useNA = "ifany" )

ces19phone$immigration_culture<-Recode(as.numeric(ces19phone$p22_b), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_culture)<-NULL

#checks
#val_labels(ces19phone$immigration_culture)
# table(ces19phone$immigration_culture ,ces19phone$p22_b , useNA = "ifany" )

ces19phone$immigration_crime<-Recode(as.numeric(ces19phone$p22_c), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_crime)<-NULL
#checks
#val_labels(ces19phone$immigration_crime)
# table(ces19phone$immigration_crime, ces19phone$p22_c , useNA = "ifany" )

ces19phone$immigration_rate<-Recode(as.numeric(ces19phone$q39), "1=1; 2=0; 3=0.5; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$immigration_rate)<-NULL
#checks
#val_labels(ces19phone$immigration_rate)
# table(ces19phone$immigration_rate, ces19phone$q39 , useNA = "ifany" )

#recode Racial Minorities sentiment (p21_a)
#1 = pro-racial minority sentiment 0 = anti-racial minority sentiment
# look_for(ces19phone, "minor")
ces19phone$minorities_culture<-Recode(as.numeric(ces19phone$p21_a), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$minorities_culture)<-NULL

# table(ces19phone$minorities_culture, ces19phone$p21_a , useNA = "ifany" )

ces19phone$minorities_help<-Recode(as.numeric(ces19phone$p35_a), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)

ces19phone %>%
  mutate(minorities=rowMeans(select(., c("minorities_culture", "minorities_help")), na.rm=T))->ces19phone

#checks
#val_labels(ces19phone$minorities_help)
#table(ces19phone$minorities_help)

#Combine the 4 immigration variables and divide by 4
ces19phone %>%
  mutate(immigration=rowMeans(select(., c("immigration_economy", "immigration_culture", "immigration_crime", "immigration_rate")), na.rm=T))->ces19phone

#Check distribution of immigration
# qplot(ces19phone$immigration, geom="histogram")

#Calculate Cronbach's alpha
library(psych)

ces19phone %>%
  select(immigration_economy, immigration_culture, immigration_crime, immigration_rate) %>%
  alpha(.)

## Or Create a 5 variable immigration/racial minority sentiment index by dividing by 5
ces19phone %>%
  mutate(immigration2=rowMeans(select(., c("immigration_economy", "immigration_culture", "immigration_crime", "immigration_rate", "minorities_help")), na.rm=T))->ces19phone


#Calculate Cronbach's alpha
library(psych)

ces19phone %>%
  select(immigration_economy, immigration_culture, immigration_crime, immigration_rate, minorities_help) %>%
  alpha(.)

#recode Previous Vote (q60)
# look_for(ces19phone, "party did you vote")
ces19phone$q60
ces19phone$past_vote<-Recode(ces19phone$q60, "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; else=NA")
val_labels(ces19phone$past_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$past_vote)
# table(ces19phone$past_vote, ces19phone$q60 , useNA = "ifany" )

# #recode Jagmeet Singh (q22)
# look_for(ces19phone, "Singh")
# ces19phone$Singh<-Recode(ces19phone$q22, "-6=NA; -8=NA; -9=NA")
# #checks
# table(ces19phone$Singh)
# ces19phone$Jagmeet_Singh<-(ces19phone$Singh /100)
# table(ces19phone$Jagmeet_Singh)
#
# #recode Justin Trudeau (q20)
# look_for(ces19phone, "Trudeau")
# ces19phone$Trudeau<-Recode(ces19phone$q20, "-6=NA; -8=NA; -9=NA")
# #checks
# table(ces19phone$Trudeau)
# ces19phone$Justin_Trudeau<-(ces19phone$Trudeau /100)
# table(ces19phone$Justin_Trudeau)
#
# #recode Andrew Scheer (q21)
# look_for(ces19phone, "Scheer")
# ces19phone$Scheer<-Recode(ces19phone$q21, "-6=NA; -8=NA; -9=NA")
# #checks
# table(ces19phone$Scheer)
# ces19phone$Andrew_Scheer<-(ces19phone$Scheer /100)
# table(ces19phone$Andrew_Scheer)
#
# #recode Francois Blanchet (q23)
# look_for(ces19phone, "Blanchet")
# ces19phone$Blanchet<-Recode(ces19phone$q23, "-6=NA; -8=NA; -9=NA")
# #checks
# table(ces19phone$Blanchet)
# ces19phone$Francois_Blanchet<-(ces19phone$Blanchet /100)
# table(ces19phone$Francois_Blanchet)

#recode Liberal leader (q20)
ces19phone$liberal_leader<-Recode(as.numeric(ces19phone$q20), "0=1; -9:-6=NA")
#val_labels(ces19phone$liberal_leader)<-NULL
#checks
#table(ces19phone$Trudeau19)
#ces19phone$liberal_leader<-(ces19phone$Trudeau19 /100)
# table(ces19phone$liberal_leader)

#recode Conservative leader (q21)
ces19phone$conservative_leader<-Recode(as.numeric(ces19phone$q21), "0=1; -9:-6=NA")
#val_labels(ces19phone$conservative_leader)<-NULL
#checks
#table(ces19phone$Scheer19)
#ces19phone$conservative_leader<-(ces19phone$Scheer19 /100)
# table(ces19phone$conservative_leader)

#recode NDP leader (q22)
ces19phone$ndp_leader<-Recode(as.numeric(ces19phone$q22), "0=1; -9:-6=NA")
#val_labels(ces19phone$ndp_leader)<-NULL
#checks
#table(ces19phone$Singh19)
#ces19phone$ndp_leader<-(ces19phone$Singh19 /100)
# table(ces19phone$ndp_leader)

#recode Bloc leader (q23)
ces19phone$bloc_leader<-Recode(as.numeric(ces19phone$q23), "0=1; -9:-6=NA")
#val_labels(ces19phone$bloc_leader)<-NULL
#checks
#table(ces19phone$Blanchet19)
#ces19phone$bloc_leader<-(ces19phone$Blanchet19 /100)
# table(ces19phone$bloc_leader)

#recode Green leader (q24)
ces19phone$green_leader<-Recode(as.numeric(ces19phone$q24), "0=1; -9:-6=NA")
#val_labels(ces19phone$green_leader)<-NULL
#checks
# table(ces19phone$green_leader)

#recode Environment (q27_b)
# look_for(ces19phone, "enviro")
ces19phone$environment<-Recode(as.numeric(ces19phone$q27_b), "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
#val_labels(ces19phone$environment)<-NULL
#checks
val_labels(ces19phone$environment)
# table(ces19phone$environment, ces19phone$q27_b , useNA = "ifany" )

#recode Redistribution (p44)
# look_for(ces19phone, "rich")
ces19phone$redistribution<-Recode(as.numeric(ces19phone$p44), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$redistribution)<-NULL
#checks
val_labels(ces19phone$redistribution)
# table(ces19phone$redistribution)

#recode Pro-Redistribution (p44)
ces19phone$pro_redistribution<-Recode(ces19phone$p44, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces19phone$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces19phone$pro_redistribution)
# table(ces19phone$pro_redistribution)

#recode Liberal rating (q14)
# look_for(ces19phone, "Liberal")
ces19phone$Liberal_therm<-Recode(as.numeric(ces19phone$q14), "996=NA; 998=NA; 999=NA")

#checks
# table(ces19phone$Liberal_therm)
ces19phone$Liberal_rating<-(ces19phone$Liberal_therm /100)
# table(ces19phone$Liberal_rating)

ces19phone$liberal_rating<-Recode(as.numeric(ces19phone$q14), "0=1; -9:-6=NA")
# table(ces19phone$liberal_rating)

#recode Conservative rating (q15)
# look_for(ces19phone, "Conservative")
ces19phone$Conservative_therm<-Recode(as.numeric(ces19phone$q15), "996=NA; 998=NA; 999=NA")
#checks
# table(ces19phone$Conservative_therm)
ces19phone$Conservative_rating<-(ces19phone$Conservative_therm /100)
# table(ces19phone$Conservative_rating)

ces19phone$conservative_rating<-Recode(as.numeric(ces19phone$q15), "0=1; -9:-6=NA")
# table(ces19phone$conservative_rating)

#recode NDP rating (q16)
# look_for(ces19phone, "NDP")
ces19phone$NDP_therm<-Recode(as.numeric(ces19phone$q16), "-6=NA; -8=NA; -9=NA")
#checks
# table(ces19phone$NDP_therm)
ces19phone$NDP_rating<-(ces19phone$NDP_therm /100)
# table(ces19phone$NDP_rating)

ces19phone$ndp_rating<-Recode(as.numeric(ces19phone$q16), "0=1; -9:-6=NA")
# table(ces19phone$ndp_rating)

#recode Bloc rating (q17)
# look_for(ces19phone, "bloc")
ces19phone$bloc_rating<-Recode(as.numeric(ces19phone$q17), "0=1; -9:-6=NA")
# table(ces19phone$bloc_rating)

#recode Green rating (q18)
# look_for(ces19phone, "green")
ces19phone$green_rating<-Recode(as.numeric(ces19phone$q18), "0=1; -9:-6=NA")
# table(ces19phone$green_rating)

#recode Manage economy (q33)
# look_for(ces19phone, "economy")
ces19phone$q33

ces19phone$manage_economy<-Recode(ces19phone$q33, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19phone$manage_economy)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$manage_economy)
# table(ces19phone$manage_economy)

#recode Manage environment (q34)
# look_for(ces19phone, "environment")

ces19phone$manage_environment<-Recode(ces19phone$q34, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19phone$manage_environment)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$manage_environment)
# table(ces19phone$manage_environment)

#recode Addressing Main Issue (q8)
# look_for(ces19phone, "issue")
ces19phone$address_issue<-Recode(ces19phone$q8, "1=1; 2=2; 3=3; 4=4; 5=5; 7=NA; 6=2; else=NA")
val_labels(ces19phone$address_issue)<-c(Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$address_issue)
# table(ces19phone$address_issue)

#recode Market Liberalism (p20_a and p20_f)
# look_for(ces19phone, "leave")
# look_for(ces19phone, "blame")

ces19phone$market1<-Recode(as.numeric(ces19phone$p20_a), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
ces19phone$market2<-Recode(as.numeric(ces19phone$p20_f), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$market1)<-c(Strongly_disagree=0, Somewhat_disagree=0.25, Neither=0.5, Somewhat_agree=0.75, Strongly_agree=1)
#checks
# table(ces19phone$market1, ces19phone$p20_a , useNA = "ifany" )
# table(ces19phone$market2, ces19phone$p20_f , useNA = "ifany" )

ces19phone %>%
  mutate(market_liberalism=rowMeans(select(., num_range("market", 1:2)), na.rm=T))->ces19phone


ces19phone %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
# qplot(ces19phone$market_liberalism, geom="histogram")
# table(ces19phone$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19phone %>%
  select(market1, market2) %>%
  alpha(.)
#For some reason the cronbach's alpha doesn't work here.
#Check correlation
ces19phone %>%
  select(market1, market2) %>%
  cor(., use="complete.obs")
#Weakly correlated, but correlated

#recode Moral Traditionalism (p35_b, p20_e,  p35_c)
# look_for(ces19phone, "women")
# look_for(ces19phone, "gays")
ces19phone$moral_1<-Recode(as.numeric(ces19phone$p20_e), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
ces19phone$moral_2<-Recode(as.numeric(ces19phone$p35_b), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
ces19phone$moral_3<-Recode(as.numeric(ces19phone$p35_c), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$moral_1)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
# table(ces19phone$moral_1, ces19phone$p20_e , useNA = "ifany" )
# table(ces19phone$moral_2 ,ces19phone$p35_b, useNA = "ifany" )
# table(ces19phone$moral_3 , ces19phone$p35_c , useNA = "ifany" )
ces19phone %>%
  names()

ces19phone %>%
  mutate(moral_traditionalism=rowMeans(select(., num_range("moral_", 1:3)), na.rm=T))->ces19phone

#Check distribution of moral_traditionalism
# qplot(ces19phone$moral_traditionalism, geom="histogram")
# table(ces19phone$moral_traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19phone %>%
  select(moral_1, moral_2, moral_3) %>%
  alpha(.)

#recode Political Disaffection (p20_i and p20_n)
# look_for(ces19phone, "politicians")
ces19phone$disaffection1<-Recode(as.numeric(ces19phone$p20_i), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
ces19phone$disaffection2<-Recode(as.numeric(ces19phone$p20_n), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$disaffection1)<-c(Strongly_disagree=0, Somewhat_disagree=0.25, Neither=0.5, Somewhat_agree=0.75, Strongly_agree=1)
#checks
# table(ces19phone$disaffection1 , ces19phone$p20_i , useNA = "ifany" )
# table(ces19phone$disaffection2 , ces19phone$p20_n , useNA = "ifany" )

ces19phone %>%
  mutate(political_disaffection=rowMeans(select(., num_range("disaffection", 1:2)), na.rm=T))->ces19phone

ces19phone %>%
  select(contains("disaffection")) %>%
  summary()
#Check distribution of disaffection
# qplot(ces19phone$political_disaffection, geom="histogram")
# table(ces19phone$political_disaffection, useNA="ifany")



#recode Continentalism (p43)
# look_for(ces19phone, "united states")
ces19phone$continentalism<-Recode(as.numeric(ces19phone$p43), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$continentalism)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
#val_labels(ces19phone$continentalism)
# table(ces19phone$continentalism, ces19phone$p43 , useNA = "ifany" )

#recode Quebec Sovereignty (q43)
# look_for(ces19phone, "quebec")
ces19phone$quebec_sovereignty<-Recode(as.numeric(ces19phone$q43), "1=1; 2=0.75; -9=0.5; 3=0.25; 4=0; else=NA", as.numeric=T)
#val_labels(ces19phone$quebec_sovereignty)<-c(Much_less=0, Somewhat_less=0.25, Dont_know=0.5, Somewhat_more=0.75, Much_more=1)
#checks
val_labels(ces19phone$quebec_sovereignty)
# table(ces19phone$quebec_sovereignty , ces19phone$q43 , useNA = "ifany" )

#recode Personal Retrospective (q47)
# look_for(ces19phone, "situation")
ces19phone$personal_retrospective<-Recode(as.numeric(ces19phone$q47), "1=1; 2=0; 3=0.5; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces19phone$personal_retrospective)
# table(ces19phone$personal_retrospective , ces19phone$q47, useNA = "ifany" )

#recode National Retrospective (q31)
# look_for(ces19phone, "economy")
ces19phone$national_retrospective<-Recode(as.numeric(ces19phone$q31), "1=1; 2=0; 3=0.5; -9=0.5; else=NA", as.numeric=T)
#val_labels(ces19phone$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces19phone$national_retrospective)
# table(ces19phone$national_retrospective, ces19phone$q31, useNA = "ifany" )

#recode Defence (q27_d)
# look_for(ces19phone, "defence")
ces19phone$defence<-Recode(as.numeric(ces19phone$q27_d), "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
#val_labels(ces19phone$defence)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces19phone$defence)
# table(ces19phone$defence, ces19phone$q27_d , useNA = "ifany" )

#recode Crime and Justice (q27_c)
# look_for(ces19phone, "justice")
ces19phone$justice<-Recode(as.numeric(ces19phone$q27_c), "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
#val_labels(ces19phone$justice)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
val_labels(ces19phone$justice)
# table(ces19phone$justice , ces19phone$q27_c , useNA = "ifany" )

#recode Education (q27_a)
# look_for(ces19phone, "education")
ces19phone$education<-Recode(as.numeric(ces19phone$q27_a), "3=0.5; 1=1; 2=0; -9=0.5; else=NA")
#val_labels(ces19phone$education)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
#val_labels(ces19phone$education)
# table(ces19phone$education, ces19phone$q27_a , useNA = "ifany" )


#### mip ####
#In another script I coded the MIP responses according to the categories
# Provided in the 2015 CES to maximize comparability

source(here("data-raw/recode_scripts/ces19_phone_mip_recode.R"))
#The original response is ces19phone$q7
ces19phone$q7
#The lowered case response is
ces19phone$q7_lower
#The variable that includes the recodes contained the Excel file is
ces19phone$q7_out
 table(as_factor(ces19phone$q7_out))

#recode Most Important Question (q7_out)
ces19phone$mip<-Recode(ces19phone$q7_out, "75=1; 71=2; 77=2; 18=2; 5=3; 2=3; 90:91=3; 65:66=4; 13=5; 39=18; 10=6;
                                          36=19; 15:16=7; 30=7; 29=18; 56:58=8; 14=9; 50=9; 22:26=10; 7=11; 83=11;
                                          48=12; 79=12; 34=13; 55=14; 73:74=14; 76=14; 49=14; 60:64=15; 72=15;
                                          80:82=16; 84=0; 92:93=11; 94:96=0; 8=0; 31:32=7; 35=0; 1=0; else=NA")
val_labels(ces19phone$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9,
                              Deficit_Debt=10, Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Inflation=18, Housing=19)


source(here('data-raw/recode_scripts/ces19_phone_vismin_recode.R'))

#recode Immigration (q39)
# look_for(ces19phone, "admit")
ces19phone$immigration_rates<-Recode(as.numeric(ces19phone$q39), "1=0; 2=1; 3=0.5; -9=0.5; else=NA", as.numeric=T)
#checks
# table(ces19phone$immigration_rates, ces19phone$q39 , useNA = "ifany" )

#recode Capital Punishment (Missing)

#recode Crime (q27_c) (spending question)
# look_for(ces19phone, "crime")
ces19phone$crime<-Recode(as.numeric(ces19phone$q27_c), "1=1; 2=0; 3=0.5; -9=0.5; else=NA")
#checks
# table(ces19phone$crime, ces19phone$q27_c , useNA = "ifany" )

#recode Gay Rights (p35_c) (should do more question)
# look_for(ces19phone, "gays")
ces19phone$gay_rights<-Recode(as.numeric(ces19phone$p35_c), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; -9=0.5; else=NA")
#checks
# table(ces19phone$gay_rights, ces19phone$p35_c , useNA = "ifany" )

#recode Abortion (missing)

#recode Stay Home (p20_e)
# look_for(ces19phone, "women")
ces19phone$stay_home<-Recode(as.numeric(ces19phone$p20_e), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; -9=0.5; else=NA")
#checks
# table(ces19phone$stay_home, ces19phone$p20_e , useNA = "ifany" )

#recode Moral Trad (abortion, lifestyles, stay home, values, marriage childen, morals)
ces19phone$trad1<-ces19phone$stay_home
ces19phone$trad2<-ces19phone$gay_rights
# table(ces19phone$trad1)
# table(ces19phone$trad2)

ces19phone %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", 1:7)), na.rm=T))->ces19phone

ces19phone %>%
  mutate(traditionalism2=rowMeans(select(., num_range("trad", 1:2)), na.rm=T))->ces19phone

#Check distribution of traditionalism
# qplot(ces19phone$traditionalism, geom="histogram")
# table(ces19phone$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces19phone %>%
  select(trad1, trad2) %>%
  psych::alpha(.)
#Check correlation
ces19phone %>%
  select(trad1, trad2) %>%
  cor(., use="complete.obs")


ces19phone %>%
  select(starts_with("trad")) %>%
  summary()

#recode 2nd Dimension (stay home, immigration, gay rights, crime)
ces19phone$author1<-ces19phone$stay_home
ces19phone$author2<-ces19phone$immigration_rates
ces19phone$author3<-ces19phone$gay_rights
ces19phone$author4<-ces19phone$crime
# table(ces19phone$author1)
# table(ces19phone$author2)
# table(ces19phone$author3)
# table(ces19phone$author4)
ces19phone %>%
  mutate(authoritarianism=rowMeans(select(. ,num_range("author", 1:4)), na.rm=T))->ces19phone

ces19phone %>%
  select(starts_with("author")) %>%
  summary()
#Check distribution of traditionalism
# qplot(ces19phone$authoritarianism, geom="histogram")
# table(ces19phone$authoritarianism, useNA="ifany")

#Calculate Cronbach's alpha
ces19phone %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces19phone %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")

#recode Ideology (p42)
# look_for(ces19phone, "scale")
ces19phone$ideology<-Recode(as.numeric(ces19phone$p42), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA")
#val_labels(ces19phone$ideology)<-c(Left=0, Right=1)
#checks
val_labels(ces19phone$ideology)
# table(ces19phone$ideology, ces19phone$p42 , useNA = "ifany" )

#### recode Immigration sentiment (p22_a) ####
# look_for(ces19phone, "immigr")
#ces19phone$immigration_job<-Recode(ces19phone$p22_a, "1=0; 2=0.25; 3=0.75; 4=1; -9=0.5; else=NA", as.numeric=T)
ces19phone$immigration_job<-Recode(as.numeric(ces19phone$p22_a), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#checks
# table(ces19phone$immigration_job, ces19phone$p22_a, useNA = "ifany" )

#### recode turnout (p2) ####
# look_for(ces19phone, "vote")
ces19phone$turnout<-Recode(ces19phone$p2, "1=1; 2:5=0; -9=0; else=NA")
val_labels(ces19phone$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces19phone$turnout)
# table(ces19phone$turnout)
# table(ces19phone$turnout, ces19phone$vote)

# recode satisfaction with democracy (q6, p4)
# look_for(ces19phone, "dem")
ces19phone$satdem<-Recode(as.numeric(ces19phone$p4), "1=1; 2=0.75; 3=0.25; 4=0; -9=0.5; else=NA", as.numeric=T)
#checks
# table(ces19phone$satdem, ces19phone$p4, useNA = "ifany" )

ces19phone$satdem2<-Recode(as.numeric(ces19phone$q6), "1=1; 2=0.75; 3=0.25; 4=0; -9=0.5; else=NA", as.numeric=T)
#checks
# table(ces19phone$satdem2, ces19phone$q6, useNA = "ifany" )

# recode Quebec Sovereignty (q43) (Right=more sovereignty)
# look_for(ces19phone, "sovereignty")
ces19phone$quebec_sov<-Recode(as.numeric(ces19phone$q43), "1=1; 2=0.75; -9=0.5; 3=0.25; 4=0; else=NA")
#checks
# table(ces19phone$quebec_sov, ces19phone$q43, useNA = "ifany" )
#val_labels(ces19phone$quebec_sov)<-NULL

# recode immigration society (p22_b)
# look_for(ces19phone, "culture")
ces19phone$immigration_soc<-Recode(as.numeric(ces19phone$p22_b), "5=0; 4=0.25; 2=0.75; 1=1; 3=0.5; -9=0.5; else=NA", as.numeric=T)
#checks
# table(ces19phone$immigration_soc, ces19phone$p22_b, useNA = "ifany" )

#recode Postgrad (q61)
# look_for(ces19phone, "education")
ces19phone$postgrad<-Recode(as.numeric(ces19phone$q61), "10:11=1; 1:9=0; else=NA")
#checks
# table(ces19phone$postgrad)

#recode efficacy rich (p20_n)
# look_for(ces19phone, "rich")
ces19phone$efficacy_rich<-Recode(as.numeric(ces19phone$p20_n), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
# table(ces19phone$efficacy_rich)
# table(ces19phone$efficacy_rich, ces19phone$p20_n)

# recode political interest (p27)
# look_for(ces19phone, "interest")
ces19phone$pol_interest<-Recode(as.numeric(ces19phone$p27), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
# table(ces19phone$pol_interest, ces19phone$p27, useNA = "ifany" )

#recode foreign born (q64)
# look_for(ces19phone, "born")
ces19phone$foreign<-Recode(ces19phone$q64, "1:2=0; 3:13=1; else=NA")
val_labels(ces19phone$foreign)<-c(No=0, Yes=1)
#checks
val_labels(ces19phone$foreign)
# table(ces19phone$foreign, ces19phone$q64, useNA="ifany")

#recode Environment Spend (q27_b)
# look_for(ces19phone, "env")
ces19phone$enviro_spend<-Recode(as.numeric(ces19phone$q27_b), "1=1; 2=0; 3=0.5; -9=0.5; else=NA")
#checks
# table(ces19phone$enviro_spend , ces19phone$q27_b , useNA = "ifany" )

#recode duty (q76 )
look_for(ces19phone, "duty")
ces19phone$duty<-Recode(ces19phone$q76 , "1=1; 2:3=0; else=NA")
val_labels(ces19phone$duty)<-c(No=0, Yes=1)
#checks
val_labels(ces19phone$duty)
table(ces19phone$duty, ces19phone$q76, useNA="ifany")

#### recode Women - how much should be done (p35_b) ####
look_for(ces19phone, "women")
table(ces19phone$p35_b)
ces19phone$women<-Recode(as.numeric(ces19phone$p35_b), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19phone$women,  useNA = "ifany")

#### recode Race - how much should be done (p35_a) ####
look_for(ces19phone, "racial")
table(ces19phone$p35_a)
ces19phone$race<-Recode(as.numeric(ces19phone$p35_a), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces19phone$race,  useNA = "ifany")

#recode Previous Vote (q60)
# look_for(ces19phone, "vote")
ces19phone$previous_vote<-Recode(ces19phone$q60, "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; else=NA")
val_labels(ces19phone$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces19phone$previous_vote)
table(ces19phone$previous_vote)

#glimpse(ces19phone)

# add mode
ces19phone$mode<-rep("Phone", nrow(ces19phone))
ces19phone$election<-rep(2019, nrow(ces19phone))
# Save the file
save(ces19phone, file=here("data/ces19phone.rda"))
