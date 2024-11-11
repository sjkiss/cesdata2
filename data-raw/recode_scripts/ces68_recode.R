#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data

ces68<-read_dta(file=here("data-raw/ces1968.dta"))


#recode Gender (var401)
# look_for(ces68, "sex")
ces68$male<-Recode(ces68$var401, "1=1; 2=0")
val_labels(ces68$male)<-c(Female=0, Male=1)
names(ces68)
#checks
val_labels(ces68$male)
# table(ces68$male)

#recode Union Respondent (var363)
# look_for(ces68, "union")
ces68$var363
ces68$union<-Recode(ces68$var363, "1=0; 2:4=1; 5=NA")
val_labels(ces68$var363)
val_labels(ces68$union)<-c(None=0, Union=1)
#checks
val_labels(ces68$union)
# table(ces68$union)

#recode Union Combined (var363 and var379)
ces68 %>%
  mutate(union_both=case_when(
    var363==2 | var379==2 ~ 1,
    var363==3 | var379==3 ~ 1,
    var363==4 | var379==4 ~ 1,
    #This should only be missing if BOTH are not members, right?
    var363==1 | var379==1 ~ 0,
    #Note var379 is the spousal activity variable
    var379==0 ~ 0,
    #This should only be missing if BOTH are no reply, right?
    var363==5 | var379==5 ~ NA_real_

  ))->ces68

val_labels(ces68$union_both)<-c(None=0, Union=1)
#checks
val_labels(ces68$union_both)
# table(ces68$union_both)
# table(ces68$union_both, ces68$var363)
# table(ces68$union, ces68$var363,useNA = "ifany")
# table(ces68$union_both, ces68$var379,useNA = "ifany")

#recode Education (var334)

ces68$degree<-Recode(ces68$var334, "17:20=1; 25:26=1; 1:16=0; 21:24=0; 27=0; 30=NA")
val_labels(ces68$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces68$degree)
# table(ces68$degree)

#recode Region (var001)
# look_for(ces68, "province")
ces68$region<-Recode(ces68$var001, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces68$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces68$region)
# table(ces68$region)

#recode Province (var001)
# look_for(ces68, "province")
ces68$prov<-Recode(ces68$var001, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; 7=8; 8=9; 9=10")
val_labels(ces68$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces68$prov)
table(ces68$prov)

#recode Quebec (var001)
# look_for(ces68, "province")
ces68$quebec<-Recode(ces68$var001, "0:3=0; 5:9=0; 4=1")
val_labels(ces68$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces68$quebec)
# table(ces68$quebec)

#recode Age (age)
# look_for(ces68, "age")
ces68$age<-ces68$age
#check
# table(ces68$age)

#recode Religion (var340)
ces68$religion<-Recode(ces68$var340, "0=0; 10=0; 1=1; 2:6=2; 7:8=1; 11:19=2; 21:24=2; 26:32=2; 36:39=2; 41=2; 40=NA; else=3")
val_labels(ces68$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces68$religion)
# table(ces68$religion)

#recode Language (var357)
# look_for(ces68, "language")
ces68$language<-Recode(ces68$var357, "2=0; 1=1; else=NA")
val_labels(ces68$language)<-c(French=0, English=1)
#checks
val_labels(ces68$language)
# table(ces68$language)

#recode Non-charter Language (var357)
# look_for(ces68, "language")
ces68$non_charter_language<-Recode(ces68$var357, "1:2=0; 8=0; 0=1; 3:7=1; 9=1; else=NA")
val_labels(ces68$non_charter_language)<-c(Charter=0, Non_Charter=1)
#checks
val_labels(ces68$non_charter_language)
# table(ces68$non_charter_language)

#recode Employment (var324)
# look_for(ces68, "employ")
ces68$employment<-Recode(ces68$var324, "0=0; 8:9=0; 1:7=1; else=NA")
val_labels(ces68$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces68$employment)
# table(ces68$employment)

#recode Sector (var326)
# look_for(ces68, "sector")
# look_for(ces68, "business")
ces68$var326
# table(ces68$var326)
# table(as_factor(ces68$var326))
ces68$sector<-Recode(ces68$var326, "1:2=1; 4=1; 3=0; 0=0; 5:9=0; else=NA")
val_labels(ces68$sector)<-c(Private=0, Public=1)
#checks
val_labels(ces68$sector)
# table(ces68$sector)

#recode Party ID (var122)
# look_for(ces68, "affiliation")
ces68$party_id<-Recode(ces68$var122, "1=1; 2=2; 3=3; 4:11=0; else=NA")
val_labels(ces68$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces68$party_id)
# table(ces68$party_id)

#recode Party ID 2 (var122)
# look_for(ces68, "affiliation")
ces68$party_id2<-Recode(ces68$var122, "1=1; 2=2; 3=3; 4:11=0; else=NA")
val_labels(ces68$party_id2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces68$party_id2)
table(ces68$party_id2, ces68$var122)

#recode Vote (var180)
# look_for(ces68, "vote")
ces68$vote<-Recode(ces68$var180, "2=1; 3=2; 4=3; 5:8=0; else=NA")
val_labels(ces68$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces68$vote)
# table(ces68$vote)

#recode Occupation (var324)
# look_for(ces68, "occupation")
ces68$occupation<-Recode(ces68$var324, "1=1; 2=2; 3:4=3; 5=4; 6:7=5; else=NA")
val_labels(ces68$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces68$occupation)
# table(ces68$occupation)

#recode Income (var404)

# look_for(ces68, "income")
ces68$income<-Recode(ces68$var404, "1:3=1; 4:5=2; 6:7=3; 8:9=4; 0=5; else=NA")
val_labels(ces68$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces68$income)
# table(ces68$income)

#income2
# look_for(ces68, "income")
#Simon's Version'
# ces68$income2<-Recode(ces68$var404, "1:4=1; 5:7=2; 8:9=3; 0=4; else=NA")

ces68 %>%
  mutate(income2=case_when(
    var405==1 ~ 5,
    var404==1 ~ 1,
    var404==2 ~ 1,
    var404==3 ~ 1,
    var404==4 ~ 1,
    var404==5 ~ 2,
    var404==6 ~ 2,
    var404==7 ~ 2,
    var404==8 ~ 3,
    var404==9 ~ 3,
    var404==0 ~ 4,
  ))->ces68
# table(ces68$income2)
val_labels(ces68$income2)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)

#checks
val_labels(ces68$income)
# table(ces68$income)
#Terciles
ces68$income_tertile<-Recode(ces68$var404, "1:5=1; 6:8=2; 0=3; 9=3; else=NA")
prop.table(table(ces68$income_tertile))
val_labels(ces68$income_tertile)<-c(Lowest=1, Middle=2, Highest=3)


#recode Community Size (var002)
# look_for(ces68, "community")
ces68$size<-Recode(ces68$var002, "8:9=1; 7=2; 5:6=3; 4=4; 1:3=5; else=NA")
val_labels(ces68$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces68$size)
# table(ces68$size)

#recode Religiosity (var341)
# look_for(ces68, "church")
ces68$religiosity<-Recode(ces68$var341, "5=1; 4=2; 3=3; 2=4; 1=5; else=NA")
val_labels(ces68$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces68$religiosity)
# table(ces68$religiosity)

#recode Liberal leader (var208)
# look_for(ces68, "Trudeau")
ces68$liberal_leader<-Recode(as.numeric(ces68$var208), "2=10; 3=20; 4=30; 5=40; 6=50; 7=60; 8=70; 9=80; 10=90; 11=100; else=NA")
#checks
# table(ces68$liberal_leader)

#recode conservative leader (var209)
# look_for(ces68, "Stanfield")
ces68$conservative_leader<-Recode(as.numeric(ces68$var209), "2=10; 3=20; 4=30; 5=40; 6=50; 7=60; 8=70; 9=80; 10=90; 11=100; else=NA")
#checks
# table(ces68$conservative_leader)

#recode NDP leader (var210)
# look_for(ces68, "Douglas")
ces68$ndp_leader<-Recode(as.numeric(ces68$var210), "2=10; 3=20; 4=30; 5=40; 6=50; 7=60; 8=70; 9=80; 10=90; 11=100; else=NA")
#checks
# table(ces68$ndp_leader)

#recode liberal rating (var223)
# look_for(ces68, "liberal")
ces68$liberal_rating<-Recode(as.numeric(ces68$var223), "2=10; 3=20; 4=30; 5=40; 6=50; 7=60; 8=70; 9=80; 10=90; 11=100; else=NA")
#checks
# table(ces68$liberal_rating)

#recode conservative rating (var224)
# look_for(ces68, "conservative")
ces68$conservative_rating<-Recode(as.numeric(ces68$var224), "2=10; 3=20; 4=30; 5=40; 6=50; 7=60; 8=70; 9=80; 10=90; 11=100; else=NA")
#checks
# table(ces68$conservative_rating)

#recode NDP rating (var225)
# look_for(ces68, "ndp")
ces68$ndp_rating<-Recode(as.numeric(ces68$var225), "2=10; 3=20; 4=30; 5=40; 6=50; 7=60; 8=70; 9=80; 10=90; 11=100; else=NA")
#checks
# table(ces68$ndp_rating)

#recode Personal Retrospective (var113)
# look_for(ces68, "financial")
ces68$personal_retrospective<-Recode(as.numeric(ces68$var113), "1=1; 2=0; 3=0.5; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces68$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
#val_labels(ces68$personal_retrospective)
# table(ces68$personal_retrospective, ces68$var113 , useNA = "ifany" )

#recode turnout (var179)
# look_for(ces68, "vote")
ces68$turnout<-Recode(ces68$var179, "1=1; 2=0;  8=0; else=NA")
val_labels(ces68$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces68$turnout)
# table(ces68$turnout)
# table(ces68$turnout, ces68$vote)

#### recode political efficacy ####
#recode No Say (var089)
# look_for(ces68, "no say")
ces68$efficacy_internal<-Recode(as.numeric(ces68$var089), "1=0; 2=1; 3=0.5; else=NA", as.numeric=T)
#val_labels(ces68$efficacy_internal)<-c(low=0, high=1)
#checks
val_labels(ces68$efficacy_internal)
# table(ces68$efficacy_internal)
# table(ces68$efficacy_internal, ces68$var089 , useNA = "ifany" )

#recode MPs lose touch (var090)
# look_for(ces68, "touch")
ces68$efficacy_external<-Recode(as.numeric(ces68$var090), "1=0; 2=1; 3=0.5; else=NA", as.numeric=T)
#val_labels(ces68$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces68$efficacy_external)
# table(ces68$efficacy_external)
# table(ces68$efficacy_external, ces68$var090 , useNA = "ifany" )

#recode Official Don't Care (var087)
# look_for(ces68, "don't care")
ces68$efficacy_external2<-Recode(as.numeric(ces68$var087), "1=0; 2=1; 3=0.5; else=NA", as.numeric=T)
#val_labels(ces68$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces68$efficacy_external2)
# table(ces68$efficacy_external2)
# table(ces68$efficacy_external2, ces68$var087 , useNA = "ifany" )

ces68 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces68

ces68 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
# qplot(ces68$political_efficacy, geom="histogram")
# table(ces68$political_efficacy, useNA="ifany")

#recode foreign born (var352)
# look_for(ces68, "born")
ces68$foreign<-Recode(ces68$var352, "1=0; 2:11=1; else=NA")
val_labels(ces68$foreign)<-c(No=0, Yes=1)
#checks
#val_labels(ces68$foreign)
# table(ces68$foreign, ces68$var352, useNA="ifany")

#recode Most Important Question (var031)
# look_for(ces68, "most")
ces68$mip<-Recode(ces68$var031, "1=8; 2=15; 3=6; 4=19; 5=18; 6=16; 7=0; 8=14; 9=16; 10=12; 11=9; 12=4;
		                            13=11; 14=6; 15=7; 16=14; 17=15; 18:19=0; else=NA")
val_labels(ces68$mip)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                         Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Inflation=18, Housing=19)
# table(ces68$mip)

#recode Previous Vote (var192)
# look_for(ces68, "vote")
ces68$previous_vote<-Recode(ces68$var192, "2=1; 3=2; 4=3; 5:8=0; else=NA")
val_labels(ces68$previous_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
#val_labels(ces68$previous_vote)
#table(ces68$previous_vote)


#Empty variables that are not available pre-88
# ces68$redistribution<-rep(NA, nrow(ces68))
# ces68$market_liberalism<-rep(NA, nrow(ces68))
# ces68$traditionalism2<-rep(NA, nrow(ces68))
# ces68$immigration_rates<-rep(NA, nrow(ces68))

# Add mode
ces68$mode<-rep("Phone", nrow(ces68))

#Add Election
ces68$election<-c(rep(1968, nrow(ces68)))
glimpse(ces68)
save(ces68, file=here("data/ces68.rda"))
