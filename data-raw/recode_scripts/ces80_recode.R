#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data

#ces7980<-read_sav(file=here("data-raw/1974_1979_1980.sav"))

#recode Gender (V2156)
# look_for(ces7980, "sex")
ces7980$male80<-Recode(ces7980$V2156, "1=1; 2=0; 0=NA")
val_labels(ces7980$male80)<-c(Female=0, Male=1)
#checks
val_labels(ces7980$male80)
table(ces7980$male80)
# No union respondent variable
#Convert from 1979


#No Union Household variable
#Convert from 1979


#No Education variable

#recode Region (V2002)
# look_for(ces7980, "province")
ces7980$region80<-Recode(ces7980$V2002, "0:3=1; 5=2; 6:9=3; 4=NA; 99=NA")
val_labels(ces7980$region80)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces7980$region80)
table(ces7980$region80)

#recode Quebec (V2002)
# look_for(ces7980, "province")
ces7980$quebec80<-Recode(ces7980$V2002, "0:3=0; 5:9=0; 4=1; 99=NA")
val_labels(ces7980$quebec80)<-c(Other=0, Quebec=1)
#checks
val_labels(ces7980$quebec80)
table(ces7980$quebec80)

#recode Age (V2155)
# look_for(ces7980, "age")
ces7980$age80<-ces7980$V1535
ces7980$age80<-Recode(ces7980$V2155, "0=NA")
#check
table(ces7980$age80)

#No Religion variable

#Recode Language (V2013)
# look_for(ces7980, "language")
ces7980$language80<-Recode(ces7980$V2013, "2=0; 1=1; 0=NA")
val_labels(ces7980$language80)<-c(French=0, English=1)
#checks
val_labels(ces7980$language80)
table(ces7980$language80)

#No Non-Charter Language variable

#No Employment variable

#No Sector variable

#recode Party ID (V2043)
# look_for(ces7980, "federal")
ces7980$party_id80<-Recode(ces7980$V2043, "1=1; 2=2; 3=3; 0=0; 4:7=0; else=NA")
val_labels(ces7980$party_id80)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces7980$party_id80)
table(ces7980$party_id80)

#recode Vote (V2062)
# look_for(ces7980, "vote")
ces7980$vote80<-Recode(ces7980$V2062, "1=1; 2=2; 3=3; 4:5=0; else=NA")
val_labels(ces7980$vote80)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces7980$vote80)
table(ces7980$vote80)

# No Occupation variable

# No Income variable


#recode Most Important Question (V2021)
look_for(ces7980, "issue")
ces7980 %>%
  mutate(mip80=Recode(ces7980$V2021,"0=NA; 1=18; 2=6;3=19; 4:5=7;
                      6:7=10;8=9;9=9;10:11=18;12=7;15:16=15;
                      17=19;18:19=15;20:25=16;29:43=0;50=5;51:52=9;53=5;54=5;
                      55:57=5;58=16;59:60=5;61=5;62=7;70:76=0;77:78=14;79=0;80:81=0;87=0;else=NA"))->ces7980

val_labels(ces7980$mip80)<-c(Other=0, Environment=1, Crime=2, Ethics=3, Education=4, Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
                             Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14, Social_Programs=15, Brokerage=16, Inflation=18, Housing=19)


# Foreign born
#recode foreign born (V1517)
look_for(ces7980, "birth")
ces7980$foreign80<-Recode(ces7980$V1517, "1=0; 2:12=1; else=NA")
val_labels(ces7980$foreign80)<-c(No=0, Yes=1)
#checks
val_labels(ces7980$foreign80)
table(ces7980$foreign80, ces7980$V1517, useNA="ifany")

#recode Liberal leader (V2080)
look_for(ces7980, "Trudeau")
ces7980$liberal_leader80<-Recode(ces7980$V2080, "0=NA")
#checks
table(ces7980$liberal_leader80)

#recode conservative leader (V2083)
look_for(ces7980, "Clark")
ces7980$conservative_leader80<-Recode(ces7980$V2083, "0=NA")
#checks
table(ces7980$conservative_leader80)

#recode NDP leader (V2086)
look_for(ces7980, "Broadbent")
ces7980$ndp_leader80<-Recode(ces7980$V2086, "0=NA")
#checks
table(ces7980$ndp_leader80)

#recode liberal rating (V2082)
look_for(ces7980, "therm")
ces7980$liberal_rating80<-Recode(ces7980$V2082, "0=NA")
#checks
table(ces7980$liberal_rating80)

#recode conservative rating (V2085)
look_for(ces7980, "therm")
ces7980$conservative_rating80<-Recode(ces7980$V2085, "0=NA")
#checks
table(ces7980$conservative_rating80)

#recode NDP rating (V2088)
look_for(ces7980, "therm")
ces7980$ndp_rating80<-Recode(ces7980$V2088, "0=NA")
#checks
table(ces7980$ndp_rating80)

#recode turnout (V2061)
look_for(ces7980, "vote")
ces7980$turnout80<-Recode(ces7980$V2061, "1=1; 2=0;  8=0; else=NA")
val_labels(ces7980$turnout80)<-c(No=0, Yes=1)
#checks
val_labels(ces7980$turnout80)
table(ces7980$turnout80)
table(ces7980$turnout80, ces7980$vote80)


ces7980 %>%
  mutate(election80=case_when(
    V4008==1 ~ 1980
  ))->ces7980
# Save the file
# Note: Do not save the ces7980.rda file out here
# because it will overwrite the recodes done in the
# ces7980_recode.R script
# Instead, if you look at the end, this scxript is source
# so it is executed after the general ces7980 recodes
# and then everything is saved out.
