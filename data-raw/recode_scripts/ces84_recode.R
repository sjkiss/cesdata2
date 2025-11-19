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
table(ces84$degree)

#recode Education 2 (VAR262)
# look_for(ces84, "education")
ces84$education<-Recode(ces84$VAR362, "1:3=1; 4=2; 5:6=3; 7=4; 8=5; 9=1; else=NA")
val_labels(ces84$education)<-c(Less_than_HS=1, HS=2, College=3, Some_uni=4, Bachelor=5)
#checks
val_labels(ces84$education)
table(ces84$education)

#recode Region (VAR003)
# look_for(ces84, "region")
ces84$region<-Recode(ces84$VAR003, "0:3=1; 5=2; 6:9=3; 4=NA")
val_labels(ces84$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces84$region)
# table(ces84$region)

#recode Province (VAR003)
# look_for(ces84, "province")
ces84$prov<-Recode(ces84$VAR003, "0=1; 1=2; 2=3; 3=4; 4=5; 5=6; 6=7; 7=8; 8=9; 9=10")
val_labels(ces84$prov)<-c(NL=1, PE=2, NS=3, NB=4, QC=5, ON=6, MB=7, SK=8, AB=9, BC=10)
#checks
val_labels(ces84$prov)
table(ces84$prov)

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

#recode Unemployed (VAR524)
# look_for(ces84, "employment")
ces84$unemployed<-Recode(ces84$VAR524, "4=1; 6=1; 1=0; else=NA")
val_labels(ces84$unemployed)<-c(Employed=0, Unemployed=1)
#checks
val_labels(ces84$unemployed)
table(ces84$unemployed)

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

#recode Party ID 2 (VAR081)
# look_for(ces84, "fed. id")
ces84$party_id2<-Recode(ces84$VAR081, "1=1; 2=2; 3=3; 0=0; 4:19=0; else=NA")
val_labels(ces84$party_id2)<-c(Other=0, Liberal=1, Conservative=2, NDP=3)
#checks
val_labels(ces84$party_id2)
 table(ces84$party_id2, ces84$VAR081)

 #recode Party closeness (VAR082 )
 look_for(ces84, "fed. id")
 ces84$party_close<-Recode(ces84$VAR082 , "1=1; 2=0.5; 3=0; else=NA")
 #checks
 table(ces84$VAR082  , ces84$party_close, useNA = "ifany" )

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
ces84 %>%
  rename(SOC=VAR526)->ces84
ces84 %>%
  filter(SOC==11) %>%
  select(SOC, VAR525)
val_labels(ces84$VAR525)
var_label(ces84$SOC)
ces84 %>%
  mutate(occupation_oesch=case_when(
    #Additional codes are taken from the codebook for the CES84
    #11 is teacher
    SOC==11~14,
    #machine operator
    SOC==12~7,
    #foreman
    SOC==13~6,
    #mechaniec
    SOC==14~7,
    #maintenance
    SOC==15~8,
    SOC==16~6,
    SOC==17~13,
SOC==18~6,
SOC==31~NA_integer_,
SOC==32~NA_integer_,
SOC==33~NA_integer_,
SOC==34~NA_integer_,
SOC>=1111&SOC<=1141~9,
SOC==1142~10,
SOC==1143~9,
SOC==1145~9,
SOC==1146~10,
SOC==1147|SOC==1149~9,
SOC>=1171&SOC<=1179~9,
SOC>=2111&SOC<=2119~5,
SOC>=2131&SOC<=2139~5,
SOC>=2140&SOC<=2159~5,
#Surveyors, Draughtspersons
SOC>=2160&SOC<=2161~6,
SOC>=2163&SOC<=2169~6,
SOC>=2181&SOC<=2189~5,
SOC==2311~9,
SOC>=2313&SOC<=2319~13,
SOC>=2331&SOC<=2339~14,
SOC>=2341&SOC<=2349~9,
#Library Supervisors
SOC==2350~13,
#Librarians, Archivists, Curators
SOC==2351~13,
SOC==2353|SOC==2359~14,
SOC==2391|SOC==2399~9,
SOC==2511|SOC==2519|SOC==2513~13,
SOC==2711|SOC==2719~13,
SOC==2731|SOC==2733~13,
SOC==2739~14,
SOC>=2791&SOC<=2799~14,
SOC>=3111&SOC<=3115~13,
SOC==3117~14,
SOC==3119~6,
#Nursing
SOC>=3130&SOC<=3139~14,
SOC==3151~13,
SOC>=3152&SOC<=3154~14,
SOC>=3155&SOC<=3157~6,
SOC==3158~15,
SOC>=3161&SOC<=3169~6,
SOC>=3311&SOC<=3339~14,
SOC>=3351&SOC<=3359~13,
SOC>=3360&SOC<=3373~15,
SOC==3375~15,
SOC==3379~15,
SOC>=4110&SOC<=4113~11,
#Accountant
SOC==4130~11,
SOC==4131~11,
#Cashiers
SOC==4133~12,
SOC>=4135&SOC<=4139~11,
SOC>=4140&SOC<=4143 ~11,
#Stock clerks, shipping receivers
SOC>=4150&SOC<=4159~11,
#Library clerks
SOC>=4160&SOC<=4169~11,
#Receptionists
SOC>=4170&SOC<=4176~11,
#Messengers telephone clerks
SOC>=4175&SOC<=4177~12,
#General reception
SOC==4179~11,
SOC>=4190&SOC<=4199~11,
SOC==5130~10,
SOC==5131~10,
SOC==5133~10,
SOC==5135~15,
SOC>=5141&SOC<=5149~16,
#Sales occupations services
SOC>=5170&SOC<=5179~10,
SOC==5190~10,
SOC==5191~10,
SOC>=5193&SOC<=5199~16,
SOC>=6111&SOC<=6113~15,
SOC==6115~16,
SOC==6116~9,
SOC==6117~10,
SOC==6119~15,
#food and beverage preparation
#restaurant supervisors
SOC==6120~16,
#Chef
SOC==6121~15,
#bartender
SOC>=6123&SOC<=6129~16,
#Hotel and Lodging
SOC==6135~12,
SOC>=6130&SOC<=6133~16,
SOC==6139~16,
#Personal Service
SOC==6141~15,
SOC==6142~16,
#Barbers
SOC==6143~15,
SOC==6144|SOC==6145~15,
SOC==6147~15,
SOC==6149~16,
# Apparel and Furnishings
SOC>=6160&SOC<=6169~16,
#janitors, elevators
SOC>=6190&SOC<=6199~16,
#Famers
SOC>=7113&SOC<=7119~7,
#Farm labourers
SOC==7180~6,
SOC>=7181&SOC<=7195~8,
SOC>=7196&SOC<=7197~7,
SOC==7199~8,
SOC==7311~6,
SOC>=7313&SOC<=7319~7,
#Forestry foreman
SOC==7510~6,
SOC==7511~7,
SOC==7513~8,
SOC==7516~7,
SOC>=7517&SOC<=7519~7,
SOC==7710~6,
SOC>=7711&SOC<=7719~8,
SOC==8110~6,
SOC>=8111&SOC<=8119~8,
SOC==8130~6,
SOC>=8133&SOC<=8143~7,
SOC==8146~6,
SOC>=8148&SOC<=8149~8,
SOC==8150~6,
SOC>=8151&SOC<=8155~8,
SOC==8158|SOC==8159~8,
SOC==8156~7,
SOC==8160~6,
SOC>=8160&SOC<=8173~8,
SOC==8176~7,
SOC>=8178&SOC<=8179~8,
SOC>=8210&SOC<=8227~7,
SOC>=8228&SOC<=8229~8,
SOC==8230~6,
SOC>=8231&SOC<=8235~7,
SOC==8236~7,
SOC>=8238&SOC<=8239~8,
SOC==8250~6,
SOC>=8251&SOC<=8253~8,
SOC==8256~7,
SOC==8258|SOC==8259~8,
SOC==8250~6,
SOC>=8261&SOC<=8299~8,
SOC==8290~6,
SOC>=8293&SOC<=8295~7,
SOC==8296~7,
SOC>=8298&SOC<=8299~8,
SOC==8310~6,
SOC>=8311&SOC<=8319~7,
SOC==8330~6,
SOC>=8331&SOC<=8339~7,
SOC==8350~6,
SOC>=8351&SOC<=8351~7,
SOC==8370~6,
SOC>=8371&SOC<=8379~7,
SOC==8390~6,
SOC>=8391&SOC<=8399~7,
SOC==8510~6,
SOC>=8511&SOC<=8525~8,
SOC==8526~7,
SOC>=8527&SOC<=8529~8,
SOC==8530~6,
SOC>=8531&SOC<=8535~8,
SOC>=8536&SOC<=8537~7,
SOC>=8538&SOC<=8539~8,
SOC==8540~6,
SOC==8541~7,
SOC==8546~7,
SOC==8548|SOC==8549~8,
SOC==8550~6,
SOC>=8551&SOC<=8569~7,
SOC==8570~6,
SOC>=8571&SOC<=8575~8,
SOC==8576~7,
SOC>=8578&SOC<=8579~8,
SOC==8580~6,
SOC>=8581&SOC<=8589~7,
SOC==8590~6,
SOC>=8591&SOC<=8595~8,
SOC==8596~7,
SOC==8598|SOC==8599~8,
SOC==8710~6,
SOC>=8711&SOC<=8719~8,
SOC==8730~6,
SOC>=8731&SOC<=8739~7,
SOC==8780~6,
SOC>=8781&SOC<=8797~7,
SOC==8798|SOC==8799~8,
SOC==9111~6,
SOC==9113~6,
SOC==9110~6,
SOC==9119~11,
SOC==9130~6,
SOC==9133~7,
SOC==9131~7,
SOC==9135~8,
#This one is probably a mistake
# but was coded by 84 as afarmer.
SOC==7179~7,
SOC==9139~8,
SOC==9155~8,
SOC==9157~8,
#bus driver
SOC==9170~6,
SOC==9171~15,
#Truck drivers
SOC==9175~7,
SOC==9179~15,
SOC==9310~6,
SOC>=9311&SOC<=9319~8,
SOC==9510~6,
SOC>=9511&SOC<=9519~7,
SOC>=9530&SOC<=9539~6,
SOC>=9550&SOC<=9559~6,
SOC==9910~6,
SOC==9916~7,
SOC==9918~8,
SOC==9590~6,
SOC==9591~7,
SOC==9599~7,
SOC==9919~NA_integer_
  ))->ces84
val_labels(ces84$occupation_oesch)<-c(`Technical experts`=5, `Technicians`=6,
                                      `Skilled manual`=7, `Low-skilled manual`=8,
                                      'Higher-grade managers'=9, `Lower-grade managers`=10,
                                      `Skilled clerks`=11, `Unskilled clerks`=12,
                                      `Socio-cultural professionals`=13, `Socio-cultural (semi-professionals)`=14,
                                      `Skilled service`=15, `Low-skilled service`=16)


#Check
with(ces84, table(as_factor(occupation_oesch)))

#This deletes self-employed from oesch
ces84 %>%
  mutate(occupation_oesch=case_when(
    VAR533==2~occupation_oesch,
    VAR533==1~NA_integer_
  ))->ces84
#Create occupation_oesch_5
ces84 %>%
  mutate(occupation_oesch_5=case_when(
    VAR533==1~3,
    occupation_oesch==1|occupation_oesch==2|occupation_oesch==5|occupation_oesch==9|occupation_oesch==13~1,
    occupation_oesch==6|occupation_oesch==10|occupation_oesch==14~2,
    occupation_oesch==7|occupation_oesch==11|occupation_oesch==15~4,
    occupation_oesch==8|occupation_oesch==12|occupation_oesch==16~5
  ))->ces84
val_labels(ces84$occupation_oesch_5)<-c(`Higher-grade service`=1,
                                         `Lower-grade service`=2,
                                         `Self-employed`=3,
                                         `Skilled manual`=4,
                                         `Unskilled manual`=5)

# Test
#Let's compare
# ces84 %>%
#   count(VAR525, SOC,occupation_oesch) %>%
#   as_factor() %>% view()

# #Add occupation Oesch 6
# ces84 %>%
#   mutate(occupation_oesch_6=case_when(
#
#   ))
# ces84 %>%
#   mutate(occupation_oesch_6=case_when(
# #Managers
#     VAR533==1~"Self-employed",
#     VAR525==2~"Managers",
#     VAR525==1~"Professionals",
#     VAR525==5~"Skilled Workers",
#     VAR525==6~"Unskilled Workers",
#     VAR525==4~"Unskilled Workers",
#     VAR525==3~"Semi-Professionals Associate Managers",
#     VAR525==7~"Skilled Workers"
#   ))->ces84
#ces84$occupation_oesch_6<-factor(ces84$occupation_oesch_6, levels=c("Unskilled Workers", "Skilled Workers",
#                                          "Semi-Professionals Associate Managers",
#                                          "Self-employed","Professionals", "Managers"))
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

#recode Provincial Vote (VAR262)
# look_for(ces84, "vote")
ces84$prov_vote<-car::Recode(as.numeric(ces84$VAR262), "1=1; 2=2; 3=3; 4=0; 6=4; 7:14=0; else=NA")
val_labels(ces84$prov_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, PQ=4, Green=5)
#checks
val_labels(ces84$prov_vote)
table(ces84$prov_vote)

#Empty variables that are not available pre-88
# ces84$redistribution<-rep(NA, nrow(ces84))
# ces84$market_liberalism<-rep(NA, nrow(ces84))
# ces84$traditionalism2<-rep(NA, nrow(ces84))
# ces84$immigration_rates<-rep(NA, nrow(ces84))
glimpse(ces84)
lookfor(ces84, "home")


#### Inequality - problem (VAR324) ####
ces84$inequality<-Recode(as.numeric(ces84$VAR324), "1=1; 2=0.75; 7=0.5; 3=0.25; 4=0; else=NA")
#checks
table(ces84$inequality,ces84$VAR324, useNA = "ifany")

#### Redistribution - high taxes for rich (VAR329) #### (Left-Right)
ces84$redistribtion_tax<-Recode(as.numeric(ces84$VAR329), "1=0; 2=0.25; 7=0.5; 3=0.75; 4=1; else=NA")
#checks
table(ces84$redistribtion_tax,ces84$VAR329, useNA = "ifany")

#recode Homeowner (VAR418)
ces84$homeowner<-Recode(as.numeric(ces84$VAR418), "1=1; 2=0; else=NA")
#checks
table(ces84$homeowner,ces84$VAR418, useNA = "ifany")

#### Add Subjective social class ####

lookfor(ces84, "belong")

table(ces84$VAR307, useNA = "ifany")
ces84 %>%
  mutate(sub_class=case_when(
    VAR307==1|VAR308==1~"Upper Class",
    VAR307==2|VAR308==2~"Upper-Middle Class",
    VAR307==3|VAR308==3~"Middle Class",
    VAR307==4|VAR308==4~"Working Class",
    VAR307==5|VAR308==5~"Lower Class",
    TRUE~NA_character_
  ))->ces84
#table(ces84$VAR307, ces84$sub_class, useNA = "ifany")
#table(ces84$VAR307, ces84$VAR308, useNA = "ifany")
ces84$sub_class<-factor(ces84$sub_class, levels=c("Lower Class", "Working Class", "Middle Class", "Upper-Middle Class", "Upper Class"))
#table(ces84$sub_class)


# recode Subjective class - belong to class (VAR306)
look_for(ces84, "class")
ces84$class_belong<-Recode(ces84$VAR306, "2=0; 1=1; else=NA")
val_labels(ces84$class_belong)<-c(No=0, Yes=1)
#checks
val_labels(ces84$class_belong)
table(ces84$class_belong)

# recode Subjective class - closeness to others in class (VAR309)
look_for(ces84, "class")
ces84$class_close<-Recode(ces84$VAR309, "3=0; 1=1; 2=0.5; else=NA")
val_labels(ces84$class_close)<-c(Not_close=0, Fairly_close=0.5, Very_close=1)
#checks
val_labels(ces84$class_close)
table(ces84$class_close)

# recode Class conflict (VAR310)
look_for(ces84, "class")
ces84$class_conflict<-Recode(ces84$VAR310, "2=0; 1=1; else=NA")
val_labels(ces84$class_conflict)<-c(No=0, Yes=1)
#checks
val_labels(ces84$class_conflict)
table(ces84$class_conflict)

# recode Liberal class favouritism (VAR062)
look_for(ces84, "class")
ces84$liberal_class<-Recode(as.numeric(ces84$VAR062) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces84$liberal_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces84$liberal_class)
table(ces84$liberal_class, ces84$VAR062  , useNA = "ifany")

# recode Conservative class favouritism (VAR063)
ces84$conservative_class<-Recode(as.numeric(ces84$VAR063) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces84$conservative_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces84$conservative_class)
table(ces84$conservative_class, ces84$VAR063  , useNA = "ifany")

# recode NDP class favouritism (VAR064)
ces84$ndp_class<-Recode(as.numeric(ces84$VAR064) , "1=0; 2=0.17; 3=0.33; 4=0.5; 5=0.67; 6=0.83; 7=1; else=NA")
#val_labels(ces84$ndp_class<-c(Working_class=0, Middle_class=1)
#checks
#val_labels(ces84$ndp_class)
table(ces84$ndp_class, ces84$VAR064  , useNA = "ifany")

#### Add Own vs Rent ####
ces84 %>%
  mutate(own_rent=case_when(
    VAR418==1~'Own',
    VAR418==2~'Rent',
    VAR418==8~'Other'
  ))->ces84

ces84$own_rent<-factor(ces84$own_rent, levels=c("Own", "Rent", "Other"))

table(ces84$own_rent)

# recode Housing - gov't provide adequate (VAR322)
look_for(ces84, "housing")
ces84$housing<-Recode(as.numeric(ces84$VAR322) , "1=0; 2=0.25; 7=0.5; 3=0.75; 4=1; else=NA")
#val_labels(ces84$home_future)<-c(Agree=0, Disagree=1) # LEFT TO rIGHT #
#checks
#val_labels(ces84b$housing)
table(ces84$housing)

# Gay rights - homosexual teachers (VAR336)
look_for(ces84, "homo")
ces84$trad2<-Recode(as.numeric(ces84$VAR336) , "1=0; 2=0.25; 7=0.5; 3=0.75; 4=1; else=NA")
#checks
table(ces84$trad2)

# Abortion (VAR335)
look_for(ces84, "abortion")
ces84$abortion<-Recode(as.numeric(ces84$VAR335) , "1=0; 2=0.25; 7=0.5; 3=0.75; 4=1; else=NA")
#checks
table(ces84$abortion)

#recode Women's movement thermometer (VAR291)
ces84$feminism_rating<-Recode(as.numeric(ces84$VAR291 /100), "9.97:9.99=NA")
table(ces84$feminism_rating)

#recode non-whites thermometer (VAR290)
ces84$racial_rating<-Recode(as.numeric(ces84$VAR290 /100), "9.97:9.99=NA")
table(ces84$racial_rating)

#recode whites thermometer (VAR289)
ces84$whites_rating<-Recode(as.numeric(ces84$VAR289 /100), "9.97:9.99=NA")
table(ces84$whites_rating)

#recode francophone thermometer (VAR286)
ces84$francophone_rating<-Recode(as.numeric(ces84$VAR286 /100), "9.97:9.99=NA")
table(ces84$francophone_rating)

#recode CEOs thermometer (VAR296)
ces84$CEOs_rating<-Recode(as.numeric(ces84$VAR296 /100), "9.97:9.99=NA")
table(ces84$CEOs_rating)


#Add mode

ces84$mode<-rep("Phone", nrow(ces84))
#Add Election
ces84$election<-rep(1984, nrow(ces84))
# Save the file
save(ces84, file=here("data/ces84.rda"))
# names(ces84)
# table(as_factor(ces84$occupation_oesch))
