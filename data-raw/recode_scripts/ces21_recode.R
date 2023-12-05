
#File to Recode 2021 CES Data
#Load original file
library(haven)
library(here)
library(car)
library(labelled)
library(stringr)
library(here)
#Load data
ces21<-read_dta(file=here("data-raw/ces21.dta"))


#recode Gender
ces21$male<-Recode(ces21$cps21_genderid, "1=1; 2=0; else=NA")
val_labels(ces21$male)<-c(Female=0, Male=1)
#checks
val_labels(ces21$male)
table(ces21$male)

# recode Union Household (cps21_union)
look_for(ces21, "union")
ces21$union<-Recode(ces21$cps21_union, "1=1; 2=0; else=NA")
val_labels(ces21$union)<-c(None=0, Union=1)
# Checks
val_labels(ces21$union)
table(ces21$union , useNA = "ifany" )

#Union Combined variable (identical copy of union) ### Respondent only
ces21$union_both<-ces21$union
#checks
val_labels(ces21$union_both)
table(ces21$union_both , useNA = "ifany" )

#recode Education (cps21_education)
look_for(ces21, "education")
ces21$degree<-Recode(ces21$cps21_education, "9:11=1; 1:8=0; else=NA")
val_labels(ces21$degree)<-c(nodegree=0, degree=1)
#checks
val_labels(ces21$degree)
table(ces21$degree ,ces21$cps21_education , useNA = "ifany" )

#recode Region (cps21_province)
look_for(ces21, "province")
ces21$region<-Recode(ces21$cps21_province, "1:3=3; 4:5=1; 7=1; 9=2; 10=1; 12=3; else=NA")
val_labels(ces21$region)<-c(Atlantic=1, Ontario=2, West=3)
#checks
val_labels(ces21$region)
table(ces21$region , ces21$cps21_province , useNA = "ifany" )

#recode Quebec (cps21_province)
look_for(ces21, "province")
ces21$quebec<-Recode(ces21$cps21_province, "1:5=0; 7=0; 9:10; 11=1; 12=0; else=NA")
val_labels(ces21$quebec)<-c(Other=0, Quebec=1)
#checks
val_labels(ces21$quebec)
table(ces21$quebec ,ces21$cps21_province , useNA = "ifany" )

#recode Age (cps21_age)
look_for(ces21, "age")
ces21$age<-ces21$cps21_age
#checks
table(ces21$age)

#recode Religion (cps21_religion)
look_for(ces21, "relig")
ces21$religion<-Recode(ces21$cps21_religion, "1:2=0; 3:7=3; 8:9=2; 10:11=1; 12:21=2; 22=3; else=NA")
val_labels(ces21$religion)<-c(None=0, Catholic=1, Protestant=2, Other=3)
#checks
val_labels(ces21$religion)
table(ces21$religion , ces21$cps21_religion , useNA = "ifany" )

#recode Language (UserLanguage)
look_for(ces21, "language")
ces21$language<-Recode(ces21$UserLanguage, "'FR-CA'=0; 'EN'=1; else=NA")
val_labels(ces21$language)<-c(French=0, English=1)
#checks
val_labels(ces21$language)
table(ces21$language)

#recode Non-charter Language (pes21_lang)
look_for(ces21, "language")
ces21$non_charter_language<-Recode(ces21$pes21_lang, "1:2=1; 3:18=0; else=NA")
val_labels(ces21$non_charter_language)<-c(Charter=0, Non_Charter=1)
table(as_factor(ces21$pes21_lang),ces21$non_charter_language , useNA = "ifany" )
#checks
val_labels(ces21$non_charter_language)
table(ces21$non_charter_language)

#recode Employment (cps21_employment)
look_for(ces21, "employment")
ces21$employment<-Recode(ces21$cps21_employment, "3:8=0; 1:2=1; 9:11=1; else=NA")
val_labels(ces21$employment)<-c(Unemployed=0, Employed=1)
#checks
val_labels(ces21$employment)
table(ces21$employment , ces21$cps21_employment , useNA = "ifany" )

#No Sector available

#recode Party ID (pid_party_en)
look_for(ces21, "pid")
ces21$party_id<-Recode(ces21$pid_party_en, "'Liberal Party'=1; 'Conservative Party'=2; 'NDP'=3; 4='Bloc Québécois'; 'Green Party'=5; else=NA")
val_labels(ces21$party_id)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces21$party_id)
table(ces21$party_id, ces21$pid_party_en , useNA = "ifany" )

#recode Vote (pes21_votechoice2021)
look_for(ces21, "party did you vote")
ces21$vote<-Recode(ces21$pes21_votechoice2021, "1=1; 2=2; 3=3; 4=4; 5=5; 7=0; 6=6; else=NA")
val_labels(ces21$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5, PPC=6)
#checks
val_labels(ces21$vote)
table(ces21$vote , ces21$pes21_votechoice2021 , useNA = "ifany" )

# #recode Occupation (pes21_occ_text)
# look_for(ces21, "occupation")
# val_labels(ces21$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#
# #recode Occupation3 as 6 class schema with self-employed (pes21_occ_text)
# look_for(ces21, "employ")
# ces21$occupation3<-ifelse(ces21$cps21_employment==3, 6, ces21$occupation)
# val_labels(ces21$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
# #checks
# val_labels(ces21$occupation3)
# table(ces21$occupation3, ces21$q68 , useNA = "ifany" )

#recode Income (cps21_income_cat & cps21_income_number)
look_for(ces21, "income")
ces21 %>%
  mutate(income=case_when(
    cps21_income_cat==1 | cps21_income_number> -1 & cps21_income_number < 31001 ~ 1,
    cps21_income_cat==2 | cps21_income_number> 30000 & cps21_income_number < 61001 ~ 2,
    cps21_income_cat==3 | cps21_income_number> 60000 & cps21_income_number < 91001 ~ 3,
    cps21_income_cat==4 | cps21_income_number> 90000 & cps21_income_number < 151001 ~ 4,
    cps21_income_cat==5 | cps21_income_number> 150000 & cps21_income_number < 99999999999999999 ~ 5,
  ))->ces21
val_labels(ces21$income)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces21$income)
table(ces21$income, ces21$cps21_income_cat , useNA = "ifany" )

#recode Religiosity (cps21_rel_imp)
look_for(ces21, "relig")
ces21$religiosity<-Recode(ces21$cps21_rel_imp, "1=5; 2=4; 5=3; 3=2; 4=1; else=NA")
val_labels(ces21$religiosity)<-c(Lowest=1, Lower_Middle=2, Middle=3, Upper_Middle=4, Highest=5)
#checks
val_labels(ces21$religiosity)
table(ces21$religiosity, ces21$cps21_rel_imp , useNA = "ifany")

#recode Community Size (pes21_rural_urban)
look_for(ces21, "urban")
ces21$size<-Recode(ces21$pes21_rural_urban, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA")
val_labels(ces21$size)<-c(Rural=1, Under_10K=2, Under_100K=3, Under_500K=4, City=5)
#checks
val_labels(ces21$size)
table(ces21$size, ces21$pes21_rural_urban , useNA = "ifany" )

#recode Native-born (cps21_bornin_canada)
ces21$native<-Recode(ces21$cps21_bornin_canada, "1=1; 2=0; else=NA")
val_labels(ces21$native)<-c(Foreign=0, Native=1)
#checks
val_labels(ces21$native)
table(ces21$native, ces21$cps21_bornin_canada , useNA = "ifany" )

#recode Previous Vote (cps21_vote_2019)
look_for(ces21, "party did you vote")
ces21$past_vote<-Recode(ces21$cps21_vote_2019, "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; else=NA")
val_labels(ces21$past_vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
#checks
val_labels(ces21$past_vote)
table(ces21$past_vote, ces21$cps21_vote_2019 , useNA = "ifany" )

#recode Liberal leader
ces21$liberal_leader2<-Recode(as.numeric(ces21$cps21_lead_rating_23), "-99=NA")
#checks
ces21$liberal_leader<-(ces21$liberal_leader2 /100)
table(ces21$liberal_leader)

#recode Conservative leader
ces21$conservative_leader2<-Recode(as.numeric(ces21$cps21_lead_rating_24), "-99=NA")
#checks
ces21$conservative_leader<-(ces21$conservative_leader2 /100)
table(ces21$conservative_leader)

#recode NDP leader
ces21$ndp_leader2<-Recode(as.numeric(ces21$cps21_lead_rating_25), "-99=NA")
#checks
ces21$ndp_leader<-(ces21$ndp_leader2 /100)
table(ces21$ndp_leader)

#recode Bloc leader
ces21$bloc_leader2<-Recode(as.numeric(ces21$cps21_lead_rating_26), "-99=NA")
#checks
ces21$bloc_leader<-(ces21$bloc_leader2 /100)
table(ces21$bloc_leader)

#recode Green leader
ces21$green_leader2<-Recode(as.numeric(ces21$cps21_lead_rating_27), "-99=NA")
#checks
ces21$green_leader<-(ces21$green_leader2 /100)
table(ces21$green_leader)

#recode PPC leader
ces21$ppc_leader2<-Recode(as.numeric(ces21$cps21_lead_rating_29), "-99=NA")
#checks
ces21$ppc_leader<-(ces21$ppc_leader2 /100)
table(ces21$ppc_leader)

#recode Environment (cps21_spend_env)
look_for(ces21, "enviro")
ces21$environment<-Recode(as.numeric(ces21$cps21_spend_env), "3=0.5; 1=1; 2=0; else=NA")
#val_labels(ces21$environment)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
#val_labels(ces21$environment)
table(ces21$environment, ces21$cps21_spend_env , useNA = "ifany" )

#recode Redistribution (pes21_gap)
look_for(ces21, "rich")
ces21$redistribution<-Recode(as.numeric(ces21$pes21_gap), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; else=NA", as.numeric=T)
#val_labels(ces21$redistribution)<-c(Much_less=0, Somewhat_less=0.25, Same_amount=0.5, Somewhat_more=0.75, Much_more=1)
#checks
table(ces21$redistribution)

#recode Pro-Redistribution (p44)
ces21$pro_redistribution<-Recode(ces21$pes21_gap, "1:2=1; 3:5=0; else=NA", as.numeric=T)
val_labels(ces21$pro_redistribution)<-c(Non_Pro=0, Pro=1)
#checks
val_labels(ces21$pro_redistribution)
table(ces21$pro_redistribution)

#recode Liberal rating
look_for(ces21, "parties")
ces21$liberal_rating<-Recode(as.numeric(ces21$cps21_party_rating_23), "-99=NA")
table(ces21$liberal_rating)

#recode Conservative rating
ces21$conservative_rating<-Recode(as.numeric(ces21$cps21_party_rating_24), "-99=NA")
table(ces21$conservative_rating)

#recode NDP rating
ces21$NDP_rating<-Recode(as.numeric(ces21$cps21_party_rating_25), "-99=NA")
table(ces21$NDP_rating)

#recode Bloc rating
ces21$bloc_rating<-Recode(as.numeric(ces21$cps21_party_rating_26), "-99=NA")
table(ces21$bloc_rating)

#recode Green rating
ces21$green_rating<-Recode(as.numeric(ces21$cps21_party_rating_27), "-99=NA")
table(ces21$green_rating)

#recode PPC rating
ces21$ppc_rating<-Recode(as.numeric(ces21$cps21_party_rating_29), "-99=NA")
table(ces21$ppc_rating)

#Calculate Cronbach's alpha
library(psych)

#recode Market Liberalism (pes21_privjobs and pes21_blame)
look_for(ces21, "leave")
look_for(ces21, "blame")

ces21$market1<-Recode(as.numeric(ces21$pes21_privjobs), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
ces21$market2<-Recode(as.numeric(ces21$pes21_blame), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA", as.numeric=T)
#checks
table(ces21$market1, ces21$pes21_privjobs , useNA = "ifany" )
table(ces21$market2, ces21$pes21_blame , useNA = "ifany" )
ces21$market1
ces21 %>%
  mutate(market_liberalism=rowMeans(select(., num_range("market", 1:2)), na.rm=T))->ces21

ces21 %>%
  select(starts_with("market")) %>%
  summary()
#Check distribution of market_liberalism
qplot(ces21$market_liberalism, geom="histogram")
table(ces21$market_liberalism, useNA="ifany")

#Calculate Cronbach's alpha
ces21 %>%
  select(market1, market2) %>%
  alpha(.)
#For some reason the cronbach's alpha doesn't work here.
#Check correlation
ces21 %>%
  select(market1, market2) %>%
  cor(., use="complete.obs")

#recode Quebec Accommodation (pes21_doneqc ) (Left=more accom)
look_for(ces21, "quebec")
ces21$quebec_accom<-Recode(as.numeric(ces21$pes21_doneqc), "2=0.25; 1=0; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces21$quebec_accom)

#recode Personal Retrospective (cps21_own_fin_retro)
look_for(ces21, "situation")
ces21$personal_retrospective<-Recode(as.numeric(ces21$cps21_own_fin_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
#val_labels(ces21$personal_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces21$personal_retrospective)
table(ces21$personal_retrospective , ces21$cps21_own_fin_retro, useNA = "ifany" )

#recode National Retrospective (cps21_econ_retro)
look_for(ces21, "economy")
ces21$national_retrospective<-Recode(as.numeric(ces21$cps21_econ_retro), "1=1; 2=0.5; 3=0; 4=0.5; else=NA", as.numeric=T)
val_labels(ces21$national_retrospective)<-c(Worse=0, Same=0.5, Better=1)
#checks
val_labels(ces21$national_retrospective)
table(ces21$national_retrospective, ces21$cps21_econ_retro, useNA = "ifany" )

#recode Education (cps21_spend_educ)
look_for(ces21, "education")
ces21$education<-Recode(as.numeric(ces21$cps21_spend_educ), "3:4=0.5; 1=1; 2=0; else=NA")
#val_labels(ces21$education)<-c(Spend_less=0, Spend_same=0.5, Spend_more=1)
#checks
#val_labels(ces21$education)
table(ces21$education, ces21$cps21_spend_educ , useNA = "ifany" )

#recode Break Promise (pes21_keepromises)
look_for(ces21, "promise")
ces21$promise<-Recode(as.numeric(ces21$pes21_keepromises), "1=0; 2=0.5; 3=1; 4=1; 5=0.5; else=NA", as.numeric=T)
val_labels(ces21$promise)<-c(low=0, high=1)
#checks
val_labels(ces21$promise)
table(ces21$promise)
table(ces21$promise, ces21$pes21_keepromises , useNA = "ifany" )

#recode Trust (pes21_trust)
look_for(ces21, "trust")
val_labels(ces21$pes21_trust)
ces21$trust<-Recode(ces21$pes21_trust, "1=1; 2=0; else=NA", as.numeric=T)
val_labels(ces21$trust)<-c(no=0, yes=1)
#checks
val_labels(ces21$trust)
table(ces21$trust)
table(ces21$trust, ces21$pes21_trust , useNA = "ifany" )


#recode Immigration (cps21_imm)
look_for(ces21, "admit")
ces21$immigration_rates<-Recode(as.numeric(ces21$cps21_imm), "1=0; 2=1; 3=0.5; 4=0.5; else=NA", as.numeric=T)
#checks
table(ces21$immigration_rates, ces21$cps21_imm , useNA = "ifany" )

#recode Environment (cps21_spend_env) (spending question)
look_for(ces21, "env")
ces21$enviro<-Recode(as.numeric(ces21$cps21_spend_env), "1=0; 2=0.5; 3=1; else=NA")
#checks
table(ces21$enviro , ces21$cps21_spend_env , useNA = "ifany" )

#recode Capital Punishment (Missing)

#recode Crime (cps21_spend_just_law) (spending question)
look_for(ces21, "crime")
ces21$crime<-Recode(as.numeric(ces21$cps21_spend_just_law), "1=0; 2=0; 3=1; else=NA")
#checks
table(ces21$crime, ces21$cps21_spend_just_law , useNA = "ifany" )

#recode Gay Rights (pes21_donegl) (should do more question)
look_for(ces21, "gays")
ces21$gay_rights<-Recode(as.numeric(ces21$pes21_donegl), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces21$gay_rights, ces21$pes21_donegl , useNA = "ifany" )

#recode Abortion (pes21_abort2)
look_for(ces21, "abortion")
ces21$abortion<-Recode(as.numeric(ces21$pes21_abort2), "1=1; 2=0.5; 3=0; else=NA")
#checks
table(ces21$abortion, ces21$pes21_abort2 , useNA = "ifany" )

#recode Lifestyle (pes21_newerlife)
look_for(ces21, "lifestyle")
ces21$lifestyles<-Recode(as.numeric(ces21$pes21_newerlife), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces21$lifestyles, ces21$pes21_newerlife, useNA = "ifany")

#recode Women (pes21_donew)
look_for(ces21, "women")
ces21$women<-Recode(as.numeric(ces21$pes21_donew), "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1; else=NA")
#checks
table(ces21$women, ces21$pes21_donew, useNA = "ifany")

#recode Moral Trad (abortion, lifestyles, stay home, values, marriage, childen, morals)
ces21$trad1<-ces21$women
ces21$trad2<-ces21$gay_rights
ces21$trad3<-ces21$abortion
table(ces21$trad1)
table(ces21$trad2)
table(ces21$trad3)
ces21 %>%
  mutate(traditionalism=rowMeans(select(., num_range("trad", 1:3)), na.rm=T))->ces21
#Check distribution of traditionalism
qplot(ces21$traditionalism, geom="histogram")
table(ces21$traditionalism, useNA="ifany")

#Calculate Cronbach's alpha
ces21 %>%
  select(trad1, trad2, trad3) %>%
  psych::alpha(.)
#Check correlation
ces21 %>%
  select(trad1, trad2, trad3) %>%
  cor(., use="complete.obs")

#recode Moral Traditionalism 2 (women & gay rights) (Left-Right)
ces21 %>%
  mutate(traditionalism2=rowMeans(select(., num_range("trad", 1:2)), na.rm=T))->ces21


#Check distribution of traditionalism2
qplot(ces21$traditionalism2, geom="histogram")
table(ces21$traditionalism2, useNA="ifany")

#Calculate Cronbach's alpha
ces21 %>%
  select(trad1, trad2) %>%
  psych::alpha(.)
#Check correlation
ces21 %>%
  select(trad1, trad2) %>%
  cor(., use="complete.obs")

#recode 2nd Dimension (lifestyles, immigration, gay rights, crime)
ces21$author1<-ces21$lifestyles
ces21$author2<-ces21$immigration_rates
ces21$author3<-ces21$gay_rights
ces21$author4<-ces21$crime
table(ces21$author1)
table(ces21$author2)
table(ces21$author3)
table(ces21$author4)

ces21 %>%
  mutate(authoritarianism=rowMeans(select(. ,num_range("author", 1:4)), na.rm=T))->ces21


ces21 %>%
  select(starts_with("author")) %>%
  summary()
tail(names(ces21))
#Check distribution of traditionalism
qplot(ces21$authoritarianism, geom="histogram")


#Calculate Cronbach's alpha
ces21 %>%
  select(author1, author2, author3, author4) %>%
  psych::alpha(.)

#Check correlation
ces21 %>%
  select(author1, author2, author3, author4) %>%
  cor(., use="complete.obs")

#recode Ideology (cps21_lr_scale_bef_1)
look_for(ces21, "scale")
ces21$ideology<-Recode(as.numeric(ces21$cps21_lr_scale_bef_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; -99=NA; else=NA")
#val_labels(ces21$ideology)<-c(Left=0, Right=1)
#checks
#val_labels(ces21$ideology)
table(ces21$ideology, ces21$cps21_lr_scale_bef_1 , useNA = "ifany" )

# recode Immigration sentiment (pes21_immigjobs)
look_for(ces21, "immigr")
ces21$immigration_job<-Recode(as.numeric(ces21$pes21_immigjobs), "1=0; 2=0.25; 3=0.75; 4=1; else=NA", as.numeric=T)
#checks
table(ces21$immigration_job, ces21$pes21_immigjobs, useNA = "ifany" )

# recode turnout (pes21_turnout2021)
look_for(ces21, "vote")
ces21$turnout<-Recode(ces21$pes21_turnout2021, "1=1; 2:5=0; 6:8=NA; else=NA")
val_labels(ces21$turnout)<-c(No=0, Yes=1)
#checks
val_labels(ces21$turnout)
table(ces21$turnout)
table(ces21$turnout, ces21$vote)

#### recode political efficacy ####
#recode No Say (cps21_govt_say)
look_for(ces21, "have any say")
ces21$efficacy_internal<-Recode(as.numeric(ces21$cps21_govt_say), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#val_labels(ces21$efficacy_internal)<-c(low=0, high=1)
#checks
#val_labels(ces21$efficacy_internal)
table(ces21$efficacy_internal)
table(ces21$efficacy_internal, ces21$cps21_govt_say , useNA = "ifany" )

#recode MPs lose touch (pes21_losetouch)
look_for(ces21, "lose touch")
ces21$efficacy_external<-Recode(as.numeric(ces21$pes21_losetouch), "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces21$efficacy_external)<-c(low=0, high=1)
#checks
#val_labels(ces21$efficacy_external)
table(ces21$efficacy_external)
table(ces21$efficacy_external, ces21$pes21_losetouch , useNA = "ifany" )

#recode Official Don't Care (pes21_govtcare)
look_for(ces21, "care much")
ces21$efficacy_external2<-Recode(as.numeric(ces21$pes21_govtcare), "1=1; 2=0.25; 3=0.5; 4=0.75; 5=0; 6=0.5; else=NA", as.numeric=T)
#val_labels(ces21$efficacy_external2)<-c(low=0, high=1)
#checks
#val_labels(ces21$efficacy_external2)
table(ces21$efficacy_external2)
table(ces21$efficacy_external2, ces21$pes21_govtcare , useNA = "ifany" )

ces21 %>%
  mutate(political_efficacy=rowMeans(select(., c("efficacy_external", "efficacy_external2", "efficacy_internal")), na.rm=T))->ces21

ces21 %>%
  select(starts_with("efficacy")) %>%
  summary()
#Check distribution of political_efficacy
qplot(ces21$political_efficacy, geom="histogram")
table(ces21$political_efficacy, useNA="ifany")

#Calculate Cronbach's alpha
library(psych)
ces21 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  psych::alpha(.)
#Check correlation
ces21 %>%
  select(efficacy_external, efficacy_external2, efficacy_internal) %>%
  cor(., use="complete.obs")

# recode satisfaction with democracy (cps21_demsat & pes21_dem_sat)
look_for(ces21, "dem")
ces21$satdem<-Recode(as.numeric(ces21$pes21_dem_sat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces21$satdem, ces21$pes21_dem_sat, useNA = "ifany" )

ces21$satdem2<-Recode(as.numeric(ces21$cps21_demsat), "1=1; 2=0.75; 3=0.25; 4=0; 5=0.5; else=NA", as.numeric=T)
#checks
table(ces21$satdem2, ces21$cps21_demsat, useNA = "ifany" )

# Add mip as missing variable.
ces21$mip<-rep(NA, nrow(ces21))
ces21$sector<-rep(NA, nrow(ces21))

#recode Postgrad (cps21_education)
look_for(ces21, "education")
ces21$postgrad<-Recode(as.numeric(ces21$cps21_education), "10:11=1; 1:9=0; else=NA")
#checks
table(ces21$postgrad)

#recode Perceive Inequality (pes21_inequal)
look_for(ces21, "ineq")
ces21$inequality<-Recode(as.numeric(ces21$pes21_inequal), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; else=NA")
#checks
table(ces21$inequality)
table(ces21$inequality, ces21$pes21_inequal)

#recode efficacy rich (pes21_populism_8)
look_for(ces21, "rich")
ces21$efficacy_rich<-Recode(as.numeric(ces21$pes21_populism_8), "1=1; 2=0.75; 3=0.5; 4=0.25; 5=0; 6=0.5; else=NA")
#checks
table(ces21$efficacy_rich)
table(ces21$efficacy_rich, ces21$pes21_populism_8)

# recode political interest (cps21_interest_gen_1)
look_for(ces21, "interest")
ces21$pol_interest<-Recode(as.numeric(ces21$cps21_interest_gen_1), "0=0; 1=0.1; 2=0.2; 3=0.3; 4=0.4; 5=0.5; 6=0.6; 7=0.7; 8=0.8; 9=0.9; 10=1; else=NA", as.numeric=T)
#checks
table(ces21$pol_interest, ces21$cps21_interest_gen_1, useNA = "ifany" )

#recode foreign born (cps21_bornin_canada)

look_for(ces21, "born")
ces21$foreign<-Recode(ces21$cps21_bornin_canada, "1=0; 2=1; else=NA")
val_labels(ces21$foreign)<-c(No=0, Yes=1)
#checks
#val_labels(ces21$foreign)
table(ces21$foreign, ces21$cps21_bornin_canada, useNA="ifany")
#
#recode Environment Spend (cps21_spend_env)
look_for(ces21, "env")
ces21$enviro_spend<-Recode(as.numeric(ces21$cps21_spend_env), "1=0; 2=0.5; 3=1; else=NA")
#checks
table(ces21$enviro_spend , ces21$cps21_spend_env , useNA = "ifany" )

# #### Resave the file in the .rda file
#devtools::use_data(ces21)
save(ces21, file=here("data/ces21.rda"))
