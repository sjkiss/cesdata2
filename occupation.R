
# Coding occupation from CES 1990s onward (Polacko, Kiss, Graefe 2022)

# occupation is a 4 category occupation variable
# occupation3 is a 5 category variable that also includes self-employed, which is only available since 1979
# no occupation variable is available in 2000. Richard Johnston is trying to track it down

#### 1993 ####
#recode Occupation (CPSPINPR)
look_for(ces93, "occupation")
look_for(ces93, "pinporr")
ces93$occupation<-Recode(ces93$CPSPINPR, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces93$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces93$occupation)
table(ces93$occupation)

#recode Occupation3 as 5 class schema with self-employed (REFN5)
look_for(ces93, "employ")
ces93$CPSJOB3
ces93$CPSJOB1
ces93$occupation3<-ifelse(ces93$CPSJOB3==1, 6, ces93$occupation)
val_labels(ces93$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces93$occupation3)
table(ces93$occupation3)

#### 1997 ####
# (ces97, "occupation")
# (ces97, "pinporr")
ces97$occupation<-Recode(ces97$pinporr, "1:2:=1; 4:5=1; 3=2; 6:7=2; 9=3; 12=3; 14=3; 8=4; 10=4; 13=4; 15:16=5; else=NA")
val_labels(ces97$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces97$occupation)
table(ces97$occupation)

#recode Occupation3 as 5 class schema with self-employed (cpsm4)
# (ces97, "employ")
ces97$occupation3<-ifelse(ces97$cpsm4==8, 6, ces97$occupation)
val_labels(ces97$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces97$occupation3)
table(ces97$occupation3)

#### 2004 ####
look_for(ces0411, "pinporr")
lookfor(ces0411, "occupation")
ces0411 %>%
  mutate(occupation04=case_when(
    ces04_PES_SD3 >0 & ces04_PES_SD3 <1100 ~ 2,
    ces04_PINPORR==1 ~ 1,
    ces04_PINPORR==2 ~ 1,
    ces04_PINPORR==4 ~ 1,
    ces04_PINPORR==5 ~ 1,
    ces04_PINPORR==3 ~ 2,
    ces04_PINPORR==6 ~ 2,
    ces04_PINPORR==7 ~ 2,
    ces04_PINPORR==9 ~ 3,
    ces04_PINPORR==12 ~ 3,
    ces04_PINPORR==14 ~ 3,
    ces04_PINPORR==8 ~ 4,
    ces04_PINPORR==10 ~ 4,
    ces04_PINPORR==13 ~ 5,
    ces04_PINPORR==15 ~ 5,
    ces04_PINPORR==16 ~ 5,
  ))->ces0411
val_labels(ces0411$occupation04)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation04)
table(ces0411$occupation04)
ces0411$ces04_CPS_S4

#recode Occupation3 as 5 class schema with self-employed (ces04_CPS_S4)
look_for(ces0411, "employ")
ces0411$occupation04_3<-ifelse(ces0411$ces04_CPS_S4==1, 6, ces0411$occupation04)
val_labels(ces0411$occupation04_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation04_3)
table(ces0411$occupation04_3)

#### 2006 ####
look_for(ces0411, "occupation")
look_for(ces0411, "employ")
ces0411 %>%
  mutate(occupation06=case_when(
    ces06_PES_SD3 >0 & ces06_PES_SD3 <1100 ~ 2,
    ces06_PES_SD3 >1099 & ces06_PES_SD3 <1200 ~ 1,
    ces06_PES_SD3 >2099 & ces06_PES_SD3 <2200 ~ 1,
    ces06_PES_SD3 >3099 & ces06_PES_SD3 <3200 ~ 1,
    ces06_PES_SD3 >4099 & ces06_PES_SD3 <4200 ~ 1,
    ces06_PES_SD3 >5099 & ces06_PES_SD3 <5200 ~ 1,
    ces06_PES_SD3 >1199 & ces06_PES_SD3 <1501 ~ 3,
    ces06_PES_SD3 >2199 & ces06_PES_SD3 <3100 ~ 3,
    ces06_PES_SD3 >3199 & ces06_PES_SD3 <4100 ~ 3,
    ces06_PES_SD3 >4199 & ces06_PES_SD3 <5100 ~ 3,
    ces06_PES_SD3 >5199 & ces06_PES_SD3 <6701 ~ 3,
    ces06_PES_SD3 >7199 & ces06_PES_SD3 <7400 ~ 4,
    ces06_PES_SD3 >7399 & ces06_PES_SD3 <7701 ~ 5,
    ces06_PES_SD3 >8199 & ces06_PES_SD3 <8400 ~ 4,
    ces06_PES_SD3 >8399 & ces06_PES_SD3 <8701 ~ 5,
    ces06_PES_SD3 >9199 & ces06_PES_SD3 <9300 ~ 4,
    ces06_PES_SD3 >9299 & ces06_PES_SD3 <9701 ~ 5,
    (ces04_PES_SD3 >0 & ces04_PES_SD3 <1001) & ces06_RECALL==1~ 2,
    ces04_PINPORR==1 & ces06_RECALL==1~ 1,
    ces04_PINPORR==2 & ces06_RECALL==1~ 1,
    ces04_PINPORR==4 & ces06_RECALL==1~ 1,
    ces04_PINPORR==5 & ces06_RECALL==1~ 1,
    ces04_PINPORR==3 & ces06_RECALL==1~ 2,
    ces04_PINPORR==6 & ces06_RECALL==1~ 2,
    ces04_PINPORR==7 & ces06_RECALL==1~ 2,
    ces04_PINPORR==9 & ces06_RECALL==1~ 3,
    ces04_PINPORR==12 & ces06_RECALL==1~ 3,
    ces04_PINPORR==14 & ces06_RECALL==1~ 3,
    ces04_PINPORR==8 & ces06_RECALL==1~ 4,
    ces04_PINPORR==10 & ces06_RECALL==1~ 4,
    ces04_PINPORR==13 & ces06_RECALL==1~ 5,
    ces04_PINPORR==15 & ces06_RECALL==1~ 5,
    ces04_PINPORR==16 & ces06_RECALL==1~ 5,
  ))->ces0411
val_labels(ces0411$occupation06)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation06)
table(ces0411$occupation06)

#recode Occupation3 as 5 class schema with self-employed (ces06_CPS_S4)
# look_for(ces0411, "employ")
ces0411$occupation06_3<-ifelse(ces0411$ces06_CPS_S4==1, 6, ces0411$occupation06)
val_labels(ces0411$occupation06_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation06_3)
table(ces0411$occupation06_3)

#### 2008 ####
#recode Occupation (ces08_PES_S3_NOCS)
look_for(ces0411, "occupation")
look_for(ces0411, "employ")
ces0411 %>%
  mutate(occupation08=case_when(
    ces08_PES_S3_NOCS >0 & ces08_PES_S3_NOCS <1100 ~ 2,
    ces08_PES_S3_NOCS >1099 & ces08_PES_S3_NOCS <1200 ~ 1,
    ces08_PES_S3_NOCS >2099 & ces08_PES_S3_NOCS <2200 ~ 1,
    ces08_PES_S3_NOCS >3099 & ces08_PES_S3_NOCS <3200 ~ 1,
    ces08_PES_S3_NOCS >4099 & ces08_PES_S3_NOCS <4200 ~ 1,
    ces08_PES_S3_NOCS >5099 & ces08_PES_S3_NOCS <5200 ~ 1,
    ces08_PES_S3_NOCS >1199 & ces08_PES_S3_NOCS <1501 ~ 3,
    ces08_PES_S3_NOCS >2199 & ces08_PES_S3_NOCS <3100 ~ 3,
    ces08_PES_S3_NOCS >3199 & ces08_PES_S3_NOCS <4100 ~ 3,
    ces08_PES_S3_NOCS >4199 & ces08_PES_S3_NOCS <5100 ~ 3,
    ces08_PES_S3_NOCS >5199 & ces08_PES_S3_NOCS <6701 ~ 3,
    ces08_PES_S3_NOCS >7199 & ces08_PES_S3_NOCS <7400 ~ 4,
    ces08_PES_S3_NOCS >7399 & ces08_PES_S3_NOCS <7701 ~ 5,
    ces08_PES_S3_NOCS >8199 & ces08_PES_S3_NOCS <8400 ~ 4,
    ces08_PES_S3_NOCS >8399 & ces08_PES_S3_NOCS <8701 ~ 5,
    ces08_PES_S3_NOCS >9199 & ces08_PES_S3_NOCS <9300 ~ 4,
    ces08_PES_S3_NOCS >9599 & ces08_PES_S3_NOCS <9701 ~ 5,
  ))->ces0411
val_labels(ces0411$occupation08)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation08)
ces0411 %>%
  filter(str_detect(survey, "PES08")) %>%
  select(occupation08) %>%
  group_by(occupation08) %>%
  summarise(total=sum(occupation08), missing=sum(is.na(occupation08)))

#recode Occupation3 as 5 class schema with self-employed (ces08_CPS_S4)
# look_for(ces0411, "employ")
ces0411$occupation08_3<-ifelse(ces0411$ces08_CPS_S4==1, 6, ces0411$occupation08)
val_labels(ces0411$occupation08_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces0411$occupation08_3)
table(ces0411$occupation08_3)

#### 2011 ####
#recode Occupation (NOC_PES11)
# look_for(ces0411, "occupation")
class(ces0411$NOC_PES11)
val_labels(ces0411$NOC_PES11)
ces0411$occupation11<-Recode(as.numeric(ces0411$NOC_PES11), "1:1099=2; 1100:1199=1;
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
 6400:6799=3;
 7200:7399=4; 7400:7700=5; 8200:8399=4; 8400:8700=5; 9200:9399=4; 9400:9700=5; else=NA")
val_labels(ces0411$occupation11)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces0411$occupation11)
table(ces0411$occupation11, useNA = "ifany" )

#Show occupations of those missing responses on NOC variable
ces0411 %>%
  select(NOC_PES11, occupation11, survey) %>%
  group_by(occupation11) %>%
  filter(is.na(occupation11)) %>%
  filter(str_detect(survey, "CPS11")) %>%
  count(NOC_PES11)

#recode Occupation3 as 5 class schema with self-employed (CPS11_91)
look_for(ces0411, "employ")
table(ces0411$occupation11, useNA = "ifany")
ces0411$CPS11_91
ces0411$occupation11_3<-ifelse(ces0411$CPS11_91==1, 6, ces0411$occupation11)
val_labels(ces0411$occupation11_3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
table(ces0411$occupation11, ces0411$occupation11_3)
#checks
val_labels(ces0411$occupation11_3)
table(ces0411$occupation11_3, useNA = "ifany" )

#### 2015 ####
#recode Occupation (PES15_NOC)
look_for(ces15phone, "occupation")
ces15phone$occupation<-Recode(as.numeric(ces15phone$PES15_NOC), "0:1099=2;
1100:1199=1;
2100:2199=1;
 3000:3199=1;
 4000:4099=1;
 4100:4199=1;
 5100:5199=1;
 1200:1599=3;
 2200:2999=3;
 3200:3299=3;
 3400:3500=3;
 4200:4499=3;
 5200:5999=3;
 6200:6399=3;
 6400:6799=3;
 7200:7399=4;
 7400:7700=5;
 8200:8399=4; 8400:8700=5; 9200:9599=4; 9600:9700=5; else=NA")
val_labels(ces15phone$occupation)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5)
#checks
val_labels(ces15phone$occupation)
table(ces15phone$occupation)

#Count missing values
ces15phone %>%
  select(PES15_NOC, occupation) %>%
  group_by(occupation) %>%
  filter(is.na(occupation)) %>%
  count(PES15_NOC)

ces15phone %>%
  select(occupation, CPS15_91) %>%
  filter(is.na(occupation)) %>%
  group_by(occupation, CPS15_91) %>%
  count()
#Show occupations of those missing responses on NOC variable
ces15phone %>%
  select(PES15_NOC, occupation) %>%
  group_by(occupation) %>%
  summarise(n=n())

#recode Occupation3 as 5 class schema with self-employed (CPS15_91)
look_for(ces15phone, "employ")
ces15phone$occupation3<-ifelse(ces15phone$CPS15_91==1, 6, ces15phone$occupation)
val_labels(ces15phone$occupation3)<-c(Professional=1, Managers=2, Routine_Nonmanual=3, Skilled=4, Unskilled=5, Self_employed=6)
#checks
val_labels(ces15phone$occupation3)
table(ces15phone$occupation3)
table(is.na(ces15phone$occupation3))
table(is.na(ces15phone$occupation))

