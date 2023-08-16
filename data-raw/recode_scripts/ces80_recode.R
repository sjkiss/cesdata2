#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
#load data
if (!file.exists(here("data/ces7980.rda"))) {
  #If it does not exist read in the original raw data file
  ces7980<-read_sav(file=here("data-raw/1974_1979_1980.sav"))
} else {
  load("data/ces7980.rda")
}
#recode Gender (V2156)
# look_for(ces7980, "sex")
ces7980$male80<-Recode(ces7980$V2156, "1=1; 2=0; 0=NA")
val_labels(ces7980$male80)<-c(Female=0, Male=1)
#checks
val_labels(ces7980$male80)
table(ces7980$male80)

#No Union Household variable

#No Union Combined variable

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

# Save the file
save(ces7980, file=here("data/ces7980.rda"))
