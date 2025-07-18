# This script loads and recodes CES25 Kiss module
library(haven)
library(here)
#Load data
ces25b<-read_dta(here("data-raw/CES 25 Kiss Module Final.dta"))
library(labelled)
library(car)
look_for(ces25b, "class")
look_for(ces25b, "vote")
look_for(ces25b, "vote")
ces25b$vote<-Recode(ces25b$cps25_votechoice , "1=1; 2=2; 3=3; 4=4; 5=5; 6=0; 8=2; else=NA")
val_labels(ces25b$vote)<-c(Other=0, Liberal=1, Conservative=2, NDP=3, Bloc=4, Green=5)
table(as_factor(ces25b$vote))

lookfor(ces25b, "income")
# This is a hacky way to assign respondents to income tertiles
# convert original income to numeric
ces25b$cps25_income2<-as.numeric(ces25b$cps25_income)

# Set don't knows to missing
# We could set these to the median 4 if we want to rescue missing data
# talk to matt
ces25b %>%
  mutate(cps25_income2=case_when(
    cps25_income==9~ NA_integer_,
    TRUE~as.numeric(cps25_income)
  ))->ces25b
#weight the survey
ces25b_des<-as_survey_design(subset(ces25b, !is.na(cps25_weight_kiss_module)), weights=cps25_weight_kiss_module)
#now calculate the income tertiles
ces25b_des %>%
  summarise(income_tertile=survey_quantile(cps25_income2, c(0.33,0.66)))

# tertiles are 3 and 6
#assignh income values of 3 and below to 1
# between 3 and 7to be middle
# set 7 and above to be highest
ces25b %>%
  mutate(income_tertile=case_when(
    cps25_income2<4~1,
    cps25_income2>3&cps25_income2<7~2,
    cps25_income2>6~3,
      ))->ces25b
#reweight the ces25b file now that the income tertile has been assigned
ces25b_des<-as_survey_design(subset(ces25b, !is.na(cps25_weight_kiss_module)), weights=cps25_weight_kiss_module)
# check the income tertiles after weighting
svytable(~income_tertile, design=ces25b_des)

#Write out the dataset
# #### Resave the file in the .rda file
save(ces25b, file=here("data/ces25b.rda"))
