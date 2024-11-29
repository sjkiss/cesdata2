# #### Add Molly's Most Important Problem to 2019 Phone ####
library(readxl)
# This reads in the codes assigned to unique open ended responses
mip<-read_excel(path=here("data-raw/mip_2019.xlsx"), col_names=T)

library(stringr)
#This converts the original open ended response variable to lower text
ces19phone$q7_lower<-str_to_lower(ces19phone$q7)
ces19phone$q7_lower

# glimpse mip
glimpse(mip)
# q7_lower is the raw unique values from ces19phone
# mip_cat is the category title assigned to each response by RA
# q7_out is the code assigned to each response
# The numbers come from the CPS15 data-set
# mip15 is the categorical title for each category from the CPS15
# CPS15_1 numeric code from CPS15
#Run this command and it will show there are ton of missing values
# It is because I have combined two different things in the same spreadsheet
# mip15 and CPS15_1 are just the 30 or so category codes
# from CPS15
# q7_out and q7_lower are the 2000 or so unique responses to MIP
# provided in CES19
table(mip$mip15, useNA = "ifany")
#This starts with the spreadsheet prepared by RA
mip %>%
  #It selects the unique original lower case responses q7_lower
  # And the codes assigned by RA
  select(q7_lower, q7_out) %>%
  #Merges them back into ces19phone by the variable q7_lower
  # Which is the lower case original responses
  # Saves in a new data farme out
  full_join(ces19phone, .)->out

# Now we want o use the numeric codes in out$q7_out and give them value labels
out$q7_out

#use names in mip$mip15 for the numbers in CPS15_1.
#What I'm doing here is taking the same value labels from 2015 and
# applying them to 2019.
mip %>%
  #I'm dumping out all the excess rows of the spreadsheet that do not have a value for CPS15_1
  #S'o I'm just keeping the rows that have the values and the value labels for the issues from 2015
  filter(!is.na(CPS15_1)) %>%
  #Now I'm just selecting those two variables and naming them label and value
  select(label=mip15, CPS15_1) %>%
  #This turns the CPS15_1 variable into a numeric variable
  mutate(value=as.numeric(CPS15_1))->mip_labels
mip_labels
#This takes the value labels in the value column and it makes them the names of the label variable
names(mip_labels$value)<-mip_labels$label
mip_labels$value
#This then turns out$mip into a labelled variable using the named mip_labels$value label that was justr created
names(out)
names(out)
out$q7_out
out$q7_out<-labelled(out$q7_out, labels=mip_labels$value)

#This performs a check
table(as_factor(out$q7_out))

#Overwrite ces19phone with out
ces19phone<-out
rm(out)

