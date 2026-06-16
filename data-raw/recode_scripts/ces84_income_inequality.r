#This script calculates ginis for each district in 1984
fed81<-read.csv(file=here("data-raw/statscan/1981_household_income/fed_household_income_1981_final.csv"))
names(fed81)
# look at fed
fed81$FED
fed81 %>%
  filter(str_detect(FED, "^Canada|^British Columbia|^Alberta|^Saskatchewan|Manitoba|^Ontario|^Quebec|^New Brun|^Nova Scotia|^Prince Edward Island|^Newfoundland", negate=T)) ->fed81
names(fed81)

#Rigth points of income intervals
right_edges<-c(5000,9999,14999,19999,24999,29999,39999,Inf)
fed81 %>%
  pivot_longer(4:11, names_to=c("Income"), values_to=c("Count")) %>%
  mutate(right_edges=rep(right_edges, length(unique(.$FED))))->fed81
library(binsmooth)

#This code runs the splinebins function on each federal electoral district
fed81 %>%
  #group_by(FED) %>%
  #nest by federal electoral district
  nest(-FED) %>%
  #For each fed, run the function
  mutate(fit = map(data, ~ {
    edges <- .x$right_edges
    edges[is.na(edges)] <- Inf
    #Count is the C'ount variable from above
    #avg_income is the average income from above
    splinebins(edges, .x$Count, m = .x$Avg_Income[1])
  }))->spline_fits
#Unnest the fitWarn variable
spline_fits %>%
  mutate(warn=map_lgl(fit, "fitWarn")) %>%
  #calcluate the gini
  mutate(gini=map_dbl(fit, gini)) %>%
  mutate(fed_avg_income=map_int(data, ~first(.x$`Avg_Income`))) %>%
mutate(fed_median_income=map_int(data, ~first(.x$`Median_Income`))) %>%
unnest(data) ->fed81_ginis
# fed81_ginis %>%
#   ggplot(., aes(x=gini))+geom_histogram()
# fed81_ginis %>%
# write_csv(file=here('data-raw/statscan/1981_household_income/1981_fed_household_income_data.csv'))

# merge with ces84


#Note in order for this to run, the script ces84_recode_constituency.R has to run
#source("data-raw/recode_scripts/ces84_recode_constituency.R")
ces84 %>%
  mutate(constituency=str_to_title(constituency))->ces84
ces84$constituency
fed81_ginis$FED
#Try the first join
#detach("package:joyn")
fed81_ginis %>%
  select(FED, Province_Territory, fed_avg_income, fed_median_income, gini,warn) %>%
  distinct()->fed81_ginis_distinct
fed81_ginis_distinct %>% view()
ces84 %>%
  full_join(., fed81_ginis_distinct, by=join_by("constituency"=="FED"), keep=T)->out


#This code prints the mismatches between the two datasets; so it is a useful diagnostic
# to locate where the mismatches are occurring.
out %>%
  filter(
    #This filters any rows that have missing values on the 84 data set and any values
    # that are not missing on the FED
    (is.na(constituency)&!is.na(FED))|
      #This filters out any rows that have constituency data in the 84 but are missing in the FED
      (!is.na(constituency)&is.na(FED))) %>%
  select(constituency, FED, fed_median_income) %>% view()

out %>%
  #This filters out all the respondents that do not have constituencies
  filter(!is.na(constituency)) %>%
  #And this then filters out how many respondents have missing data on MEDIAN INCOME
  filter(is.na(fed_median_income)) %>%
  count(constituency) # There are 115 constituencies that have not been matched

# Remove hyphens in both to see what we get.

ces84 %>%
  mutate(constituency=str_replace_all(constituency, "-", " ")) ->ces84
# Remove hyphens in both to see what we get.

fed81_ginis_distinct %>%
  mutate(FED=str_replace_all(FED, "-", " ")) ->fed81_ginis_distinct
# Replace Bret with Breton

ces84 %>%
  mutate(constituency=str_replace_all(constituency, "Cape Bret E", "Cape Breton East")) %>%
  mutate(constituency=str_replace_all(constituency, "Saint Henri", "Saint Henri-Westmount"))->ces84

# Rematch
ces84 %>%
  full_join(fed81_ginis_distinct, by=join_by("constituency"=="FED"), keep=T)->out
out$constituency
#This code prints the mismatches between the two datasets; so it is a useful diagnostic
# to locate where the mismatches are occurring.
out %>%
  filter(
    #This filters any rows that have missing values on the 84 data set and any values
    # that are not missing on the FED
    (is.na(constituency)&!is.na(FED))|
      #This filters out any rows that have constituency data in the 84 but are missing in the FED
      (!is.na(constituency)&is.na(FED))) %>%
  select(constituency, FED, fed_median_income) %>% view()

out %>%
  #This filters out all the respondents that do not have constituencies
  filter(!is.na(constituency)) %>%
  #And this then filters out how many respondents have missing data on MEDIAN INCOME
  filter(is.na(fed_median_income)) %>%
  count(constituency) # There are 113


#Remove all the French directions out of FED

fed81_ginis_distinct %>%
  mutate(FED=str_remove_all(FED, " \\(Nord\\)| \\(Sud\\)| \\(Est\\)| \\(Ouest\\)| \\(Nord Centre\\)")) ->fed81_ginis

#Remove Winn. and replace with Winnipeg

ces84 %>%
  mutate(constituency=str_replace_all(constituency, "Winn. ", "Winnipeg "))->ces84
#Replace Pr. with Prince Rv. with River and Pt. with Port
ces84 %>%
  mutate(constituency=str_replace_all(constituency, "Pr\\.","Prince")) %>%
  mutate(constituency=str_replace_all(constituency, "Pt\\.", "Port")) %>%
  mutate(constituency=str_replace_all(constituency, "Rv\\.", "River")) %>%
  mutate(constituency=str_replace_all(constituency, " V$", " Valley"))->ces84
ces84 %>%
  filter(str_detect(constituency, "Bulkley")) %>%
  select(constituency)
ces84 %>%
  filter(str_detect(constituency, "Proven")) %>%
  select(constituency)
# Remove St Jean Ouest, sT.Jean Est and St. Jean

fed81_ginis_distinct %>%
  mutate(FED=str_remove_all(FED, " \\(Saint Jean Est\\)|\\(Saint Jean Ouest\\)|\\(Saint Jean\\)"))->fed81_ginis
# Correct Kootenay West
ces84 %>%
  filter(str_detect(constituency, "kootenay")) %>% as_factor() %>% select(constituency) %>%
  print(n=50)
fed81_ginis_distinct %>%
  filter(str_detect(FED, "Kootenay"))
fed81_ginis_distinct %>%
  mutate(FED=str_remove_all(FED," Revelstoke"))->fed81_ginis_distinct
ces84$constituency
ces84 %>%
  mutate(constituency=str_replace_all(constituency, "W\\.", "White"))->ces84
# Rematch
ces84 %>%
  full_join(fed81_ginis_distinct, by=join_by("constituency"=="FED"), keep=T)->out
out$constituency
#This code prints the mismatches between the two datasets; so it is a useful diagnostic
# to locate where the mismatches are occurring.
out %>%
  filter(
    #This filters any rows that have missing values on the 84 data set and any values
    # that are not missing on the FED
    (is.na(constituency)&!is.na(FED))|
      #This filters out any rows that have constituency data in the 84 but are missing in the FED
      (!is.na(constituency)&is.na(FED))) %>%
  select(constituency, FED, fed_median_income) %>% view()

out %>%
  #This filters out all the respondents that do not have constituencies
  filter(!is.na(constituency)) %>%
  #And this then filters out how many respondents have missing data on MEDIAN INCOME
  filter(is.na(fed_median_income)) %>%
  count(constituency) # There are 59 that are missing. So I think that got us two.


# Let's try the fuzzy match!
library(fedmatch)
#Assign an id variable to fed81_ginis
fed81_ginis_distinct$id<-1:nrow(fed81_ginis_distinct)

# Get rid of accents
norm <- function(x) stringi::stri_trans_general(tolower(trimws(x)), "Latin-ASCII")
ces84$constituency <- norm(ces84$constituency)
fed81_ginis_distinct$FED<-norm(fed81_ginis_distinct$FED)
library(fedmatch)
#conduct fuzzy merge
merge_plus(ces84, fed81_ginis_distinct, by.x="constituency",
           by.y="FED",
           unique_key_1="respid", unique_key_2="id",
           match_type = "fuzzy", fuzzy_settings=build_fuzzy_settings(maxDist=0.25)) ->basic_merge
#Check the ridings that were not matched
basic_merge$data1_nomatch %>% select(constituency, VAR006) %>%
  distinct() %>%
  view()
#There are 6 respondents who just have "Ontario".
# Go back and fix those.
ces84 %>%
  filter(constituency=="ontario") %>%
  select(VAR006, prov)
# Now check the unsuccesful data2 matches
basic_merge$data2_nomatch %>% select(FED)

#Now check the successful matches.
#basic_merge$matches %>%select(constituency, FED) %>% distinct() %>%  View()
basic_merge$matches$warn
# fed81_ginis_distinct %>% group_by(warn) %>%
#   summarize(avg=mean(fed_avg_income))
# fed81_ginis_distinct %>% group_by(warn) %>%
#   summarize(avg_gini=mean(gini))

#How many respondents are in bad ridings
table(basic_merge$matches$warn)
