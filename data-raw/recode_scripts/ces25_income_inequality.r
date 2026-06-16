# Run these lines if you have not run the main ces25_recode.R recode script
# If you are calling this script from within ces25_recode.R then comment these lines out

library(haven)
library(here)
library(tidyverse)
library(srvyr)
library(survey)


# Load in federal
census2021<-read.csv(file=here("data-raw/statscan/2022_federal_representation_order/98-401-X2021029_English_CSV_data.csv"))
census2021 %>%
  count(GEO_LEVEL)
census2021 %>%
  filter(str_detect(GEO_LEVEL,"Federal electoral district"))->census2021
# census2021 %>%
#   filter(str_detect(CHARACTERISTIC_NAME, "income")) %>% view()

census2021 %>%
  filter((CHARACTERISTIC_ID>260&CHARACTERISTIC_ID<281&CHARACTERISTIC_ID!=276)|
           str_detect(CHARACTERISTIC_NAME, "Gini index on adjusted household total income")|
          CHARACTERISTIC_ID==252) %>%
  select(GEO_NAME,CHARACTERISTIC_NAME, CHARACTERISTIC_ID, C1_COUNT_TOTAL) %>%
  pivot_wider(., id_cols=GEO_NAME,names_from=CHARACTERISTIC_NAME, values_from = C1_COUNT_TOTAL) %>%
  rename(`Gini`=22, `Income`=2)->census2021_income_data

#install.packages("binsmooth")
library(binsmooth)

names(census2021_income_data)<-str_trim(names(census2021_income_data))
census2021_income_data %>% names()
#Define midpoints
right_edges<-c(5000,9999,14999,19999,24999,29999,34999,39999,44999,49999,59999,69999,79999,89999,99999,124999,149999,199999,Inf)

#Pivot down the income categories to get the counts

census2021_income_data %>%
  pivot_longer(.,
               #pivot down the income categories
               cols=3:21,
               #Story bin ranges in category and population counts in each category in count
               names_to=c("Category"), values_to = c("Count")) %>%
  mutate(right_edges=rep(right_edges, length(unique(census2021_income_data$GEO_NAME)))) ->census2021_income_data

#census2021_income_data %>% view()

# Run the splinebins fit
fits <- census2021_income_data %>%
  group_by(GEO_NAME) %>%
  nest() %>%
  mutate(fit = map(data, ~ {
    edges <- .x$right_edges
    edges[is.na(edges)] <- Inf
    splinebins(edges, .x$Count, m = .x$Income[1])
  }))
fits   %>%
  mutate(gini_cdf=map(fit, gini)) %>%
  unnest(gini_cdf) %>%
  mutate(gini_original=map_dbl(data, ~first(.x$Gini))) ->ginis
ginis %>%
  mutate(error_pct=(gini_cdf-gini_original)/gini_original) ->ginis

ginis %>%
  mutate(error_pct_squared=error_pct^2) ->ginis
ginis %>%
  ggplot(., aes(x=error_pct))+geom_histogram()+labs(title="distribution of percent errors")
ggsave(filename=here('data-raw/statscan/2022_federal_representation_order/histogram_percent_errors.png'))
ginis %>%
  unnest(data) %>%
  select(-fit) %>%
  write_csv(.,
            file=here("data-raw/statscan/2022_federal_representation_order/statistics_canada_federal_electoral_districts_ginis_benchmarking.csv"))


ginis %>%
  ggplot(., aes(x=gini_original, y=gini_cdf))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Comparing Statscan Ginis with Ginis from Splinebins")+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  # 2. Force 1 unit on the X-axis to match 1 unit on the Y-axis visually
  coord_fixed()
ggsave(filename = here("data-raw/statscan/2022_federal_representation_order/diagnosing_errors_cdf_interpolated_ginis.png"))


ces25$fed
names(census2021)
census2021$ALT_GEO_CODE
census2021 %>%
  select(feduid=ALT_GEO_CODE, fedname=GEO_NAME) %>%
  distinct()->fed2023
ces25 %>%
  select(feduid, fedname) %>%
  distinct() ->ces25_fed2023
#install.packages("joyn")
library(joyn)

fed2023 %>%
  joyn(., ces25_fed2023, keep="full", keep_common_vars=T) ->joyned
view(joyned)
#Check
joyned %>%
  filter(.joyn=="y") ->no_match_y
fed2023 %>%
  filter(feduid %in% no_match_y$feduid)

#Check
census2021 %>%
  filter(ALT_GEO_CODE==59016) %>%
  select(GEO_NAME)
ces25 %>%
  filter(feduid==59016) %>%
  select(feduid, fedname, cps25_ResponseId) %>%
  print(n=20)
# # Read in PCCF
# library(haven)
pccf<-read_sav(file=here("data-raw/statscan/2025_pccf/PCFRF_FCPCEF_V2503_2021.sav"))
names(pccf)
# glimpse(pccf)
# ces25
# library(labelled)
# lookfor(ces25, "code")
# lookfor(ces25, "fsa")
# lookfor(ces25, "forward")
# ces25$cps25_postalcode
#Load data
ces25<-read_dta(here("data-raw/ces25.dta"))
