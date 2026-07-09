#File to Recode CES Data
library(tidyverse)
library(car)
library(labelled)
library(here)
library(haven)
library(binsmooth)

# ==============================================================================
# LOAD DATA
# ==============================================================================
ces84 <- read_sav(file = here("data-raw/1984.sav"))
source("data-raw/recode_scripts/ces84_recode_constituency.R")
var_label(ces84$VAR001)
ces84$respid <- as.character(ces84$VAR001)

fed81_raw <- read.csv(file = here("data-raw/statscan/1981_household_income/fed_full_1981_with_schooling.csv"))
names(fed81_raw)

# Remove provincial/territorial totals -- keep individual FEDs only
fed81_raw <- fed81_raw %>%
  filter(str_detect(FED,
                    "^Canada|^British Columbia|^Alberta|^Saskatchewan|^Northwest|Manitoba|^Ontario$|^Quebec?|^New Brun|^Nova Scotia|^Prince Edward Island|^Newfoundland",
                    negate = TRUE))

# Drop the territories for Gini estimation: smallest classified populations in
# the dataset, structurally different (single-FED territories with no
# province to nest under), and Nunatsiaq in particular fails to produce a
# stable fit even with a supplied mean.
fed81_wide <- fed81_raw %>%
  filter(!Province_Territory %in% c("Yukon", "Northwest Territories", "Nunavut"))

fed81_wide %>% distinct(FED)

# ==============================================================================
# INCOME GINI
# ==============================================================================
# Right endpoints of each income bracket. binsmooth's bEdges must be the same
# length as bCounts (right endpoints only, left edge of first bin assumed 0;
# the final edge value is always ignored -- top bin is unbounded regardless).
income_cols <- c("Under_5000", "X5000_9999", "X10000_14999", "X15000_19999",
                 "X20000_24999", "X25000_29999", "X30000_39999", "X40000_and_over")
income_right_edges <- c(5000, 9999, 14999, 19999, 24999, 29999, 39999, Inf)
stopifnot(length(income_right_edges) == length(income_cols))

income_fits <- fed81_wide %>%
  select(FED, Avg_Income, Median_Income, all_of(income_cols)) %>%
  pivot_longer(all_of(income_cols), names_to = "Income_Category", values_to = "Income_Count") %>%
  nest(data = -FED) %>%
  # Avg_Income is StatCan's own published mean per FED -- an independently
  # sourced mean, not derived from the bin structure itself.
  mutate(fit = map(data, ~ splinebins(income_right_edges, .x$Income_Count, m = .x$Avg_Income[1]))) %>%
  mutate(
    income_gini       = map_dbl(fit, gini),
    income_fitWarn     = map_lgl(fit, "fitWarn"),
    fed_avg_income     = map_dbl(data, ~ first(.x$Avg_Income)),
    fed_median_income  = map_dbl(data, ~ first(.x$Median_Income))
  ) %>%
  select(FED, income_gini, income_fitWarn, fed_avg_income, fed_median_income)

# ==============================================================================
# EDUCATION GINI
# ==============================================================================
# Trades / Other-non-university (no cert) / Other-non-university (with cert)
# cannot be defensibly ordered against each other in years of schooling --
# StatCan's own documentation notes the credential hierarchy is only loosely
# tied to in-class duration, not a strict per-person years ordering. Merged
# into a single "postsecondary, non-university" band rather than forcing an
# arbitrary order among the three. See education_gini_crosswalk.xlsx.
fed81_wide <- fed81_wide %>%
  mutate(Postsec_NonUniv = Trades_cert + Other_nonuniv_no_cert + Other_nonuniv_with_cert)

schooling_cols <- c("Less_than_Grade9", "Grades9_13_no_cert", "Grades9_13_with_cert",
                    "Postsec_NonUniv", "Univ_no_degree", "Univ_with_degree")

right_schooling_edges <- c(8, 11, 12, 14, 16, NA)   # length 6, matches schooling_cols
stopifnot(length(right_schooling_edges) == length(schooling_cols))

# Midpoints, used only to derive an internally-consistent mean years of
# schooling per FED (external convenience -- never passed as bEdges). The top
# bin's proxy upper value mirrors binsmooth's own fallback convention (2x the
# previous edge) purely to get a representative point for the mean.
left_schooling_edges    <- c(0, head(right_schooling_edges, -1))
top_proxy               <- 2 * right_schooling_edges[length(right_schooling_edges) - 1]
schooling_edges_for_mid <- c(right_schooling_edges[-length(right_schooling_edges)], top_proxy)
schooling_bin_mids      <- (left_schooling_edges + schooling_edges_for_mid) / 2

category_years <- tibble(
  Schooling_Category = schooling_cols,
  Schooling_Years     = schooling_bin_mids
)

education_long <- fed81_wide %>%
  select(FED, all_of(schooling_cols)) %>%
  pivot_longer(all_of(schooling_cols), names_to = "Schooling_Category", values_to = "Schooling_Count") %>%
  left_join(category_years, by = "Schooling_Category") %>%
  group_by(FED) %>%
  mutate(mean_schooling = if_else(
    sum(Schooling_Count, na.rm = TRUE) > 0,
    sum(Schooling_Count * Schooling_Years, na.rm = TRUE) / sum(Schooling_Count, na.rm = TRUE),
    NA_real_
  )) %>%
  ungroup()

education_fits <- education_long %>%
  nest(data = -FED) %>%
  mutate(fit = map(data, ~ stepbins(right_schooling_edges, .x$Schooling_Count, m = .x$mean_schooling[1]))) %>%
  mutate(
    education_gini = map_dbl(fit, gini),
    mean_schooling = map_dbl(data, ~ first(.x$mean_schooling))
  ) %>%
  select(FED, education_gini, mean_schooling)

# ==============================================================================
# COMBINE INTO ONE CLEAN PER-FED TABLE
# ==============================================================================
# One row per FED -- income_fits and education_fits are each already distinct
# by FED, so this join cannot duplicate rows.
fed81_ginis <- fed81_wide %>%
  select(FED, Province_Territory, Avg_Value_Dwelling) %>%
  left_join(income_fits, by = "FED") %>%
  left_join(education_fits, by = "FED")

stopifnot(nrow(fed81_ginis) == nrow(fed81_wide))
length(unique(fed81_ginis$FED))

# Quick histograms
fed81_ginis %>% ggplot(aes(x = income_gini)) + geom_histogram()
fed81_ginis %>% ggplot(aes(x = education_gini)) + geom_histogram()
fed81_ginis
#write_csv(fed81_ginis, here("data-raw/statscan/1981_household_income/1981_fed_ginis.csv"))

# ==============================================================================
# MERGE WITH CES84
# ==============================================================================
# Note: ces84_recode_constituency.R must have already run (sourced above).
ces84 <- ces84 %>%
  mutate(constituency = str_to_title(constituency))

# First join attempt
out <- ces84 %>%
  full_join(fed81_ginis, by = join_by(constituency == FED), keep = TRUE)

# Diagnostic: mismatches between the two datasets
out %>%
  filter((is.na(constituency) & !is.na(FED)) | (!is.na(constituency) & is.na(FED))) %>%
  select(constituency, FED, fed_median_income) %>%
  view()

out %>%
  filter(!is.na(constituency)) %>%
  filter(is.na(fed_median_income)) %>%
  count(constituency) # There are 115 constituencies that have not been matched

# ---- Name-cleaning pass 1: hyphens ------------------------------------------
ces84 <- ces84 %>%
  mutate(constituency = str_replace_all(constituency, "-", " "))
fed81_ginis <- fed81_ginis %>%
  mutate(FED = str_replace_all(FED, "-", " "))

ces84 <- ces84 %>%
  mutate(constituency = str_replace_all(constituency, "Cape Bret E", "Cape Breton East")) %>%
  mutate(constituency = str_replace_all(constituency, "Saint Henri", "Saint Henri-Westmount"))

# Rematch
out <- ces84 %>%
  full_join(fed81_ginis, by = join_by(constituency == FED), keep = TRUE)

out %>%
  filter((is.na(constituency) & !is.na(FED)) | (!is.na(constituency) & is.na(FED))) %>%
  select(constituency, FED, fed_median_income) %>%
  view()

out %>%
  filter(!is.na(constituency)) %>%
  filter(is.na(fed_median_income)) %>%
  count(constituency) # There are 113

# ---- Name-cleaning pass 2: French directions --------------------------------
fed81_ginis <- fed81_ginis %>%
  mutate(FED = str_remove_all(FED, " \\(Nord\\)| \\(Sud\\)| \\(Est\\)| \\(Ouest\\)| \\(Nord Centre\\)"))

ces84 <- ces84 %>%
  mutate(constituency = str_replace_all(constituency, "Winn. ", "Winnipeg "))

ces84 <- ces84 %>%
  mutate(constituency = str_replace_all(constituency, "Pr\\.", "Prince")) %>%
  mutate(constituency = str_replace_all(constituency, "Pt\\.", "Port")) %>%
  mutate(constituency = str_replace_all(constituency, "Rv\\.", "River")) %>%
  mutate(constituency = str_replace_all(constituency, " V$", " Valley"))

ces84 %>% filter(str_detect(constituency, "Bulkley")) %>% select(constituency)
ces84 %>% filter(str_detect(constituency, "Proven")) %>% select(constituency)

# ---- Name-cleaning pass 3: Saint Jean variants -------------------------------
fed81_ginis <- fed81_ginis %>%
  mutate(FED = str_remove_all(FED, " \\(Saint Jean Est\\)|\\(Saint Jean Ouest\\)|\\(Saint Jean\\)"))

# ---- Name-cleaning pass 4: Kootenay West -------------------------------------
ces84 %>% filter(str_detect(constituency, "kootenay")) %>% as_factor() %>% select(constituency) %>% print(n = 50)
fed81_ginis %>% filter(str_detect(FED, "Kootenay"))
fed81_ginis <- fed81_ginis %>%
  mutate(FED = str_remove_all(FED, " Revelstoke"))

ces84 <- ces84 %>%
  mutate(constituency = str_replace_all(constituency, "W\\.", "White"))

# Rematch
out <- ces84 %>%
  full_join(fed81_ginis, by = join_by(constituency == FED), keep = TRUE)

out %>%
  filter((is.na(constituency) & !is.na(FED)) | (!is.na(constituency) & is.na(FED))) %>%
  select(constituency, FED, fed_median_income) %>%
  view()

out %>%
  filter(!is.na(constituency)) %>%
  filter(is.na(fed_median_income)) %>%
  count(constituency) # There are 59 that are missing. So I think that got us two.

# ==============================================================================
# FUZZY MATCH REMAINDER
# ==============================================================================
library(fedmatch)

fed81_ginis$id <- 1:nrow(fed81_ginis)

# Strip accents for matching
norm <- function(x) stringi::stri_trans_general(tolower(trimws(x)), "Latin-ASCII")
ces84 <- ces84 %>% mutate(constituency = norm(constituency))
fed81_ginis <- fed81_ginis %>% mutate(FED = norm(FED))

basic_merge <- merge_plus(ces84, fed81_ginis,
                          by.x = "constituency", by.y = "FED",
                          unique_key_1 = "respid", unique_key_2 = "id",
                          match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(maxDist = 0.25))

# Check the ridings that were not matched
basic_merge$data1_nomatch %>% select(constituency, VAR006) %>% distinct() %>% view()
basic_merge$data2_nomatch %>% select(FED)

# Check the successful matches / flag any bad ridings
table(basic_merge$matches$warn)
