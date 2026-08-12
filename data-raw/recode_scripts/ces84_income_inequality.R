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
# This script is sourced by ces84_recode.R, which already:
#   1. loads   ces84 <- read_sav(here("data-raw/1984.sav"))   (line ~8)
#   2. sources ces84_recode_constituency.R  (creates ces84$constituency)
# so ces84 + constituency already exist here -- do NOT reload them.
#
# To run THIS script on its own (top to bottom), uncomment the two lines below
# and also source the constituency recode; leave them commented under the
# normal ces84_recode.R pipeline.
# ces84 <- read_sav(file = here("data-raw/1984.sav"))
# source(here("data-raw/recode_scripts/ces84_recode_constituency.R"))
# ces84$respid <- as.character(ces84$VAR001)
# var_label(ces84$VAR001)

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
# schooling per FED (external convenience -- never passed as bEdges).
#
# IMPORTANT: the top bin's proxy value is NOT binsmooth's own "double the
# previous edge" fallback convention. That convention is a reasonable guess
# for an income bracket (a "$200,000+" bin plausibly has a mean above
# $200,000), but it's badly wrong for years-of-schooling: doubling the
# previous edge (16 -> 32) would assume the average "university with degree"
# holder has 32 years of schooling, which is absurd -- the category is
# overwhelmingly bachelor's holders, not doctorates. Tested on the 2021 data
# (where the true sub-category breakdown is available to check against):
# using the doubling convention inflated the comparable Gini from ~0.16 to
# ~0.31 purely as a modeling artifact, not genuine dispersion.
#
# 1981 has no finer breakdown to compute a true value from (unlike 2021,
# where "bachelor's-or-higher" can be decomposed into real bachelor's/
# master's/doctorate counts). Graduate degree attainment was rarer in 1981
# than in 2021 -- where graduate-degree holders were already only ~28% of
# all bachelor's-or-higher holders -- so 17 (modestly above a pure
# bachelor's value of 16) is used as a conservative, clearly-flagged
# assumption rather than an empirically-derived figure. Revisit if a better
# historical estimate of the 1981 bachelor's/graduate split becomes available.
left_schooling_edges    <- c(0, head(right_schooling_edges, -1))
top_proxy               <- 17
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

# Top-bin population share (diagnostic): binsmooth always treats the top bin
# as unbounded, so a larger population share there means the fitted Gini
# depends more on the assumed mean/tail shape than on genuine measured
# dispersion. Report alongside the Gini so a 1984-vs-2025 comparison can
# distinguish "the Gini moved because more people are now in an uncapped
# category" from "within-group dispersion genuinely changed."
education_fits <- education_long %>%
  nest(data = -FED) %>%
  mutate(fit = map(data, ~ stepbins(right_schooling_edges, .x$Schooling_Count, m = .x$mean_schooling[1]))) %>%
  mutate(
    education_gini = map_dbl(fit, gini),
    mean_schooling = map_dbl(data, ~ first(.x$mean_schooling)),
    top_bin_share  = map_dbl(data, ~ {
      first(.x$Schooling_Count[.x$Schooling_Category == "Univ_with_degree"]) / sum(.x$Schooling_Count)
    })
  ) %>%
  select(FED, education_gini, mean_schooling, top_bin_share)

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

write_csv(fed81_ginis, here("data-raw/statscan/1981_household_income/1981_fed_ginis.csv"))

# ==============================================================================
# MERGE WITH CES84
# ==============================================================================
# Requires ces84 (loaded + constituency recoded at the top of this script) and
# fed81_ginis (built above).
#
# Goal: attach each 1981 FED's inequality measures to the 1984 CES respondents
# who live in that riding. The anchor is the riding NAME. The two sources spell
# ridings differently (CES truncates names, StatCan carries full bilingual
# names), so we harmonise the obvious differences, then fuzzy-match the residual
# -- and GUARD against the failure mode that a greedy fuzzy match silently
# assigns a respondent to the WRONG riding.
library(fedmatch)

ces84 <- ces84 %>%
  mutate(constituency = str_to_title(constituency))

# ---- Name harmonisation: CES side -------------------------------------------
# Hyphens -> spaces (StatCan uses spaces), plus known CES abbreviations.
ces84 <- ces84 %>%
  mutate(
    constituency = str_replace_all(constituency, "-", " "),
    constituency = str_replace_all(constituency, "Cape Bret E", "Cape Breton East"),
    constituency = str_replace_all(constituency, "Saint Henri", "Saint Henri Westmount"),
    constituency = str_replace_all(constituency, "Winn. ", "Winnipeg "),
    constituency = str_replace_all(constituency, "Pr\\.", "Prince"),
    constituency = str_replace_all(constituency, "Pt\\.", "Port"),
    constituency = str_replace_all(constituency, "Rv\\.", "River"),
    constituency = str_replace_all(constituency, " V$", " Valley"),
    constituency = str_replace_all(constituency, "W\\.", "White")
  )

# ---- Name harmonisation: FED side -------------------------------------------
# Hyphens -> spaces, then drop the bilingual direction qualifiers StatCan adds.
fed81_ginis <- fed81_ginis %>%
  mutate(
    FED = str_replace_all(FED, "-", " "),
    FED = str_remove_all(FED, " \\(Nord\\)| \\(Sud\\)| \\(Est\\)| \\(Ouest\\)| \\(Nord Centre\\)"),
    FED = str_remove_all(FED, " \\(Saint Jean Est\\)|\\(Saint Jean Ouest\\)|\\(Saint Jean\\)"),
    FED = str_remove_all(FED, " Revelstoke")
  )

# ---- Accent/case normalisation (both sides) ---------------------------------
norm <- function(x) stringi::stri_trans_general(tolower(trimws(x)), "Latin-ASCII")
ces84       <- ces84       %>% mutate(constituency = norm(constituency))
fed81_ginis <- fed81_ginis %>% mutate(FED = norm(FED))

# ---- Keys for merge_plus ----------------------------------------------------
if (!"respid" %in% names(ces84)) ces84$respid <- as.character(ces84$VAR001)
stopifnot(anyDuplicated(ces84$respid) == 0)   # respid must uniquely key CES
fed81_ginis$id <- seq_len(nrow(fed81_ginis))

# ==============================================================================
# FUZZY MATCH ON RIDING NAME
# ==============================================================================
basic_merge <- merge_plus(ces84, fed81_ginis,
  by.x = "constituency", by.y = "FED",
  unique_key_1 = "respid", unique_key_2 = "id",
  match_type = "fuzzy", fuzzy_settings = build_fuzzy_settings(maxDist = 0.25))

matches <- as_tibble(basic_merge$matches)

# ==============================================================================
# GUARDS -- fail loudly rather than write a silently-wrong file
# ==============================================================================
# (a) No real FED may be "stolen": if a CES riding's name exactly equals an
#     existing FED name, it must map to THAT fed, not a fuzzy neighbour.
stolen <- matches %>%
  filter(constituency %in% fed81_ginis$FED, constituency != FED) %>%
  distinct(constituency, FED)
if (nrow(stolen) > 0) {
  print(as.data.frame(stolen))
  stop("Fuzzy match reassigned a riding whose exact FED exists (above). Fix name cleaning first.")
}

# (b) No FED may collect two different CES ridings (many-to-one collision).
collide <- matches %>% distinct(constituency, FED) %>% count(FED) %>% filter(n > 1)
if (nrow(collide) > 0) {
  print(as.data.frame(collide))
  stop("Two different CES ridings matched the same FED (above). Inspect before trusting the merge.")
}

# ---- Coverage report --------------------------------------------------------
n_unmatched <- nrow(basic_merge$data1_nomatch)
message(sprintf("Matched %d / %d CES respondents to a 1981 FED Gini (%d respondents unmatched).",
                nrow(matches), nrow(ces84), n_unmatched))
if (n_unmatched > 0) {
  message("Unmatched CES ridings (no 1981 Gini available -- e.g. territories, which are dropped above):")
  print(basic_merge$data1_nomatch %>% distinct(constituency) %>% as.data.frame())
}

# ==============================================================================
# SAVE: 1984 CES respondents with 1981 riding inequality measures
# ==============================================================================
# One row per matched CES respondent (keyed by respid so it can be joined back
# onto the main recoded ces84 object), carrying the riding-level Gini measures.
ces84_income_inequality <- matches %>%
  select(respid, constituency, FED, Province_Territory,
         income_gini, income_fitWarn, fed_avg_income, fed_median_income,
         education_gini, mean_schooling, top_bin_share, Avg_Value_Dwelling)

# write_csv(ces84_income_inequality,
#           here("data-raw/statscan/1981_household_income/ces84_with_1981_ginis.csv"))
