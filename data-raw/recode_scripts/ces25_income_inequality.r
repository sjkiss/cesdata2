# Run these lines if you have not run the main ces25_recode.R recode script
# If you are calling this script from within ces25_recode.R then comment these lines out

library(haven)
library(here)
library(tidyverse)
library(srvyr)
library(survey)
library(labelled)
library(binsmooth)

# ==============================================================================
# LOAD CENSUS DATA
# ==============================================================================
# Load in federal
#census2021<-read.csv(file=here("data-raw/statscan/2022_federal_representation_order/98-401-X2021029_English_CSV_data.csv"))
#save(census2021,
#     file=here("data-raw/statscan/2022_federal_representation_order/2022_federal_representation_order_statistics_canada_profiles.rdata"), compress="xz")
load("data-raw/statscan/2022_federal_representation_order/2022_federal_representation_order_statistics_canada_profiles.rdata")
census2021 %>%
  count(GEO_LEVEL)
census2021 %>%
  filter(str_detect(GEO_LEVEL,"Federal electoral district"))->census2021

fed_lookup <- census2021 %>% distinct(GEO_NAME, ALT_GEO_CODE)

# ==============================================================================
# INCOME GINI
# ==============================================================================
census2021 %>%
  filter((CHARACTERISTIC_ID>260&CHARACTERISTIC_ID<281&CHARACTERISTIC_ID!=276)|
           str_detect(CHARACTERISTIC_NAME, "Gini index on adjusted household total income")|
          CHARACTERISTIC_ID==252) %>%
  select(GEO_NAME,CHARACTERISTIC_NAME, CHARACTERISTIC_ID, C1_COUNT_TOTAL) %>%
  pivot_wider(., id_cols=GEO_NAME,names_from=CHARACTERISTIC_NAME, values_from = C1_COUNT_TOTAL) %>%
  rename(`Gini`=22, `Income`=2)->census2021_income_data

names(census2021_income_data)<-str_trim(names(census2021_income_data))
census2021_income_data %>% names()

# Right points of income intervals
income_right_edges<-c(5000,9999,14999,19999,24999,29999,34999,39999,44999,49999,59999,69999,79999,89999,99999,124999,149999,199999,Inf)

# Pivot down the income categories to get the counts
census2021_income_data %>%
  pivot_longer(.,
               cols=3:21,
               names_to=c("Category"), values_to = c("Count")) %>%
  mutate(right_edges=rep(income_right_edges, length(unique(census2021_income_data$GEO_NAME)))) ->census2021_income_long

# Run the splinebins fit
income_fits <- census2021_income_long %>%
  group_by(GEO_NAME) %>%
  nest() %>%
  mutate(fit = map(data, ~ {
    edges <- .x$right_edges
    edges[is.na(edges)] <- Inf
    splinebins(edges, .x$Count, m = .x$Income[1])
  })) %>%
  mutate(
    income_gini         = map_dbl(fit, gini),
    income_fitWarn       = map_lgl(fit, "fitWarn"),
    fed_avg_income       = map_dbl(data, ~ first(.x$Income)),
    statcan_income_gini  = map_dbl(data, ~ first(.x$Gini))
  ) %>%
  select(GEO_NAME, income_gini, income_fitWarn, fed_avg_income, statcan_income_gini)

# Benchmarking: compare spline-fit Gini against StatCan's own published Gini
# (adjusted household total income). Note this compares two different income
# concepts -- ours is unadjusted household income brackets, StatCan's is
# equivalized/adjusted for household size -- so a systematic offset between
# the two is expected, not necessarily an error.
income_fits %>%
  mutate(error_pct = (income_gini - statcan_income_gini) / statcan_income_gini) %>%
  ggplot(aes(x = error_pct)) + geom_histogram() + labs(title = "distribution of percent errors")
ggsave(filename=here('data-raw/statscan/2022_federal_representation_order/histogram_percent_errors.png'))

income_fits %>%
  ggplot(aes(x = statcan_income_gini, y = income_gini)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Comparing Statscan Ginis with Ginis from Splinebins") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  coord_fixed()
ggsave(filename = here("data-raw/statscan/2022_federal_representation_order/diagnosing_errors_cdf_interpolated_ginis.png"))

# ==============================================================================
# EDUCATION GINI
# ==============================================================================
# Using StatCan's 2021 Census "Highest certificate, diploma or degree"
# hierarchy for the population aged 15+. Same underlying issue as the 1981
# schooling data: several categories cannot be defensibly ordered against
# each other in years of schooling.
#
# "postsec_sub_bach" merges four StatCan leaf categories (non-apprenticeship
# trades, apprenticeship, college/CEGEP, university cert below bachelor) using
# StatCan's own pre-aggregated "Postsecondary certificate or diploma below
# bachelor level" total (ID 2002) rather than manually summing the four leaf
# IDs -- StatCan's random rounding for confidentiality means a manual sum can
# differ from the published aggregate by a few units. These four categories
# don't have a defensible years-based ordering against each other, mirroring
# the trades/other-non-university issue in the 1981 data.
#
# The above-bachelor tier is reordered relative to StatCan's own published
# listing (which lists cert-above-bachelor -> professional degree -> Master's
# -> doctorate). Based on typical program length (Canadian professional
# programs in medicine/dentistry/veterinary medicine/optometry require ~4
# years beyond a bachelor's, vs. ~1-2 years for a master's), Master's is
# placed BEFORE the professional-degree category here. This is a judgment
# call, not a StatCan figure -- flag it if reporting these numbers, and
# reconsider if you have a better basis for ordering these.
bin_names <- c("no_cert", "hs_diploma", "postsec_sub_bach", "bachelors",
               "cert_above_bach", "masters", "professional_degree", "doctorate")

education_ids <- setNames(c(1999, 2000, 2002, 2009, 2010, 2012, 2011, 2013), bin_names)

# binsmooth's bEdges = right endpoint of each bin only, length == length(bCounts),
# left edge of first bin assumed 0, final edge always ignored (top bin unbounded
# regardless of value).
right_edges <- setNames(c(11, 12, 15, 16, 17, 18, 20, NA), bin_names)

# Plain unnamed vectors for the midpoint arithmetic -- R's named-vector
# arithmetic silently shifts the attached names by one position here
# (verified); using unnamed vectors and reattaching bin_names explicitly
# avoids that landmine.
right_edges_num <- unname(right_edges)
left_edges_num  <- c(0, head(right_edges_num, -1))
top_proxy       <- 2 * right_edges_num[length(right_edges_num) - 1]
edges_for_mid   <- c(right_edges_num[-length(right_edges_num)], top_proxy)
bin_mids        <- (left_edges_num + edges_for_mid) / 2
category_years  <- tibble(Category = bin_names, Years = bin_mids)

census2021_education_data <- census2021 %>%
  filter(CHARACTERISTIC_ID %in% education_ids) %>%
  mutate(Category = names(education_ids)[match(CHARACTERISTIC_ID, education_ids)]) %>%
  select(GEO_NAME, Category, C1_COUNT_TOTAL) %>%
  pivot_wider(id_cols = GEO_NAME, names_from = Category, values_from = C1_COUNT_TOTAL) %>%
  select(GEO_NAME, all_of(bin_names))  # enforce exact bin order, don't rely on pivot_wider's default order

# Weighted mean years of schooling per FED (internal estimate -- no
# independently published "average years of schooling" exists in this table,
# unlike income's StatCan-published Avg_Income).
education_long <- census2021_education_data %>%
  pivot_longer(cols = -GEO_NAME, names_to = "Category", values_to = "Count") %>%
  left_join(category_years, by = "Category") %>%
  group_by(GEO_NAME) %>%
  mutate(mean_schooling = if_else(
    sum(Count, na.rm = TRUE) > 0,
    sum(Count * Years, na.rm = TRUE) / sum(Count, na.rm = TRUE),
    NA_real_
  )) %>%
  ungroup()

# Run the stepbins fit. stepbins() rather than splinebins() -- schooling bins
# include several near-point-mass categories (hs_diploma, bachelors), which
# destabilize a smooth spline fit; this was also true of the 1981 schooling
# data and stepbins() was the more reliable choice there.
education_fits <- education_long %>%
  nest(data = -GEO_NAME) %>%
  mutate(fit = map(data, ~ {
    counts <- .x$Count[match(bin_names, .x$Category)]  # enforce bin order to match right_edges
    stepbins(unname(right_edges), counts, m = .x$mean_schooling[1])
  })) %>%
  mutate(
    education_gini = map_dbl(fit, gini),
    mean_schooling = map_dbl(data, ~ first(.x$mean_schooling))
  ) %>%
  select(GEO_NAME, education_gini, mean_schooling)

education_fits %>%
  ggplot(aes(x = education_gini)) + geom_histogram() + labs(title = "Distribution of education Ginis")

# ---- Top-bin population share (diagnostic) ---------------------------------
# binsmooth always treats the top bin as unbounded, so the LARGER the
# population share sitting in it, the more the fitted Gini depends on the
# assumed mean/tail shape rather than genuine measured dispersion. Report
# this share alongside the Gini so a 1984-vs-2025 comparison can distinguish
# "the Gini moved because more people are now in an uncapped category" from
# "the Gini moved because within-group dispersion genuinely changed."
top_bin_share <- education_long %>%
  group_by(GEO_NAME) %>%
  summarise(top_bin_share = Count[Category == "doctorate"] / sum(Count), .groups = "drop")

# ---- HARMONIZED EDUCATION GINI (bachelor's-or-higher collapsed, matching
# 1981's top-bin structure, for the 1984-vs-2025 comparison) ----------------
# IMPORTANT: the harmonized fit reuses `mean_schooling` computed from the
# FULL-granularity data above as its `m` argument, rather than recomputing a
# separate mean from the collapsed 4-bin midpoints. This matters a lot: an
# earlier version of this pipeline used binsmooth's own "double the previous
# edge" fallback convention to estimate the collapsed top bin's mean (a
# reasonable guess for an income bracket, where a "$200,000+" bin plausibly
# does have a mean above $200,000). For years-of-schooling that convention is
# badly wrong -- the collapsed "bachelor's-or-higher" bin is overwhelmingly
# bachelor's holders, not doctorate holders, and supplying an inflated mean
# forced the algorithm to manufacture dispersion that isn't really there.
# Tested: this pushed the mean harmonized Gini from ~0.16 up to ~0.31 despite
# the true, data-derived top-bin mean implying a Gini within 0.01 of the
# full-granularity estimate. Reusing the true overall mean here (which we can
# compute because we have the full sub-category breakdown for 2021, unlike
# 1981) avoids that artifact entirely -- verified: correlation between
# education_gini and education_gini_harmonized is 0.99 with this fix.
right_edges_harmonized <- c(11, 12, 15, NA)

education_fits_harmonized <- education_long %>%
  nest(data = -GEO_NAME) %>%
  mutate(fit = map(data, ~ {
    top_total <- sum(.x$Count[.x$Category %in%
                     c("bachelors", "cert_above_bach", "masters", "professional_degree", "doctorate")])
    counts_h <- c(
      .x$Count[.x$Category == "no_cert"],
      .x$Count[.x$Category == "hs_diploma"],
      .x$Count[.x$Category == "postsec_sub_bach"],
      top_total
    )
    stepbins(right_edges_harmonized, counts_h, m = .x$mean_schooling[1])
  })) %>%
  mutate(education_gini_harmonized = map_dbl(fit, gini)) %>%
  select(GEO_NAME, education_gini_harmonized)

# ==============================================================================
# COMBINE INTO ONE CLEAN PER-FED TABLE
# ==============================================================================
census2021_ginis <- fed_lookup %>%
  left_join(income_fits, by = "GEO_NAME") %>%
  left_join(education_fits, by = "GEO_NAME") %>%
  left_join(education_fits_harmonized, by = "GEO_NAME") %>%
  left_join(top_bin_share, by = "GEO_NAME")

stopifnot(nrow(census2021_ginis) == nrow(fed_lookup))

write_csv(census2021_ginis,
          file = here("data-raw/statscan/2022_federal_representation_order/statistics_canada_federal_electoral_districts_ginis_2021.csv"))

# ==============================================================================
# LOAD CES25 AND MERGE
# ==============================================================================
ces25 <- read_dta(here("data-raw/ces25.dta"))

ces25 %>%
  left_join(census2021_ginis, by = c("feduid" = "ALT_GEO_CODE")) -> ces25

# Diagnostic: any feduid values that failed to match a FED. In this data these
# are StatCan/CES placeholder codes (e.g. XX999, blank fedname) for "outside
# Canada" or "prefer not to say" responses, not real ridings -- worth
# confirming that's still true if this is rerun on updated data.
ces25 %>%
  distinct(feduid, fedname) %>%
  anti_join(census2021_ginis, by = c("feduid" = "ALT_GEO_CODE")) %>%
  print(n = 50)

cat("Respondents with no matched income_gini:", sum(is.na(ces25$income_gini)), "of", nrow(ces25), "\n")
