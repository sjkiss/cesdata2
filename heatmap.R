#### MIP data mining: 2021 vs 2025 salience shift + 2025 co-occurrence
#### Assumes: ces21 has the *_mip dummies from the 2021 recode script,
####          ces25 has the .dum dummies from ces25_mip_recode (revised).
#### Run those recode scripts first so the dummies exist.

library(tidyverse)

# --- 2021 shares ---
shares21 <- ces21 %>%
  summarise(
    Environment      = mean(replace_na(enviro_mip.x, 0)),
    Crime            = mean(replace_na(crime_mip.x, 0)),
    Ethics           = mean(replace_na(ethics_mip.x, 0)),
    Education        = mean(replace_na(education_mip.x, 0)),
    Energy           = mean(replace_na(energy_mip.x, 0)),
    Jobs             = mean(replace_na(jobs_mip.x, 0)),
    Economy          = mean(replace_na(economy_mip.x, 0)),
    Health           = mean(replace_na(health_mip.x, 0)),
    Taxes            = mean(replace_na(taxes_mip.x, 0)),
    `Deficit / Debt` = mean(replace_na(debt_mip.x, 0)),
    Democracy        = mean(replace_na(democracy_mip.x, 0)),
    `Foreign Affairs`= mean(replace_na(foreign_mip.x, 0)),
    Immigration      = mean(replace_na(immigration_mip.x, 0)),
    `Socio-Cultural` = mean(replace_na(socio_cultural_mip.x, 0)),
    `Social Programs`= mean(replace_na(social_mip.x, 0)),
    Brokerage        = mean(replace_na(brokerage_mip.x, 0)),
    `Free Trade`     = NA_real_,
    Inflation        = mean(replace_na(inflation_mip.x, 0)),
    Housing          = mean(replace_na(housing_mip.x, 0)),
    `COVID-19`       = mean(replace_na(covid_mip.x, 0))
  ) %>%
  pivot_longer(everything(), names_to = "issue", values_to = "share") %>%
  mutate(year = "2021")

# --- 2025 shares ---
shares25 <- ces25 %>%
  summarise(
    Environment      = mean(enviro.dum, na.rm = TRUE),
    Crime            = mean(crime.dum, na.rm = TRUE),
    Ethics           = mean(ethics.dum, na.rm = TRUE),
    Education        = mean(education.dum, na.rm = TRUE),
    Energy           = mean(energy.dum, na.rm = TRUE),
    Jobs             = mean(jobs.dum, na.rm = TRUE),
    Economy          = mean(economy.dum, na.rm = TRUE),
    Health           = mean(health.dum, na.rm = TRUE),
    Taxes            = mean(taxes.dum, na.rm = TRUE),
    `Deficit / Debt` = mean(debt.dum, na.rm = TRUE),
    Democracy        = mean(democracy.dum, na.rm = TRUE),
    `Foreign Affairs`= mean(foreign.dum, na.rm = TRUE),
    Immigration      = mean(immigration.dum, na.rm = TRUE),
    `Socio-Cultural` = mean(socio_cultural.dum, na.rm = TRUE),
    `Social Programs`= mean(social.dum, na.rm = TRUE),
    Brokerage        = mean(brokerage.dum, na.rm = TRUE),
    `Free Trade`     = mean(free_trade.dum, na.rm = TRUE),
    Inflation        = mean(inflation.dum, na.rm = TRUE),
    Housing          = mean(housing.dum, na.rm = TRUE),
    `COVID-19`       = mean(covid.dum, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "issue", values_to = "share") %>%
  mutate(year = "2025")

# --- combine and plot ---
shares <- bind_rows(shares21, shares25)

issue_order <- shares %>%
  filter(year == "2025") %>%
  arrange(share) %>%
  pull(issue)

p_shift <- shares %>%
  mutate(issue = factor(issue, levels = issue_order)) %>%
  ggplot(aes(x = issue, y = share, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("2021" = "grey60", "2025" = "#c8102e")) +
  labs(
    title = "Most Important Problem: 2021 vs 2025",
    subtitle = "Share of respondents whose open-ended answer mentioned each issue",
    x = NULL, y = "Share of respondents", fill = "CES wave",
    caption = "Free Trade: no 2021 dictionary."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_shift)

# percentage-point change table
shares %>%
  pivot_wider(names_from = year, values_from = share) %>%
  mutate(pp_change = (`2025` - `2021`) * 100) %>%
  arrange(desc(pp_change)) %>%
  print(n = 25)

## =====================================================================
## PART 2 — 2025 issue co-occurrence heatmap
## =====================================================================
## How often does a respondent who mentions issue A also mention issue B?

dum_labels <- c(
  enviro.dum = "Environment",   crime.dum = "Crime",
  ethics.dum = "Ethics",        education.dum = "Education",
  energy.dum = "Energy",        jobs.dum = "Jobs",
  economy.dum = "Economy",      health.dum = "Health",
  taxes.dum = "Taxes",          debt.dum = "Deficit/Debt",
  democracy.dum = "Democracy",  foreign.dum = "Foreign Affairs",
  immigration.dum = "Immigration", socio_cultural.dum = "Socio-Cultural",
  social.dum = "Social Programs", brokerage.dum = "Brokerage",
  free_trade.dum = "Free Trade", inflation.dum = "Inflation",
  housing.dum = "Housing",      covid.dum = "COVID-19"
)

M <- ces25 %>%
  select(all_of(names(dum_labels))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  as.matrix()
colnames(M) <- dum_labels[colnames(M)]

# raw co-occurrence counts (diagonal = total mentions of each issue)
co_counts <- crossprod(M)

# Jaccard similarity: overlap / union — better than raw counts because it
# isn't dominated by whichever issues are simply most common
n_i     <- diag(co_counts)
jaccard <- co_counts / (outer(n_i, n_i, "+") - co_counts)
diag(jaccard) <- NA  # blank the diagonal so the scale isn't wrecked by 1s

# tidy for ggplot
co_long <- as_tibble(jaccard, rownames = "issue_a") %>%
  pivot_longer(-issue_a, names_to = "issue_b", values_to = "jaccard")

# order both axes by total mention count so big issues cluster together
ord <- names(sort(n_i, decreasing = TRUE))

p_cooc <- co_long %>%
  mutate(issue_a = factor(issue_a, levels = ord),
         issue_b = factor(issue_b, levels = rev(ord))) %>%
  ggplot(aes(issue_a, issue_b, fill = jaccard)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(jaccard), "",
                               sprintf("%.2f", jaccard))), size = 2.6) +
  scale_fill_gradient(low = "white", high = "#c8102e", na.value = "grey95") +
  labs(
    title = "Issue Co-occurrence in 2025 MIP Responses",
    subtitle = "Jaccard similarity: respondents mentioning both issues / mentioning either",
    x = NULL, y = NULL, fill = "Jaccard"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_cooc)
ggsave("mip_2025_cooccurrence.png", p_cooc, width = 10, height = 9, dpi = 300)

# top 15 co-occurring pairs as a table (each pair once)
top_pairs <- co_long %>%
  filter(!is.na(jaccard), as.character(issue_a) < as.character(issue_b)) %>%
  arrange(desc(jaccard)) %>%
  slice_head(n = 15)
print(top_pairs)



## ---- Dictionary patch: terms found in the uncoded residual ----

patch_and_merge <- function(data, dumcol, terms) {
  d   <- dictionary(list(x = terms))
  hit <- runDictionary(data, cps25_imp_iss, d)$x
  data[[dumcol]] <- pmax(data[[dumcol]], ifelse(hit >= 1, 1, 0), na.rm = TRUE)
  data
}

ces25 <- ces25 %>%
  patch_and_merge("inflation.dum",
                  c("linflation", "cost*", "rising costs", "couts", "coûts")) %>%
  patch_and_merge("education.dum",
                  c("léducation", "leducation", "lducation")) %>%
  patch_and_merge("foreign.dum",
                  c("étatsunis", "etatsunis", "létatsunis", "letatsunis",
                    "sovereignity", "souveraineté*", "lindépendance", "lindependance",
                    "stand up for canada")) %>%
  patch_and_merge("social.dum",
                  c("aîné*", "aînés", "les aînés")) %>%
  patch_and_merge("enviro.dum",
                  c("développement durable", "durable")) %>%
  patch_and_merge("socio_cultural.dum",
                  c("misinformation", "désinformation", "desinformation", "fake news")) %>%
  patch_and_merge("brokerage.dum",
                  c("western alienation", "alienation", "alberta"))

## expand the idk / non-answer dictionary
dict_idk2 <- dictionary(list(idk = c(
  "na", "n/a", "none", "not sure", "unsure", "unknown", "no idea",
  "do not know", "dont know", "don't know", "aucun", "aucune", "rien",
  "aucune idée", "je ne sais pas", "everything", "tout", "good", "ok",
  "okay", "yes", "non", "nothing", "idk", "no comment")))
ces25.idk2 <- runDictionary(ces25, cps25_imp_iss, dict_idk2)
ces25$idk.dum <- pmax(ces25$idk.dum, ifelse(ces25.idk2$idk >= 1, 1, 0), na.rm = TRUE)

## ---- recompute diagnostics ----
dum_cols <- c("enviro.dum","crime.dum","ethics.dum","education.dum","energy.dum",
              "jobs.dum","economy.dum","health.dum","taxes.dum","debt.dum",
              "democracy.dum","foreign.dum","immigration.dum","socio_cultural.dum",
              "social.dum","brokerage.dum","free_trade.dum","inflation.dum",
              "housing.dum","covid.dum")

ces25 <- ces25 %>%
  mutate(n_issues = rowSums(across(all_of(dum_cols)), na.rm = TRUE))

ces25 %>%
  summarise(
    uncoded_share    = mean(n_issues == 0 & idk.dum == 0, na.rm = TRUE),
    multi_code_share = mean(n_issues >= 2, na.rm = TRUE)
  )

ces25 %>%
  filter(n_issues == 0, idk.dum == 0) %>%
  count(cps25_imp_iss, sort = TRUE) %>%
  print(n = 50)


prop.table(table(as_factor(ces21$mip)))

ces21 %>%
  filter(ethics_mip.x == 1) %>%
  count(mip_lower, sort = TRUE) %>%
  print(n = 30)
