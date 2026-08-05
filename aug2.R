#### Recode script for ces25 most important problem
#### Dictionaries aligned with the 2021 coding scheme (codes 1-20)
#### v3 -- path fixed, package list trimmed, convert() namespaced
####
#### 2021 scheme: Other=0, Environment=1, Crime=2, Ethics=3, Education=4,
#### Energy=5, Jobs=6, Economy=7, Health=8, Taxes=9, Deficit_Debt=10,
#### Democracy=11, Foreign_Affairs=12, Immigration=13, Socio_Cultural=14,
#### Social_Programs=15, Brokerage=16, Free_Trade=17, Inflation=18,
#### Housing=19, COVID19=20
####
#### RUN THIS IN A FRESH SESSION. Session > Restart R, and turn off
#### "Restore .RData into workspace at startup" in Preferences. A stale
#### ces25 from .RData will get cleaned twice and give wrong counts
#### without ever throwing an error.


## ---------------------------------------------------------------------
## Packages
## Trimmed from v2. Dropped: tm, NLP, SnowballC, tidytext,
## quanteda.textstats, progressr, lubridate, magrittr, rio -- none are
## used here and several mask quanteda functions. rio::convert() in
## particular masks quanteda::convert() and breaks runDictionary().
## ---------------------------------------------------------------------
library(tidyverse)
library(haven)
library(quanteda)
library(crayon)
library(progress)
library(tictoc)
library(gtsummary)


## ---------------------------------------------------------------------
## Data
## Path is relative to the cesdata2 project root.
## NOTE: data-raw/ is the convention for an R data package. If
## file.exists("DESCRIPTION") is TRUE and data/ces25.rda exists, use
## data("ces25") instead -- that is the processed version the rest of
## the pipeline expects, and the raw .dta may differ.
## ---------------------------------------------------------------------
ces25 <- haven::read_dta("data-raw/ces25.dta")

stopifnot("cps25_imp_iss" %in% names(ces25))


## ---------------------------------------------------------------------
## Function
## convert() is namespaced explicitly -- rio, if it ever gets attached,
## masks it with a completely different signature.
## pull_count() guards against a dictionary key with zero hits anywhere
## in the corpus: convert() drops the column entirely, so df$key returns
## NULL and the assignment silently dies.
## ---------------------------------------------------------------------
runDictionary <- function(dataA, word, dictionaryA) {
  tictoc::tic()
  dataA <- dataA %>%
    mutate(word = {{word}})
  corpusA <- quanteda::tokens(dataA$word)
  dfmA <- quanteda::dfm(quanteda::tokens_lookup(corpusA,
                                                dictionaryA,
                                                nested_scope = "dictionary"))
  pb <- progress::progress_bar$new(
    format = crayon::yellow(" matching [:bar] :percent in :elapsed"),
    total = 100, clear = FALSE, width = 60)
  purrr::walk(1:100, ~{pb$tick(); Sys.sleep(0.01)})
  message(crayon::green("100% expressions/words found"))
  tictoc::toc()
  dataB <- quanteda::convert(dfmA, to = "data.frame")
  return(dataB)
}

pull_count <- function(df, key) {
  if (key %in% names(df)) df[[key]] else 0L
}


## =====================================================================
## TEXT CLEANING  -- this is where the multi-word fix lives
##
## Three changes from Katie's original:
##  (1) str_to_lower() instead of str_replace_all(x, "[A-Z]", tolower).
##      [A-Z] does not cover accented capitals, so "Économie",
##      "Éducation", "États-Unis" were never lowercased and never
##      matched the dictionaries.
##  (2) The two-word swap line is DELETED. sub("^(\\w+)\\s+(\\w+)$",
##      "\\2 \\1") reverses any response that is exactly two words --
##      "social programs" becomes "programs social". Most MIP responses
##      are one or two words, so this alone breaks phrase matching for
##      the majority of the file. (In the 2021-aligned draft it was a
##      no-op anyway: the caret was a modifier letter U+02C6 "ˆ", not
##      "^", so the regex never anchored.)
##  (3) Separators are normalised BEFORE phrases are compounded, and
##      every multi-word dictionary entry is compounded into one token.
## =====================================================================

x <- ces25$cps25_imp_iss

# (1) lowercase, accents included
x <- stringr::str_to_lower(x)

# (2) hyphens/slashes -> space; apostrophes deleted.
#     Order matters: this has to happen before compounding so that
#     "health-care", "co-vid", "sans-abris", "états-unis", "don't know"
#     all reach the phrase map in their spaced/stripped form.
x <- stringr::str_replace_all(x, "[\u2010-\u2015\\-/]", " ")
x <- stringr::str_replace_all(x, "['\u2018\u2019`]", "")
x <- stringr::str_squish(x)

# (3) compound every multi-word dictionary entry into one token.
#     Longer patterns come first where two patterns overlap.
#     Entries without a trailing \\b are deliberate: they let the plural
#     ride along ("social programs" -> "socialprograms"), which the glob
#     entries in the dictionaries then catch.
phrase_map <- c(
  # -- non-answers --
  "\\bje ne sais pas\\b"          = "jenesaispas",
  "\\bje ne saos pas\\b"          = "jenesaispas",
  "\\bje bsais pas\\b"            = "jenesaispas",
  "\\bjw sais pas\\b"             = "jenesaispas",
  "\\bje sais pas\\b"             = "jenesaispas",
  "\\bje ne connais\\b"           = "jenesaispas",
  "\\bjai pas de reponse\\b"      = "jaipasdereponse",
  "\\bdont know\\b"               = "dontknow",
  "\\bprefer not\\b"              = "prefernot",
  # -- democracy --
  "\\bfirst past the post\\b"     = "firstpastthepost",
  # -- foreign affairs / US relations (all US variants -> one token) --
  "\\bles etats? unis\\b"         = "etatsunis",
  "\\bles \u00e9tats? unis\\b"    = "etatsunis",
  "\\b\u00e9tats? unis\\b"        = "etatsunis",
  "\\betats? unis\\b"             = "etatsunis",
  "\\bthe states\\b"              = "thestates",
  "\\bcanadian identity\\b"       = "canadianidentity",
  "\\bkeeping canada\\b"          = "keepingcanada",
  "\\blint\u00e9grit\u00e9 territoriale\\b" = "lintegriteterritoriale",
  "\\blintegrite territoriale\\b" = "lintegriteterritoriale",
  "\\bforeign policy\\b"          = "foreignpolicy",
  # -- free trade --
  "\\bbig beautiful bill\\b"      = "bigbeautifulbill",
  "\\bfree trade\\b"              = "freetrade",
  "\\btrade war\\b"               = "tradewar",
  # -- inflation --
  "\\bcost of living\\b"          = "costofliving",
  "\\brising cost"                = "risingcost",
  # -- economy --
  "\\bmiddle class\\b"            = "middleclass",
  "\\bsmall business"             = "smallbusiness",
  "\\binterest rate"              = "interestrate",
  # -- social programs --
  "\\bsocial program"             = "socialprogram",
  "\\bsocial service"             = "socialservice",
  "\\bsocial assistance\\b"       = "socialassistance",
  "\\bsocial aid\\b"              = "socialaid",
  "\\baide sociale\\b"            = "aidesociale",
  "\\bbasic income\\b"            = "basicincome",
  "\\bold people\\b"              = "oldpeople",
  # -- socio-cultural --
  "\\bfirst nation"               = "firstnation",
  # -- crime --
  "\\bcatch and release\\b"       = "catchandrelease",
  # -- deficit / debt --
  "\\bgovernment waste\\b"        = "governmentwaste",
  # -- education --
  "\\bstudent loan"               = "studentloan",
  # -- environment --
  "\\bclimate change\\b"          = "climatechange",
  # -- health --
  "\\bhealth care\\b"             = "healthcare",
  "\\bbien \u00eatre\\b"          = "bienetre",
  "\\bbien etre\\b"               = "bienetre",
  # -- housing --
  "\\bsans abris?\\b"             = "sansabris",
  # -- covid --
  "\\bco vid\\b"                  = "covid",
  # -- brokerage --
  "\\bloi 21\\b"                  = "loi21",
  "\\bbill 21\\b"                 = "loi21",
  "\\bbill 96\\b"                 = "bill96"
)

x <- stringr::str_replace_all(x, phrase_map)

# (4) strip whatever punctuation is left
x <- stringr::str_replace_all(x, "[[:punct:]]", "")

ces25$cps25_imp_iss <- stringr::str_squish(x)


## =====================================================================
## 1. ENVIRONMENT  (oil/gas/pipeline removed -> Energy)
## =====================================================================
dict_enviro <- dictionary(list(enviro = c(
  "environ*", "climat*", "\u00e9colog*", "ecolog*", "envir*", "pollut*",
  "environnement", "environmental", "environment", "lenvironnement",
  "climate", "climatechange", "warming", "climatiques", "rechauffement",
  "enviroment", "enviromental", "ecology", "co2", "polluer", "pollute",
  "pollution", "planet", "plan\u00e8te", "earth", "green", "greener",
  "sustainability", "water", "renewable*", "ges", "environnemental",
  "enviournment", "envioroment", "enviornment",
  "enironment", "environement", "environmentalism")))
ces25.enviro <- runDictionary(ces25, cps25_imp_iss, dict_enviro)
ces25$enviro_mip <- pull_count(ces25.enviro, "enviro")
ces25 <- ces25 %>% mutate(enviro.dum = ifelse(enviro_mip >= 1, 1, 0))


## =====================================================================
## 2. CRIME  (security/military moved to Foreign_Affairs; safety/guns stay)
## =====================================================================
dict_crime <- dictionary(list(crime = c(
  "crime", "crimes", "crimin*", "criminal", "criminals", "gang", "gangs",
  "safe*", "safety", "gun", "guns", "firearm", "firearms", "violence",
  "illegal", "cop", "law", "terroris*", "catchandrelease", "protests")))
ces25.crime <- runDictionary(ces25, cps25_imp_iss, dict_crime)
ces25$crime_mip <- pull_count(ces25.crime, "crime")
ces25 <- ces25 %>% mutate(crime.dum = ifelse(crime_mip >= 1, 1, 0))


## =====================================================================
## 3. ETHICS  ("rights" moved to Socio_Cultural)
## =====================================================================
dict_ethics <- dictionary(list(ethics = c(
  "corrupt*", "corupt*", "honest*", "honnet*", "honntet", "ethic*",
  "transparen*", "accountab*", "responsib*", "truth", "v\u00e9rit\u00e9*",
  "lies", "lying", "lie*", "liar", "dishonesty", "integr*", "int\u00e9gri*",
  "moral*", "trust", "trustworthy", "credibility", "greed", "promesses",
  "promise*", "fair*", "justice", "governance", "integrity")))
ces25.ethics <- runDictionary(ces25, cps25_imp_iss, dict_ethics)
ces25$ethics_mip <- pull_count(ces25.ethics, "ethics")
ces25 <- ces25 %>% mutate(ethics.dum = ifelse(ethics_mip >= 1, 1, 0))


## =====================================================================
## 4. EDUCATION
## =====================================================================
dict_education <- dictionary(list(education = c(
  "educat*", "\u00e9ducat*", "educ", "ducation", "lducation", "leducation",
  "deducation", "school", "schools", "schooling", "university", "tuition",
  "tuitition", "student", "students", "studentloan*")))
ces25.education <- runDictionary(ces25, cps25_imp_iss, dict_education)
ces25$education_mip <- pull_count(ces25.education, "education")
ces25 <- ces25 %>% mutate(education.dum = ifelse(education_mip >= 1, 1, 0))


## =====================================================================
## 5. ENERGY  (pulled from Katie's Environment dictionary)
## =====================================================================
dict_energy <- dictionary(list(energy = c(
  "pipeline*", "oil", "gas", "carbon", "carbone", "fossil", "fossiles",
  "energy", "energie", "\u00e9nergie", "\u00e9nerg\u00e9tiques", "nergtiques",
  "nuclear", "nucl\u00e9aire")))
ces25.energy <- runDictionary(ces25, cps25_imp_iss, dict_energy)
ces25$energy_mip <- pull_count(ces25.energy, "energy")
ces25 <- ces25 %>% mutate(energy.dum = ifelse(energy_mip >= 1, 1, 0))


## =====================================================================
## 6. JOBS  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_jobs <- dictionary(list(jobs = c(
  "job*", "employ*", "emploi", "emploie", "work", "career", "income",
  "salar*", "wage", "wages", "unempl*", "unemployed", "unemployment",
  "doeuvre", "personnel")))
ces25.jobs <- runDictionary(ces25, cps25_imp_iss, dict_jobs)
ces25$jobs_mip <- pull_count(ces25.jobs, "jobs")
ces25 <- ces25 %>% mutate(jobs.dum = ifelse(jobs_mip >= 1, 1, 0))


## =====================================================================
## 7. ECONOMY  (narrowed: taxes/debt/inflation/jobs/trade removed)
## =====================================================================
dict_economy <- dictionary(list(economy = c(
  "econom*", "\u00e9con*", "economie", "\u00e9conomie", "economics",
  "economist", "\u00e9conomiste", "recession", "growth", "market", "industr*",
  "commerciale", "business*", "finance", "finances", "financ*",
  "financi\u00e8re", "money", "argent", "largent", "dollars", "invest*",
  "investissements", "wealth", "recovery", "interestrate*", "middleclass",
  "smallbusiness*", "infrastructure*", "evonomy", "enconomie", "ecomomie",
  "econamy", "ecomomy", "econics", "ecomony", "leconomie",
  "l\u00e9conomie")))
ces25.economy <- runDictionary(ces25, cps25_imp_iss, dict_economy)
ces25$economy_mip <- pull_count(ces25.economy, "economy")
ces25 <- ces25 %>% mutate(economy.dum = ifelse(economy_mip >= 1, 1, 0))


## =====================================================================
## 8. HEALTH  (renamed from healthcare; disability moved to Social_Programs)
## =====================================================================
dict_health <- dictionary(list(health = c(
  "health*", "heath*", "healthcare", "care", "sant*",
  "sant\u00e9", "sante", "soins", "soin", "mental", "pharma*", "pharmacare",
  "drug*", "medic*", "medical", "medicine", "doctor*", "docteur",
  "prescript*", "hospitals", "wellbeing", "bienetre", "bien\u00eatre", "life",
  "medicade", "medicaid")))
ces25.health <- runDictionary(ces25, cps25_imp_iss, dict_health)
ces25$health_mip <- pull_count(ces25.health, "health")
ces25 <- ces25 %>% mutate(health.dum = ifelse(health_mip >= 1, 1, 0))


## =====================================================================
## 9. TAXES  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_taxes <- dictionary(list(taxes = c(
  "tax", "tax*", "taxe", "taxes", "taxed", "taxing", "taxation", "taxs",
  "impot", "impots", "imp\u00f4t*", "impts", "dimpot", "dimpo", "dimpt",
  "dimp\u00f4ts", "tqxes")))
ces25.taxes <- runDictionary(ces25, cps25_imp_iss, dict_taxes)
ces25$taxes_mip <- pull_count(ces25.taxes, "taxes")
ces25 <- ces25 %>% mutate(taxes.dum = ifelse(taxes_mip >= 1, 1, 0))


## =====================================================================
## 10. DEFICIT_DEBT  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_debt <- dictionary(list(debt = c(
  "debt", "dept", "debit", "dette*", "deficit", "deficits", "d\u00e9ficit",
  "defic*", "dficit", "dficits", "budget*", "budg\u00e8*", "budg\u00e9taire",
  "budgtaire", "buget", "fiscal*", "spend*", "spending", "depenses*",
  "d\u00e9penses", "dpense", "austerity", "lendettement", "governmentwaste")))
ces25.debt <- runDictionary(ces25, cps25_imp_iss, dict_debt)
ces25$debt_mip <- pull_count(ces25.debt, "debt")
ces25 <- ces25 %>% mutate(debt.dum = ifelse(debt_mip >= 1, 1, 0))


## =====================================================================
## 11. DEMOCRACY  (Katie's "election" renamed; "citizen*" added per Simon)
## =====================================================================
dict_democracy <- dictionary(list(democracy = c(
  "democr*", "d\u00e9mocr*", "elect*", "\u00e9lect*", "vot*", "voter",
  "voting", "vote", "ballot*", "scrutin", "representation", "proportional",
  "firstpastthepost", "constitution", "federal", "citizen*", "citoyen*")))
ces25.democracy <- runDictionary(ces25, cps25_imp_iss, dict_democracy)
ces25$democracy_mip <- pull_count(ces25.democracy, "democracy")
ces25 <- ces25 %>% mutate(democracy.dum = ifelse(democracy_mip >= 1, 1, 0))


## =====================================================================
## 12. FOREIGN_AFFAIRS  (Katie's security + Trump + borders, per Matt's
##     rule: Trump alone / USA relations / sovereignty -> here)
## =====================================================================
dict_foreign <- dictionary(list(foreign = c(
  # Trump / USA relations / sovereignty
  "trump", "trumps", "turmp", "dtrump", "usatrump", "bufoontrump", "donald",
  "president", "pr\u00e9sident", "us", "usa", "states", "thestates",
  "america*", "american", "americans", "americain*", "am\u00e9ricain*",
  "etatsunis", "sovereignty", "sovreignty", "souverain*", "annex",
  "annexation", "annexion", "51st", "independ*", "ind\u00e9pend*",
  "canadianidentity", "keepingcanada", "neighbours",
  "lintegriteterritoriale", "fronti\u00e8re*", "border",
  # Security / defence / international
  "security", "segurity", "scurit", "s\u00e9curit\u00e9", "defense", "defence",
  "d\u00e9fense", "military", "armes", "weapons", "war", "wars", "china",
  "israel", "palestin*", "gaza", "international", "relations", "global",
  "interference", "ing\u00e9rence", "g\u00e9opolitiques", "foreign",
  "foreignpolicy")))
ces25.foreign <- runDictionary(ces25, cps25_imp_iss, dict_foreign)
ces25$foreign_mip <- pull_count(ces25.foreign, "foreign")
ces25 <- ces25 %>% mutate(foreign.dum = ifelse(foreign_mip >= 1, 1, 0))


## =====================================================================
## 13. IMMIGRATION  (minority/discrimination moved to Socio_Cultural;
##     "foreign" removed -> Foreign_Affairs)
## =====================================================================
dict_immigration <- dictionary(list(immigration = c(
  "immigr*", "imigr*", "imagrat*", "immagr*", "immegr*", "immgr*",
  "inmigr*", "limmigration", "limigration", "dimmigrant", "emigration",
  "\u00e9migrat*", "illgale", "ill\u00e9gale", "refugee*",
  "r\u00e9fugi\u00e9s", "visa*")))
ces25.immigration <- runDictionary(ces25, cps25_imp_iss, dict_immigration)
ces25$immigration_mip <- pull_count(ces25.immigration, "immigration")
ces25 <- ces25 %>% mutate(immigration.dum = ifelse(immigration_mip >= 1, 1, 0))


## =====================================================================
## 14. SOCIO_CULTURAL  (Katie's women + race + indigenous, plus lgbtq,
##     rights, minority, identity, values per Simon)
## =====================================================================
dict_socio <- dictionary(list(socio_cultural = c(
  # women / abortion / gender
  "women*", "womens", "woman", "womans", "abort*", "unborn",
  "reproductive", "femme", "femmes", "gender", "maternity",
  # race
  "race", "racis*", "black", "white", "antisemitism", "islamophob*",
  # indigenous / reconciliation
  "indigenous", "indig*", "indeginous", "aboriginal", "autochtone*",
  "reconciliation", "reconcil*", "firstnation*", "native", "treaty", "trc",
  # identity / rights / values / lgbtq
  "lgbt*", "trans", "identity", "values", "equal*", "equity", "rights",
  "droit*", "freedom", "libert*", "minorit*", "discrimination", "divers*",
  "inclusiv*", "woke", "cultur*")))
ces25.socio <- runDictionary(ces25, cps25_imp_iss, dict_socio)
ces25$socio_cultural_mip <- pull_count(ces25.socio, "socio_cultural")
ces25 <- ces25 %>% mutate(socio_cultural.dum = ifelse(socio_cultural_mip >= 1, 1, 0))


## =====================================================================
## 15. SOCIAL_PROGRAMS  (Katie's welfare + seniors; "social*" added;
##     disability moved here from health)
## =====================================================================
dict_social <- dictionary(list(social = c(
  # social programs / welfare
  "social*", "socialservice*", "socialprogram*", "socialassistance",
  "aidesociale", "socialaid", "welfare", "childcare", "daycare", "dental",
  "child*", "enfant*", "family", "families", "famille*", "familiale",
  "parental", "basicincome", "redistribution", "poverty", "pauvr*", "poor",
  "homeless*", "disab*", "service*", "public", "benefits", "funding",
  "funds",
  # seniors
  "senior*", "senoir*", "pension*", "aines", "ain\u00e9*", "aine", "ainees",
  "elderly", "elder*", "oas", "cpp", "aging", "vieillesse",
  "vieillissement", "retirement", "retraite", "retir*", "retirees",
  "oldpeople", "65", "veteran*", "ei", "odsp")))
ces25.social <- runDictionary(ces25, cps25_imp_iss, dict_social)
ces25$social_mip <- pull_count(ces25.social, "social")
ces25 <- ces25 %>% mutate(social.dum = ifelse(social_mip >= 1, 1, 0))


## =====================================================================
## 16. BROKERAGE  (Katie's Quebec + federalism + "unity" per Matt)
## =====================================================================
dict_brokerage <- dictionary(list(brokerage = c(
  "quebec", "qubec", "qiebec", "qu\u00e9bec", "qu\u00e9b\u00e9cois",
  "francophone", "franco*", "francais", "fran\u00e7ais", "lacit", "laicit",
  "laicite", "loi21", "bill96", "federalis*", "f\u00e9d\u00e9ral*",
  "provinc*", "autonomie", "unity", "unit\u00e9", "bloc", "langue")))
ces25.brokerage <- runDictionary(ces25, cps25_imp_iss, dict_brokerage)
ces25$brokerage_mip <- pull_count(ces25.brokerage, "brokerage")
ces25 <- ces25 %>% mutate(brokerage.dum = ifelse(brokerage_mip >= 1, 1, 0))


## =====================================================================
## 17. FREE_TRADE  (Katie's tariff dict, per Matt: tariffs/trade/threat)
##     "economist/économiste" removed -> Economy
## =====================================================================
dict_free_trade <- dictionary(list(free_trade = c(
  "tariff*", "tarif*", "tarrif*", "tarriff*", "terrifs", "thariff",
  "taxestariffs", "economytariffs", "trade", "freetrade",
  "tradewar", "bigbeautifulbill")))
ces25.free_trade <- runDictionary(ces25, cps25_imp_iss, dict_free_trade)
ces25$free_trade_mip <- pull_count(ces25.free_trade, "free_trade")
ces25 <- ces25 %>% mutate(free_trade.dum = ifelse(free_trade_mip >= 1, 1, 0))


## =====================================================================
## 18. INFLATION  (pulled from Katie's Economy dictionary)
## =====================================================================
dict_inflation <- dictionary(list(inflation = c(
  "inflation", "costofliving", "cost", "co\u00fbt", "cout", "lecout",
  "living", "prices", "price", "prix", "expensive", "afford*",
  "afforability", "risingcost", "food")))
ces25.inflation <- runDictionary(ces25, cps25_imp_iss, dict_inflation)
ces25$inflation_mip <- pull_count(ces25.inflation, "inflation")
ces25 <- ces25 %>% mutate(inflation.dum = ifelse(inflation_mip >= 1, 1, 0))


## =====================================================================
## 19. HOUSING
## =====================================================================
dict_housing <- dictionary(list(housing = c(
  "housing", "house*", "home", "homes", "dwelling", "affordable",
  "unaffordable", "rent", "rents", "rental", "renting", "loyer", "maison*",
  "logement*", "sansabris", "itin\u00e9rance", "propri*")))
ces25.housing <- runDictionary(ces25, cps25_imp_iss, dict_housing)
ces25$housing_mip <- pull_count(ces25.housing, "housing")
ces25 <- ces25 %>% mutate(housing.dum = ifelse(housing_mip >= 1, 1, 0))


## =====================================================================
## 20. COVID19
## =====================================================================
dict_covid <- dictionary(list(covid = c(
  "covid", "pandem*", "pand\u00e9m*", "pand\u00e8*", "vaccin*",
  "vax*", "virus", "corona*", "lockdown*", "epidemic", "\u00e9pid\u00e9mie")))
ces25.covid <- runDictionary(ces25, cps25_imp_iss, dict_covid)
ces25$covid_mip <- pull_count(ces25.covid, "covid")
ces25 <- ces25 %>% mutate(covid.dum = ifelse(covid_mip >= 1, 1, 0))


## =====================================================================
## Non-answers (idk)
## =====================================================================
dict_idk <- dictionary(list(idk = c(
  "99", "unsure", "dontknow", "jenesaispas", "jaipasdereponse",
  "idk", "prefernot", "nothing", "no", "none", "na")))
ces25.idk <- runDictionary(ces25, cps25_imp_iss, dict_idk)
ces25$idk_mip <- pull_count(ces25.idk, "idk")
ces25 <- ces25 %>% mutate(idk.dum = ifelse(idk_mip >= 1, 1, 0))


## =====================================================================
## GUARD: nothing in any dictionary should still contain a space.
## A space here means the entry can never match a word-level token --
## exactly the failure Simon flagged. Run this after any edit.
## =====================================================================
all_dicts <- list(dict_enviro, dict_crime, dict_ethics, dict_education,
                  dict_energy, dict_jobs, dict_economy, dict_health,
                  dict_taxes, dict_debt, dict_democracy, dict_foreign,
                  dict_immigration, dict_socio, dict_social, dict_brokerage,
                  dict_free_trade, dict_inflation, dict_housing, dict_covid,
                  dict_idk)
leftover <- grep(" ", unlist(lapply(all_dicts, unlist)), value = TRUE)
if (length(leftover) > 0) {
  warning("Multi-word entries not compounded: ",
          paste(leftover, collapse = ", "))
}


## =====================================================================
## OTHER (2021 code 0) -- everything that fell through
## =====================================================================
cat_dums <- c("enviro.dum", "crime.dum", "ethics.dum", "education.dum",
              "energy.dum", "jobs.dum", "economy.dum", "health.dum",
              "taxes.dum", "debt.dum", "democracy.dum", "foreign.dum",
              "immigration.dum", "socio_cultural.dum", "social.dum",
              "brokerage.dum", "free_trade.dum", "inflation.dum",
              "housing.dum", "covid.dum")

ces25 <- ces25 %>%
  mutate(n_cats = rowSums(across(all_of(cat_dums))),
         other.dum = ifelse(n_cats == 0 & idk.dum == 0 &
                              !is.na(cps25_imp_iss) &
                              cps25_imp_iss != "", 1, 0))


## =====================================================================
## Summary of results (aligned to 2021 categories)
## =====================================================================
ces25 %>%
  tbl_summary(
    include = c(enviro.dum, crime.dum, ethics.dum, education.dum, energy.dum,
                jobs.dum, economy.dum, health.dum, taxes.dum, debt.dum,
                democracy.dum, foreign.dum, immigration.dum,
                socio_cultural.dum, social.dum, brokerage.dum, free_trade.dum,
                inflation.dum, housing.dum, covid.dum, idk.dum, other.dum),
    label = list(
      enviro.dum         ~ "Environment",
      crime.dum          ~ "Crime",
      ethics.dum         ~ "Ethics",
      education.dum      ~ "Education",
      energy.dum         ~ "Energy",
      jobs.dum           ~ "Jobs",
      economy.dum        ~ "Economy",
      health.dum         ~ "Health",
      taxes.dum          ~ "Taxes",
      debt.dum           ~ "Deficit / Debt",
      democracy.dum      ~ "Democracy",
      foreign.dum        ~ "Foreign Affairs",
      immigration.dum    ~ "Immigration",
      socio_cultural.dum ~ "Socio-Cultural",
      social.dum         ~ "Social Programs",
      brokerage.dum      ~ "Brokerage",
      free_trade.dum     ~ "Free Trade",
      inflation.dum      ~ "Inflation",
      housing.dum        ~ "Housing",
      covid.dum          ~ "COVID-19",
      idk.dum            ~ "Don't know / did not answer",
      other.dum          ~ "Other / uncategorized"))


## =====================================================================
## Validation
## =====================================================================
# Did the phrase compounding actually fire?
sum(stringr::str_detect(ces25$cps25_imp_iss, "socialprogram"), na.rm = TRUE)
sum(stringr::str_detect(ces25$cps25_imp_iss, "firstnation"), na.rm = TRUE)
sum(stringr::str_detect(ces25$cps25_imp_iss, "costofliving"), na.rm = TRUE)
sum(stringr::str_detect(ces25$cps25_imp_iss, "etatsunis"), na.rm = TRUE)

# Does tokens_lookup() handle spaced phrases natively? If this returns 1s,
# the compounding above is belt-and-braces rather than necessary -- and the
# original failures trace to the two-word swap line, not the tokenizer.
quanteda::dfm(quanteda::tokens_lookup(
  quanteda::tokens(c("social programs", "first nations")),
  dictionary(list(w = c("social programs", "first nations")))))

# Uncategorized responses, most frequent first -- the refinement step
ces25 %>%
  filter(other.dum == 1) %>%
  count(cps25_imp_iss, sort = TRUE) %>%
  print(n = 100)

# Spot-check any category
ces25 %>% filter(enviro.dum == 1) %>% select(cps25_imp_iss) %>% slice_sample(n = 10)
